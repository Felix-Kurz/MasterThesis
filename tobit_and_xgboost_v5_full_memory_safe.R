# Full-sample, memory-budgeted V5 model pipeline for the MACH XXL capsule.
#
# Statistical scope:
#   - Uses every person/row available in the 1994-2012 model window.
#   - Computes person and coworker features on the full BG population first.
#   - Matches to Lohnzettel only after those population features are complete.
#   - Fits the same main Tobit V1, XGBoost V1, and XGBoost V4 specifications.
#   - Runs the temporal and censored-only validation exercises.
#
# Memory design:
#   - 430 GiB application budget inside a roughly 512 GB XXL capsule.
#   - data.table by-reference feature construction.
#   - bounded CV sample for choosing nrounds; final XGBoost fit remains full-sample.
#   - histogram XGBoost, bounded threads, one dense matrix/model at a time.
#   - no full data copies embedded in model objects.
#   - plots use a bounded random sample; numerical summaries use all observations.
#   - checkpoints permit restart without retaining old model matrices in memory.

library(data.table)
library(survival)
library(xgboost)
library(ggplot2)

options(dplyr.summarise.inform = FALSE)

#########################################
# 0) CONFIG
#########################################

firstyear <- 1994L
lastyear <- 2012L

pre_model_file <- file.path(
  "pre_model_output_full_v4",
  "pre_model_full_history_full_v4.RData"
)
lohnzettel_file <- "lohnzettel_dta.RData"
output_dir <- "model_output_v5_full_memory_safe"
plots_dir <- file.path(output_dir, "plots")
checkpoint_dir <- file.path(output_dir, "checkpoints")

# Leave substantial room below the XXL capsule's nominal 512 GB allocation.
memory_budget_gib <- 430
xgb_native_headroom_gib <- 24
xgb_matrix_multiplier <- 3.5
tobit_matrix_multiplier <- 8

# CV selects nrounds on at most this many rows. The final fit uses every row.
xgb_cv_max_rows <- 1000000L
xgb_nfold <- 5L
xgb_nthread <- 16L
xgb_nrounds_max <- 1000L
xgb_early_stopping_rounds <- 50L

plot_sample_n <- 250000L
save_full_feature_table <- FALSE
make_plots <- TRUE
resume_from_checkpoints <- TRUE

censored_split_train_share <- 0.8
censored_split_seed <- 123L
model_seed <- 123L

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)

#########################################
# 1) MEMORY, TIMING, AND CHECKPOINT HELPERS
#########################################

gib <- function(bytes) bytes / 1024^3

rss_gib <- function() {
  status_file <- "/proc/self/status"
  if (!file.exists(status_file)) return(NA_real_)

  status <- readLines(status_file, warn = FALSE)
  rss_line <- grep("^VmRSS:", status, value = TRUE)
  if (length(rss_line) != 1L) return(NA_real_)

  rss_kib <- suppressWarnings(as.numeric(sub("^VmRSS:[[:space:]]*([0-9]+).*$", "\\1", rss_line)))
  rss_kib / 1024^2
}

r_heap_gib <- function(run_gc = TRUE) {
  g <- if (run_gc) gc(verbose = FALSE) else gc(verbose = FALSE, reset = FALSE)
  sum(g[, "(Mb)"], na.rm = TRUE) / 1024
}

memory_checkpoint <- function(stage, collect = TRUE) {
  if (collect) gc(verbose = FALSE)
  rss <- rss_gib()
  heap <- r_heap_gib(run_gc = FALSE)

  cat(sprintf(
    "\n[MEMORY] %-42s RSS=%s GiB | R heap=%.1f GiB | budget=%.0f GiB\n",
    stage,
    ifelse(is.finite(rss), sprintf("%.1f", rss), "unavailable"),
    heap,
    memory_budget_gib
  ))

  if (is.finite(rss) && rss > memory_budget_gib) {
    stop(
      "Memory budget exceeded at '", stage, "': ",
      round(rss, 1), " GiB RSS > ", memory_budget_gib, " GiB."
    )
  }

  invisible(rss)
}

preflight_xgb <- function(n_rows, n_features, stage) {
  rss <- memory_checkpoint(paste0(stage, " preflight"))
  matrix_gib <- gib(as.double(n_rows) * as.double(n_features) * 8)
  base_gib <- if (is.finite(rss)) rss else r_heap_gib()
  estimated_peak <- base_gib + xgb_matrix_multiplier * matrix_gib + xgb_native_headroom_gib

  cat(sprintf(
    "[PREFLIGHT] %s: rows=%s, features=%d, dense matrix=%.1f GiB, conservative peak=%.1f GiB\n",
    stage, format(n_rows, big.mark = ",", scientific = FALSE),
    n_features, matrix_gib, estimated_peak
  ))

  if (estimated_peak > memory_budget_gib) {
    stop(
      "Refusing to start ", stage, ". Conservative peak estimate is ",
      round(estimated_peak, 1), " GiB, above the ", memory_budget_gib,
      " GiB application budget. Reduce retained data or the model feature set first."
    )
  }

  invisible(estimated_peak)
}

preflight_tobit <- function(n_rows, n_features, stage) {
  rss <- memory_checkpoint(paste0(stage, " preflight"))
  design_gib <- gib(as.double(n_rows) * as.double(n_features + 3L) * 8)
  base_gib <- if (is.finite(rss)) rss else r_heap_gib()
  estimated_peak <- base_gib + tobit_matrix_multiplier * design_gib + 12

  cat(sprintf(
    "[PREFLIGHT] %s: design matrix=%.1f GiB, conservative peak=%.1f GiB\n",
    stage, design_gib, estimated_peak
  ))

  if (estimated_peak > memory_budget_gib) {
    stop(
      "Refusing to start ", stage, ". Conservative peak estimate is ",
      round(estimated_peak, 1), " GiB, above the ", memory_budget_gib, " GiB budget."
    )
  }

  invisible(estimated_peak)
}

model_runtime_log <- data.table(
  Stage = character(),
  Model = character(),
  Start_Time = as.POSIXct(character()),
  End_Time = as.POSIXct(character()),
  Runtime_Seconds = numeric(),
  Runtime_Minutes = numeric()
)

timed_model_run <- function(stage, model, expr) {
  start_time <- Sys.time()
  cat("\n=== TIMER START:", stage, "-", model, "===\n")
  result <- eval.parent(substitute(expr))
  end_time <- Sys.time()
  seconds <- as.numeric(difftime(end_time, start_time, units = "secs"))

  model_runtime_log <<- rbind(
    model_runtime_log,
    data.table(
      Stage = stage,
      Model = model,
      Start_Time = start_time,
      End_Time = end_time,
      Runtime_Seconds = seconds,
      Runtime_Minutes = seconds / 60
    )
  )

  cat(sprintf("=== TIMER END: %.2f hours ===\n", seconds / 3600))
  result
}

checkpoint_path <- function(name) file.path(checkpoint_dir, paste0(name, ".rds"))

read_valid_checkpoint <- function(name, expected_rows) {
  path <- checkpoint_path(name)
  if (!resume_from_checkpoints || !file.exists(path)) return(NULL)

  result <- readRDS(path)
  if (is.null(result$n_rows) || result$n_rows != expected_rows) {
    stop("Checkpoint row count does not match current data: ", path)
  }

  cat("Loaded checkpoint:", path, "\n")
  result
}

write_checkpoint <- function(name, result) {
  saveRDS(result, checkpoint_path(name), compress = FALSE)
  invisible(result)
}

#########################################
# 2) CONSTANTS AND INPUT VALIDATION
#########################################

missing_files <- c(pre_model_file, lohnzettel_file)[
  !file.exists(c(pre_model_file, lohnzettel_file))
]
if (length(missing_files) > 0L) {
  stop("Missing required input file(s): ", paste(missing_files, collapse = ", "))
}

required_pre_model_columns <- c(
  "penr", "benr", "contryear", "dailywage_final", "censored",
  "age", "exper", "tenure", "frau", "stsl_missing",
  "austrian_dummy", "germany_dummy", "eu_dummy",
  "educ_cat", "educ_missing", "educ_imputed",
  "whitecollar_dummy", "bluecollar_dummy", "othercollar_dummy",
  "subcat", "juj", "return", "year_entry", "age_entry",
  "live_birth_event", "n_children", "bundesland", "nace08_2"
)

required_lohnzettel_columns <- c(
  "penr", "benr", "year", "lz_ywage", "lz_dwage", "lz_golden_p67_2_6_10"
)

topcoding <- c(
  632, 686, 763, 850, 959, 1090, 1220, 1351, 1417, 1482, 1569, 1656,
  1744, 1787, 1874, 1918, 2005, 2049, 2092, 2180, 2311, 2441, 2616,
  2747, 2834, 2965, 3052, 3095, 3139, 3226, 3270, 3360, 3450, 3630,
  3750, 3840, 3930, 4020, 4110, 4200, 4230, 4440, 4530, 4650, 4860,
  4980, 5130, 5220, 5370
)

vpi_year <- 1986:2024
vpi_inflation <- c(
  0, 1.4, 2, 2.5, 3.3, 3.3, 4.1, 3.6, 3.0, 2.2, 1.9, 1.3, 0.9, 0.6,
  2.3, 2.7, 1.8, 1.3, 2.1, 2.3, 1.5, 2.2, 3.2, 0.5, 1.9, 3.3, 2.4,
  2.0, 1.7, 0.9, 0.9, 2.1, 2.0, 1.5, 1.4, 2.8, 8.6, 7.8, 2.9
)
vpi_reverse_index <- cumprod(vpi_inflation / 100 + 1)
cpi_2000_lookup <- vpi_reverse_index[vpi_year == 2000] / vpi_reverse_index
names(cpi_2000_lookup) <- as.character(vpi_year)

#########################################
# 3) LOAD AND TRIM FULL PRE-MODEL DATA
#########################################

loaded_pre_model_objects <- load(pre_model_file)
if (!("pre_model_full_history" %in% loaded_pre_model_objects)) {
  stop("Input file does not contain pre_model_full_history: ", pre_model_file)
}

missing_pre_model_columns <- setdiff(required_pre_model_columns, names(pre_model_full_history))
if (length(missing_pre_model_columns) > 0L) {
  stop("Missing pre-model column(s): ", paste(missing_pre_model_columns, collapse = ", "))
}

setDT(pre_model_full_history)
pre_model_rows_loaded <- nrow(pre_model_full_history)
pre_model_persons_loaded <- uniqueN(pre_model_full_history$penr)

# This is the only unavoidable large row-subset copy from the compressed RData input.
analysis_data <- pre_model_full_history[
  contryear >= firstyear & contryear <= lastyear,
  required_pre_model_columns,
  with = FALSE
]
rm(pre_model_full_history)
gc(verbose = FALSE)
memory_checkpoint("pre-model load and year trim")

# Conservative allowance for permanent feature columns plus grouping temporaries.
feature_build_gib <- gib(as.double(nrow(analysis_data)) * 22 * 8)
feature_build_base_gib <- rss_gib()
if (!is.finite(feature_build_base_gib)) feature_build_base_gib <- r_heap_gib()
feature_build_peak_gib <- feature_build_base_gib + 3 * feature_build_gib + 16
cat(sprintf(
  "[PREFLIGHT] feature construction conservative peak=%.1f GiB\n",
  feature_build_peak_gib
))
if (feature_build_peak_gib > memory_budget_gib) {
  stop(
    "Refusing feature construction: conservative peak estimate is ",
    round(feature_build_peak_gib, 1), " GiB, above the ", memory_budget_gib,
    " GiB application budget."
  )
}

if (nrow(analysis_data) == 0L) stop("No rows in the 1994-2012 model window.")

missing_years <- setdiff(firstyear:lastyear, sort(unique(analysis_data$contryear)))
if (length(missing_years) > 0L) {
  stop("Analysis data is missing year(s): ", paste(missing_years, collapse = ", "))
}

duplicate_row <- anyDuplicated(
  analysis_data,
  by = c("penr", "benr", "contryear")
)
if (duplicate_row > 0L) {
  stop("Duplicate penr-benr-contryear key at analysis row ", duplicate_row, ".")
}

if (any(!is.finite(analysis_data$dailywage_final))) {
  stop("analysis_data contains non-finite dailywage_final values.")
}

analysis_rows <- nrow(analysis_data)
sampled_penr <- unique(analysis_data$penr)
analysis_persons <- length(sampled_penr)

cat("Full input rows:", pre_model_rows_loaded, "\n")
cat("Full input persons:", pre_model_persons_loaded, "\n")
cat("Analysis rows 1994-2012:", analysis_rows, "\n")
cat("Analysis persons 1994-2012:", analysis_persons, "\n")

#########################################
# 4) FULL-POPULATION FEATURE CONSTRUCTION
#########################################

analysis_data[, `:=`(
  num_years = .N,
  mean_wage = mean(dailywage_final, na.rm = TRUE),
  mean_censored = mean(censored, na.rm = TRUE)
), by = penr]

multi_year <- analysis_data$num_years > 1L
analysis_data[
  multi_year,
  mean_wage := (mean_wage - dailywage_final / num_years) * (num_years / (num_years - 1))
]
analysis_data[
  multi_year,
  mean_censored := (mean_censored - censored / num_years) * (num_years / (num_years - 1))
]
rm(multi_year)
memory_checkpoint("person leave-one-out features")

analysis_data[, `:=`(
  firm_size = .N,
  mean_wage_coworkers = mean(dailywage_final, na.rm = TRUE),
  mean_censored_coworkers = mean(censored, na.rm = TRUE)
), by = .(benr, contryear)]

multi_person_firm <- analysis_data$firm_size > 1L
analysis_data[
  multi_person_firm,
  mean_wage_coworkers :=
    (mean_wage_coworkers - dailywage_final / firm_size) * (firm_size / (firm_size - 1))
]
analysis_data[
  multi_person_firm,
  mean_censored_coworkers :=
    (mean_censored_coworkers - censored / firm_size) * (firm_size / (firm_size - 1))
]
rm(multi_person_firm)
memory_checkpoint("firm leave-one-out features")

analysis_data[, cpi_2000 := unname(cpi_2000_lookup[as.character(contryear)])]
analysis_data[, topcode_threshold := topcoding[contryear - 1971L]]
analysis_data[, daily_topcode := topcode_threshold * 14 / 30]
analysis_data[, log_daily_topcode := log(daily_topcode * cpi_2000)]
analysis_data[, firm_size_square := firm_size^2 / 100]
analysis_data[, oneyear_dummy := as.integer(num_years == 1L)]
analysis_data[, oneperson_dummy := as.integer(firm_size == 1L)]
analysis_data[, smallfirm_dummy := as.integer(firm_size < 10L)]
analysis_data[, agegroup := 5L]
analysis_data[age <= 60, agegroup := 4L]
analysis_data[age <= 50, agegroup := 3L]
analysis_data[age <= 40, agegroup := 2L]
analysis_data[age <= 30, agegroup := 1L]
analysis_data[, bg_censored_flag := censored]
analysis_data[, interval_upper := dailywage_final]
analysis_data[censored == 1, interval_upper := log_daily_topcode]
analysis_data[, interval_lower := dailywage_final]
analysis_data[, c("cpi_2000", "topcode_threshold", "daily_topcode", "log_daily_topcode") := NULL]

if (save_full_feature_table) {
  model_data <- analysis_data
  save(model_data, file = file.path(output_dir, "model_data_v5.RData"), compress = FALSE)
  rm(model_data)
}
memory_checkpoint("full-population features complete")

#########################################
# 5) LOAD LZ, THEN INNER-MATCH
#########################################

loaded_lohnzettel_objects <- load(lohnzettel_file)
if (!("lohnzettel" %in% loaded_lohnzettel_objects)) {
  stop("Input file does not contain lohnzettel: ", lohnzettel_file)
}

missing_lz_columns <- setdiff(required_lohnzettel_columns, names(lohnzettel))
if (length(missing_lz_columns) > 0L) {
  stop("Missing Lohnzettel column(s): ", paste(missing_lz_columns, collapse = ", "))
}

setDT(lohnzettel)
lz_comparison_data <- lohnzettel[
  year >= firstyear & year <= lastyear & penr %in% sampled_penr,
  required_lohnzettel_columns,
  with = FALSE
]
rm(lohnzettel, sampled_penr)
gc(verbose = FALSE)

setnames(lz_comparison_data, "year", "contryear")
lz_comparison_data[, cpi_2000 := unname(cpi_2000_lookup[as.character(contryear)])]
lz_comparison_data[, lz_ywage_log := log(pmax(lz_ywage * cpi_2000, 1))]
lz_comparison_data[, lz_dwage_log := log(pmax(lz_dwage * cpi_2000, 1))]
lz_comparison_data[, lz_golden_log := log(pmax(lz_golden_p67_2_6_10 * cpi_2000 + 1, 1))]
lz_comparison_data <- lz_comparison_data[
  is.finite(lz_dwage_log),
  .(penr, benr, contryear, lz_ywage_log, lz_dwage_log, lz_golden_log)
]

lz_duplicate_row <- anyDuplicated(
  lz_comparison_data,
  by = c("penr", "benr", "contryear")
)
if (lz_duplicate_row > 0L) {
  stop("Duplicate penr-benr-contryear LZ key at row ", lz_duplicate_row, ".")
}

lz_rows <- nrow(lz_comparison_data)
lz_persons <- uniqueN(lz_comparison_data$penr)

# The inner join cannot have more rows than the unique LZ side. Estimate the
# output plus ordering/join workspace before asking data.table to build it.
analysis_bytes_per_row <- as.numeric(object.size(analysis_data)) / nrow(analysis_data)
join_output_gib <- gib(
  as.double(lz_rows) * (analysis_bytes_per_row + 3 * 8)
)
join_base_gib <- rss_gib()
if (!is.finite(join_base_gib)) join_base_gib <- r_heap_gib()
join_peak_gib <- join_base_gib + 1.8 * join_output_gib + 12
cat(sprintf("[PREFLIGHT] BG-LZ join conservative peak=%.1f GiB\n", join_peak_gib))
if (join_peak_gib > memory_budget_gib) {
  stop(
    "Refusing BG-LZ join: conservative peak estimate is ",
    round(join_peak_gib, 1), " GiB, above the ", memory_budget_gib,
    " GiB application budget."
  )
}

key_cols <- c("penr", "benr", "contryear")
setkeyv(analysis_data, key_cols)
setkeyv(lz_comparison_data, key_cols)
comparison_data <- analysis_data[
  lz_comparison_data,
  on = key_cols,
  nomatch = 0L
]
comparison_data[, bg_dwage_log := dailywage_final]

rm(analysis_data, lz_comparison_data)
gc(verbose = FALSE)
memory_checkpoint("BG-LZ inner match")

if (nrow(comparison_data) == 0L) stop("The BG-LZ comparison sample is empty.")
if (anyDuplicated(comparison_data, by = key_cols) > 0L) {
  stop("Duplicate key created by BG-LZ match.")
}

comparison_rows <- nrow(comparison_data)
comparison_persons <- uniqueN(comparison_data$penr)

sample_flow <- data.table(
  Stage = c(
    "Full pre-model input",
    "Analysis window 1994-2012",
    "LZ rows for analysis persons/window",
    "Matched BG-LZ model sample"
  ),
  Rows = c(pre_model_rows_loaded, analysis_rows, lz_rows, comparison_rows),
  Persons = c(pre_model_persons_loaded, analysis_persons, lz_persons, comparison_persons)
)
fwrite(sample_flow, file.path(output_dir, "sample_flow_v5.csv"))
print(sample_flow)

#########################################
# 6) MODEL SPECIFICATIONS AND MATRIX HELPERS
#########################################

feature_cols_v1 <- c(
  "age", "mean_wage", "mean_censored", "firm_size", "firm_size_square",
  "oneyear_dummy", "oneperson_dummy", "smallfirm_dummy",
  "mean_wage_coworkers", "mean_censored_coworkers"
)

feature_cols_v4 <- c(
  "age", "exper", "tenure", "mean_wage", "mean_censored", "firm_size",
  "firm_size_square", "oneyear_dummy", "oneperson_dummy", "smallfirm_dummy",
  "mean_wage_coworkers", "mean_censored_coworkers", "interval_lower",
  "interval_upper", "bg_censored_flag", "frau", "stsl_missing",
  "austrian_dummy", "germany_dummy", "eu_dummy", "educ_cat", "educ_missing",
  "educ_imputed", "whitecollar_dummy", "bluecollar_dummy", "othercollar_dummy",
  "subcat", "juj", "return", "year_entry", "age_entry", "num_years",
  "live_birth_event", "n_children", "contryear", "bundesland", "nace08_2"
)

all_feature_cols <- unique(c(feature_cols_v1, feature_cols_v4))
non_numeric_features <- all_feature_cols[
  !vapply(comparison_data[, all_feature_cols, with = FALSE], is.numeric, logical(1))
]
if (length(non_numeric_features) > 0L) {
  stop("Non-numeric model feature(s): ", paste(non_numeric_features, collapse = ", "))
}

if (any(!is.finite(comparison_data$interval_lower)) ||
    any(!is.finite(comparison_data$interval_upper))) {
  stop("Non-finite Tobit interval endpoints in comparison_data.")
}

prepare_matrix <- function(data, feature_cols, row_index = NULL) {
  selected <- if (is.null(row_index)) {
    data[, feature_cols, with = FALSE]
  } else {
    data[row_index, feature_cols, with = FALSE]
  }

  x <- as.matrix(selected)
  rm(selected)

  for (j in seq_len(ncol(x))) {
    bad <- !is.finite(x[, j])
    if (any(bad)) x[bad, j] <- 0
    rm(bad)
  }

  storage.mode(x) <- "double"
  x
}

make_dmatrix <- function(data, feature_cols, row_index = NULL, label = NULL) {
  x <- prepare_matrix(data, feature_cols, row_index)
  if (!is.null(label) && length(label) != nrow(x)) {
    stop(
      "DMatrix label mismatch: ", length(label), " labels for ", nrow(x),
      " matrix rows."
    )
  }
  # Older XGBoost builds treat an explicitly supplied label = NULL as a
  # zero-length label and reject prediction matrices. Omit the argument.
  dmat <- if (is.null(label)) {
    xgb.DMatrix(data = x)
  } else {
    xgb.DMatrix(data = x, label = label)
  }
  rm(x)
  gc(verbose = FALSE)
  dmat
}

xgb_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  tree_method = "hist",
  max_bin = 256,
  nthread = xgb_nthread,
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  reg_alpha = 0.1,
  reg_lambda = 1
)

cv_best_nrounds <- function(data, feature_cols, candidate_index, seed, stage) {
  set.seed(seed)
  cv_n <- min(length(candidate_index), xgb_cv_max_rows)
  cv_position <- if (length(candidate_index) > cv_n) {
    sample.int(length(candidate_index), cv_n)
  } else {
    seq_along(candidate_index)
  }
  cv_index <- candidate_index[cv_position]

  cat(stage, "CV rows:", length(cv_index), "\n")
  dcv <- make_dmatrix(
    data,
    feature_cols,
    row_index = cv_index,
    label = data$lz_dwage_log[cv_index]
  )

  set.seed(seed)
  cv <- xgb.cv(
    params = xgb_params,
    data = dcv,
    nrounds = xgb_nrounds_max,
    nfold = xgb_nfold,
    early_stopping_rounds = xgb_early_stopping_rounds,
    verbose = FALSE
  )

  best <- cv$best_iteration
  if (is.null(best) || !is.finite(best)) {
    best <- which.min(cv$evaluation_log$test_rmse_mean)
  }

  cv_log <- as.data.table(cv$evaluation_log)
  best_rmse <- min(cv_log$test_rmse_mean, na.rm = TRUE)
  rm(cv, dcv)
  gc(verbose = FALSE)
  memory_checkpoint(paste0(stage, " CV complete"))

  list(best_nrounds = as.integer(best), best_cv_rmse = best_rmse, cv_log = cv_log)
}

#########################################
# 7) FULL-SAMPLE SUMMARY HELPERS
#########################################

observation_groups <- function(censored) {
  list(
    All = rep(TRUE, length(censored)),
    Uncensored = censored == 0,
    Censored = censored == 1
  )
}

summarise_log_model <- function(truth, prediction, censored, method, evaluation_sample = NULL) {
  groups <- observation_groups(censored)
  result <- rbindlist(lapply(names(groups), function(group_name) {
    valid <- groups[[group_name]] & is.finite(truth) & is.finite(prediction)
    error <- prediction[valid] - truth[valid]
    data.table(
      Evaluation_Sample = evaluation_sample,
      Method = method,
      Target = "LZ (true wages)",
      Observation_Type = group_name,
      n_obs = length(error),
      RMSE = round(sqrt(mean(error^2)), 4),
      MAE = round(mean(abs(error)), 4),
      Mean_Error = round(mean(error), 4),
      SD_Error = round(sd(error), 4)
    )
  }))
  result
}

summarise_wage_model <- function(truth_log, prediction_log, censored, method,
                                 evaluation_sample = NULL) {
  groups <- observation_groups(censored)
  result <- rbindlist(lapply(names(groups), function(group_name) {
    valid <- groups[[group_name]] & is.finite(truth_log) & is.finite(prediction_log)
    true_wage <- exp(truth_log[valid])
    pred_wage <- exp(prediction_log[valid])
    finite <- is.finite(true_wage) & is.finite(pred_wage)
    true_wage <- true_wage[finite]
    pred_wage <- pred_wage[finite]
    error <- pred_wage - true_wage

    data.table(
      Evaluation_Sample = evaluation_sample,
      Method = method,
      Target = "LZ daily wages (level)",
      Observation_Type = group_name,
      n_obs = length(error),
      Wage_RMSE = round(sqrt(mean(error^2)), 2),
      Wage_MAE = round(mean(abs(error)), 2),
      Wage_Median_AE = round(median(abs(error)), 2),
      Mean_Error = round(mean(error), 2),
      SD_Error = round(sd(error), 2),
      Mean_True_Wage = round(mean(true_wage), 2),
      Median_True_Wage = round(median(true_wage), 2)
    )
  }))
  result
}

summarise_error_detail <- function(truth, prediction, censored, method,
                                   evaluation_sample = NULL) {
  groups <- observation_groups(censored)
  result <- rbindlist(lapply(names(groups), function(group_name) {
    valid <- groups[[group_name]] & is.finite(truth) & is.finite(prediction)
    error <- prediction[valid] - truth[valid]
    data.table(
      Evaluation_Sample = evaluation_sample,
      method = method,
      Observation_Type = group_name,
      n_obs = length(error),
      mean_error = round(mean(error), 4),
      median_error = round(median(error), 4),
      sd_error = round(sd(error), 4),
      mae = round(mean(abs(error)), 4),
      rmse = round(sqrt(mean(error^2)), 4)
    )
  }))
  result
}

sample_positions <- function(n, max_n, seed) {
  if (n <= max_n) return(seq_len(n))
  set.seed(seed)
  sort(sample.int(n, max_n))
}

save_density_plot <- function(plot_data, value_col, title, x_label, out_path,
                              x_limits = NULL) {
  p <- ggplot(plot_data, aes_string(x = value_col, color = "method", fill = "method")) +
    geom_density(alpha = 0.18, size = 0.9) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(title = title, x = x_label, y = "Density", color = "Method", fill = "Method")
  if (!is.null(x_limits)) p <- p + coord_cartesian(xlim = x_limits)
  ggsave(out_path, plot = p, width = 11, height = 6.5)
}

#########################################
# 8) MODEL FIT HELPERS
#########################################

apply_tobit_imputation <- function(data, linear_prediction, sigma_hat, seed) {
  output <- linear_prediction
  censored_index <- which(data$bg_censored_flag == 1)
  if (length(censored_index) == 0L) return(output)

  set.seed(seed)
  k <- pnorm(
    (data$dailywage_final[censored_index] - linear_prediction[censored_index]) / sigma_hat
  )
  probability <- k + runif(length(k)) * (1 - k)
  probability <- pmin(pmax(probability, .Machine$double.eps), 1 - .Machine$double.eps)
  output[censored_index] <- linear_prediction[censored_index] + sigma_hat * qnorm(probability)
  output
}

run_tobit_full <- function(data, feature_cols, model_name, seed) {
  preflight_tobit(nrow(data), length(feature_cols), model_name)
  formula_string <- paste(
    "survival::Surv(interval_lower, interval_upper, type = 'interval2') ~",
    paste(feature_cols, collapse = " + ")
  )

  model <- survival::survreg(
    as.formula(formula_string),
    data = data,
    dist = "gaussian",
    model = FALSE,
    x = FALSE,
    y = FALSE
  )
  linear_prediction <- predict(model, newdata = data, type = "response")
  prediction <- apply_tobit_imputation(data, linear_prediction, model$scale, seed)
  rm(linear_prediction)
  gc(verbose = FALSE)

  list(
    n_rows = nrow(data),
    model_name = model_name,
    feature_cols = feature_cols,
    formula = formula_string,
    model = model,
    sigma_hat = model$scale,
    predictions = prediction,
    summary = summarise_log_model(
      data$lz_dwage_log, prediction, data$bg_censored_flag, model_name
    )
  )
}

run_xgb_full <- function(data, feature_cols, model_name, seed) {
  preflight_xgb(nrow(data), length(feature_cols), model_name)
  all_index <- seq_len(nrow(data))
  cv_result <- cv_best_nrounds(data, feature_cols, all_index, seed, model_name)

  dtrain <- make_dmatrix(
    data,
    feature_cols,
    label = data$lz_dwage_log
  )
  memory_checkpoint(paste0(model_name, " DMatrix ready"))

  set.seed(seed)
  model <- xgb.train(
    params = xgb_params,
    data = dtrain,
    nrounds = cv_result$best_nrounds,
    watchlist = list(train = dtrain),
    verbose = FALSE
  )
  prediction <- predict(model, dtrain)
  importance <- xgb.importance(feature_names = feature_cols, model = model)

  rm(dtrain, all_index)
  gc(verbose = FALSE)
  memory_checkpoint(paste0(model_name, " fit complete"))

  list(
    n_rows = nrow(data),
    model_name = model_name,
    feature_cols = feature_cols,
    model = model,
    best_nrounds = cv_result$best_nrounds,
    best_cv_rmse = cv_result$best_cv_rmse,
    cv_log = cv_result$cv_log,
    predictions = prediction,
    importance = importance,
    summary = summarise_log_model(
      data$lz_dwage_log, prediction, data$bg_censored_flag, model_name
    )
  )
}

run_tobit_split <- function(data, train_index, test_index, feature_cols,
                            model_name, seed, train_label, test_label) {
  preflight_tobit(length(train_index), length(feature_cols), model_name)
  formula_string <- paste(
    "survival::Surv(interval_lower, interval_upper, type = 'interval2') ~",
    paste(feature_cols, collapse = " + ")
  )

  model <- survival::survreg(
    as.formula(formula_string),
    data = data[train_index],
    dist = "gaussian",
    model = FALSE,
    x = FALSE,
    y = FALSE
  )

  train_linear <- predict(model, newdata = data[train_index], type = "response")
  test_linear <- predict(model, newdata = data[test_index], type = "response")
  train_prediction <- apply_tobit_imputation(data[train_index], train_linear, model$scale, seed)
  test_prediction <- apply_tobit_imputation(data[test_index], test_linear, model$scale, seed + 1L)
  rm(train_linear, test_linear)
  gc(verbose = FALSE)

  summary <- rbind(
    summarise_log_model(
      data$lz_dwage_log[train_index], train_prediction,
      data$bg_censored_flag[train_index], model_name, train_label
    ),
    summarise_log_model(
      data$lz_dwage_log[test_index], test_prediction,
      data$bg_censored_flag[test_index], model_name, test_label
    )
  )

  list(
    n_rows = nrow(data),
    model_name = model_name,
    feature_cols = feature_cols,
    formula = formula_string,
    model = model,
    sigma_hat = model$scale,
    train_predictions = train_prediction,
    test_predictions = test_prediction,
    summary = summary
  )
}

run_xgb_split <- function(data, train_index, test_index, feature_cols,
                          model_name, seed, train_label, test_label) {
  preflight_xgb(length(train_index), length(feature_cols), model_name)
  cv_result <- cv_best_nrounds(data, feature_cols, train_index, seed, model_name)

  dtrain <- make_dmatrix(
    data,
    feature_cols,
    row_index = train_index,
    label = data$lz_dwage_log[train_index]
  )
  memory_checkpoint(paste0(model_name, " train DMatrix ready"))

  set.seed(seed)
  model <- xgb.train(
    params = xgb_params,
    data = dtrain,
    nrounds = cv_result$best_nrounds,
    watchlist = list(train = dtrain),
    verbose = FALSE
  )
  train_prediction <- predict(model, dtrain)
  rm(dtrain)
  gc(verbose = FALSE)

  dtest <- make_dmatrix(data, feature_cols, row_index = test_index)
  test_prediction <- predict(model, dtest)
  rm(dtest)
  gc(verbose = FALSE)

  importance <- xgb.importance(feature_names = feature_cols, model = model)
  memory_checkpoint(paste0(model_name, " split fit complete"))

  summary <- rbind(
    summarise_log_model(
      data$lz_dwage_log[train_index], train_prediction,
      data$bg_censored_flag[train_index], model_name, train_label
    ),
    summarise_log_model(
      data$lz_dwage_log[test_index], test_prediction,
      data$bg_censored_flag[test_index], model_name, test_label
    )
  )

  list(
    n_rows = nrow(data),
    model_name = model_name,
    feature_cols = feature_cols,
    model = model,
    best_nrounds = cv_result$best_nrounds,
    best_cv_rmse = cv_result$best_cv_rmse,
    cv_log = cv_result$cv_log,
    train_predictions = train_prediction,
    test_predictions = test_prediction,
    importance = importance,
    summary = summary
  )
}

#########################################
# 9) MAIN FULL-SAMPLE MODELS, SEQUENTIALLY
#########################################

tobit_v1 <- read_valid_checkpoint("main_tobit_v1", comparison_rows)
if (is.null(tobit_v1)) {
  tobit_v1 <- timed_model_run(
    "In-sample common comparison", "Tobit V1",
    run_tobit_full(comparison_data, feature_cols_v1, "Tobit V1", model_seed)
  )
  write_checkpoint("main_tobit_v1", tobit_v1)
}
set(comparison_data, j = "tobit_v1_prediction", value = tobit_v1$predictions)
tobit_v1$predictions <- NULL
memory_checkpoint("Tobit V1 retained as model + one prediction column")

xgb_v1 <- read_valid_checkpoint("main_xgb_v1", comparison_rows)
if (is.null(xgb_v1)) {
  xgb_v1 <- timed_model_run(
    "In-sample common comparison", "XGBoost V1",
    run_xgb_full(comparison_data, feature_cols_v1, "XGBoost V1", model_seed)
  )
  write_checkpoint("main_xgb_v1", xgb_v1)
}
set(comparison_data, j = "xgb_v1_prediction", value = xgb_v1$predictions)
xgb_v1$predictions <- NULL
memory_checkpoint("XGBoost V1 retained as model + one prediction column")

xgb_v4 <- read_valid_checkpoint("main_xgb_v4", comparison_rows)
if (is.null(xgb_v4)) {
  xgb_v4 <- timed_model_run(
    "In-sample common comparison", "XGBoost V4",
    run_xgb_full(comparison_data, feature_cols_v4, "XGBoost V4", model_seed)
  )
  write_checkpoint("main_xgb_v4", xgb_v4)
}
set(comparison_data, j = "xgb_v4_prediction", value = xgb_v4$predictions)
xgb_v4$predictions <- NULL
memory_checkpoint("all main models complete")

#########################################
# 10) MAIN FULL-SAMPLE OUTPUTS
#########################################

main_predictions <- list(
  "BG Baseline" = comparison_data$bg_dwage_log,
  "Tobit V1" = comparison_data$tobit_v1_prediction,
  "XGBoost V1" = comparison_data$xgb_v1_prediction,
  "XGBoost V4" = comparison_data$xgb_v4_prediction
)

comprehensive_rmse_table <- rbindlist(lapply(names(main_predictions), function(method) {
  summarise_log_model(
    comparison_data$lz_dwage_log,
    main_predictions[[method]],
    comparison_data$bg_censored_flag,
    method
  )
}))
setorder(comprehensive_rmse_table, Observation_Type, RMSE)
fwrite(comprehensive_rmse_table, file.path(output_dir, "comprehensive_rmse_comparison_v5.csv"))

comprehensive_wage_level_table <- rbindlist(lapply(names(main_predictions), function(method) {
  summarise_wage_model(
    comparison_data$lz_dwage_log,
    main_predictions[[method]],
    comparison_data$bg_censored_flag,
    method
  )
}))
setorder(comprehensive_wage_level_table, Observation_Type, Wage_RMSE)
fwrite(comprehensive_wage_level_table, file.path(output_dir, "comprehensive_wage_level_error_v5.csv"))

corrected_error_summary_with_bg <- rbindlist(lapply(names(main_predictions), function(method) {
  summarise_error_detail(
    comparison_data$lz_dwage_log,
    main_predictions[[method]],
    comparison_data$bg_censored_flag,
    method
  )
}))
setorder(corrected_error_summary_with_bg, Observation_Type, rmse)
fwrite(
  corrected_error_summary_with_bg,
  file.path(output_dir, "corrected_error_summary_with_bg_baseline_v5.csv")
)

print(comprehensive_rmse_table)
print(comprehensive_wage_level_table)

save(
  tobit_v1, xgb_v1, xgb_v4,
  comprehensive_rmse_table,
  comprehensive_wage_level_table,
  corrected_error_summary_with_bg,
  file = file.path(output_dir, "model_objects_v5.RData"),
  compress = FALSE
)

fwrite(as.data.table(xgb_v1$importance), file.path(output_dir, "xgb_v1_importance_v5.csv"))
fwrite(as.data.table(xgb_v4$importance), file.path(output_dir, "xgb_v4_importance_v5.csv"))

if (make_plots) {
  plot_position <- sample_positions(comparison_rows, plot_sample_n, model_seed)
  truth_plot <- comparison_data$lz_dwage_log[plot_position]
  censored_plot <- comparison_data$bg_censored_flag[plot_position]

  error_plot_data <- rbindlist(lapply(names(main_predictions), function(method) {
    data.table(
      method = method,
      error = main_predictions[[method]][plot_position] - truth_plot,
      censored = censored_plot
    )
  }))
  save_density_plot(
    error_plot_data,
    "error",
    "Prediction Error Distribution (bounded plot sample)",
    "Prediction error (ln wage units)",
    file.path(plots_dir, "error_density_all.png"),
    c(-2, 2)
  )

  wage_plot_data <- rbindlist(c(
    list(data.table(method = "LZ True Wage", predicted_wage = truth_plot, censored = censored_plot)),
    lapply(names(main_predictions), function(method) {
      data.table(
        method = method,
        predicted_wage = main_predictions[[method]][plot_position],
        censored = censored_plot
      )
    })
  ))
  save_density_plot(
    wage_plot_data,
    "predicted_wage",
    "Predicted Wage Distribution (bounded plot sample)",
    "ln daily wage",
    file.path(plots_dir, "predicted_wage_density_full_all.png"),
    c(0, 7)
  )

  rm(plot_position, truth_plot, censored_plot, error_plot_data, wage_plot_data)
  gc(verbose = FALSE)
}

rm(main_predictions)
memory_checkpoint("main outputs complete")

#########################################
# 11) TEMPORAL VALIDATION: 1994-2011 -> 2012
#########################################

temporal_train_index <- which(comparison_data$contryear >= firstyear &
                                comparison_data$contryear <= 2011L)
temporal_test_index <- which(comparison_data$contryear == 2012L)
if (length(temporal_train_index) == 0L || length(temporal_test_index) == 0L) {
  stop("Temporal split produced an empty train or test sample.")
}

temporal_tobit_v1 <- read_valid_checkpoint("temporal_tobit_v1", comparison_rows)
if (is.null(temporal_tobit_v1)) {
  temporal_tobit_v1 <- timed_model_run(
    "Temporal validation", "Tobit V1",
    run_tobit_split(
      comparison_data, temporal_train_index, temporal_test_index,
      feature_cols_v1, "Tobit V1 Temporal", model_seed,
      "Train 1994-2011", "Test 2012"
    )
  )
  write_checkpoint("temporal_tobit_v1", temporal_tobit_v1)
}

temporal_xgb_v1 <- read_valid_checkpoint("temporal_xgb_v1", comparison_rows)
if (is.null(temporal_xgb_v1)) {
  temporal_xgb_v1 <- timed_model_run(
    "Temporal validation", "XGBoost V1",
    run_xgb_split(
      comparison_data, temporal_train_index, temporal_test_index,
      feature_cols_v1, "XGBoost V1 Temporal", model_seed,
      "Train 1994-2011", "Test 2012"
    )
  )
  write_checkpoint("temporal_xgb_v1", temporal_xgb_v1)
}

temporal_xgb_v4 <- read_valid_checkpoint("temporal_xgb_v4", comparison_rows)
if (is.null(temporal_xgb_v4)) {
  temporal_xgb_v4 <- timed_model_run(
    "Temporal validation", "XGBoost V4",
    run_xgb_split(
      comparison_data, temporal_train_index, temporal_test_index,
      feature_cols_v4, "XGBoost V4 Temporal", model_seed,
      "Train 1994-2011", "Test 2012"
    )
  )
  write_checkpoint("temporal_xgb_v4", temporal_xgb_v4)
}
memory_checkpoint("temporal models complete")

temporal_models <- list(
  "Tobit V1 Temporal" = temporal_tobit_v1,
  "XGBoost V1 Temporal" = temporal_xgb_v1,
  "XGBoost V4 Temporal" = temporal_xgb_v4
)

temporal_rmse_table <- rbind(
  summarise_log_model(
    comparison_data$lz_dwage_log[temporal_train_index],
    comparison_data$bg_dwage_log[temporal_train_index],
    comparison_data$bg_censored_flag[temporal_train_index],
    "BG Baseline Temporal", "Train 1994-2011"
  ),
  summarise_log_model(
    comparison_data$lz_dwage_log[temporal_test_index],
    comparison_data$bg_dwage_log[temporal_test_index],
    comparison_data$bg_censored_flag[temporal_test_index],
    "BG Baseline Temporal", "Test 2012"
  ),
  rbindlist(lapply(temporal_models, function(result) result$summary))
)
setorder(temporal_rmse_table, Evaluation_Sample, Observation_Type, RMSE)
fwrite(temporal_rmse_table, file.path(output_dir, "temporal_split_rmse_v5.csv"))

temporal_wage_level_table <- rbind(
  summarise_wage_model(
    comparison_data$lz_dwage_log[temporal_train_index],
    comparison_data$bg_dwage_log[temporal_train_index],
    comparison_data$bg_censored_flag[temporal_train_index],
    "BG Baseline Temporal", "Train 1994-2011"
  ),
  summarise_wage_model(
    comparison_data$lz_dwage_log[temporal_test_index],
    comparison_data$bg_dwage_log[temporal_test_index],
    comparison_data$bg_censored_flag[temporal_test_index],
    "BG Baseline Temporal", "Test 2012"
  ),
  rbindlist(lapply(temporal_models, function(result) {
    rbind(
      summarise_wage_model(
        comparison_data$lz_dwage_log[temporal_train_index], result$train_predictions,
        comparison_data$bg_censored_flag[temporal_train_index], result$model_name,
        "Train 1994-2011"
      ),
      summarise_wage_model(
        comparison_data$lz_dwage_log[temporal_test_index], result$test_predictions,
        comparison_data$bg_censored_flag[temporal_test_index], result$model_name,
        "Test 2012"
      )
    )
  }))
)
setorder(temporal_wage_level_table, Evaluation_Sample, Observation_Type, Wage_RMSE)
fwrite(temporal_wage_level_table, file.path(output_dir, "temporal_split_wage_level_error_v5.csv"))

for (name in names(temporal_models)) {
  temporal_models[[name]]$train_predictions <- NULL
  temporal_models[[name]]$test_predictions <- NULL
}
temporal_tobit_v1_compact <- temporal_models[["Tobit V1 Temporal"]]
temporal_xgb_v1_compact <- temporal_models[["XGBoost V1 Temporal"]]
temporal_xgb_v4_compact <- temporal_models[["XGBoost V4 Temporal"]]
save(
  temporal_tobit_v1_compact,
  temporal_xgb_v1_compact,
  temporal_xgb_v4_compact,
  temporal_rmse_table,
  temporal_wage_level_table,
  file = file.path(output_dir, "temporal_split_models_v5.RData"),
  compress = FALSE
)

rm(
  temporal_models, temporal_tobit_v1, temporal_xgb_v1, temporal_xgb_v4,
  temporal_tobit_v1_compact, temporal_xgb_v1_compact, temporal_xgb_v4_compact,
  temporal_train_index, temporal_test_index
)
gc(verbose = FALSE)
memory_checkpoint("temporal outputs released")

#########################################
# 12) CENSORED-ONLY PERSON-LEVEL SPLIT
#########################################

censored_index <- which(comparison_data$bg_censored_flag == 1)
if (length(censored_index) == 0L) stop("No censored observations available.")

censored_persons <- unique(comparison_data$penr[censored_index])
if (length(censored_persons) < 2L) stop("Need at least two censored persons.")

set.seed(censored_split_seed)
n_train_persons <- floor(censored_split_train_share * length(censored_persons))
n_train_persons <- max(1L, min(n_train_persons, length(censored_persons) - 1L))
censored_train_persons <- sample(censored_persons, n_train_persons)
is_train <- comparison_data$penr[censored_index] %in% censored_train_persons
censored_train_index <- censored_index[is_train]
censored_test_index <- censored_index[!is_train]
rm(censored_persons, censored_train_persons, censored_index, is_train)
gc(verbose = FALSE)

censored_xgb_v1 <- read_valid_checkpoint("censored_xgb_v1", comparison_rows)
if (is.null(censored_xgb_v1)) {
  censored_xgb_v1 <- timed_model_run(
    "Censored-only validation", "XGBoost V1",
    run_xgb_split(
      comparison_data, censored_train_index, censored_test_index,
      feature_cols_v1, "XGBoost V1 Censored-Only", model_seed,
      "Censored train", "Censored test"
    )
  )
  write_checkpoint("censored_xgb_v1", censored_xgb_v1)
}

censored_xgb_v4 <- read_valid_checkpoint("censored_xgb_v4", comparison_rows)
if (is.null(censored_xgb_v4)) {
  censored_xgb_v4 <- timed_model_run(
    "Censored-only validation", "XGBoost V4",
    run_xgb_split(
      comparison_data, censored_train_index, censored_test_index,
      feature_cols_v4, "XGBoost V4 Censored-Only", model_seed,
      "Censored train", "Censored test"
    )
  )
  write_checkpoint("censored_xgb_v4", censored_xgb_v4)
}
memory_checkpoint("censored-only models complete")

censored_models <- list(
  "XGBoost V1 Censored-Only" = censored_xgb_v1,
  "XGBoost V4 Censored-Only" = censored_xgb_v4
)

censored_rmse_table <- rbind(
  summarise_log_model(
    comparison_data$lz_dwage_log[censored_train_index],
    comparison_data$bg_dwage_log[censored_train_index],
    comparison_data$bg_censored_flag[censored_train_index],
    "BG Baseline Censored-Only", "Censored train"
  ),
  summarise_log_model(
    comparison_data$lz_dwage_log[censored_test_index],
    comparison_data$bg_dwage_log[censored_test_index],
    comparison_data$bg_censored_flag[censored_test_index],
    "BG Baseline Censored-Only", "Censored test"
  ),
  rbindlist(lapply(censored_models, function(result) result$summary))
)
censored_rmse_table <- censored_rmse_table[n_obs > 0L]
setorder(censored_rmse_table, Evaluation_Sample, Observation_Type, RMSE)
fwrite(censored_rmse_table, file.path(output_dir, "censored_only_xgb_train_test_rmse_v5.csv"))

censored_wage_level_table <- rbind(
  summarise_wage_model(
    comparison_data$lz_dwage_log[censored_train_index],
    comparison_data$bg_dwage_log[censored_train_index],
    comparison_data$bg_censored_flag[censored_train_index],
    "BG Baseline Censored-Only", "Censored train"
  ),
  summarise_wage_model(
    comparison_data$lz_dwage_log[censored_test_index],
    comparison_data$bg_dwage_log[censored_test_index],
    comparison_data$bg_censored_flag[censored_test_index],
    "BG Baseline Censored-Only", "Censored test"
  ),
  rbindlist(lapply(censored_models, function(result) {
    rbind(
      summarise_wage_model(
        comparison_data$lz_dwage_log[censored_train_index], result$train_predictions,
        comparison_data$bg_censored_flag[censored_train_index], result$model_name,
        "Censored train"
      ),
      summarise_wage_model(
        comparison_data$lz_dwage_log[censored_test_index], result$test_predictions,
        comparison_data$bg_censored_flag[censored_test_index], result$model_name,
        "Censored test"
      )
    )
  }))
)
censored_wage_level_table <- censored_wage_level_table[n_obs > 0L]
setorder(censored_wage_level_table, Evaluation_Sample, Observation_Type, Wage_RMSE)
fwrite(
  censored_wage_level_table,
  file.path(output_dir, "censored_only_xgb_train_test_wage_level_error_v5.csv")
)

censored_error_summary <- rbindlist(lapply(names(censored_models), function(name) {
  result <- censored_models[[name]]
  rbind(
    summarise_error_detail(
      comparison_data$lz_dwage_log[censored_train_index], result$train_predictions,
      comparison_data$bg_censored_flag[censored_train_index], name, "Censored train"
    ),
    summarise_error_detail(
      comparison_data$lz_dwage_log[censored_test_index], result$test_predictions,
      comparison_data$bg_censored_flag[censored_test_index], name, "Censored test"
    )
  )
}))
censored_error_summary <- censored_error_summary[
  Observation_Type == "Censored" & n_obs > 0L
]
setorder(censored_error_summary, Evaluation_Sample, rmse)
fwrite(
  censored_error_summary,
  file.path(output_dir, "censored_only_xgb_train_test_error_summary_v5.csv")
)

for (name in names(censored_models)) {
  censored_models[[name]]$train_predictions <- NULL
  censored_models[[name]]$test_predictions <- NULL
}
censored_xgb_v1_compact <- censored_models[["XGBoost V1 Censored-Only"]]
censored_xgb_v4_compact <- censored_models[["XGBoost V4 Censored-Only"]]
save(
  censored_xgb_v1_compact,
  censored_xgb_v4_compact,
  censored_rmse_table,
  censored_wage_level_table,
  censored_error_summary,
  file = file.path(output_dir, "censored_only_xgb_train_test_models_v5.RData"),
  compress = FALSE
)

rm(
  censored_models, censored_xgb_v1, censored_xgb_v4,
  censored_xgb_v1_compact, censored_xgb_v4_compact,
  censored_train_index, censored_test_index
)
gc(verbose = FALSE)
memory_checkpoint("censored-only outputs released")

#########################################
# 13) PREDICTION BUNDLE AND FINAL LOG
#########################################

predict_true_wage_v5 <- function(bg_data, model_obj, feature_columns = model_obj$feature_cols) {
  local_data <- as.data.table(bg_data)
  prediction_matrix <- prepare_matrix(local_data, feature_columns)
  dnew <- xgb.DMatrix(prediction_matrix)
  rm(prediction_matrix)
  on.exit({
    rm(dnew)
    gc(verbose = FALSE)
  }, add = TRUE)
  predict(model_obj$model, dnew)
}

prediction_bundle <- list(
  predict_true_wage_v5 = predict_true_wage_v5,
  latest_model = xgb_v4,
  feature_columns = xgb_v4$feature_cols
)
save(
  prediction_bundle,
  file = file.path(output_dir, "predict_true_wage_v5_function.RData"),
  compress = FALSE
)

fwrite(model_runtime_log, file.path(output_dir, "model_runtime_v5.csv"))
memory_checkpoint("pipeline complete")

cat("\n=== FULL-SAMPLE MEMORY-SAFE V5 PIPELINE COMPLETE ===\n")
cat("Application memory budget:", memory_budget_gib, "GiB\n")
cat("CV rows per XGBoost fit: at most", xgb_cv_max_rows, "\n")
cat("Final XGBoost fits used their complete training samples.\n")

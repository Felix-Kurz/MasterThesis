# V6 in-sample Tobit V1 and XGBoost V4 on the complete full-history pre-model dataset.
# The model comparison itself remains restricted to the 1994-2012
# Lohnzettel/model window.

library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(survival)
library(purrr)
library(data.table)
library(stringr)
library(ggplot2)
library(scales)
library(xgboost)

options(dplyr.summarise.inform = FALSE)

model_runtime_log <- tibble(
  Stage = character(),
  Model = character(),
  Start_Time = as.POSIXct(character()),
  End_Time = as.POSIXct(character()),
  Runtime_Seconds = numeric(),
  Runtime_Minutes = numeric()
)

format_elapsed <- function(seconds) {
  sprintf(
    "%.2f seconds / %.2f minutes / %.2f hours",
    seconds,
    seconds / 60,
    seconds / 3600
  )
}

timed_model_run <- function(stage, model, expr) {
  start_time <- Sys.time()
  cat("\n=== TIMER START:", stage, "-", model, "===\n")
  cat("Start:", format(start_time), "\n")

  result <- eval.parent(substitute(expr))

  end_time <- Sys.time()
  runtime_seconds <- as.numeric(difftime(end_time, start_time, units = "secs"))

  cat("End:", format(end_time), "\n")
  cat("Runtime:", format_elapsed(runtime_seconds), "\n")
  cat("=== TIMER END:", stage, "-", model, "===\n")

  model_runtime_log <<- bind_rows(
    model_runtime_log,
    tibble(
      Stage = stage,
      Model = model,
      Start_Time = start_time,
      End_Time = end_time,
      Runtime_Seconds = runtime_seconds,
      Runtime_Minutes = runtime_seconds / 60
    )
  )

  attr(result, "runtime_seconds") <- runtime_seconds
  result
}

#########################################
# FILE OVERVIEW
#########################################
# Purpose:
#   Compare BG baseline, Tobit V1, and XGBoost V4 predictions against Lohnzettel
#   daily wages on the common BG-LZ comparison sample.
#
# Main benchmark:
#   In-sample comparison on the same common sample for direct comparability
#   with the Card-style Tobit benchmark.
#
# Training:
#   XGBoost V4 uses five-fold CV (up to 1000 rounds, early stopping after 50)
#   to choose the round count, then fits the final model on the full common sample.
#   Built-in evaluation output is printed every round in both stages.
#   No temporal validation or censored-only training experiment is run.
#
# Capsule launch (tcsh, inside the 64-CPU capsule's runtime environment):
#   setenv OMP_NUM_THREADS 4
#   printenv OMP_NUM_THREADS
#   rstudio
#   Start a fresh RStudio/R process from this terminal so it inherits the setting.
#   In R, verify with Sys.getenv("OMP_NUM_THREADS"); expected value: "4".
#   XGBoost also explicitly requests 4 threads through xgb_params below.
#
# Main inputs:
#   pre_model_output_full_v4/
#     pre_model_full_history_full_v4.RData
#   lohnzettel_dta.RData
#
# Main outputs:
#   model_output_v6_4threads_only_insamplev4_full_sample/
#     sample_flow_v6.csv
#     model_data_v6.RData
#     comprehensive_rmse_comparison_v6.csv
#     comprehensive_wage_level_error_v6.csv
#     corrected_error_summary_with_bg_baseline_v6.csv
#     model_runtime_v6.csv
#     model_objects_v6.RData
#     predict_true_wage_v6_function.RData
#     plots/*.png

#########################################
# 0) CONFIG
#########################################

firstyear <- 1994
lastyear  <- 2012

# Keep FALSE for the intended full-sample run. The debug branch is retained
# unchanged from V2 so a small diagnostic run remains possible if needed.
use_debug_sample <- FALSE
sample_n_penr <- 10000
sample_seed <- 123

# Centralized full-sample input and isolated output paths:
pre_model_file <- file.path(
  "pre_model_output_full_v4",
  "pre_model_full_history_full_v4.RData"
)
lohnzettel_file <- "lohnzettel_dta.RData"
output_dir <- "model_output_v6_4threads_only_insamplev4_full_sample"

plots_dir   <- file.path(output_dir, "plots")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

#########################################
# 1) LOAD INPUTS
#########################################

missing_input_files <- c(pre_model_file, lohnzettel_file)[
  !file.exists(c(pre_model_file, lohnzettel_file))
]

if (length(missing_input_files) > 0) {
  stop(
    "Missing required input file(s): ",
    paste(missing_input_files, collapse = ", ")
  )
}

loaded_pre_model_objects <- load(pre_model_file)
if (!("pre_model_full_history" %in% loaded_pre_model_objects)) {
  stop("Input file does not contain the expected object: pre_model_full_history")
}

loaded_lohnzettel_objects <- load(lohnzettel_file)
if (!("lohnzettel" %in% loaded_lohnzettel_objects)) {
  stop("Input file does not contain the expected object: lohnzettel")
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
missing_pre_model_columns <- setdiff(
  required_pre_model_columns,
  names(pre_model_full_history)
)
if (length(missing_pre_model_columns) > 0) {
  stop(
    "pre_model_full_history is missing required column(s): ",
    paste(missing_pre_model_columns, collapse = ", ")
  )
}

required_lohnzettel_columns <- c(
  "penr", "benr", "year", "lz_ywage", "lz_dwage",
  "lz_golden_p67_2_6_10"
)
missing_lohnzettel_columns <- setdiff(
  required_lohnzettel_columns,
  names(lohnzettel)
)
if (length(missing_lohnzettel_columns) > 0) {
  stop(
    "lohnzettel is missing required column(s): ",
    paste(missing_lohnzettel_columns, collapse = ", ")
  )
}

cat("Loaded pre-model file:", pre_model_file, "\n")
cat("Loaded Lohnzettel file:", lohnzettel_file, "\n")
cat("Loaded pre_model_full_history rows:", nrow(pre_model_full_history), "\n")
cat("Loaded lohnzettel rows:", nrow(lohnzettel), "\n")

#########################################
# 2) SHARED CONSTANTS
#########################################

topcoding <- c(
  632, 686, 763, 850, 959, 1090, 1220, 1351, 1417, 1482, 1569, 1656,
  1744, 1787, 1874, 1918, 2005, 2049, 2092, 2180, 2311, 2441, 2616,
  2747, 2834, 2965, 3052, 3095, 3139, 3226, 3270, 3360, 3450, 3630,
  3750, 3840, 3930, 4020, 4110, 4200, 4230, 4440, 4530, 4650, 4860,
  4980, 5130, 5220, 5370
)

vpi_1986 <- data.frame(
  contryear = 1986:2024,
  year_inflation = c(
    0, 1.4, 2, 2.5, 3.3, 3.3, 4.1, 3.6, 3.0, 2.2, 1.9, 1.3, 0.9, 0.6,
    2.3, 2.7, 1.8, 1.3, 2.1, 2.3, 1.5, 2.2, 3.2, 0.5, 1.9, 3.3, 2.4,
    2.0, 1.7, 0.9, 0.9, 2.1, 2.0, 1.5, 1.4, 2.8, 8.6, 7.8, 2.9
  )
) %>%
  mutate(
    vpi = year_inflation / 100 + 1,
    reverse_index = cumprod(vpi),
    cpi_2000 = reverse_index[contryear == 2000] / reverse_index,
    cpi_2012 = reverse_index[contryear == 2012] / reverse_index
  )

#########################################
# 3) ANALYSIS SAMPLE FROM PRE-MODEL DATA
#########################################

analysis_data <- pre_model_full_history %>%
  filter(contryear >= firstyear, contryear <= lastyear)

all_penr <- analysis_data %>%
  distinct(penr)

if (nrow(all_penr) == 0) {
  stop("No persons available in analysis_data after year restriction.")
}

missing_analysis_years <- setdiff(
  firstyear:lastyear,
  sort(unique(analysis_data$contryear))
)
if (length(missing_analysis_years) > 0) {
  stop(
    "Analysis data is missing required year(s): ",
    paste(missing_analysis_years, collapse = ", ")
  )
}

analysis_duplicate_row <- anyDuplicated(
  as.data.frame(analysis_data[c("penr", "benr", "contryear")])
)
if (analysis_duplicate_row > 0) {
  stop(
    "Duplicate penr-benr-contryear key detected in analysis_data at row ",
    analysis_duplicate_row, "."
  )
}

nonfinite_analysis_wages <- sum(!is.finite(analysis_data$dailywage_final))
if (nonfinite_analysis_wages > 0) {
  stop(
    "analysis_data contains ", nonfinite_analysis_wages,
    " non-finite dailywage_final value(s)."
  )
}

if (use_debug_sample) {
  set.seed(sample_seed)

  n_available_penr <- nrow(all_penr)
  n_to_sample <- min(sample_n_penr, n_available_penr)

  sampled_penr <- all_penr %>%
    slice_sample(n = n_to_sample) %>%
    pull(penr)

  analysis_data <- analysis_data %>%
    filter(penr %in% sampled_penr)

  cat("DEBUG SAMPLE ACTIVE:", length(sampled_penr), "persons\n")
} else {
  sampled_penr <- all_penr %>%
    pull(penr)

  cat("USING FULL PRE-MODEL DATASET CONTENT:", length(sampled_penr), "persons\n")
}

cat("Analysis data rows after time/sample restriction:", nrow(analysis_data), "\n")
cat("Analysis data persons after time/sample restriction:", n_distinct(analysis_data$penr), "\n")
cat("Analysis year range:", min(analysis_data$contryear), "to", max(analysis_data$contryear), "\n")
cat("Analysis duplicate keys:", analysis_duplicate_row, "\n")
cat("Analysis non-finite wages:", nonfinite_analysis_wages, "\n")

#########################################
# 4) LZ DATA PREP
#########################################

lz_comparison_data <- lohnzettel %>%
  filter(penr %in% sampled_penr) %>%
  filter(year >= firstyear, year <= lastyear) %>%
  rename(contryear = year) %>%
  left_join(vpi_1986 %>% select(contryear, cpi_2000), by = "contryear") %>%
  mutate(
    lz_ywage_log = log(pmax(lz_ywage * cpi_2000, 1)),
    lz_dwage_log = log(pmax(lz_dwage * cpi_2000, 1)),
    lz_golden_log = log(pmax(lz_golden_p67_2_6_10 * cpi_2000 + 1, 1))
  ) %>%
  select(penr, benr, contryear, lz_ywage_log, lz_dwage_log, lz_golden_log) %>%
  filter(is.finite(lz_dwage_log))

if (nrow(lz_comparison_data) == 0) {
  stop("No valid Lohnzettel rows remain for the selected persons and years.")
}

lz_duplicate_row <- anyDuplicated(
  as.data.frame(lz_comparison_data[c("penr", "benr", "contryear")])
)
if (lz_duplicate_row > 0) {
  stop(
    "Duplicate penr-benr-contryear key detected in lz_comparison_data at row ",
    lz_duplicate_row,
    ". Resolve this before the BG-LZ join to prevent row multiplication."
  )
}

cat("LZ comparison rows:", nrow(lz_comparison_data), "\n")
cat("LZ comparison persons:", n_distinct(lz_comparison_data$penr), "\n")
cat("LZ duplicate keys:", lz_duplicate_row, "\n")

#########################################
# 5) BUILD SHARED INPUT FEATURES
#########################################

model_data <- analysis_data %>%
  group_by(penr) %>%
  mutate(
    num_years = n(),
    mean_wage = mean(dailywage_final, na.rm = TRUE),
    mean_censored = mean(censored, na.rm = TRUE)
  ) %>%
  mutate(
    mean_wage = ifelse(
      num_years > 1,
      (mean_wage - dailywage_final / num_years) * (num_years / (num_years - 1)),
      mean_wage
    ),
    mean_censored = ifelse(
      num_years > 1,
      (mean_censored - censored / num_years) * (num_years / (num_years - 1)),
      mean_censored
    )
  ) %>%
  ungroup() %>%
  group_by(benr, contryear) %>%
  mutate(
    firm_size = n(),
    mean_wage_coworkers = mean(dailywage_final, na.rm = TRUE),
    mean_censored_coworkers = mean(censored, na.rm = TRUE)
  ) %>%
  mutate(
    mean_wage_coworkers = ifelse(
      firm_size > 1,
      (mean_wage_coworkers - dailywage_final / firm_size) * (firm_size / (firm_size - 1)),
      mean_wage_coworkers
    ),
    mean_censored_coworkers = ifelse(
      firm_size > 1,
      (mean_censored_coworkers - censored / firm_size) * (firm_size / (firm_size - 1)),
      mean_censored_coworkers
    )
  ) %>%
  ungroup() %>%
  left_join(vpi_1986 %>% select(contryear, cpi_2000), by = "contryear") %>%
  mutate(
    topcode_threshold = topcoding[contryear - 1971],
    daily_topcode = topcode_threshold * 14 / 30,
    log_daily_topcode = log(daily_topcode * cpi_2000),
    firm_size_square = firm_size^2 / 100,
    oneyear_dummy = ifelse(num_years == 1, 1, 0),
    oneperson_dummy = ifelse(firm_size == 1, 1, 0),
    smallfirm_dummy = ifelse(firm_size < 10, 1, 0),
    agegroup = case_when(
      age <= 30 ~ 1L,
      age > 30 & age <= 40 ~ 2L,
      age > 40 & age <= 50 ~ 3L,
      age > 50 & age <= 60 ~ 4L,
      TRUE ~ 5L
    ),
    bg_censored_flag = censored,
    interval_upper = ifelse(censored == 0, dailywage_final, log_daily_topcode),
    interval_lower = dailywage_final
  ) %>%
  select(-cpi_2000, -topcode_threshold, -daily_topcode, -log_daily_topcode)

save(model_data, file = file.path(output_dir, "model_data_v6.RData"))

cat("Model data rows:", nrow(model_data), "\n")

#########################################
# 6) MATCHED COMPARISON DATASET
#########################################

comparison_data <- model_data %>%
  left_join(
    lz_comparison_data,
    by = c("penr", "benr", "contryear")
  ) %>%
  filter(!is.na(lz_dwage_log)) %>%
  mutate(
    bg_dwage_log = dailywage_final
  )

if (nrow(comparison_data) == 0) {
  stop("The BG-LZ comparison sample is empty after matching.")
}

comparison_duplicate_row <- anyDuplicated(
  as.data.frame(comparison_data[c("penr", "benr", "contryear")])
)
if (comparison_duplicate_row > 0) {
  stop(
    "Duplicate penr-benr-contryear key detected in comparison_data at row ",
    comparison_duplicate_row, "."
  )
}

cat("Comparison rows with BG and LZ:", nrow(comparison_data), "\n")
cat("Comparison persons with BG and LZ:", n_distinct(comparison_data$penr), "\n")
cat("Comparison duplicate keys:", comparison_duplicate_row, "\n")

sample_flow <- tibble(
  Stage = c(
    "Loaded pre-model full history",
    paste0("Pre-model analysis window ", firstyear, "-", lastyear),
    paste0("Valid LZ rows for selected persons ", firstyear, "-", lastyear),
    "Final matched BG-LZ comparison sample"
  ),
  Observations = c(
    nrow(pre_model_full_history),
    nrow(analysis_data),
    nrow(lz_comparison_data),
    nrow(comparison_data)
  ),
  Persons = c(
    n_distinct(pre_model_full_history$penr),
    n_distinct(analysis_data$penr),
    n_distinct(lz_comparison_data$penr),
    n_distinct(comparison_data$penr)
  )
)

cat("\n=== SAMPLE FLOW ===\n")
print(sample_flow, n = Inf)

write.csv(
  sample_flow,
  file.path(output_dir, "sample_flow_v6.csv"),
  row.names = FALSE
)

#########################################
# 7) MODEL SPECIFICATIONS
#########################################

feature_cols_v1 <- c(
  "age",
  "mean_wage",
  "mean_censored",
  "firm_size",
  "firm_size_square",
  "oneyear_dummy",
  "oneperson_dummy",
  "smallfirm_dummy",
  "mean_wage_coworkers",
  "mean_censored_coworkers"
)

feature_cols_v4 <- c(
  "age",
  "exper",
  "tenure",
  "mean_wage",
  "mean_censored",
  "firm_size",
  "firm_size_square",
  "oneyear_dummy",
  "oneperson_dummy",
  "smallfirm_dummy",
  "mean_wage_coworkers",
  "mean_censored_coworkers",
  "interval_lower",
  "interval_upper",
  "bg_censored_flag",
  "frau",
  "stsl_missing",
  "austrian_dummy",
  "germany_dummy",
  "eu_dummy",
  "educ_cat",
  "educ_missing",
  "educ_imputed",
  "whitecollar_dummy",
  "bluecollar_dummy",
  "othercollar_dummy",
  "subcat",
  "juj",
  "return",
  "year_entry",
  "age_entry",
  "num_years",
  "live_birth_event",
  "n_children",
  "contryear",
  "bundesland",
  "nace08_2"
)

#########################################
# 8) SHARED HELPERS
#########################################

prepare_matrix <- function(df, feature_cols) {
  df %>%
    select(all_of(feature_cols)) %>%
    mutate(across(everything(), ~ ifelse(is.na(.) | is.infinite(.), 0, .))) %>%
    as.matrix()
}

build_eval_table <- function(df, pred_col, method_name) {
  pred_sym <- rlang::sym(pred_col)

  df %>%
    transmute(
      method = method_name,
      lz_true = lz_dwage_log,
      bg_observed = bg_dwage_log,
      pred = !!pred_sym,
      censored = bg_censored_flag,
      error = pred - lz_true,
      abs_error = abs(pred - lz_true),
      squared_error = (pred - lz_true)^2,
      bg_error = bg_observed - lz_true,
      bg_abs_error = abs(bg_observed - lz_true),
      bg_squared_error = (bg_observed - lz_true)^2
    )
}

summarise_eval <- function(eval_df) {
  bind_rows(
    eval_df %>%
      summarise(
        Observation_Type = "All",
        n_obs = n(),
        RMSE = round(sqrt(mean(squared_error, na.rm = TRUE)), 4),
        MAE = round(mean(abs_error, na.rm = TRUE), 4),
        Mean_Error = round(mean(error, na.rm = TRUE), 4),
        SD_Error = round(sd(error, na.rm = TRUE), 4)
      ),
    eval_df %>%
      filter(censored == 0) %>%
      summarise(
        Observation_Type = "Uncensored",
        n_obs = n(),
        RMSE = round(sqrt(mean(squared_error, na.rm = TRUE)), 4),
        MAE = round(mean(abs_error, na.rm = TRUE), 4),
        Mean_Error = round(mean(error, na.rm = TRUE), 4),
        SD_Error = round(sd(error, na.rm = TRUE), 4)
      ),
    eval_df %>%
      filter(censored == 1) %>%
      summarise(
        Observation_Type = "Censored",
        n_obs = n(),
        RMSE = round(sqrt(mean(squared_error, na.rm = TRUE)), 4),
        MAE = round(mean(abs_error, na.rm = TRUE), 4),
        Mean_Error = round(mean(error, na.rm = TRUE), 4),
        SD_Error = round(sd(error, na.rm = TRUE), 4)
      )
  )
}

summarise_bg_baseline <- function(df, method_name = "BG Baseline") {
  eval_df <- df %>%
    transmute(
      method = method_name,
      lz_true = lz_dwage_log,
      bg_observed = bg_dwage_log,
      pred = bg_dwage_log,
      censored = bg_censored_flag,
      error = bg_dwage_log - lz_dwage_log,
      abs_error = abs(bg_dwage_log - lz_dwage_log),
      squared_error = (bg_dwage_log - lz_dwage_log)^2
    )

  summarise_eval(eval_df) %>%
    mutate(Method = method_name, Target = "LZ (true wages)") %>%
    select(Method, Target, Observation_Type, n_obs, RMSE, MAE, Mean_Error, SD_Error)
}

build_wage_level_eval_table <- function(df, pred_col, method_name) {
  pred_sym <- rlang::sym(pred_col)

  df %>%
    transmute(
      method = method_name,
      lz_true_log = lz_dwage_log,
      pred_log = !!pred_sym,
      censored = bg_censored_flag
    ) %>%
    mutate(
      true_wage = exp(lz_true_log),
      pred_wage = exp(pred_log),
      error = pred_wage - true_wage,
      abs_error = abs(error),
      squared_error = error^2
    ) %>%
    filter(
      is.finite(true_wage),
      is.finite(pred_wage),
      is.finite(error)
    )
}

summarise_wage_level_eval <- function(eval_df) {
  bind_rows(
    eval_df %>%
      summarise(
        Observation_Type = "All",
        n_obs = n(),
        Wage_RMSE = round(sqrt(mean(squared_error, na.rm = TRUE)), 2),
        Wage_MAE = round(mean(abs_error, na.rm = TRUE), 2),
        Wage_Median_AE = round(median(abs_error, na.rm = TRUE), 2),
        Mean_Error = round(mean(error, na.rm = TRUE), 2),
        SD_Error = round(sd(error, na.rm = TRUE), 2),
        Mean_True_Wage = round(mean(true_wage, na.rm = TRUE), 2),
        Median_True_Wage = round(median(true_wage, na.rm = TRUE), 2)
      ),
    eval_df %>%
      filter(censored == 0) %>%
      summarise(
        Observation_Type = "Uncensored",
        n_obs = n(),
        Wage_RMSE = round(sqrt(mean(squared_error, na.rm = TRUE)), 2),
        Wage_MAE = round(mean(abs_error, na.rm = TRUE), 2),
        Wage_Median_AE = round(median(abs_error, na.rm = TRUE), 2),
        Mean_Error = round(mean(error, na.rm = TRUE), 2),
        SD_Error = round(sd(error, na.rm = TRUE), 2),
        Mean_True_Wage = round(mean(true_wage, na.rm = TRUE), 2),
        Median_True_Wage = round(median(true_wage, na.rm = TRUE), 2)
      ),
    eval_df %>%
      filter(censored == 1) %>%
      summarise(
        Observation_Type = "Censored",
        n_obs = n(),
        Wage_RMSE = round(sqrt(mean(squared_error, na.rm = TRUE)), 2),
        Wage_MAE = round(mean(abs_error, na.rm = TRUE), 2),
        Wage_Median_AE = round(median(abs_error, na.rm = TRUE), 2),
        Mean_Error = round(mean(error, na.rm = TRUE), 2),
        SD_Error = round(sd(error, na.rm = TRUE), 2),
        Mean_True_Wage = round(mean(true_wage, na.rm = TRUE), 2),
        Median_True_Wage = round(median(true_wage, na.rm = TRUE), 2)
      )
  )
}

summarise_wage_level_model <- function(df, pred_col, method_name) {
  eval_df <- build_wage_level_eval_table(df, pred_col, method_name)

  summarise_wage_level_eval(eval_df) %>%
    mutate(
      Method = method_name,
      Target = "LZ daily wages (level)"
    ) %>%
    select(
      Method, Target, Observation_Type, n_obs,
      Wage_RMSE, Wage_MAE, Wage_Median_AE,
      Mean_Error, SD_Error, Mean_True_Wage, Median_True_Wage
    )
}

plot_error_density <- function(df, title, out_path) {
  p <- ggplot(df, aes(x = error, color = method, fill = method)) +
    geom_density(alpha = 0.25, size = 1.1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(
      title = title,
      x = "Prediction Error (ln wage units)",
      y = "Density",
      color = "Method",
      fill = "Method"
    ) +
    xlim(-2, 2)

  print(p)
  ggsave(out_path, plot = p, width = 10, height = 6)
}

#########################################
# 9) TOBIT HELPER
#########################################

run_tobit_model <- function(data, feature_cols, model_name) {
  cat("\nRunning", model_name, "...\n")
  cat("Features:", paste(feature_cols, collapse = ", "), "\n")
  cat("Rows:", nrow(data), "\n")

  formula_str <- paste(
    "survival::Surv(interval_lower, interval_upper, type = 'interval2') ~",
    paste(feature_cols, collapse = " + ")
  )

  model <- survival::survreg(
    as.formula(formula_str),
    data = data,
    dist = "gaussian"
  )

  predictions <- predict(model, newdata = data, type = "response")
  sigma_hat <- model$scale

  set.seed(123)
  k <- pnorm((data$dailywage_final - predictions) / sigma_hat)
  replace_vals <- predictions + sigma_hat * qnorm(k + runif(length(k)) * (1 - k))

  results_data <- data %>%
    mutate(
      tobit_linear_pred = predictions,
      dailywage_final_orig = dailywage_final,
      tobit_imputed_pred = replace_vals,
      model_prediction = ifelse(bg_censored_flag == 1, tobit_imputed_pred, tobit_linear_pred)
    )

  eval_df <- build_eval_table(results_data, "model_prediction", model_name)
  summary_table <- summarise_eval(eval_df)

  cat("\n===", model_name, "Performance on common comparison sample ===\n")
  print(summary_table)

  list(
    model_name = model_name,
    feature_cols = feature_cols,
    formula = formula_str,
    model = model,
    sigma_hat = sigma_hat,
    data = results_data,
    evaluation = eval_df,
    summary = summary_table
  )
}

#########################################
# 10) XGBOOST HELPER
#########################################

xgb_params <- list(
  nthread = 4L,
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  reg_alpha = 0.1,
  reg_lambda = 1
)

run_xgb_model_fullsample <- function(data, feature_cols, model_name, seed = 123) {
  X_features <- prepare_matrix(data, feature_cols)
  y_target <- data$lz_dwage_log

  dtrain <- xgb.DMatrix(data = X_features, label = y_target)

  cat("\nTraining", model_name, "on full common sample...\n")
  cat("Features:", paste(feature_cols, collapse = ", "), "\n")
  cat("Rows:", nrow(X_features), "\n")

  # CV reports train/test RMSE each round; each round fits all five folds.
  set.seed(seed)
  xgb_cv <- xgb.cv(
    params = xgb_params,
    data = dtrain,
    nrounds = 1000,
    nfold = 5,
    early_stopping_rounds = 50,
    verbose = 1,
    print_every_n = 1
  )

  best_nrounds <- xgb_cv$best_iteration

  # Final full-sample fit reports training RMSE each round.
  set.seed(seed)
  xgb_model <- xgb.train(
    params = xgb_params,
    data = dtrain,
    nrounds = best_nrounds,
    watchlist = list(train = dtrain),
    verbose = 1,
    print_every_n = 1
  )

  full_pred <- predict(xgb_model, dtrain)

  results_data <- data %>%
    mutate(
      xgb_prediction = full_pred
    )

  eval_df <- build_eval_table(results_data, "xgb_prediction", model_name)
  summary_table <- summarise_eval(eval_df)

  importance <- xgb.importance(
    feature_names = colnames(X_features),
    model = xgb_model
  )

  cat("Optimal rounds:", best_nrounds,
      "Best CV RMSE:", min(xgb_cv$evaluation_log$test_rmse_mean), "\n")

  cat("\n===", model_name, "Performance on common comparison sample ===\n")
  print(summary_table)

  list(
    model_name = model_name,
    feature_cols = feature_cols,
    X_features = X_features,
    model = xgb_model,
    cv = xgb_cv,
    best_nrounds = best_nrounds,
    predictions = full_pred,
    data = results_data,
    evaluation = eval_df,
    summary = summary_table,
    importance = importance
  )
}

#########################################
# 11) RUN TOBIT V1
#########################################

tobit_v1 <- timed_model_run(
  "In-sample common comparison",
  "Tobit V1",
  run_tobit_model(
    data = comparison_data,
    feature_cols = feature_cols_v1,
    model_name = "Tobit V1"
  )
)

#########################################
# 12) RUN XGBOOST V4
#########################################

xgb_v4 <- timed_model_run(
  "In-sample common comparison",
  "XGBoost V4",
  run_xgb_model_fullsample(
    data = comparison_data,
    feature_cols = feature_cols_v4,
    model_name = "XGBoost V4",
    seed = 123
  )
)

#########################################
# 13) COMMON-SAMPLE COMPARISON TABLE
#########################################

bg_baseline_summary <- summarise_bg_baseline(comparison_data)

tobit_v1_summary <- tobit_v1$summary %>%
  mutate(Method = "Tobit V1", Target = "LZ (true wages)") %>%
  select(Method, Target, Observation_Type, n_obs, RMSE, MAE, Mean_Error, SD_Error)

xgb_v4_summary <- xgb_v4$summary %>%
  mutate(Method = "XGBoost V4", Target = "LZ (true wages)") %>%
  select(Method, Target, Observation_Type, n_obs, RMSE, MAE, Mean_Error, SD_Error)

comprehensive_rmse_table <- bind_rows(
  bg_baseline_summary,
  tobit_v1_summary,
  xgb_v4_summary
) %>%
  arrange(Observation_Type, RMSE)

cat("\n=== COMMON-SAMPLE RMSE COMPARISON ===\n")
print(comprehensive_rmse_table, n = Inf)

write.csv(
  comprehensive_rmse_table,
  file.path(output_dir, "comprehensive_rmse_comparison_v6.csv"),
  row.names = FALSE
)

comprehensive_wage_level_table <- bind_rows(
  summarise_wage_level_model(comparison_data, "bg_dwage_log", "BG Baseline"),
  summarise_wage_level_model(tobit_v1$data, "model_prediction", "Tobit V1"),
  summarise_wage_level_model(xgb_v4$data, "xgb_prediction", "XGBoost V4")
) %>%
  arrange(Observation_Type, Wage_RMSE)

cat("\n=== COMMON-SAMPLE WAGE-LEVEL ERROR COMPARISON ===\n")
print(comprehensive_wage_level_table, n = Inf)

write.csv(
  comprehensive_wage_level_table,
  file.path(output_dir, "comprehensive_wage_level_error_v6.csv"),
  row.names = FALSE
)

#########################################
# 14) CORRECTED ERROR SUMMARY WITH BG BASELINE
#########################################

bg_baseline_errors_all <- comparison_data %>%
  transmute(
    method = "BG Baseline",
    error = bg_dwage_log - lz_dwage_log,
    censored = bg_censored_flag
  ) %>%
  filter(is.finite(error))

model_errors_all <- bind_rows(
  bg_baseline_errors_all,
  tobit_v1$evaluation %>% select(method, error, censored),
  xgb_v4$evaluation %>% select(method, error, censored)
) %>%
  filter(is.finite(error))

corrected_error_summary_block <- function(df, observation_type_label) {
  df %>%
    group_by(method) %>%
    summarise(
      Observation_Type = observation_type_label,
      n_obs = n(),
      mean_error = round(mean(error), 4),
      median_error = round(median(error), 4),
      sd_error = round(sd(error), 4),
      mae = round(mean(abs(error)), 4),
      rmse = round(sqrt(mean(error^2)), 4),
      .groups = "drop"
    )
}

corrected_error_summary_all_with_bg <- corrected_error_summary_block(
  model_errors_all,
  "All"
)

corrected_error_summary_censored_with_bg <- corrected_error_summary_block(
  model_errors_all %>% filter(censored == 1),
  "Censored"
)

corrected_error_summary_uncensored_with_bg <- corrected_error_summary_block(
  model_errors_all %>% filter(censored == 0),
  "Uncensored"
)

corrected_error_summary_combined_with_bg <- bind_rows(
  corrected_error_summary_all_with_bg,
  corrected_error_summary_censored_with_bg,
  corrected_error_summary_uncensored_with_bg
) %>%
  arrange(Observation_Type, rmse)

cat("\n=== CORRECTED ERROR SUMMARY WITH BG BASELINE ===\n")
print(corrected_error_summary_combined_with_bg, n = Inf)

write.csv(
  corrected_error_summary_combined_with_bg,
  file.path(output_dir, "corrected_error_summary_with_bg_baseline_v6.csv"),
  row.names = FALSE
)

#########################################
# 15) ERROR SUMMARIES
#########################################

all_errors_corrected <- bind_rows(
  tobit_v1$evaluation %>% select(method, error, censored),
  xgb_v4$evaluation %>% select(method, error, censored)
) %>%
  filter(is.finite(error))

censored_errors_corrected <- all_errors_corrected %>%
  filter(censored == 1)

uncensored_errors_corrected <- all_errors_corrected %>%
  filter(censored == 0)

error_summary_corrected_all <- all_errors_corrected %>%
  group_by(method) %>%
  summarise(
    n_obs = n(),
    mean_error = round(mean(error), 4),
    median_error = round(median(error), 4),
    sd_error = round(sd(error), 4),
    mae = round(mean(abs(error)), 4),
    rmse = round(sqrt(mean(error^2)), 4),
    .groups = "drop"
  ) %>%
  arrange(rmse)

error_summary_corrected_censored <- censored_errors_corrected %>%
  group_by(method) %>%
  summarise(
    n_obs = n(),
    mean_error = round(mean(error), 4),
    median_error = round(median(error), 4),
    sd_error = round(sd(error), 4),
    mae = round(mean(abs(error)), 4),
    rmse = round(sqrt(mean(error^2)), 4),
    .groups = "drop"
  ) %>%
  arrange(rmse)

error_summary_corrected_uncensored <- uncensored_errors_corrected %>%
  group_by(method) %>%
  summarise(
    n_obs = n(),
    mean_error = round(mean(error), 4),
    median_error = round(median(error), 4),
    sd_error = round(sd(error), 4),
    mae = round(mean(abs(error)), 4),
    rmse = round(sqrt(mean(error^2)), 4),
    .groups = "drop"
  ) %>%
  arrange(rmse)


cat("\n=== Error Summary: ALL OBSERVATIONS ===\n")
print(error_summary_corrected_all)

cat("\n=== Error Summary: CENSORED OBSERVATIONS ONLY ===\n")
print(error_summary_corrected_censored)

cat("\n=== Error Summary: UNCENSORED OBSERVATIONS ONLY ===\n")
print(error_summary_corrected_uncensored)

#########################################
# 16) FEATURE IMPORTANCE OUTPUTS
#########################################

cat("\n=== Top Features: XGBoost V4 ===\n")
print(head(xgb_v4$importance, 25))

save(
  tobit_v1, xgb_v4,
  comprehensive_wage_level_table,
  file = file.path(output_dir, "model_objects_v6.RData")
)

cat("\n=== MODEL RUNTIME SUMMARY ===\n")
print(model_runtime_log, n = Inf)

write.csv(
  model_runtime_log,
  file.path(output_dir, "model_runtime_v6.csv"),
  row.names = FALSE
)

#########################################
# 17) SIMPLE PLOTS
#########################################

all_errors_plot <- all_errors_corrected

censored_errors_plot <- all_errors_plot %>%
  filter(censored == 1)

plot_error_density(
  all_errors_plot,
  "Prediction Error Distribution - All Observations",
  file.path(plots_dir, "error_density_all.png")
)

plot_error_density(
  censored_errors_plot,
  "Prediction Error Distribution - Censored Observations",
  file.path(plots_dir, "error_density_censored.png")
)

#########################################
# PREDICTED WAGE DISTRIBUTION PLOTS:
# TOBIT V1 + XGBOOST V4 + LZ TRUE WAGE
# BG BASELINE IS ADDED TO THE ALL-OBSERVATIONS PLOT
#########################################

predicted_wages_full <- bind_rows(
  comparison_data %>%
    transmute(
      method = "LZ True Wage",
      predicted_wage = lz_dwage_log,
      censored = bg_censored_flag
    ),
  tobit_v1$data %>%
    transmute(
      method = "Tobit V1",
      predicted_wage = model_prediction,
      censored = bg_censored_flag
    ),
  xgb_v4$data %>%
    transmute(
      method = "XGBoost V4",
      predicted_wage = xgb_prediction,
      censored = bg_censored_flag
    )
) %>%
  filter(is.finite(predicted_wage))

predicted_wages_full_all <- bind_rows(
  comparison_data %>%
    transmute(
      method = "BG Baseline",
      predicted_wage = bg_dwage_log,
      censored = bg_censored_flag
    ),
  predicted_wages_full
) %>%
  filter(is.finite(predicted_wage))

predicted_wages_full_censored <- predicted_wages_full %>%
  filter(censored == 1)

predicted_wages_full_uncensored <- predicted_wages_full %>%
  filter(censored == 0)

color_scheme_full <- c(
  "BG Baseline" = "black",
  "LZ True Wage" = "green",
  "Tobit V1" = "purple",
  "XGBoost V4" = "blue"
)

plot_predicted_wage_density_full <- function(df, title_text, subtitle_text, out_path) {
  p <- ggplot(df, aes(x = predicted_wage, color = method, fill = method)) +
    geom_density(alpha = 0.15, size = 1.1) +
    scale_color_manual(values = color_scheme_full) +
    scale_fill_manual(values = color_scheme_full) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = "ln Daily Wage",
      y = "Density",
      color = "Series",
      fill = "Series"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    ) +
    xlim(0, 7)
  
  print(p)
  ggsave(out_path, plot = p, width = 11, height = 6.5)
}

# Exploratory data figure: observed BG and LZ distributions only. The labels,
# log-wage scale, colors, density settings, and dimensions deliberately match
# the model-comparison distribution plots below.
observed_wages_bg_lz_full <- bind_rows(
  comparison_data %>%
    transmute(
      method = "BG Baseline",
      predicted_wage = bg_dwage_log,
      censored = bg_censored_flag
    ),
  comparison_data %>%
    transmute(
      method = "LZ True Wage",
      predicted_wage = lz_dwage_log,
      censored = bg_censored_flag
    )
) %>%
  filter(is.finite(predicted_wage))

plot_predicted_wage_density_full(
  observed_wages_bg_lz_full,
  "BG and LZ Log Daily-Wage Distributions",
  "Full matched sample, 1994-2012",
  file.path(plots_dir, "exploratory_wage_density_bg_lz_full.png")
)

# All observations
plot_predicted_wage_density_full(
  predicted_wages_full_all,
  "Predicted Wage Distribution: BG, Tobit, XGBoost, and LZ",
  "All observations",
  file.path(plots_dir, "predicted_wage_density_full_all.png")
)

# Censored only
plot_predicted_wage_density_full(
  predicted_wages_full_censored,
  "Predicted Wage Distribution: Tobit, XGBoost, and LZ",
  "Censored observations only",
  file.path(plots_dir, "predicted_wage_density_full_censored.png")
)

# Uncensored only
plot_predicted_wage_density_full(
  predicted_wages_full_uncensored,
  "Predicted Wage Distribution: Tobit, XGBoost, and LZ",
  "Uncensored observations only",
  file.path(plots_dir, "predicted_wage_density_full_uncensored.png")
)
#########################################
# 18) PREDICTION FUNCTION FOR LATEST MODEL
#########################################

predict_true_wage_v6 <- function(bg_data,
                                 model_obj = xgb_v4,
                                 feature_columns = model_obj$feature_cols) {
  prediction_features <- bg_data %>%
    select(all_of(feature_columns)) %>%
    mutate(across(everything(), ~ ifelse(is.na(.) | is.infinite(.), 0, .))) %>%
    as.matrix()

  predict(model_obj$model, xgb.DMatrix(data = prediction_features))
}

save(
  predict_true_wage_v6,
  file = file.path(output_dir, "predict_true_wage_v6_function.RData")
)

cat("\n=== V6 modeling pipeline complete ===\n")
cat("Models included: Tobit V1, XGBoost V4 (plus BG baseline)\n")
cat("Main comparison: in-sample, all eligible matched persons in 1994-2012\n")
cat("Output folder:", output_dir, "\n")

#########################################
# 19) QUICK DATA CHECKS
#########################################

model_data %>%
  count(penr, benr, contryear) %>%
  filter(n > 1)

lz_comparison_data %>%
  count(penr, benr, contryear) %>%
  filter(n > 1)

nrow(model_data)
nrow(lz_comparison_data)
nrow(comparison_data)

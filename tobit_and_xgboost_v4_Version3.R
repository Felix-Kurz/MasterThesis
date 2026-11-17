library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(survival)
library(censReg)
library(VGAM)
library(purrr)
library(data.table)
library(stringr)
library(ggplot2)
library(scales)
library(xgboost)

options(dplyr.summarise.inform = FALSE)

#########################################
# 0) CONFIG
#########################################

firstyear <- 1994
lastyear  <- 2012

use_debug_sample <- TRUE
sample_n_penr <- 10000
sample_seed <- 123

# Centralized output paths:
# Change only this variable to switch output folders.
output_dir <- "model_output_v4_10k"

results_dir <- file.path(output_dir, "results_by_year")
plots_dir   <- file.path(output_dir, "plots")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

#########################################
# 1) LOAD INPUTS
#########################################

#load("pre_model_output/pre_model_full_history.RData")   # object: pre_model_full_history
load("pre_model_output/pre_model_full_history_10k.RData")   # object: pre_model_full_history

load("lohnzettel_dta.RData")                            # object: lohnzettel

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
cat("Analysis year range:", min(analysis_data$contryear), "to", max(analysis_data$contryear), "\n")

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

cat("LZ comparison rows:", nrow(lz_comparison_data), "\n")

#########################################
# 5) BUILD TOBIT / XGBOOST INPUT FEATURES
#########################################

tobit_data <- analysis_data %>%
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

save(tobit_data, file = file.path(output_dir, "tobit_data_v4.RData"))

cat("Tobit data rows:", nrow(tobit_data), "\n")

#########################################
# 6) TOBIT IMPUTATION
#########################################

imputed_data_list <- list()

min_year <- min(tobit_data$contryear, na.rm = TRUE)
max_year <- max(tobit_data$contryear, na.rm = TRUE)

for (year in min_year:max_year) {
  cat("Processing Tobit year:", year, "\n")

  year_data <- tobit_data %>% filter(contryear == year)

  if (nrow(year_data) == 0) next

  year_results <- year_data %>%
    group_by(frau, agegroup) %>%
    do({
      grp <- .

      if (nrow(grp) <= 10) {
        out <- grp %>%
          mutate(
            predict = dailywage_final,
            dailywage_final_orig = dailywage_final,
            dailywage_final_pred = dailywage_final,
            sigma_hat = 1
          )
        return(out)
      }

      formula_str <- paste(
        "survival::Surv(interval_lower, interval_upper, type = 'interval2') ~",
        "age + mean_wage + mean_censored + firm_size + firm_size_square +",
        "oneyear_dummy + oneperson_dummy + smallfirm_dummy +",
        "mean_wage_coworkers + mean_censored_coworkers"
      )

      tryCatch({
        model <- survival::survreg(as.formula(formula_str), data = grp, dist = "gaussian")
        predictions <- predict(model, newdata = grp, type = "response")
        sigma_hat <- model$scale

        set.seed(123)
        k <- pnorm((grp$dailywage_final - predictions) / sigma_hat)
        replace_vals <- predictions + sigma_hat * qnorm(k + runif(length(k)) * (1 - k))

        grp %>%
          mutate(
            predict = predictions,
            dailywage_final_orig = dailywage_final,
            dailywage_final_pred = replace_vals,
            sigma_hat = sigma_hat
          )
      }, error = function(e) {
        grp %>%
          mutate(
            predict = dailywage_final,
            dailywage_final_orig = dailywage_final,
            dailywage_final_pred = dailywage_final,
            sigma_hat = 1
          )
      })
    }) %>%
    ungroup() %>%
    mutate(
      dailywage_final = ifelse(censored == 1, dailywage_final_pred, dailywage_final),
      educ_proxy = case_when(
        age_entry < 18 ~ 1L,
        age_entry >= 18 & age_entry <= 21 ~ 2L,
        age_entry > 21 ~ 3L,
        TRUE ~ NA_integer_
      )
    )

  save(
    year_results,
    file = file.path(results_dir, paste0("tobit_year_", year, ".RData"))
  )

  imputed_data_list[[as.character(year)]] <- year_results
}

final_imputed_data <- bind_rows(imputed_data_list)

save(final_imputed_data, file = file.path(output_dir, "final_imputed_data_v4.RData"))

#########################################
# 7) TOBIT EVALUATION
#########################################

imputation_eval <- final_imputed_data %>%
  filter(!is.na(dailywage_final_orig), !is.na(dailywage_final_pred)) %>%
  mutate(
    was_imputed = (dailywage_final != dailywage_final_orig),
    residual = dailywage_final_pred - dailywage_final_orig,
    squared_error = residual^2
  )

rmse_by_censoring <- imputation_eval %>%
  group_by(was_imputed) %>%
  summarise(
    rmse = sqrt(mean(squared_error, na.rm = TRUE)),
    mae = mean(abs(residual), na.rm = TRUE),
    n_obs = n(),
    mean_residual = mean(residual, na.rm = TRUE),
    median_residual = median(residual, na.rm = TRUE),
    sd_residual = sd(residual, na.rm = TRUE)
  ) %>%
  mutate(
    censoring_status = ifelse(was_imputed, "Censored (Imputed)", "Uncensored")
  )

rmse_overall <- imputation_eval %>%
  summarise(
    was_imputed = NA,
    rmse = sqrt(mean(squared_error, na.rm = TRUE)),
    mae = mean(abs(residual), na.rm = TRUE),
    n_obs = n(),
    mean_residual = mean(residual, na.rm = TRUE),
    median_residual = median(residual, na.rm = TRUE),
    sd_residual = sd(residual, na.rm = TRUE),
    censoring_status = "Overall"
  )

complete_rmse_summary <- bind_rows(rmse_by_censoring, rmse_overall)

print(complete_rmse_summary)

#########################################
# 8) TRAINING DATA FOR XGBOOST
#########################################

training_data <- tobit_data %>%
  left_join(
    lz_comparison_data,
    by = c("penr", "benr", "contryear")
  ) %>%
  filter(!is.na(lz_dwage_log)) %>%
  mutate(
    bg_dwage_log = dailywage_final
  )

cat("Training rows with BG and LZ:", nrow(training_data), "\n")

#########################################
# 9) SHARED XGBOOST HELPERS
#########################################

xgb_params <- list(
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

prepare_matrix <- function(df, feature_cols) {
  df %>%
    select(all_of(feature_cols)) %>%
    mutate(across(everything(), ~ ifelse(is.na(.) | is.infinite(.), 0, .))) %>%
    as.matrix()
}

run_xgb_model <- function(data, feature_cols, model_name, seed = 123) {
  X_features <- prepare_matrix(data, feature_cols)
  y_target <- data$lz_dwage_log

  set.seed(seed)
  n_obs <- length(y_target)
  train_indices <- sample(seq_len(n_obs), size = floor(0.8 * n_obs))

  X_train <- X_features[train_indices, , drop = FALSE]
  X_test  <- X_features[-train_indices, , drop = FALSE]
  y_train <- y_target[train_indices]
  y_test  <- y_target[-train_indices]

  bg_train <- data$bg_dwage_log[train_indices]
  bg_test  <- data$bg_dwage_log[-train_indices]

  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dtest  <- xgb.DMatrix(data = X_test, label = y_test)

  cat("\nTraining", model_name, "...\n")
  cat("Features:", paste(feature_cols, collapse = ", "), "\n")
  cat("Training rows:", nrow(X_train), "Test rows:", nrow(X_test), "\n")

  set.seed(seed)
  xgb_cv <- xgb.cv(
    params = xgb_params,
    data = dtrain,
    nrounds = 1000,
    nfold = 5,
    early_stopping_rounds = 50,
    verbose = FALSE
  )

  best_nrounds <- xgb_cv$best_iteration

  set.seed(seed)
  xgb_model <- xgb.train(
    params = xgb_params,
    data = dtrain,
    nrounds = best_nrounds,
    watchlist = list(train = dtrain, test = dtest),
    verbose = FALSE
  )

  train_pred <- predict(xgb_model, dtrain)
  test_pred  <- predict(xgb_model, dtest)

  results <- list(
    model_name = model_name,
    feature_cols = feature_cols,
    train_indices = train_indices,
    X_features = X_features,
    model = xgb_model,
    cv = xgb_cv,
    best_nrounds = best_nrounds,
    train_pred = train_pred,
    test_pred = test_pred,
    y_train = y_train,
    y_test = y_test,
    bg_train = bg_train,
    bg_test = bg_test,
    censored_test = data$bg_censored_flag[-train_indices],
    censored_train = data$bg_censored_flag[train_indices],
    train_rmse_lz = sqrt(mean((train_pred - y_train)^2)),
    test_rmse_lz = sqrt(mean((test_pred - y_test)^2)),
    train_r2_lz = cor(train_pred, y_train)^2,
    test_r2_lz = cor(test_pred, y_test)^2,
    train_rmse_bg = sqrt(mean((bg_train - y_train)^2)),
    test_rmse_bg = sqrt(mean((bg_test - y_test)^2)),
    train_r2_bg = cor(bg_train, y_train)^2,
    test_r2_bg = cor(bg_test, y_test)^2
  )

  results$prediction_comparison <- data.frame(
    lz_true = results$y_test,
    bg_observed = results$bg_test,
    xgb_predicted = results$test_pred,
    censored = results$censored_test,
    bg_error = abs(results$bg_test - results$y_test),
    xgb_error = abs(results$test_pred - results$y_test),
    improvement = abs(results$bg_test - results$y_test) - abs(results$test_pred - results$y_test)
  ) %>%
    mutate(
      bg_better = improvement < 0,
      xgb_better = improvement > 0,
      bg_pct_error = abs((bg_observed - lz_true) / lz_true) * 100,
      xgb_pct_error = abs((xgb_predicted - lz_true) / lz_true) * 100
    )

  results$importance <- xgb.importance(
    feature_names = colnames(X_features),
    model = xgb_model
  )

  cat("Optimal rounds:", best_nrounds,
      "Best CV RMSE:", min(xgb_cv$evaluation_log$test_rmse_mean), "\n")

  cat("\n===", model_name, "Performance ===\n")
  cat("Train RMSE vs LZ:", round(results$train_rmse_lz, 4),
      "R²:", round(results$train_r2_lz, 4), "\n")
  cat("Test RMSE vs LZ:", round(results$test_rmse_lz, 4),
      "R²:", round(results$test_r2_lz, 4), "\n")
  cat("BG baseline Test RMSE:", round(results$test_rmse_bg, 4),
      "R²:", round(results$test_r2_bg, 4), "\n")
  cat("RMSE reduction vs BG:", round(results$test_rmse_bg - results$test_rmse_lz, 4), "\n")

  results
}

#########################################
# 10) XGBOOST V1
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

xgb_v1 <- run_xgb_model(
  data = training_data,
  feature_cols = feature_cols_v1,
  model_name = "XGBoost V1",
  seed = 123
)

#########################################
# 11) XGBOOST V2
#########################################

feature_cols_v2 <- c(
  "age",
  "mean_wage",
  "mean_censored",
  "firm_size",
  "firm_size_square",
  "oneyear_dummy",
  "oneperson_dummy",
  "smallfirm_dummy",
  "mean_wage_coworkers",
  "mean_censored_coworkers",
  "frau",
  "birthyear",
  "num_years",
  "interval_lower",
  "interval_upper",
  "bg_censored_flag"
)

xgb_v2 <- run_xgb_model(
  data = training_data,
  feature_cols = feature_cols_v2,
  model_name = "XGBoost V2",
  seed = 123
)

#########################################
# 12) XGBOOST V4 / LATEST ACTIVE MODEL
#########################################
# This is the post-split successor to the old V3.
# It uses the richer pre-model dataset variables.

feature_cols_v4 <- c(
  # Core wage / censoring structure
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

  # Demographics
  "frau",
  "stsl_missing",
  "austrian_dummy",
  "germany_dummy",
  "eu_dummy",

  # Education
  "educ_cat",
  "educ_missing",
  "educ_imputed",

  # Job / collar / transitions
  "whitecollar_dummy",
  "bluecollar_dummy",
  "othercollar_dummy",
  "subcat",
  "juj",
  "return",

  # Career path
  "year_entry",
  "age_entry",
  "num_years",

  # Family
  "live_birth_event",
  "n_children",

  # Time / geography / industry
  "contryear",
  "bundesland",
  "nace08_2"
)

xgb_v4 <- run_xgb_model(
  data = training_data,
  feature_cols = feature_cols_v4,
  model_name = "XGBoost V4 Latest",
  seed = 123
)

cat("\n=== Comparison: V2 vs V4 Latest ===\n")
cat("V2 Test RMSE:", round(xgb_v2$test_rmse_lz, 4), "R²:", round(xgb_v2$test_r2_lz, 4), "\n")
cat("V4 Test RMSE:", round(xgb_v4$test_rmse_lz, 4), "R²:", round(xgb_v4$test_r2_lz, 4), "\n")
cat("RMSE reduction V4 over V2:", round(xgb_v2$test_rmse_lz - xgb_v4$test_rmse_lz, 4), "\n")
cat("R² improvement V4 over V2:", round(xgb_v4$test_r2_lz - xgb_v2$test_r2_lz, 4), "\n")

#########################################
# 13) TOBIT CORRECTED AGAINST LZ
#########################################

tobit_corrected <- final_imputed_data %>%
  inner_join(
    lz_comparison_data %>% select(penr, benr, contryear, lz_dwage_log),
    by = c("penr", "benr", "contryear")
  ) %>%
  mutate(
    error_vs_lz = dailywage_final_pred - lz_dwage_log,
    error_vs_censored = dailywage_final_pred - dailywage_final_orig,
    was_imputed = (dailywage_final != dailywage_final_orig)
  ) %>%
  filter(is.finite(error_vs_lz))

#########################################
# 14) COMPARATIVE ERROR SUMMARIES
#########################################

tobit_errors_corrected_all <- tobit_corrected %>%
  transmute(error = error_vs_lz, method = "Tobit", was_imputed = was_imputed)

tobit_errors_corrected_censored <- tobit_corrected %>%
  filter(was_imputed) %>%
  transmute(error = error_vs_lz, method = "Tobit", was_imputed = was_imputed)

xgb_v1_errors_all <- xgb_v1$prediction_comparison %>%
  transmute(error = xgb_predicted - lz_true, method = "XGBoost V1", was_imputed = (censored == 1))

xgb_v2_errors_all <- xgb_v2$prediction_comparison %>%
  transmute(error = xgb_predicted - lz_true, method = "XGBoost V2", was_imputed = (censored == 1))

xgb_v4_errors_all <- xgb_v4$prediction_comparison %>%
  transmute(error = xgb_predicted - lz_true, method = "XGBoost V4", was_imputed = (censored == 1))

all_errors_corrected <- bind_rows(
  tobit_errors_corrected_all,
  xgb_v1_errors_all,
  xgb_v2_errors_all,
  xgb_v4_errors_all
) %>%
  filter(is.finite(error))

censored_errors_corrected <- bind_rows(
  tobit_errors_corrected_censored,
  xgb_v1_errors_all %>% filter(was_imputed),
  xgb_v2_errors_all %>% filter(was_imputed),
  xgb_v4_errors_all %>% filter(was_imputed)
) %>%
  filter(is.finite(error))

error_summary_corrected_all <- all_errors_corrected %>%
  group_by(method) %>%
  summarise(
    n_obs = n(),
    mean_error = round(mean(error), 4),
    median_error = round(median(error), 4),
    sd_error = round(sd(error), 4),
    mae = round(mean(abs(error)), 4),
    rmse = round(sqrt(mean(error^2)), 4)
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
    rmse = round(sqrt(mean(error^2)), 4)
  ) %>%
  arrange(rmse)

cat("\n=== Error Summary: ALL OBSERVATIONS ===\n")
print(error_summary_corrected_all)

cat("\n=== Error Summary: CENSORED OBSERVATIONS ONLY ===\n")
print(error_summary_corrected_censored)

#########################################
# 15) COMPREHENSIVE RMSE TABLE
#########################################

tobit_vs_bg_all <- tobit_corrected %>%
  summarise(
    Method = "Tobit",
    Target = "BG (censored data)",
    Observation_Type = "All",
    n_obs = n(),
    RMSE = round(sqrt(mean(error_vs_censored^2)), 4),
    MAE = round(mean(abs(error_vs_censored)), 4),
    Mean_Error = round(mean(error_vs_censored), 4),
    SD_Error = round(sd(error_vs_censored), 4)
  )

tobit_vs_bg_uncensored <- tobit_corrected %>%
  filter(!was_imputed) %>%
  summarise(
    Method = "Tobit",
    Target = "BG (censored data)",
    Observation_Type = "Uncensored",
    n_obs = n(),
    RMSE = round(sqrt(mean(error_vs_censored^2)), 4),
    MAE = round(mean(abs(error_vs_censored)), 4),
    Mean_Error = round(mean(error_vs_censored), 4),
    SD_Error = round(sd(error_vs_censored), 4)
  )

tobit_vs_bg_censored <- tobit_corrected %>%
  filter(was_imputed) %>%
  summarise(
    Method = "Tobit",
    Target = "BG (censored data)",
    Observation_Type = "Censored",
    n_obs = n(),
    RMSE = round(sqrt(mean(error_vs_censored^2)), 4),
    MAE = round(mean(abs(error_vs_censored)), 4),
    Mean_Error = round(mean(error_vs_censored), 4),
    SD_Error = round(sd(error_vs_censored), 4)
  )

tobit_vs_lz_all <- tobit_corrected %>%
  summarise(
    Method = "Tobit",
    Target = "LZ (true wages)",
    Observation_Type = "All",
    n_obs = n(),
    RMSE = round(sqrt(mean(error_vs_lz^2)), 4),
    MAE = round(mean(abs(error_vs_lz)), 4),
    Mean_Error = round(mean(error_vs_lz), 4),
    SD_Error = round(sd(error_vs_lz), 4)
  )

tobit_vs_lz_uncensored <- tobit_corrected %>%
  filter(!was_imputed) %>%
  summarise(
    Method = "Tobit",
    Target = "LZ (true wages)",
    Observation_Type = "Uncensored",
    n_obs = n(),
    RMSE = round(sqrt(mean(error_vs_lz^2)), 4),
    MAE = round(mean(abs(error_vs_lz)), 4),
    Mean_Error = round(mean(error_vs_lz), 4),
    SD_Error = round(sd(error_vs_lz), 4)
  )

tobit_vs_lz_censored <- tobit_corrected %>%
  filter(was_imputed) %>%
  summarise(
    Method = "Tobit",
    Target = "LZ (true wages)",
    Observation_Type = "Censored",
    n_obs = n(),
    RMSE = round(sqrt(mean(error_vs_lz^2)), 4),
    MAE = round(mean(abs(error_vs_lz)), 4),
    Mean_Error = round(mean(error_vs_lz), 4),
    SD_Error = round(sd(error_vs_lz), 4)
  )

xgb_summary_block <- function(obj, method_name) {
  bind_rows(
    obj$prediction_comparison %>%
      summarise(
        Method = method_name,
        Target = "LZ (true wages)",
        Observation_Type = "All",
        n_obs = n(),
        RMSE = round(sqrt(mean((xgb_predicted - lz_true)^2)), 4),
        MAE = round(mean(abs(xgb_predicted - lz_true)), 4),
        Mean_Error = round(mean(xgb_predicted - lz_true), 4),
        SD_Error = round(sd(xgb_predicted - lz_true), 4)
      ),
    obj$prediction_comparison %>%
      filter(censored == 0) %>%
      summarise(
        Method = method_name,
        Target = "LZ (true wages)",
        Observation_Type = "Uncensored",
        n_obs = n(),
        RMSE = round(sqrt(mean((xgb_predicted - lz_true)^2)), 4),
        MAE = round(mean(abs(xgb_predicted - lz_true)), 4),
        Mean_Error = round(mean(xgb_predicted - lz_true), 4),
        SD_Error = round(sd(xgb_predicted - lz_true), 4)
      ),
    obj$prediction_comparison %>%
      filter(censored == 1) %>%
      summarise(
        Method = method_name,
        Target = "LZ (true wages)",
        Observation_Type = "Censored",
        n_obs = n(),
        RMSE = round(sqrt(mean((xgb_predicted - lz_true)^2)), 4),
        MAE = round(mean(abs(xgb_predicted - lz_true)), 4),
        Mean_Error = round(mean(xgb_predicted - lz_true), 4),
        SD_Error = round(sd(xgb_predicted - lz_true), 4)
      )
  )
}

comprehensive_rmse_table <- bind_rows(
  tobit_vs_bg_all,
  tobit_vs_bg_uncensored,
  tobit_vs_bg_censored,
  tobit_vs_lz_all,
  tobit_vs_lz_uncensored,
  tobit_vs_lz_censored,
  xgb_summary_block(xgb_v1, "XGBoost V1"),
  xgb_summary_block(xgb_v2, "XGBoost V2"),
  xgb_summary_block(xgb_v4, "XGBoost V4")
) %>%
  arrange(Observation_Type, Method, Target)

print(comprehensive_rmse_table, n = Inf)

write.csv(
  comprehensive_rmse_table,
  file.path(output_dir, "comprehensive_rmse_comparison_v4.csv"),
  row.names = FALSE
)

#########################################
# 16) FEATURE IMPORTANCE OUTPUTS
#########################################

cat("\n=== Top Features: XGBoost V1 ===\n")
print(head(xgb_v1$importance, 15))

cat("\n=== Top Features: XGBoost V2 ===\n")
print(head(xgb_v2$importance, 15))

cat("\n=== Top Features: XGBoost V4 Latest ===\n")
print(head(xgb_v4$importance, 25))

save(
  xgb_v1, xgb_v2, xgb_v4,
  file = file.path(output_dir, "xgboost_models_v4.RData")
)

#########################################
# 17) SIMPLE ERROR PLOTS
#########################################

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

plot_error_density(
  all_errors_corrected,
  "Prediction Error Distribution - All Observations",
  file.path(plots_dir, "error_density_all.png")
)

plot_error_density(
  censored_errors_corrected,
  "Prediction Error Distribution - Censored Observations",
  file.path(plots_dir, "error_density_censored.png")
)

#########################################
# 18) PREDICTION FUNCTION FOR LATEST MODEL
#########################################

predict_true_wage_v4 <- function(bg_data,
                                 model_obj = xgb_v4,
                                 feature_columns = xgb_v4$feature_cols) {
  prediction_features <- bg_data %>%
    select(all_of(feature_columns)) %>%
    mutate(across(everything(), ~ ifelse(is.na(.) | is.infinite(.), 0, .))) %>%
    as.matrix()

  predict(model_obj$model, xgb.DMatrix(data = prediction_features))
}

save(
  predict_true_wage_v4,
  file = file.path(output_dir, "predict_true_wage_v4_function.RData")
)

cat("\n=== V4 modeling pipeline complete ===\n")
cat("Latest active model: XGBoost V4\n")
cat("Output folder:", output_dir, "\n")




#tests for issues with RMSE increase
tobit_data %>%
  count(penr, benr, contryear) %>%
  filter(n>1)

lz_comparison_data %>%
  count(penr, benr, contryear) %>%
  filter(n>1)

nrow(tobit_data)
nrow(lz_comparison_data)
nrow(training_data)

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

use_debug_sample <- FALSE
sample_n_penr <- 10000
sample_seed <- 123

# Centralized output paths:
#output_dir <- "model_output_v5_10k"
output_dir <- "model_output_v5"

results_dir <- file.path(output_dir, "results_by_year")
plots_dir   <- file.path(output_dir, "plots")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

#########################################
# 1) LOAD INPUTS
#########################################

load("pre_model_output/pre_model_full_history.RData")
#load("pre_model_output/pre_model_full_history_10k.RData")
load("lohnzettel_dta.RData")

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

save(model_data, file = file.path(output_dir, "model_data_v5.RData"))

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

cat("Comparison rows with BG and LZ:", nrow(comparison_data), "\n")

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
    watchlist = list(train = dtrain),
    verbose = FALSE
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

tobit_v1 <- run_tobit_model(
  data = comparison_data,
  feature_cols = feature_cols_v1,
  model_name = "Tobit V1"
)

#########################################
# 12) RUN TOBIT V4
#########################################

tobit_v4 <- run_tobit_model(
  data = comparison_data,
  feature_cols = feature_cols_v4,
  model_name = "Tobit V4"
)

#########################################
# 13) RUN XGBOOST V1
#########################################

xgb_v1 <- run_xgb_model_fullsample(
  data = comparison_data,
  feature_cols = feature_cols_v1,
  model_name = "XGBoost V1",
  seed = 123
)

#########################################
# 14) RUN XGBOOST V4
#########################################

xgb_v4 <- run_xgb_model_fullsample(
  data = comparison_data,
  feature_cols = feature_cols_v4,
  model_name = "XGBoost V4",
  seed = 123
)

#########################################
# 15) COMMON-SAMPLE COMPARISON TABLE
#########################################

bg_baseline_summary <- summarise_bg_baseline(comparison_data)

tobit_v1_summary <- tobit_v1$summary %>%
  mutate(Method = "Tobit V1", Target = "LZ (true wages)") %>%
  select(Method, Target, Observation_Type, n_obs, RMSE, MAE, Mean_Error, SD_Error)

tobit_v4_summary <- tobit_v4$summary %>%
  mutate(Method = "Tobit V4", Target = "LZ (true wages)") %>%
  select(Method, Target, Observation_Type, n_obs, RMSE, MAE, Mean_Error, SD_Error)

xgb_v1_summary <- xgb_v1$summary %>%
  mutate(Method = "XGBoost V1", Target = "LZ (true wages)") %>%
  select(Method, Target, Observation_Type, n_obs, RMSE, MAE, Mean_Error, SD_Error)

xgb_v4_summary <- xgb_v4$summary %>%
  mutate(Method = "XGBoost V4", Target = "LZ (true wages)") %>%
  select(Method, Target, Observation_Type, n_obs, RMSE, MAE, Mean_Error, SD_Error)

comprehensive_rmse_table <- bind_rows(
  bg_baseline_summary,
  tobit_v1_summary,
  tobit_v4_summary,
  xgb_v1_summary,
  xgb_v4_summary
) %>%
  arrange(Observation_Type, RMSE)

cat("\n=== COMMON-SAMPLE RMSE COMPARISON ===\n")
print(comprehensive_rmse_table, n = Inf)

write.csv(
  comprehensive_rmse_table,
  file.path(output_dir, "comprehensive_rmse_comparison_v5.csv"),
  row.names = FALSE
)

#########################################
# 16) CORRECTED ERROR SUMMARY WITH BG BASELINE
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
  tobit_v4$evaluation %>% select(method, error, censored),
  xgb_v1$evaluation %>% select(method, error, censored),
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
  file.path(output_dir, "corrected_error_summary_with_bg_baseline_v5.csv"),
  row.names = FALSE
)

#########################################
# 16) ERROR SUMMARIES
#########################################

all_errors_corrected <- bind_rows(
  tobit_v1$evaluation %>% select(method, error, censored),
  tobit_v4$evaluation %>% select(method, error, censored),
  xgb_v1$evaluation %>% select(method, error, censored),
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
# 17) FEATURE IMPORTANCE OUTPUTS
#########################################

cat("\n=== Top Features: XGBoost V1 ===\n")
print(head(xgb_v1$importance, 15))

cat("\n=== Top Features: XGBoost V4 ===\n")
print(head(xgb_v4$importance, 25))

save(
  tobit_v1, tobit_v4, xgb_v1, xgb_v4,
  file = file.path(output_dir, "model_objects_v5.RData")
)

#########################################
# 18) SIMPLE PLOTS
#########################################

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
# PREDICTED WAGE DISTRIBUTION PLOTS:
# BG BASELINE + 4 MODELS + LZ TRUE WAGE
#########################################

predicted_wages_full <- bind_rows(
  # comparison_data %>%
  #   transmute(
  #     method = "BG Baseline",
  #     predicted_wage = bg_dwage_log,
  #     censored = bg_censored_flag
  #   ),
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
  # tobit_v4$data %>%
  #   transmute(
  #     method = "Tobit V4",
  #     predicted_wage = model_prediction,
  #     censored = bg_censored_flag
  #   ),
  xgb_v1$data %>%
    transmute(
      method = "XGBoost V1",
      predicted_wage = xgb_prediction,
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

predicted_wages_full_censored <- predicted_wages_full %>%
  filter(censored == 1)

predicted_wages_full_uncensored <- predicted_wages_full %>%
  filter(censored == 0)

color_scheme_full <- c(
  "BG Baseline" = "black",
  "LZ True Wage" = "green",
  "Tobit V1" = "purple",
  "Tobit V4" = "brown",
  "XGBoost V1" = "red",
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

# All observations
plot_predicted_wage_density_full(
  predicted_wages_full,
  "Predicted Wage Distribution: BG, Tobit, XGBoost, and LZ",
  "All observations",
  file.path(plots_dir, "predicted_wage_density_full_all.png")
)

# Censored only
plot_predicted_wage_density_full(
  predicted_wages_full_censored,
  "Predicted Wage Distribution: BG, Tobit, XGBoost, and LZ",
  "Censored observations only",
  file.path(plots_dir, "predicted_wage_density_full_censored.png")
)

# Uncensored only
plot_predicted_wage_density_full(
  predicted_wages_full_uncensored,
  "Predicted Wage Distribution: BG, Tobit, XGBoost, and LZ",
  "Uncensored observations only",
  file.path(plots_dir, "predicted_wage_density_full_uncensored.png")
)
#########################################
# 19) OPTIONAL TEMPORAL TEST SET SCAFFOLD
#########################################
# Main comparison above is intentionally run on the same common sample
# without a random train/test split.
#
# For later work, professors suggested a temporal split:
#   train = 1994:2011
#   test  = 2012
#
# This section only creates the datasets and does not yet re-estimate
# the full model workflow on them.

temporal_train_data <- comparison_data %>%
  filter(contryear >= firstyear, contryear <= 2011)

temporal_test_data <- comparison_data %>%
  filter(contryear == 2012)

cat("\n=== Temporal split scaffold ===\n")
cat("Temporal train rows (1994-2011):", nrow(temporal_train_data), "\n")
cat("Temporal test rows (2012):", nrow(temporal_test_data), "\n")

save(
  temporal_train_data,
  temporal_test_data,
  file = file.path(output_dir, "temporal_split_scaffold_v5.RData")
)

#########################################
# 20) PREDICTION FUNCTION FOR LATEST MODEL
#########################################

predict_true_wage_v5 <- function(bg_data,
                                 model_obj = xgb_v4,
                                 feature_columns = model_obj$feature_cols) {
  prediction_features <- bg_data %>%
    select(all_of(feature_columns)) %>%
    mutate(across(everything(), ~ ifelse(is.na(.) | is.infinite(.), 0, .))) %>%
    as.matrix()

  predict(model_obj$model, xgb.DMatrix(data = prediction_features))
}

save(
  predict_true_wage_v5,
  file = file.path(output_dir, "predict_true_wage_v5_function.RData")
)

cat("\n=== V5 modeling pipeline complete ===\n")
cat("Models included: Tobit V1, Tobit V4, XGBoost V1, XGBoost V4\n")
cat("Main comparison: no random train/test split, shared common sample\n")
cat("Output folder:", output_dir, "\n")

#########################################
# 21) QUICK DATA CHECKS
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
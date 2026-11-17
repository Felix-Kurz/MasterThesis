+   mutate(
  +     mean_wage_coworkers = ifelse(
    +       firm_size > 1,
    +       (mean_wage_coworkers - dailywage_final / firm_size) * (firm_size / (firm_size - 1)),
    +       mean_wage_coworkers
    +     ),
  +     mean_censored_coworkers = ifelse(
    +       firm_size > 1,
    +       (mean_censored_coworkers - censored / firm_size) * (firm_size / (firm_size - 1)),
    +       mean_censored_coworkers
    +     )
  +   ) %>%
  +   ungroup() %>%
  +   left_join(vpi_1986 %>% select(contryear, cpi_2000), by = "contryear") %>%
  +   mutate(
    +     topcode_threshold = topcoding[contryear - 1971],
    +     daily_topcode = topcode_threshold * 14 / 30,
    +     log_daily_topcode = log(daily_topcode * cpi_2000),
    +     firm_size_square = firm_size^2 / 100,
    +     oneyear_dummy = ifelse(num_years == 1, 1, 0),
    +     oneperson_dummy = ifelse(firm_size == 1, 1, 0),
    +     smallfirm_dummy = ifelse(firm_size < 10, 1, 0),
    +     agegroup = case_when(
      +       age <= 30 ~ 1L,
      +       age > 30 & age <= 40 ~ 2L,
      +       age > 40 & age <= 50 ~ 3L,
      +       age > 50 & age <= 60 ~ 4L,
      +       TRUE ~ 5L
      +     ),
    +     bg_censored_flag = censored,
    +     interval_upper = ifelse(censored == 0, dailywage_final, log_daily_topcode),
    +     interval_lower = dailywage_final
    +   ) %>%
  +   select(-cpi_2000, -topcode_threshold, -daily_topcode, -log_daily_topcode)
> 
  > save(tobit_data, file = file.path(output_dir, "tobit_data_v4.RData"))
> 
  > cat("Tobit data rows:", nrow(tobit_data), "\n")
Tobit data rows: 326890 
> 
  > #########################################
> # 6) TOBIT IMPUTATION
  > #########################################
> 
  > imputed_data_list <- list()
> 
  > min_year <- min(tobit_data$contryear, na.rm = TRUE)
> max_year <- max(tobit_data$contryear, na.rm = TRUE)
> 
  > for (year in min_year:max_year) {
    +   cat("Processing Tobit year:", year, "\n")
    + 
      +   year_data <- tobit_data %>% filter(contryear == year)
      + 
        +   if (nrow(year_data) == 0) next
      + 
        +   year_results <- year_data %>%
          +     group_by(frau, agegroup) %>%
          +     do({
            +       grp <- .
            + 
              +       if (nrow(grp) <= 10) {
                +         out <- grp %>%
                  +           mutate(
                    +             predict = dailywage_final,
                    +             dailywage_final_orig = dailywage_final,
                    +             dailywage_final_pred = dailywage_final,
                    +             sigma_hat = 1
                    +           )
                +         return(out)
                +       }
            + 
              +       formula_str <- paste(
                +         "survival::Surv(interval_lower, interval_upper, type = 'interval2') ~",
                +         "age + mean_wage + mean_censored + firm_size + firm_size_square +",
                +         "oneyear_dummy + oneperson_dummy + smallfirm_dummy +",
                +         "mean_wage_coworkers + mean_censored_coworkers"
                +       )
              + 
                +       tryCatch({
                  +         model <- survival::survreg(as.formula(formula_str), data = grp, dist = "gaussian")
                  +         predictions <- predict(model, newdata = grp, type = "response")
                  +         sigma_hat <- model$scale
                  + 
                    +         set.seed(123)
                  +         k <- pnorm((grp$dailywage_final - predictions) / sigma_hat)
                  +         replace_vals <- predictions + sigma_hat * qnorm(k + runif(length(k)) * (1 - k))
                  + 
                    +         grp %>%
                    +           mutate(
                      +             predict = predictions,
                      +             dailywage_final_orig = dailywage_final,
                      +             dailywage_final_pred = replace_vals,
                      +             sigma_hat = sigma_hat
                      +           )
                  +       }, error = function(e) {
                    +         grp %>%
                      +           mutate(
                        +             predict = dailywage_final,
                        +             dailywage_final_orig = dailywage_final,
                        +             dailywage_final_pred = dailywage_final,
                        +             sigma_hat = 1
                        +           )
                    +       })
              +     }) %>%
          +     ungroup() %>%
          +     mutate(
            +       dailywage_final = ifelse(censored == 1, dailywage_final_pred, dailywage_final),
            +       educ_proxy = case_when(
              +         age_entry < 18 ~ 1L,
              +         age_entry >= 18 & age_entry <= 21 ~ 2L,
              +         age_entry > 21 ~ 3L,
              +         TRUE ~ NA_integer_
              +       )
            +     )
        + 
          +   save(
            +     year_results,
            +     file = file.path(results_dir, paste0("tobit_year_", year, ".RData"))
            +   )
        + 
          +   imputed_data_list[[as.character(year)]] <- year_results
          + }
Processing Tobit year: 1994 
Processing Tobit year: 1995 
Processing Tobit year: 1996 
Processing Tobit year: 1997 
Processing Tobit year: 1998 
Processing Tobit year: 1999 
Processing Tobit year: 2000 
Processing Tobit year: 2001 
Processing Tobit year: 2002 
Processing Tobit year: 2003 
Processing Tobit year: 2004 
Processing Tobit year: 2005 
Processing Tobit year: 2006 
Processing Tobit year: 2007 
Processing Tobit year: 2008 
Processing Tobit year: 2009 
Processing Tobit year: 2010 
Processing Tobit year: 2011 
Processing Tobit year: 2012 
> 
  > final_imputed_data <- bind_rows(imputed_data_list)
> 
  > save(final_imputed_data, file = file.path(output_dir, "final_imputed_data_v4.RData"))
> 
  > #########################################
> # 7) TOBIT EVALUATION
  > #########################################
> 
  > imputation_eval <- final_imputed_data %>%
  +   filter(!is.na(dailywage_final_orig), !is.na(dailywage_final_pred)) %>%
  +   mutate(
    +     was_imputed = (dailywage_final != dailywage_final_orig),
    +     residual = dailywage_final_pred - dailywage_final_orig,
    +     squared_error = residual^2
    +   )
> 
  > rmse_by_censoring <- imputation_eval %>%
  +   group_by(was_imputed) %>%
  +   summarise(
    +     rmse = sqrt(mean(squared_error, na.rm = TRUE)),
    +     mae = mean(abs(residual), na.rm = TRUE),
    +     n_obs = n(),
    +     mean_residual = mean(residual, na.rm = TRUE),
    +     median_residual = median(residual, na.rm = TRUE),
    +     sd_residual = sd(residual, na.rm = TRUE)
    +   ) %>%
  +   mutate(
    +     censoring_status = ifelse(was_imputed, "Censored (Imputed)", "Uncensored")
    +   )
> 
  > rmse_overall <- imputation_eval %>%
  +   summarise(
    +     was_imputed = NA,
    +     rmse = sqrt(mean(squared_error, na.rm = TRUE)),
    +     mae = mean(abs(residual), na.rm = TRUE),
    +     n_obs = n(),
    +     mean_residual = mean(residual, na.rm = TRUE),
    +     median_residual = median(residual, na.rm = TRUE),
    +     sd_residual = sd(residual, na.rm = TRUE),
    +     censoring_status = "Overall"
    +   )
> 
  > complete_rmse_summary <- bind_rows(rmse_by_censoring, rmse_overall)
> 
  > print(complete_rmse_summary)
# A tibble: 3 × 8
was_imputed  rmse   mae  n_obs mean_residual median_residual sd_residual censoring_status  
<lgl>       <dbl> <dbl>  <int>         <dbl>           <dbl>       <dbl> <chr>             
  1 FALSE         Inf   Inf 304664           Inf          0.0880         NaN Uncensored        
2 TRUE          Inf   Inf  22226           Inf          0.289          NaN Censored (Imputed)
3 NA            Inf   Inf 326890           Inf          0.0934         NaN Overall           
> 
  > #########################################
> # 8) TRAINING DATA FOR XGBOOST
  > #########################################
> 
  > training_data <- tobit_data %>%
  +   left_join(
    +     lz_comparison_data,
    +     by = c("penr", "benr", "contryear")
    +   ) %>%
  +   filter(!is.na(lz_dwage_log)) %>%
  +   mutate(
    +     bg_dwage_log = dailywage_final
    +   )
> 
  > cat("Training rows with BG and LZ:", nrow(training_data), "\n")
Training rows with BG and LZ: 313981 
> 
  > #########################################
> # 9) SHARED XGBOOST HELPERS
  > #########################################
> 
  > xgb_params <- list(
    +   objective = "reg:squarederror",
    +   eval_metric = "rmse",
    +   eta = 0.1,
    +   max_depth = 6,
    +   min_child_weight = 1,
    +   subsample = 0.8,
    +   colsample_bytree = 0.8,
    +   reg_alpha = 0.1,
    +   reg_lambda = 1
    + )
> 
  > prepare_matrix <- function(df, feature_cols) {
    +   df %>%
      +     select(all_of(feature_cols)) %>%
      +     mutate(across(everything(), ~ ifelse(is.na(.) | is.infinite(.), 0, .))) %>%
      +     as.matrix()
    + }
> 
  > run_xgb_model <- function(data, feature_cols, model_name, seed = 123) {
    +   X_features <- prepare_matrix(data, feature_cols)
    +   y_target <- data$lz_dwage_log
    + 
      +   set.seed(seed)
    +   n_obs <- length(y_target)
    +   train_indices <- sample(seq_len(n_obs), size = floor(0.8 * n_obs))
    + 
      +   X_train <- X_features[train_indices, , drop = FALSE]
      +   X_test  <- X_features[-train_indices, , drop = FALSE]
      +   y_train <- y_target[train_indices]
      +   y_test  <- y_target[-train_indices]
      + 
        +   bg_train <- data$bg_dwage_log[train_indices]
        +   bg_test  <- data$bg_dwage_log[-train_indices]
        + 
          +   dtrain <- xgb.DMatrix(data = X_train, label = y_train)
          +   dtest  <- xgb.DMatrix(data = X_test, label = y_test)
          + 
            +   cat("\nTraining", model_name, "...\n")
          +   cat("Features:", paste(feature_cols, collapse = ", "), "\n")
          +   cat("Training rows:", nrow(X_train), "Test rows:", nrow(X_test), "\n")
          + 
            +   set.seed(seed)
          +   xgb_cv <- xgb.cv(
            +     params = xgb_params,
            +     data = dtrain,
            +     nrounds = 1000,
            +     nfold = 5,
            +     early_stopping_rounds = 50,
            +     verbose = FALSE
            +   )
          + 
            +   best_nrounds <- xgb_cv$best_iteration
            + 
              +   set.seed(seed)
            +   xgb_model <- xgb.train(
              +     params = xgb_params,
              +     data = dtrain,
              +     nrounds = best_nrounds,
              +     watchlist = list(train = dtrain, test = dtest),
              +     verbose = FALSE
              +   )
            + 
              +   train_pred <- predict(xgb_model, dtrain)
              +   test_pred  <- predict(xgb_model, dtest)
              + 
                +   results <- list(
                  +     model_name = model_name,
                  +     feature_cols = feature_cols,
                  +     train_indices = train_indices,
                  +     X_features = X_features,
                  +     model = xgb_model,
                  +     cv = xgb_cv,
                  +     best_nrounds = best_nrounds,
                  +     train_pred = train_pred,
                  +     test_pred = test_pred,
                  +     y_train = y_train,
                  +     y_test = y_test,
                  +     bg_train = bg_train,
                  +     bg_test = bg_test,
                  +     censored_test = data$bg_censored_flag[-train_indices],
                  +     censored_train = data$bg_censored_flag[train_indices],
                  +     train_rmse_lz = sqrt(mean((train_pred - y_train)^2)),
                  +     test_rmse_lz = sqrt(mean((test_pred - y_test)^2)),
                  +     train_r2_lz = cor(train_pred, y_train)^2,
                  +     test_r2_lz = cor(test_pred, y_test)^2,
                  +     train_rmse_bg = sqrt(mean((bg_train - y_train)^2)),
                  +     test_rmse_bg = sqrt(mean((bg_test - y_test)^2)),
                  +     train_r2_bg = cor(bg_train, y_train)^2,
                  +     test_r2_bg = cor(bg_test, y_test)^2
                  +   )
                + 
                  +   results$prediction_comparison <- data.frame(
                    +     lz_true = results$y_test,
                    +     bg_observed = results$bg_test,
                    +     xgb_predicted = results$test_pred,
                    +     censored = results$censored_test,
                    +     bg_error = abs(results$bg_test - results$y_test),
                    +     xgb_error = abs(results$test_pred - results$y_test),
                    +     improvement = abs(results$bg_test - results$y_test) - abs(results$test_pred - results$y_test)
                    +   ) %>%
                    +     mutate(
                      +       bg_better = improvement < 0,
                      +       xgb_better = improvement > 0,
                      +       bg_pct_error = abs((bg_observed - lz_true) / lz_true) * 100,
                      +       xgb_pct_error = abs((xgb_predicted - lz_true) / lz_true) * 100
                      +     )
                  + 
                    +   results$importance <- xgb.importance(
                      +     feature_names = colnames(X_features),
                      +     model = xgb_model
                      +   )
                    + 
                      +   cat("Optimal rounds:", best_nrounds,
                              +       "Best CV RMSE:", min(xgb_cv$evaluation_log$test_rmse_mean), "\n")
                    + 
                      +   cat("\n===", model_name, "Performance ===\n")
                    +   cat("Train RMSE vs LZ:", round(results$train_rmse_lz, 4),
                            +       "R²:", round(results$train_r2_lz, 4), "\n")
                    +   cat("Test RMSE vs LZ:", round(results$test_rmse_lz, 4),
                            +       "R²:", round(results$test_r2_lz, 4), "\n")
                    +   cat("BG baseline Test RMSE:", round(results$test_rmse_bg, 4),
                            +       "R²:", round(results$test_r2_bg, 4), "\n")
                    +   cat("RMSE reduction vs BG:", round(results$test_rmse_bg - results$test_rmse_lz, 4), "\n")
                    + 
                      +   results
                    + }
> 
  > #########################################
> # 10) XGBOOST V1
  > #########################################
> 
  > feature_cols_v1 <- c(
    +   "age",
    +   "mean_wage",
    +   "mean_censored",
    +   "firm_size",
    +   "firm_size_square",
    +   "oneyear_dummy",
    +   "oneperson_dummy",
    +   "smallfirm_dummy",
    +   "mean_wage_coworkers",
    +   "mean_censored_coworkers"
    + )
> 
  > xgb_v1 <- run_xgb_model(
    +   data = training_data,
    +   feature_cols = feature_cols_v1,
    +   model_name = "XGBoost V1",
    +   seed = 123
    + )

Training XGBoost V1 ...
Features: age, mean_wage, mean_censored, firm_size, firm_size_square, oneyear_dummy, oneperson_dummy, smallfirm_dummy, mean_wage_coworkers, mean_censored_coworkers 
Training rows: 251184 Test rows: 62797 
Optimal rounds: 1000 Best CV RMSE: 0.1200988 

=== XGBoost V1 Performance ===
  Train RMSE vs LZ: 0.109 R²: 0.9685 
Test RMSE vs LZ: 0.1147 R²: 0.9645 
BG baseline Test RMSE: 0.2623 R²: 0.8109 
RMSE reduction vs BG: 0.1475 
> 
  > #########################################
> # 11) XGBOOST V2
  > #########################################
> 
  > feature_cols_v2 <- c(
    +   "age",
    +   "mean_wage",
    +   "mean_censored",
    +   "firm_size",
    +   "firm_size_square",
    +   "oneyear_dummy",
    +   "oneperson_dummy",
    +   "smallfirm_dummy",
    +   "mean_wage_coworkers",
    +   "mean_censored_coworkers",
    +   "frau",
    +   "birthyear",
    +   "num_years",
    +   "interval_lower",
    +   "interval_upper",
    +   "bg_censored_flag"
    + )
> 
  > xgb_v2 <- run_xgb_model(
    +   data = training_data,
    +   feature_cols = feature_cols_v2,
    +   model_name = "XGBoost V2",
    +   seed = 123
    + )

Training XGBoost V2 ...
Features: age, mean_wage, mean_censored, firm_size, firm_size_square, oneyear_dummy, oneperson_dummy, smallfirm_dummy, mean_wage_coworkers, mean_censored_coworkers, frau, birthyear, num_years, interval_lower, interval_upper, bg_censored_flag 
Training rows: 251184 Test rows: 62797 
Optimal rounds: 1000 Best CV RMSE: 0.0906782 

=== XGBoost V2 Performance ===
  Train RMSE vs LZ: 0.0764 R²: 0.9845 
Test RMSE vs LZ: 0.0843 R²: 0.9808 
BG baseline Test RMSE: 0.2623 R²: 0.8109 
RMSE reduction vs BG: 0.178 
> 
  > #########################################
> # 12) XGBOOST V4 / LATEST ACTIVE MODEL
  > #########################################
> # This is the post-split successor to the old V3.
  > # It uses the richer pre-model dataset variables.
  > 
  > feature_cols_v4 <- c(
    +   # Core wage / censoring structure
      +   "age",
    +   "exper",
    +   "tenure",
    +   "mean_wage",
    +   "mean_censored",
    +   "firm_size",
    +   "firm_size_square",
    +   "oneyear_dummy",
    +   "oneperson_dummy",
    +   "smallfirm_dummy",
    +   "mean_wage_coworkers",
    +   "mean_censored_coworkers",
    +   "interval_lower",
    +   "interval_upper",
    +   "bg_censored_flag",
    + 
      +   # Demographics
      +   "frau",
    +   "stsl_missing",
    +   "austrian_dummy",
    +   "germany_dummy",
    +   "eu_dummy",
    + 
      +   # Education
      +   "educ_cat",
    +   "educ_missing",
    +   "educ_imputed",
    + 
      +   # Job / collar / transitions
      +   "whitecollar_dummy",
    +   "bluecollar_dummy",
    +   "othercollar_dummy",
    +   "subcat",
    +   "juj",
    +   "return",
    + 
      +   # Career path
      +   "year_entry",
    +   "age_entry",
    +   "num_years",
    + 
      +   # Family
      +   "live_birth_event",
    +   "n_children",
    + 
      +   # Time / geography / industry
      +   "contryear",
    +   "bundesland",
    +   "nace08_2"
    + )
> 
  > xgb_v4 <- run_xgb_model(
    +   data = training_data,
    +   feature_cols = feature_cols_v4,
    +   model_name = "XGBoost V4 Latest",
    +   seed = 123
    + )

Training XGBoost V4 Latest ...
Features: age, exper, tenure, mean_wage, mean_censored, firm_size, firm_size_square, oneyear_dummy, oneperson_dummy, smallfirm_dummy, mean_wage_coworkers, mean_censored_coworkers, interval_lower, interval_upper, bg_censored_flag, frau, stsl_missing, austrian_dummy, germany_dummy, eu_dummy, educ_cat, educ_missing, educ_imputed, whitecollar_dummy, bluecollar_dummy, othercollar_dummy, subcat, juj, return, year_entry, age_entry, num_years, live_birth_event, n_children, contryear, bundesland, nace08_2 
Training rows: 251184 Test rows: 62797 
Optimal rounds: 1000 Best CV RMSE: 0.0684174 

=== XGBoost V4 Latest Performance ===
  Train RMSE vs LZ: 0.0575 R²: 0.9912 
Test RMSE vs LZ: 0.0642 R²: 0.9888 
BG baseline Test RMSE: 0.2623 R²: 0.8109 
RMSE reduction vs BG: 0.1981 
> 
  > cat("\n=== Comparison: V2 vs V4 Latest ===\n")

=== Comparison: V2 vs V4 Latest ===
  > cat("V2 Test RMSE:", round(xgb_v2$test_rmse_lz, 4), "R²:", round(xgb_v2$test_r2_lz, 4), "\n")
V2 Test RMSE: 0.0843 R²: 0.9808 
> cat("V4 Test RMSE:", round(xgb_v4$test_rmse_lz, 4), "R²:", round(xgb_v4$test_r2_lz, 4), "\n")
V4 Test RMSE: 0.0642 R²: 0.9888 
> cat("RMSE reduction V4 over V2:", round(xgb_v2$test_rmse_lz - xgb_v4$test_rmse_lz, 4), "\n")
RMSE reduction V4 over V2: 0.0201 
> cat("R² improvement V4 over V2:", round(xgb_v4$test_r2_lz - xgb_v2$test_r2_lz, 4), "\n")
R² improvement V4 over V2: 0.008 
> 
  > #########################################
> # 13) TOBIT CORRECTED AGAINST LZ
  > #########################################
> 
  > tobit_corrected <- final_imputed_data %>%
  +   inner_join(
    +     lz_comparison_data %>% select(penr, benr, contryear, lz_dwage_log),
    +     by = c("penr", "benr", "contryear")
    +   ) %>%
  +   mutate(
    +     error_vs_lz = dailywage_final_pred - lz_dwage_log,
    +     error_vs_censored = dailywage_final_pred - dailywage_final_orig,
    +     was_imputed = (dailywage_final != dailywage_final_orig)
    +   ) %>%
  +   filter(is.finite(error_vs_lz))
> 
  > #########################################
> # 14) COMPARATIVE ERROR SUMMARIES
  > #########################################
> 
  > tobit_errors_corrected_all <- tobit_corrected %>%
  +   transmute(error = error_vs_lz, method = "Tobit", was_imputed = was_imputed)
> 
  > tobit_errors_corrected_censored <- tobit_corrected %>%
  +   filter(was_imputed) %>%
  +   transmute(error = error_vs_lz, method = "Tobit", was_imputed = was_imputed)
> 
  > xgb_v1_errors_all <- xgb_v1$prediction_comparison %>%
  +   transmute(error = xgb_predicted - lz_true, method = "XGBoost V1", was_imputed = (censored == 1))
> 
  > xgb_v2_errors_all <- xgb_v2$prediction_comparison %>%
  +   transmute(error = xgb_predicted - lz_true, method = "XGBoost V2", was_imputed = (censored == 1))
> 
  > xgb_v4_errors_all <- xgb_v4$prediction_comparison %>%
  +   transmute(error = xgb_predicted - lz_true, method = "XGBoost V4", was_imputed = (censored == 1))
> 
  > all_errors_corrected <- bind_rows(
    +   tobit_errors_corrected_all,
    +   xgb_v1_errors_all,
    +   xgb_v2_errors_all,
    +   xgb_v4_errors_all
    + ) %>%
  +   filter(is.finite(error))
> 
  > censored_errors_corrected <- bind_rows(
    +   tobit_errors_corrected_censored,
    +   xgb_v1_errors_all %>% filter(was_imputed),
    +   xgb_v2_errors_all %>% filter(was_imputed),
    +   xgb_v4_errors_all %>% filter(was_imputed)
    + ) %>%
  +   filter(is.finite(error))
> 
  > error_summary_corrected_all <- all_errors_corrected %>%
  +   group_by(method) %>%
  +   summarise(
    +     n_obs = n(),
    +     mean_error = round(mean(error), 4),
    +     median_error = round(median(error), 4),
    +     sd_error = round(sd(error), 4),
    +     mae = round(mean(abs(error)), 4),
    +     rmse = round(sqrt(mean(error^2)), 4)
    +   ) %>%
  +   arrange(rmse)
> 
  > error_summary_corrected_censored <- censored_errors_corrected %>%
  +   group_by(method) %>%
  +   summarise(
    +     n_obs = n(),
    +     mean_error = round(mean(error), 4),
    +     median_error = round(median(error), 4),
    +     sd_error = round(sd(error), 4),
    +     mae = round(mean(abs(error)), 4),
    +     rmse = round(sqrt(mean(error^2)), 4)
    +   ) %>%
  +   arrange(rmse)
> 
  > cat("\n=== Error Summary: ALL OBSERVATIONS ===\n")

=== Error Summary: ALL OBSERVATIONS ===
  > print(error_summary_corrected_all)
# A tibble: 4 × 7
method      n_obs mean_error median_error sd_error    mae   rmse
<chr>       <int>      <dbl>        <dbl>    <dbl>  <dbl>  <dbl>
  1 XGBoost V4  62797     0.0006      -0.0029   0.0642 0.0315 0.0642
2 XGBoost V2  62797     0.0008      -0.0059   0.0843 0.0385 0.0843
3 XGBoost V1  62797     0.0004      -0.0082   0.115  0.0569 0.115 
4 Tobit      313970     0.127        0.0939   0.276  0.165  0.304 
> 
  > cat("\n=== Error Summary: CENSORED OBSERVATIONS ONLY ===\n")

=== Error Summary: CENSORED OBSERVATIONS ONLY ===
  > print(error_summary_corrected_censored)
# A tibble: 4 × 7
method     n_obs mean_error median_error sd_error    mae   rmse
<chr>      <int>      <dbl>        <dbl>    <dbl>  <dbl>  <dbl>
  1 XGBoost V4  4319    -0.0008       0.0015   0.0512 0.0373 0.0512
2 XGBoost V2  4319    -0.0017       0.0009   0.0609 0.0439 0.0609
3 XGBoost V1  4319    -0.0047      -0.0003   0.0872 0.0615 0.0873
4 Tobit      21486    -0.0002       0.0002   0.391  0.264  0.391 
> 
  > #########################################
> # 15) COMPREHENSIVE RMSE TABLE
  > #########################################
> 
  > tobit_vs_bg_all <- tobit_corrected %>%
  +   summarise(
    +     Method = "Tobit",
    +     Target = "BG (censored data)",
    +     Observation_Type = "All",
    +     n_obs = n(),
    +     RMSE = round(sqrt(mean(error_vs_censored^2)), 4),
    +     MAE = round(mean(abs(error_vs_censored)), 4),
    +     Mean_Error = round(mean(error_vs_censored), 4),
    +     SD_Error = round(sd(error_vs_censored), 4)
    +   )
> 
  > tobit_vs_bg_uncensored <- tobit_corrected %>%
  +   filter(!was_imputed) %>%
  +   summarise(
    +     Method = "Tobit",
    +     Target = "BG (censored data)",
    +     Observation_Type = "Uncensored",
    +     n_obs = n(),
    +     RMSE = round(sqrt(mean(error_vs_censored^2)), 4),
    +     MAE = round(mean(abs(error_vs_censored)), 4),
    +     Mean_Error = round(mean(error_vs_censored), 4),
    +     SD_Error = round(sd(error_vs_censored), 4)
    +   )
> 
  > tobit_vs_bg_censored <- tobit_corrected %>%
  +   filter(was_imputed) %>%
  +   summarise(
    +     Method = "Tobit",
    +     Target = "BG (censored data)",
    +     Observation_Type = "Censored",
    +     n_obs = n(),
    +     RMSE = round(sqrt(mean(error_vs_censored^2)), 4),
    +     MAE = round(mean(abs(error_vs_censored)), 4),
    +     Mean_Error = round(mean(error_vs_censored), 4),
    +     SD_Error = round(sd(error_vs_censored), 4)
    +   )
> 
  > tobit_vs_lz_all <- tobit_corrected %>%
  +   summarise(
    +     Method = "Tobit",
    +     Target = "LZ (true wages)",
    +     Observation_Type = "All",
    +     n_obs = n(),
    +     RMSE = round(sqrt(mean(error_vs_lz^2)), 4),
    +     MAE = round(mean(abs(error_vs_lz)), 4),
    +     Mean_Error = round(mean(error_vs_lz), 4),
    +     SD_Error = round(sd(error_vs_lz), 4)
    +   )
> 
  > tobit_vs_lz_uncensored <- tobit_corrected %>%
  +   filter(!was_imputed) %>%
  +   summarise(
    +     Method = "Tobit",
    +     Target = "LZ (true wages)",
    +     Observation_Type = "Uncensored",
    +     n_obs = n(),
    +     RMSE = round(sqrt(mean(error_vs_lz^2)), 4),
    +     MAE = round(mean(abs(error_vs_lz)), 4),
    +     Mean_Error = round(mean(error_vs_lz), 4),
    +     SD_Error = round(sd(error_vs_lz), 4)
    +   )
> 
  > tobit_vs_lz_censored <- tobit_corrected %>%
  +   filter(was_imputed) %>%
  +   summarise(
    +     Method = "Tobit",
    +     Target = "LZ (true wages)",
    +     Observation_Type = "Censored",
    +     n_obs = n(),
    +     RMSE = round(sqrt(mean(error_vs_lz^2)), 4),
    +     MAE = round(mean(abs(error_vs_lz)), 4),
    +     Mean_Error = round(mean(error_vs_lz), 4),
    +     SD_Error = round(sd(error_vs_lz), 4)
    +   )
> 
  > xgb_summary_block <- function(obj, method_name) {
    +   bind_rows(
      +     obj$prediction_comparison %>%
        +       summarise(
          +         Method = method_name,
          +         Target = "LZ (true wages)",
          +         Observation_Type = "All",
          +         n_obs = n(),
          +         RMSE = round(sqrt(mean((xgb_predicted - lz_true)^2)), 4),
          +         MAE = round(mean(abs(xgb_predicted - lz_true)), 4),
          +         Mean_Error = round(mean(xgb_predicted - lz_true), 4),
          +         SD_Error = round(sd(xgb_predicted - lz_true), 4)
          +       ),
      +     obj$prediction_comparison %>%
        +       filter(censored == 0) %>%
        +       summarise(
          +         Method = method_name,
          +         Target = "LZ (true wages)",
          +         Observation_Type = "Uncensored",
          +         n_obs = n(),
          +         RMSE = round(sqrt(mean((xgb_predicted - lz_true)^2)), 4),
          +         MAE = round(mean(abs(xgb_predicted - lz_true)), 4),
          +         Mean_Error = round(mean(xgb_predicted - lz_true), 4),
          +         SD_Error = round(sd(xgb_predicted - lz_true), 4)
          +       ),
      +     obj$prediction_comparison %>%
        +       filter(censored == 1) %>%
        +       summarise(
          +         Method = method_name,
          +         Target = "LZ (true wages)",
          +         Observation_Type = "Censored",
          +         n_obs = n(),
          +         RMSE = round(sqrt(mean((xgb_predicted - lz_true)^2)), 4),
          +         MAE = round(mean(abs(xgb_predicted - lz_true)), 4),
          +         Mean_Error = round(mean(xgb_predicted - lz_true), 4),
          +         SD_Error = round(sd(xgb_predicted - lz_true), 4)
          +       )
      +   )
    + }
> 
  > comprehensive_rmse_table <- bind_rows(
    +   tobit_vs_bg_all,
    +   tobit_vs_bg_uncensored,
    +   tobit_vs_bg_censored,
    +   tobit_vs_lz_all,
    +   tobit_vs_lz_uncensored,
    +   tobit_vs_lz_censored,
    +   xgb_summary_block(xgb_v1, "XGBoost V1"),
    +   xgb_summary_block(xgb_v2, "XGBoost V2"),
    +   xgb_summary_block(xgb_v4, "XGBoost V4")
    + ) %>%
  +   arrange(Observation_Type, Method, Target)
> 
  > print(comprehensive_rmse_table, n = Inf)
# A tibble: 15 × 8
Method     Target             Observation_Type  n_obs   RMSE    MAE Mean_Error SD_Error
<chr>      <chr>              <chr>             <int>  <dbl>  <dbl>      <dbl>    <dbl>
  1 Tobit      BG (censored data) All              313970 0.190  0.132      0.132    0.137 
2 Tobit      LZ (true wages)    All              313970 0.304  0.165      0.127    0.276 
3 XGBoost V1 LZ (true wages)    All               62797 0.115  0.0569     0.0004   0.115 
4 XGBoost V2 LZ (true wages)    All               62797 0.0843 0.0385     0.0008   0.0843
5 XGBoost V4 LZ (true wages)    All               62797 0.0642 0.0315     0.0006   0.0642
6 Tobit      BG (censored data) Censored          21486 0.393  0.320      0.320    0.230 
7 Tobit      LZ (true wages)    Censored          21486 0.391  0.264     -0.0002   0.391 
8 XGBoost V1 LZ (true wages)    Censored           4319 0.0873 0.0615    -0.0047   0.0872
9 XGBoost V2 LZ (true wages)    Censored           4319 0.0609 0.0439    -0.0017   0.0609
10 XGBoost V4 LZ (true wages)    Censored           4319 0.0512 0.0373    -0.0008   0.0512
11 Tobit      BG (censored data) Uncensored       292484 0.166  0.118      0.118    0.116 
12 Tobit      LZ (true wages)    Uncensored       292484 0.296  0.157      0.136    0.263 
13 XGBoost V1 LZ (true wages)    Uncensored        58478 0.116  0.0566     0.0008   0.116 
14 XGBoost V2 LZ (true wages)    Uncensored        58478 0.0858 0.0381     0.001    0.0858
15 XGBoost V4 LZ (true wages)    Uncensored        58478 0.0651 0.0311     0.0007   0.0651
> 
  > write.csv(
    +   comprehensive_rmse_table,
    +   file.path(output_dir, "comprehensive_rmse_comparison_v4.csv"),
    +   row.names = FALSE
    + )
> 
  > #########################################
> # 16) FEATURE IMPORTANCE OUTPUTS
  > #########################################
> 
  > cat("\n=== Top Features: XGBoost V1 ===\n")

=== Top Features: XGBoost V1 ===
  > print(head(xgb_v1$importance, 15))
Feature         Gain        Cover    Frequency
<char>        <num>        <num>        <num>
  1:     mean_wage_coworkers 0.6888498976 0.3533862836 0.2587079909
2:               mean_wage 0.2111306895 0.4196824695 0.3279008973
3: mean_censored_coworkers 0.0267174586 0.0349826700 0.0272014319
4:                     age 0.0240894198 0.0479294611 0.1842632062
5:           mean_censored 0.0232159053 0.0513477644 0.0509879654
6:               firm_size 0.0226417868 0.0775878867 0.1250323827
7:        firm_size_square 0.0030586203 0.0148752555 0.0250818398
8:         smallfirm_dummy 0.0002962221 0.0002082091 0.0008242858
> 
  > cat("\n=== Top Features: XGBoost V2 ===\n")

=== Top Features: XGBoost V2 ===
  > print(head(xgb_v2$importance, 15))
Feature         Gain        Cover    Frequency
<char>        <num>        <num>        <num>
  1:          interval_lower 7.196821e-01 1.706614e-01 0.1300628531
2:          interval_upper 1.453147e-01 8.725863e-02 0.0517800821
3:               mean_wage 4.576759e-02 2.507812e-01 0.2404529055
4:     mean_wage_coworkers 2.779588e-02 1.932762e-01 0.1455378821
5:                     age 1.337241e-02 4.179560e-02 0.1271357582
6:               firm_size 9.655567e-03 4.282464e-02 0.0734042795
7:               num_years 9.255065e-03 8.457105e-02 0.0890608337
8:        bg_censored_flag 8.832585e-03 1.072146e-03 0.0004311225
9:           mean_censored 8.372663e-03 3.608978e-02 0.0329695264
10:               birthyear 7.421415e-03 6.457526e-02 0.0667786072
11:        firm_size_square 1.962469e-03 1.161463e-02 0.0190828436
12: mean_censored_coworkers 1.432831e-03 9.756018e-03 0.0117083797
13:                    frau 1.059499e-03 5.682028e-03 0.0109822786
14:         smallfirm_dummy 7.522934e-05 4.138659e-05 0.0006126478
> 
  > cat("\n=== Top Features: XGBoost V4 Latest ===\n")

=== Top Features: XGBoost V4 Latest ===
  > print(head(xgb_v4$importance, 25))
Feature         Gain        Cover    Frequency
<char>        <num>        <num>        <num>
  1:          interval_lower 0.5821215309 0.1572721770 0.0910554792
2:          interval_upper 0.1630203623 0.0797773245 0.0378735830
3:     mean_wage_coworkers 0.1043328538 0.1429288216 0.1032291309
4:               mean_wage 0.0418806522 0.1905228770 0.1462555823
5:                     age 0.0145865726 0.0294649854 0.1035726554
6:        bg_censored_flag 0.0105186165 0.0007396393 0.0003649948
7:        live_birth_event 0.0105059819 0.0149055228 0.0151794916
8:           mean_censored 0.0088589591 0.0335041831 0.0224793885
9:               firm_size 0.0083802578 0.0297256495 0.0466334593
10:                  tenure 0.0081516723 0.0168013616 0.0473849193
11:                   exper 0.0080503536 0.0267188863 0.0573686019
12:               num_years 0.0061522620 0.0693143376 0.0598806252
13:                nace08_2 0.0056247451 0.0453575986 0.0491025421
14:              year_entry 0.0051480143 0.0323465104 0.0327421848
15:               age_entry 0.0033660123 0.0211952147 0.0231449674
16:               contryear 0.0031031394 0.0206058468 0.0314110271
17:              n_children 0.0029895272 0.0236465261 0.0199458949
18:              bundesland 0.0021776103 0.0122190657 0.0260219856
19: mean_censored_coworkers 0.0017848876 0.0092247225 0.0085237032
20:                educ_cat 0.0017361825 0.0070518976 0.0170688767
21:                  return 0.0016462578 0.0109394106 0.0104560289
22:        firm_size_square 0.0014744670 0.0064702256 0.0112289591
23:                     juj 0.0012339768 0.0069853625 0.0129680522
24:                    frau 0.0009458524 0.0033425045 0.0057969770
25:            educ_imputed 0.0006126076 0.0011043099 0.0050240467
Feature         Gain        Cover    Frequency
> 
  > save(
    +   xgb_v1, xgb_v2, xgb_v4,
    +   file = file.path(output_dir, "xgboost_models_v4.RData")
    + )
> 
  > #########################################
> # 17) SIMPLE ERROR PLOTS
  > #########################################
> 
  > plot_error_density <- function(df, title, out_path) {
    +   p <- ggplot(df, aes(x = error, color = method, fill = method)) +
      +     geom_density(alpha = 0.25, size = 1.1) +
      +     geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      +     theme_minimal() +
      +     theme(legend.position = "bottom") +
      +     labs(
        +       title = title,
        +       x = "Prediction Error (ln wage units)",
        +       y = "Density",
        +       color = "Method",
        +       fill = "Method"
        +     ) +
      +     xlim(-2, 2)
    + 
      +   print(p)
    +   ggsave(out_path, plot = p, width = 10, height = 6)
    + }
> 
  > plot_error_density(
    +   all_errors_corrected,
    +   "Prediction Error Distribution - All Observations",
    +   file.path(plots_dir, "error_density_all.png")
    + )
Warning messages:
  1: Removed 1240 rows containing non-finite values (stat_density). 
2: Removed 1240 rows containing non-finite values (stat_density). 
> 
  > plot_error_density(
    +   censored_errors_corrected,
    +   "Prediction Error Distribution - Censored Observations",
    +   file.path(plots_dir, "error_density_censored.png")
    + )
Warning messages:
  1: Removed 65 rows containing non-finite values (stat_density). 
2: Removed 65 rows containing non-finite values (stat_density). 
> 
  > #########################################
> # 18) PREDICTION FUNCTION FOR LATEST MODEL
  > #########################################
> 
  > predict_true_wage_v4 <- function(bg_data,
                                     +                                  model_obj = xgb_v4,
                                     +                                  feature_columns = xgb_v4$feature_cols) {
    +   prediction_features <- bg_data %>%
      +     select(all_of(feature_columns)) %>%
      +     mutate(across(everything(), ~ ifelse(is.na(.) | is.infinite(.), 0, .))) %>%
      +     as.matrix()
    + 
      +   predict(model_obj$model, xgb.DMatrix(data = prediction_features))
    + }
> 
  > save(
    +   predict_true_wage_v4,
    +   file = file.path(output_dir, "predict_true_wage_v4_function.RData")
    + )
> 
  > cat("\n=== V4 modeling pipeline complete ===\n")

=== V4 modeling pipeline complete ===
  > cat("Latest active model: XGBoost V4\n")
Latest active model: XGBoost V4
> cat("Output folder:", output_dir, "\n")
Output folder: model_output_v4_10k 
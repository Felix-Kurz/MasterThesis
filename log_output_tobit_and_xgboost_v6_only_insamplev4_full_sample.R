> write.csv(
  +   sample_flow,
  +   file.path(output_dir, "sample_flow_v6.csv"),
  +   row.names = FALSE
  + )
> 
  > #########################################
> # 7) MODEL SPECIFICATIONS
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
    > feature_cols_v4 <- c(
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
      +   "frau",
      +   "stsl_missing",
      +   "austrian_dummy",
      +   "germany_dummy",
      +   "eu_dummy",
      +   "educ_cat",
      +   "educ_missing",
      +   "educ_imputed",
      +   "whitecollar_dummy",
      +   "bluecollar_dummy",
      +   "othercollar_dummy",
      +   "subcat",
      +   "juj",
      +   "return",
      +   "year_entry",
      +   "age_entry",
      +   "num_years",
      +   "live_birth_event",
      +   "n_children",
      +   "contryear",
      +   "bundesland",
      +   "nace08_2"
      + )
    > 
      > #########################################
    > # 8) SHARED HELPERS
      > #########################################
    > 
      > prepare_matrix <- function(df, feature_cols) {
        +   df %>%
          +     select(all_of(feature_cols)) %>%
          +     mutate(across(everything(), ~ ifelse(is.na(.) | is.infinite(.), 0, .))) %>%
          +     as.matrix()
        + }
      > 
        > build_eval_table <- function(df, pred_col, method_name) {
          +   pred_sym <- rlang::sym(pred_col)
          + 
            +   df %>%
            +     transmute(
              +       method = method_name,
              +       lz_true = lz_dwage_log,
              +       bg_observed = bg_dwage_log,
              +       pred = !!pred_sym,
              +       censored = bg_censored_flag,
              +       error = pred - lz_true,
              +       abs_error = abs(pred - lz_true),
              +       squared_error = (pred - lz_true)^2,
              +       bg_error = bg_observed - lz_true,
              +       bg_abs_error = abs(bg_observed - lz_true),
              +       bg_squared_error = (bg_observed - lz_true)^2
              +     )
          + }
        > 
          > summarise_eval <- function(eval_df) {
            +   bind_rows(
              +     eval_df %>%
                +       summarise(
                  +         Observation_Type = "All",
                  +         n_obs = n(),
                  +         RMSE = round(sqrt(mean(squared_error, na.rm = TRUE)), 4),
                  +         MAE = round(mean(abs_error, na.rm = TRUE), 4),
                  +         Mean_Error = round(mean(error, na.rm = TRUE), 4),
                  +         SD_Error = round(sd(error, na.rm = TRUE), 4)
                  +       ),
              +     eval_df %>%
                +       filter(censored == 0) %>%
                +       summarise(
                  +         Observation_Type = "Uncensored",
                  +         n_obs = n(),
                  +         RMSE = round(sqrt(mean(squared_error, na.rm = TRUE)), 4),
                  +         MAE = round(mean(abs_error, na.rm = TRUE), 4),
                  +         Mean_Error = round(mean(error, na.rm = TRUE), 4),
                  +         SD_Error = round(sd(error, na.rm = TRUE), 4)
                  +       ),
              +     eval_df %>%
                +       filter(censored == 1) %>%
                +       summarise(
                  +         Observation_Type = "Censored",
                  +         n_obs = n(),
                  +         RMSE = round(sqrt(mean(squared_error, na.rm = TRUE)), 4),
                  +         MAE = round(mean(abs_error, na.rm = TRUE), 4),
                  +         Mean_Error = round(mean(error, na.rm = TRUE), 4),
                  +         SD_Error = round(sd(error, na.rm = TRUE), 4)
                  +       )
              +   )
            + }
          > 
            > summarise_bg_baseline <- function(df, method_name = "BG Baseline") {
              +   eval_df <- df %>%
                +     transmute(
                  +       method = method_name,
                  +       lz_true = lz_dwage_log,
                  +       bg_observed = bg_dwage_log,
                  +       pred = bg_dwage_log,
                  +       censored = bg_censored_flag,
                  +       error = bg_dwage_log - lz_dwage_log,
                  +       abs_error = abs(bg_dwage_log - lz_dwage_log),
                  +       squared_error = (bg_dwage_log - lz_dwage_log)^2
                  +     )
              + 
                +   summarise_eval(eval_df) %>%
                +     mutate(Method = method_name, Target = "LZ (true wages)") %>%
                +     select(Method, Target, Observation_Type, n_obs, RMSE, MAE, Mean_Error, SD_Error)
              + }
            > 
              > build_wage_level_eval_table <- function(df, pred_col, method_name) {
                +   pred_sym <- rlang::sym(pred_col)
                + 
                  +   df %>%
                  +     transmute(
                    +       method = method_name,
                    +       lz_true_log = lz_dwage_log,
                    +       pred_log = !!pred_sym,
                    +       censored = bg_censored_flag
                    +     ) %>%
                  +     mutate(
                    +       true_wage = exp(lz_true_log),
                    +       pred_wage = exp(pred_log),
                    +       error = pred_wage - true_wage,
                    +       abs_error = abs(error),
                    +       squared_error = error^2
                    +     ) %>%
                  +     filter(
                    +       is.finite(true_wage),
                    +       is.finite(pred_wage),
                    +       is.finite(error)
                    +     )
                + }
              > 
                > summarise_wage_level_eval <- function(eval_df) {
                  +   bind_rows(
                    +     eval_df %>%
                      +       summarise(
                        +         Observation_Type = "All",
                        +         n_obs = n(),
                        +         Wage_RMSE = round(sqrt(mean(squared_error, na.rm = TRUE)), 2),
                        +         Wage_MAE = round(mean(abs_error, na.rm = TRUE), 2),
                        +         Wage_Median_AE = round(median(abs_error, na.rm = TRUE), 2),
                        +         Mean_Error = round(mean(error, na.rm = TRUE), 2),
                        +         SD_Error = round(sd(error, na.rm = TRUE), 2),
                        +         Mean_True_Wage = round(mean(true_wage, na.rm = TRUE), 2),
                        +         Median_True_Wage = round(median(true_wage, na.rm = TRUE), 2)
                        +       ),
                    +     eval_df %>%
                      +       filter(censored == 0) %>%
                      +       summarise(
                        +         Observation_Type = "Uncensored",
                        +         n_obs = n(),
                        +         Wage_RMSE = round(sqrt(mean(squared_error, na.rm = TRUE)), 2),
                        +         Wage_MAE = round(mean(abs_error, na.rm = TRUE), 2),
                        +         Wage_Median_AE = round(median(abs_error, na.rm = TRUE), 2),
                        +         Mean_Error = round(mean(error, na.rm = TRUE), 2),
                        +         SD_Error = round(sd(error, na.rm = TRUE), 2),
                        +         Mean_True_Wage = round(mean(true_wage, na.rm = TRUE), 2),
                        +         Median_True_Wage = round(median(true_wage, na.rm = TRUE), 2)
                        +       ),
                    +     eval_df %>%
                      +       filter(censored == 1) %>%
                      +       summarise(
                        +         Observation_Type = "Censored",
                        +         n_obs = n(),
                        +         Wage_RMSE = round(sqrt(mean(squared_error, na.rm = TRUE)), 2),
                        +         Wage_MAE = round(mean(abs_error, na.rm = TRUE), 2),
                        +         Wage_Median_AE = round(median(abs_error, na.rm = TRUE), 2),
                        +         Mean_Error = round(mean(error, na.rm = TRUE), 2),
                        +         SD_Error = round(sd(error, na.rm = TRUE), 2),
                        +         Mean_True_Wage = round(mean(true_wage, na.rm = TRUE), 2),
                        +         Median_True_Wage = round(median(true_wage, na.rm = TRUE), 2)
                        +       )
                    +   )
                  + }
                > 
                  > summarise_wage_level_model <- function(df, pred_col, method_name) {
                    +   eval_df <- build_wage_level_eval_table(df, pred_col, method_name)
                    + 
                      +   summarise_wage_level_eval(eval_df) %>%
                      +     mutate(
                        +       Method = method_name,
                        +       Target = "LZ daily wages (level)"
                        +     ) %>%
                      +     select(
                        +       Method, Target, Observation_Type, n_obs,
                        +       Wage_RMSE, Wage_MAE, Wage_Median_AE,
                        +       Mean_Error, SD_Error, Mean_True_Wage, Median_True_Wage
                        +     )
                    + }
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
                      > #########################################
                    > # 9) TOBIT HELPER
                      > #########################################
                    > 
                      > run_tobit_model <- function(data, feature_cols, model_name) {
                        +   cat("\nRunning", model_name, "...\n")
                        +   cat("Features:", paste(feature_cols, collapse = ", "), "\n")
                        +   cat("Rows:", nrow(data), "\n")
                        + 
                          +   formula_str <- paste(
                            +     "survival::Surv(interval_lower, interval_upper, type = 'interval2') ~",
                            +     paste(feature_cols, collapse = " + ")
                            +   )
                          + 
                            +   model <- survival::survreg(
                              +     as.formula(formula_str),
                              +     data = data,
                              +     dist = "gaussian"
                              +   )
                            + 
                              +   predictions <- predict(model, newdata = data, type = "response")
                              +   sigma_hat <- model$scale
                              + 
                                +   set.seed(123)
                              +   k <- pnorm((data$dailywage_final - predictions) / sigma_hat)
                              +   replace_vals <- predictions + sigma_hat * qnorm(k + runif(length(k)) * (1 - k))
                              + 
                                +   results_data <- data %>%
                                  +     mutate(
                                    +       tobit_linear_pred = predictions,
                                    +       dailywage_final_orig = dailywage_final,
                                    +       tobit_imputed_pred = replace_vals,
                                    +       model_prediction = ifelse(bg_censored_flag == 1, tobit_imputed_pred, tobit_linear_pred)
                                    +     )
                                + 
                                  +   eval_df <- build_eval_table(results_data, "model_prediction", model_name)
                                  +   summary_table <- summarise_eval(eval_df)
                                  + 
                                    +   cat("\n===", model_name, "Performance on common comparison sample ===\n")
                                  +   print(summary_table)
                                  + 
                                    +   list(
                                      +     model_name = model_name,
                                      +     feature_cols = feature_cols,
                                      +     formula = formula_str,
                                      +     model = model,
                                      +     sigma_hat = sigma_hat,
                                      +     data = results_data,
                                      +     evaluation = eval_df,
                                      +     summary = summary_table
                                      +   )
                                  + }
                      > 
                        > #########################################
                      > # 10) XGBOOST HELPER
                        > #########################################
                      > 
                        > xgb_params <- list(
                          +   # Use 60 threads within the 64-CPU allocation, leaving some CPU headroom.
                            +   nthread = 60L,
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
                          > run_xgb_model_fullsample <- function(data, feature_cols, model_name, seed = 123) {
                            +   X_features <- prepare_matrix(data, feature_cols)
                            +   y_target <- data$lz_dwage_log
                            + 
                              +   dtrain <- xgb.DMatrix(data = X_features, label = y_target)
                              + 
                                +   cat("\nTraining", model_name, "on full common sample...\n")
                              +   cat("Features:", paste(feature_cols, collapse = ", "), "\n")
                              +   cat("Rows:", nrow(X_features), "\n")
                              + 
                                +   # CV reports train/test RMSE each round; each round fits all five folds.
                                +   set.seed(seed)
                              +   xgb_cv <- xgb.cv(
                                +     params = xgb_params,
                                +     data = dtrain,
                                +     nrounds = 1000,
                                +     nfold = 5,
                                +     early_stopping_rounds = 50,
                                +     verbose = 1,
                                +     print_every_n = 1
                                +   )
                              + 
                                +   best_nrounds <- xgb_cv$best_iteration
                                + 
                                  +   # Final full-sample fit reports training RMSE each round.
                                  +   set.seed(seed)
                                +   xgb_model <- xgb.train(
                                  +     params = xgb_params,
                                  +     data = dtrain,
                                  +     nrounds = best_nrounds,
                                  +     watchlist = list(train = dtrain),
                                  +     verbose = 1,
                                  +     print_every_n = 1
                                  +   )
                                + 
                                  +   full_pred <- predict(xgb_model, dtrain)
                                  + 
                                    +   results_data <- data %>%
                                      +     mutate(
                                        +       xgb_prediction = full_pred
                                        +     )
                                    + 
                                      +   eval_df <- build_eval_table(results_data, "xgb_prediction", model_name)
                                      +   summary_table <- summarise_eval(eval_df)
                                      + 
                                        +   importance <- xgb.importance(
                                          +     feature_names = colnames(X_features),
                                          +     model = xgb_model
                                          +   )
                                        + 
                                          +   cat("Optimal rounds:", best_nrounds,
                                                  +       "Best CV RMSE:", min(xgb_cv$evaluation_log$test_rmse_mean), "\n")
                                        + 
                                          +   cat("\n===", model_name, "Performance on common comparison sample ===\n")
                                        +   print(summary_table)
                                        + 
                                          +   list(
                                            +     model_name = model_name,
                                            +     feature_cols = feature_cols,
                                            +     X_features = X_features,
                                            +     model = xgb_model,
                                            +     cv = xgb_cv,
                                            +     best_nrounds = best_nrounds,
                                            +     predictions = full_pred,
                                            +     data = results_data,
                                            +     evaluation = eval_df,
                                            +     summary = summary_table,
                                            +     importance = importance
                                            +   )
                                        + }
                          > 
                            > #########################################
                          > # 11) RUN TOBIT V1
                            > #########################################
                          > 
                            > tobit_v1 <- timed_model_run(
                              +   "In-sample common comparison",
                              +   "Tobit V1",
                              +   run_tobit_model(
                                +     data = comparison_data,
                                +     feature_cols = feature_cols_v1,
                                +     model_name = "Tobit V1"
                                +   )
                              + )
                            
                            === TIMER START: In-sample common comparison - Tobit V1 ===
                              Start: 2026-09-23 17:41:47 
                            
                            Running Tobit V1 ...
                            Features: age, mean_wage, mean_censored, firm_size, firm_size_square, oneyear_dummy, oneperson_dummy, smallfirm_dummy, mean_wage_coworkers, mean_censored_coworkers 
                            Rows: 50100141 
                            
                            === Tobit V1 Performance on common comparison sample ===
                              # A tibble: 3 × 6
                              Observation_Type    n_obs    RMSE     MAE Mean_Error SD_Error
                            <chr>               <int>   <dbl>   <dbl>      <dbl>    <dbl>
                              1 All              50100141 Inf     Inf        Inf      NaN    
                            2 Uncensored       46509005   0.353   0.206      0.032    0.352
                            3 Censored          3591136 Inf     Inf        Inf      NaN    
                            End: 2026-09-23 18:08:55 
                            Runtime: 1627.80 seconds / 27.13 minutes / 0.45 hours 
                            === TIMER END: In-sample common comparison - Tobit V1 ===
                              > 
                              > #########################################
                            > # 12) RUN XGBOOST V4
                              > #########################################
                            > 
                              > xgb_v4 <- timed_model_run(
                                +   "In-sample common comparison",
                                +   "XGBoost V4",
                                +   run_xgb_model_fullsample(
                                  +     data = comparison_data,
                                  +     feature_cols = feature_cols_v4,
                                  +     model_name = "XGBoost V4",
                                  +     seed = 123
                                  +   )
                                + )
                            
                            === TIMER START: In-sample common comparison - XGBoost V4 ===
                              Start: 2026-09-23 18:08:55 
                            
                            Training XGBoost V4 on full common sample...
                            Features: age, exper, tenure, mean_wage, mean_censored, firm_size, firm_size_square, oneyear_dummy, oneperson_dummy, smallfirm_dummy, mean_wage_coworkers, mean_censored_coworkers, interval_lower, interval_upper, bg_censored_flag, frau, stsl_missing, austrian_dummy, germany_dummy, eu_dummy, educ_cat, educ_missing, educ_imputed, whitecollar_dummy, bluecollar_dummy, othercollar_dummy, subcat, juj, return, year_entry, age_entry, num_years, live_birth_event, n_children, contryear, bundesland, nace08_2 
                            Rows: 50100141 
                            [1]	train-rmse:3.256364+0.000029	test-rmse:3.256526+0.000189 
                            Multiple eval metrics are present. Will use test_rmse for early stopping.
                            Will train until test_rmse hasn't improved in 50 rounds.
                            
                            [2]	train-rmse:2.932944+0.000059	test-rmse:2.932967+0.000164 
                            [3]	train-rmse:2.641968+0.000038	test-rmse:2.642000+0.000187 
                            [4]	train-rmse:2.380205+0.000076	test-rmse:2.380347+0.000164 
                            [5]	train-rmse:2.145109+0.000077	test-rmse:2.145121+0.000180 
                            [6]	train-rmse:1.933716+0.000075	test-rmse:1.933739+0.000202 
                            [7]	train-rmse:1.743834+0.000088	test-rmse:1.743813+0.000173 
                            [8]	train-rmse:1.573259+0.000066	test-rmse:1.573243+0.000162 
                            [9]	train-rmse:1.420133+0.000069	test-rmse:1.420138+0.000180 
                            [10]	train-rmse:1.283027+0.000353	test-rmse:1.283036+0.000460 
                            [11]	train-rmse:1.160095+0.000712	test-rmse:1.160108+0.000782 
                            [12]	train-rmse:1.049755+0.000624	test-rmse:1.049763+0.000700 
                            [13]	train-rmse:0.951013+0.000550	test-rmse:0.951025+0.000618 
                            [14]	train-rmse:0.862808+0.000504	test-rmse:0.862816+0.000570 
                            [15]	train-rmse:0.784094+0.000466	test-rmse:0.784106+0.000528 
                            [16]	train-rmse:0.714078+0.000584	test-rmse:0.714088+0.000630 
                            [17]	train-rmse:0.651768+0.000507	test-rmse:0.651776+0.000552 
                            [18]	train-rmse:0.596520+0.000448	test-rmse:0.596532+0.000487 
                            [19]	train-rmse:0.547701+0.000408	test-rmse:0.547714+0.000443 
                            [20]	train-rmse:0.504688+0.000351	test-rmse:0.504700+0.000386 
                            [21]	train-rmse:0.466929+0.000309	test-rmse:0.466944+0.000346 
                            [22]	train-rmse:0.433915+0.000278	test-rmse:0.433930+0.000314 
                            [23]	train-rmse:0.405178+0.000251	test-rmse:0.405196+0.000285 
                            [24]	train-rmse:0.380296+0.000233	test-rmse:0.380317+0.000270 
                            [25]	train-rmse:0.358855+0.000221	test-rmse:0.358878+0.000253 
                            [26]	train-rmse:0.340502+0.000196	test-rmse:0.340527+0.000240 
                            [27]	train-rmse:0.324842+0.000197	test-rmse:0.324869+0.000236 
                            [28]	train-rmse:0.311561+0.000198	test-rmse:0.311589+0.000231 
                            [29]	train-rmse:0.300366+0.000189	test-rmse:0.300396+0.000230 
                            [30]	train-rmse:0.290967+0.000191	test-rmse:0.291001+0.000229 
                            [31]	train-rmse:0.283107+0.000187	test-rmse:0.283144+0.000226 
                            [32]	train-rmse:0.276561+0.000186	test-rmse:0.276603+0.000228 
                            [33]	train-rmse:0.271114+0.000193	test-rmse:0.271159+0.000216 
                            [34]	train-rmse:0.266622+0.000204	test-rmse:0.266667+0.000213 
                            [35]	train-rmse:0.262876+0.000213	test-rmse:0.262926+0.000200 
                            [36]	train-rmse:0.259783+0.000218	test-rmse:0.259839+0.000210 
                            [37]	train-rmse:0.257236+0.000219	test-rmse:0.257295+0.000213 
                            [38]	train-rmse:0.255136+0.000227	test-rmse:0.255195+0.000208 
                            [39]	train-rmse:0.253360+0.000195	test-rmse:0.253437+0.000242 
                            [40]	train-rmse:0.251924+0.000174	test-rmse:0.252012+0.000267 
                            [41]	train-rmse:0.250762+0.000175	test-rmse:0.250845+0.000266 
                            [42]	train-rmse:0.249788+0.000162	test-rmse:0.249861+0.000272 
                            [43]	train-rmse:0.248975+0.000166	test-rmse:0.249048+0.000275 
                            [44]	train-rmse:0.248303+0.000175	test-rmse:0.248387+0.000271 
                            [45]	train-rmse:0.247713+0.000167	test-rmse:0.247813+0.000289 
                            [46]	train-rmse:0.247234+0.000165	test-rmse:0.247353+0.000305 
                            [47]	train-rmse:0.246835+0.000166	test-rmse:0.246975+0.000311 
                            [48]	train-rmse:0.246501+0.000170	test-rmse:0.246662+0.000308 
                            [49]	train-rmse:0.246214+0.000170	test-rmse:0.246395+0.000311 
                            [50]	train-rmse:0.245934+0.000176	test-rmse:0.246132+0.000306 
                            [51]	train-rmse:0.245709+0.000198	test-rmse:0.245925+0.000322 
                            [52]	train-rmse:0.245522+0.000186	test-rmse:0.245749+0.000325 
                            [53]	train-rmse:0.245326+0.000212	test-rmse:0.245562+0.000307 
                            [54]	train-rmse:0.245175+0.000189	test-rmse:0.245418+0.000302 
                            [55]	train-rmse:0.245043+0.000195	test-rmse:0.245290+0.000299 
                            [56]	train-rmse:0.244942+0.000200	test-rmse:0.245196+0.000293 
                            [57]	train-rmse:0.244821+0.000183	test-rmse:0.245076+0.000299 
                            [58]	train-rmse:0.244721+0.000197	test-rmse:0.244979+0.000315 
                            [59]	train-rmse:0.244630+0.000182	test-rmse:0.244888+0.000314 
                            [60]	train-rmse:0.244545+0.000187	test-rmse:0.244805+0.000324 
                            [61]	train-rmse:0.244459+0.000173	test-rmse:0.244720+0.000326 
                            [62]	train-rmse:0.244404+0.000175	test-rmse:0.244664+0.000322 
                            [63]	train-rmse:0.244347+0.000180	test-rmse:0.244608+0.000319 
                            [64]	train-rmse:0.244297+0.000175	test-rmse:0.244558+0.000319 
                            [65]	train-rmse:0.244239+0.000172	test-rmse:0.244500+0.000319 
                            [66]	train-rmse:0.244182+0.000166	test-rmse:0.244444+0.000322 
                            [67]	train-rmse:0.244133+0.000159	test-rmse:0.244395+0.000339 
                            [68]	train-rmse:0.244094+0.000156	test-rmse:0.244357+0.000340 
                            [69]	train-rmse:0.244035+0.000132	test-rmse:0.244298+0.000351 
                            [70]	train-rmse:0.243997+0.000138	test-rmse:0.244261+0.000350 
                            [71]	train-rmse:0.243942+0.000145	test-rmse:0.244207+0.000346 
                            [72]	train-rmse:0.243912+0.000143	test-rmse:0.244177+0.000350 
                            [73]	train-rmse:0.243866+0.000126	test-rmse:0.244131+0.000357 
                            [74]	train-rmse:0.243821+0.000129	test-rmse:0.244089+0.000349 
                            [75]	train-rmse:0.243763+0.000117	test-rmse:0.244030+0.000359 
                            [76]	train-rmse:0.243719+0.000128	test-rmse:0.243987+0.000358 
                            [77]	train-rmse:0.243680+0.000134	test-rmse:0.243949+0.000362 
                            [78]	train-rmse:0.243656+0.000134	test-rmse:0.243925+0.000364 
                            [79]	train-rmse:0.243616+0.000139	test-rmse:0.243886+0.000368 
                            [80]	train-rmse:0.243575+0.000125	test-rmse:0.243846+0.000372 
                            [81]	train-rmse:0.243549+0.000125	test-rmse:0.243820+0.000369 
                            [82]	train-rmse:0.243512+0.000134	test-rmse:0.243784+0.000370 
                            [83]	train-rmse:0.243477+0.000118	test-rmse:0.243749+0.000379 
                            [84]	train-rmse:0.243450+0.000119	test-rmse:0.243724+0.000373 
                            [85]	train-rmse:0.243414+0.000120	test-rmse:0.243688+0.000380 
                            [86]	train-rmse:0.243383+0.000113	test-rmse:0.243658+0.000384 
                            [87]	train-rmse:0.243359+0.000117	test-rmse:0.243636+0.000383 
                            [88]	train-rmse:0.243330+0.000118	test-rmse:0.243608+0.000386 
                            [89]	train-rmse:0.243308+0.000112	test-rmse:0.243587+0.000389 
                            [90]	train-rmse:0.243281+0.000114	test-rmse:0.243562+0.000388 
                            [91]	train-rmse:0.243247+0.000106	test-rmse:0.243528+0.000399 
                            [92]	train-rmse:0.243221+0.000097	test-rmse:0.243504+0.000408 
                            [93]	train-rmse:0.243193+0.000102	test-rmse:0.243478+0.000407 
                            [94]	train-rmse:0.243160+0.000109	test-rmse:0.243446+0.000411 
                            [95]	train-rmse:0.243141+0.000106	test-rmse:0.243427+0.000415 
                            [96]	train-rmse:0.243113+0.000104	test-rmse:0.243401+0.000419 
                            [97]	train-rmse:0.243089+0.000107	test-rmse:0.243379+0.000419 
                            [98]	train-rmse:0.243062+0.000112	test-rmse:0.243352+0.000418 
                            [99]	train-rmse:0.243032+0.000116	test-rmse:0.243323+0.000414 
                            [100]	train-rmse:0.242988+0.000114	test-rmse:0.243280+0.000408 
                            [101]	train-rmse:0.242948+0.000112	test-rmse:0.243242+0.000420 
                            [102]	train-rmse:0.242925+0.000107	test-rmse:0.243222+0.000424 
                            [103]	train-rmse:0.242910+0.000107	test-rmse:0.243207+0.000424 
                            [104]	train-rmse:0.242877+0.000106	test-rmse:0.243176+0.000426 
                            [105]	train-rmse:0.242852+0.000104	test-rmse:0.243153+0.000418 
                            [106]	train-rmse:0.242835+0.000104	test-rmse:0.243138+0.000418 
                            [107]	train-rmse:0.242810+0.000102	test-rmse:0.243114+0.000418 
                            [108]	train-rmse:0.242789+0.000104	test-rmse:0.243095+0.000414 
                            [109]	train-rmse:0.242761+0.000102	test-rmse:0.243068+0.000401 
                            [110]	train-rmse:0.242739+0.000099	test-rmse:0.243048+0.000406 
                            [111]	train-rmse:0.242719+0.000098	test-rmse:0.243029+0.000409 
                            [112]	train-rmse:0.242689+0.000101	test-rmse:0.243000+0.000406 
                            [113]	train-rmse:0.242671+0.000103	test-rmse:0.242985+0.000406 
                            [114]	train-rmse:0.242647+0.000099	test-rmse:0.242961+0.000409 
                            [115]	train-rmse:0.242627+0.000102	test-rmse:0.242943+0.000411 
                            [116]	train-rmse:0.242607+0.000100	test-rmse:0.242923+0.000410 
                            [117]	train-rmse:0.242587+0.000101	test-rmse:0.242904+0.000410 
                            [118]	train-rmse:0.242566+0.000103	test-rmse:0.242884+0.000406 
                            [119]	train-rmse:0.242535+0.000100	test-rmse:0.242854+0.000407 
                            [120]	train-rmse:0.242512+0.000102	test-rmse:0.242830+0.000403 
                            [121]	train-rmse:0.242479+0.000103	test-rmse:0.242800+0.000395 
                            [122]	train-rmse:0.242453+0.000104	test-rmse:0.242776+0.000388 
                            [123]	train-rmse:0.242426+0.000102	test-rmse:0.242751+0.000380 
                            [124]	train-rmse:0.242414+0.000099	test-rmse:0.242740+0.000382 
                            [125]	train-rmse:0.242391+0.000098	test-rmse:0.242718+0.000387 
                            [126]	train-rmse:0.242372+0.000099	test-rmse:0.242700+0.000391 
                            [127]	train-rmse:0.242355+0.000095	test-rmse:0.242684+0.000394 
                            [128]	train-rmse:0.242336+0.000096	test-rmse:0.242667+0.000394 
                            [129]	train-rmse:0.242316+0.000094	test-rmse:0.242648+0.000391 
                            [130]	train-rmse:0.242300+0.000097	test-rmse:0.242633+0.000388 
                            [131]	train-rmse:0.242283+0.000099	test-rmse:0.242616+0.000390 
                            [132]	train-rmse:0.242264+0.000102	test-rmse:0.242600+0.000386 
                            [133]	train-rmse:0.242248+0.000102	test-rmse:0.242584+0.000389 
                            [134]	train-rmse:0.242224+0.000104	test-rmse:0.242561+0.000387 
                            [135]	train-rmse:0.242202+0.000099	test-rmse:0.242542+0.000393 
                            [136]	train-rmse:0.242186+0.000100	test-rmse:0.242526+0.000392 
                            [137]	train-rmse:0.242164+0.000099	test-rmse:0.242505+0.000390 
                            [138]	train-rmse:0.242142+0.000099	test-rmse:0.242485+0.000388 
                            [139]	train-rmse:0.242125+0.000099	test-rmse:0.242469+0.000386 
                            [140]	train-rmse:0.242102+0.000106	test-rmse:0.242448+0.000378 
                            [141]	train-rmse:0.242083+0.000105	test-rmse:0.242430+0.000378 
                            [142]	train-rmse:0.242065+0.000104	test-rmse:0.242414+0.000380 
                            [143]	train-rmse:0.242049+0.000105	test-rmse:0.242399+0.000381 
                            [144]	train-rmse:0.242028+0.000102	test-rmse:0.242380+0.000383 
                            [145]	train-rmse:0.242012+0.000104	test-rmse:0.242364+0.000380 
                            [146]	train-rmse:0.241997+0.000104	test-rmse:0.242350+0.000382 
                            [147]	train-rmse:0.241982+0.000103	test-rmse:0.242336+0.000383 
                            [148]	train-rmse:0.241969+0.000102	test-rmse:0.242325+0.000385 
                            [149]	train-rmse:0.241955+0.000099	test-rmse:0.242312+0.000386 
                            [150]	train-rmse:0.241940+0.000098	test-rmse:0.242299+0.000387 
                            [151]	train-rmse:0.241923+0.000104	test-rmse:0.242282+0.000377 
                            [152]	train-rmse:0.241906+0.000104	test-rmse:0.242267+0.000379 
                            [153]	train-rmse:0.241888+0.000106	test-rmse:0.242252+0.000378 
                            [154]	train-rmse:0.241876+0.000107	test-rmse:0.242240+0.000376 
                            [155]	train-rmse:0.241859+0.000107	test-rmse:0.242225+0.000376 
                            [156]	train-rmse:0.241847+0.000106	test-rmse:0.242214+0.000375 
                            [157]	train-rmse:0.241833+0.000106	test-rmse:0.242201+0.000376 
                            [158]	train-rmse:0.241819+0.000108	test-rmse:0.242189+0.000377 
                            [159]	train-rmse:0.241797+0.000101	test-rmse:0.242168+0.000383 
                            [160]	train-rmse:0.241784+0.000101	test-rmse:0.242155+0.000386 
                            [161]	train-rmse:0.241773+0.000103	test-rmse:0.242145+0.000386 
                            [162]	train-rmse:0.241756+0.000106	test-rmse:0.242130+0.000383 
                            [163]	train-rmse:0.241738+0.000109	test-rmse:0.242113+0.000379 
                            [164]	train-rmse:0.241725+0.000111	test-rmse:0.242101+0.000377 
                            [165]	train-rmse:0.241710+0.000113	test-rmse:0.242088+0.000375 
                            [166]	train-rmse:0.241694+0.000115	test-rmse:0.242074+0.000373 
                            [167]	train-rmse:0.241683+0.000113	test-rmse:0.242064+0.000375 
                            [168]	train-rmse:0.241669+0.000111	test-rmse:0.242051+0.000377 
                            [169]	train-rmse:0.241659+0.000109	test-rmse:0.242041+0.000380 
                            [170]	train-rmse:0.241642+0.000111	test-rmse:0.242027+0.000375 
                            [171]	train-rmse:0.241625+0.000110	test-rmse:0.242012+0.000375 
                            [172]	train-rmse:0.241612+0.000111	test-rmse:0.242000+0.000373 
                            [173]	train-rmse:0.241591+0.000115	test-rmse:0.241981+0.000371 
                            [174]	train-rmse:0.241578+0.000107	test-rmse:0.241970+0.000376 
                            [175]	train-rmse:0.241567+0.000106	test-rmse:0.241960+0.000375 
                            [176]	train-rmse:0.241552+0.000107	test-rmse:0.241946+0.000376 
                            [177]	train-rmse:0.241540+0.000106	test-rmse:0.241935+0.000379 
                            [178]	train-rmse:0.241524+0.000108	test-rmse:0.241920+0.000378 
                            [179]	train-rmse:0.241509+0.000112	test-rmse:0.241907+0.000375 
                            [180]	train-rmse:0.241498+0.000108	test-rmse:0.241898+0.000378 
                            [181]	train-rmse:0.241484+0.000109	test-rmse:0.241885+0.000378 
                            [182]	train-rmse:0.241473+0.000105	test-rmse:0.241874+0.000382 
                            [183]	train-rmse:0.241461+0.000106	test-rmse:0.241863+0.000380 
                            [184]	train-rmse:0.241442+0.000106	test-rmse:0.241845+0.000379 
                            [185]	train-rmse:0.241428+0.000108	test-rmse:0.241832+0.000379 
                            [186]	train-rmse:0.241415+0.000108	test-rmse:0.241820+0.000379 
                            [187]	train-rmse:0.241405+0.000110	test-rmse:0.241812+0.000379 
                            [188]	train-rmse:0.241395+0.000107	test-rmse:0.241803+0.000382 
                            [189]	train-rmse:0.241386+0.000110	test-rmse:0.241794+0.000378 
                            [190]	train-rmse:0.241371+0.000109	test-rmse:0.241780+0.000378 
                            [191]	train-rmse:0.241356+0.000112	test-rmse:0.241766+0.000373 
                            [192]	train-rmse:0.241344+0.000113	test-rmse:0.241755+0.000372 
                            [193]	train-rmse:0.241332+0.000111	test-rmse:0.241744+0.000372 
                            [194]	train-rmse:0.241317+0.000113	test-rmse:0.241731+0.000372 
                            [195]	train-rmse:0.241305+0.000111	test-rmse:0.241720+0.000374 
                            [196]	train-rmse:0.241291+0.000112	test-rmse:0.241707+0.000377 
                            [197]	train-rmse:0.241282+0.000114	test-rmse:0.241700+0.000375 
                            [198]	train-rmse:0.241270+0.000114	test-rmse:0.241689+0.000375 
                            [199]	train-rmse:0.241260+0.000113	test-rmse:0.241681+0.000375 
                            [200]	train-rmse:0.241248+0.000113	test-rmse:0.241670+0.000375 
                            [201]	train-rmse:0.241235+0.000112	test-rmse:0.241658+0.000376 
                            [202]	train-rmse:0.241220+0.000114	test-rmse:0.241644+0.000373 
                            [203]	train-rmse:0.241210+0.000116	test-rmse:0.241635+0.000372 
                            [204]	train-rmse:0.241198+0.000111	test-rmse:0.241624+0.000376 
                            [205]	train-rmse:0.241187+0.000111	test-rmse:0.241615+0.000377 
                            [206]	train-rmse:0.241171+0.000112	test-rmse:0.241600+0.000375 
                            [207]	train-rmse:0.241162+0.000111	test-rmse:0.241592+0.000376 
                            [208]	train-rmse:0.241151+0.000113	test-rmse:0.241582+0.000375 
                            [209]	train-rmse:0.241137+0.000112	test-rmse:0.241569+0.000378 
                            [210]	train-rmse:0.241124+0.000109	test-rmse:0.241558+0.000382 
                            [211]	train-rmse:0.241106+0.000109	test-rmse:0.241542+0.000382 
                            [212]	train-rmse:0.241092+0.000111	test-rmse:0.241528+0.000381 
                            [213]	train-rmse:0.241077+0.000109	test-rmse:0.241515+0.000384 
                            [214]	train-rmse:0.241069+0.000112	test-rmse:0.241507+0.000381 
                            [215]	train-rmse:0.241056+0.000109	test-rmse:0.241496+0.000383 
                            [216]	train-rmse:0.241047+0.000108	test-rmse:0.241488+0.000384 
                            [217]	train-rmse:0.241037+0.000107	test-rmse:0.241480+0.000383 
                            [218]	train-rmse:0.241024+0.000110	test-rmse:0.241468+0.000382 
                            [219]	train-rmse:0.241011+0.000108	test-rmse:0.241457+0.000385 
                            [220]	train-rmse:0.241001+0.000108	test-rmse:0.241448+0.000385 
                            [221]	train-rmse:0.240987+0.000103	test-rmse:0.241435+0.000389 
                            [222]	train-rmse:0.240971+0.000104	test-rmse:0.241422+0.000388 
                            [223]	train-rmse:0.240964+0.000102	test-rmse:0.241416+0.000389 
                            [224]	train-rmse:0.240953+0.000104	test-rmse:0.241407+0.000388 
                            [225]	train-rmse:0.240943+0.000103	test-rmse:0.241398+0.000390 
                            [226]	train-rmse:0.240930+0.000104	test-rmse:0.241386+0.000391 
                            [227]	train-rmse:0.240922+0.000104	test-rmse:0.241379+0.000392 
                            [228]	train-rmse:0.240907+0.000102	test-rmse:0.241366+0.000393 
                            [229]	train-rmse:0.240898+0.000104	test-rmse:0.241357+0.000392 
                            [230]	train-rmse:0.240889+0.000105	test-rmse:0.241349+0.000392 
                            [231]	train-rmse:0.240875+0.000106	test-rmse:0.241337+0.000391 
                            [232]	train-rmse:0.240864+0.000105	test-rmse:0.241328+0.000393 
                            [233]	train-rmse:0.240850+0.000106	test-rmse:0.241316+0.000391 
                            [234]	train-rmse:0.240839+0.000106	test-rmse:0.241305+0.000392 
                            [235]	train-rmse:0.240828+0.000105	test-rmse:0.241295+0.000393 
                            [236]	train-rmse:0.240814+0.000107	test-rmse:0.241283+0.000392 
                            [237]	train-rmse:0.240803+0.000106	test-rmse:0.241273+0.000393 
                            [238]	train-rmse:0.240793+0.000103	test-rmse:0.241264+0.000395 
                            [239]	train-rmse:0.240782+0.000105	test-rmse:0.241254+0.000394 
                            [240]	train-rmse:0.240774+0.000106	test-rmse:0.241247+0.000393 
                            [241]	train-rmse:0.240767+0.000105	test-rmse:0.241241+0.000395 
                            [242]	train-rmse:0.240756+0.000107	test-rmse:0.241231+0.000392 
                            [243]	train-rmse:0.240747+0.000107	test-rmse:0.241223+0.000392 
                            [244]	train-rmse:0.240736+0.000102	test-rmse:0.241213+0.000396 
                            [245]	train-rmse:0.240726+0.000104	test-rmse:0.241204+0.000395 
                            [246]	train-rmse:0.240714+0.000103	test-rmse:0.241194+0.000397 
                            [247]	train-rmse:0.240704+0.000103	test-rmse:0.241185+0.000397 
                            [248]	train-rmse:0.240697+0.000102	test-rmse:0.241178+0.000399 
                            [249]	train-rmse:0.240688+0.000101	test-rmse:0.241171+0.000400 
                            [250]	train-rmse:0.240680+0.000102	test-rmse:0.241164+0.000400 
                            [251]	train-rmse:0.240671+0.000105	test-rmse:0.241155+0.000397 
                            [252]	train-rmse:0.240663+0.000107	test-rmse:0.241148+0.000397 
                            [253]	train-rmse:0.240653+0.000108	test-rmse:0.241139+0.000396 
                            [254]	train-rmse:0.240643+0.000110	test-rmse:0.241131+0.000392 
                            [255]	train-rmse:0.240634+0.000109	test-rmse:0.241123+0.000393 
                            [256]	train-rmse:0.240622+0.000111	test-rmse:0.241113+0.000390 
                            [257]	train-rmse:0.240612+0.000111	test-rmse:0.241105+0.000390 
                            [258]	train-rmse:0.240605+0.000114	test-rmse:0.241098+0.000388 
                            [259]	train-rmse:0.240596+0.000115	test-rmse:0.241091+0.000389 
                            [260]	train-rmse:0.240591+0.000116	test-rmse:0.241086+0.000389 
                            [261]	train-rmse:0.240581+0.000116	test-rmse:0.241077+0.000387 
                            [262]	train-rmse:0.240572+0.000117	test-rmse:0.241070+0.000387 
                            [263]	train-rmse:0.240562+0.000117	test-rmse:0.241061+0.000386 
                            [264]	train-rmse:0.240553+0.000120	test-rmse:0.241054+0.000385 
                            [265]	train-rmse:0.240541+0.000122	test-rmse:0.241043+0.000384 
                            [266]	train-rmse:0.240531+0.000122	test-rmse:0.241034+0.000382 
                            [267]	train-rmse:0.240524+0.000122	test-rmse:0.241029+0.000382 
                            [268]	train-rmse:0.240517+0.000121	test-rmse:0.241023+0.000382 
                            [269]	train-rmse:0.240508+0.000121	test-rmse:0.241015+0.000382 
                            [270]	train-rmse:0.240498+0.000122	test-rmse:0.241005+0.000382 
                            [271]	train-rmse:0.240489+0.000125	test-rmse:0.240998+0.000380 
                            [272]	train-rmse:0.240482+0.000127	test-rmse:0.240991+0.000380 
                            [273]	train-rmse:0.240469+0.000128	test-rmse:0.240980+0.000378 
                            [274]	train-rmse:0.240461+0.000126	test-rmse:0.240973+0.000378 
                            [275]	train-rmse:0.240453+0.000125	test-rmse:0.240966+0.000377 
                            [276]	train-rmse:0.240445+0.000125	test-rmse:0.240959+0.000379 
                            [277]	train-rmse:0.240435+0.000126	test-rmse:0.240950+0.000378 
                            [278]	train-rmse:0.240424+0.000131	test-rmse:0.240940+0.000375 
                            [279]	train-rmse:0.240417+0.000132	test-rmse:0.240935+0.000375 
                            [280]	train-rmse:0.240409+0.000133	test-rmse:0.240928+0.000376 
                            [281]	train-rmse:0.240399+0.000132	test-rmse:0.240920+0.000378 
                            [282]	train-rmse:0.240392+0.000131	test-rmse:0.240913+0.000378 
                            [283]	train-rmse:0.240386+0.000130	test-rmse:0.240909+0.000378 
                            [284]	train-rmse:0.240379+0.000130	test-rmse:0.240903+0.000379 
                            [285]	train-rmse:0.240369+0.000128	test-rmse:0.240894+0.000381 
                            [286]	train-rmse:0.240360+0.000128	test-rmse:0.240886+0.000381 
                            [287]	train-rmse:0.240350+0.000130	test-rmse:0.240878+0.000379 
                            [288]	train-rmse:0.240339+0.000129	test-rmse:0.240867+0.000379 
                            [289]	train-rmse:0.240332+0.000129	test-rmse:0.240862+0.000380 
                            [290]	train-rmse:0.240325+0.000130	test-rmse:0.240856+0.000379 
                            [291]	train-rmse:0.240315+0.000133	test-rmse:0.240847+0.000373 
                            [292]	train-rmse:0.240308+0.000133	test-rmse:0.240840+0.000373 
                            [293]	train-rmse:0.240300+0.000133	test-rmse:0.240834+0.000374 
                            [294]	train-rmse:0.240289+0.000131	test-rmse:0.240825+0.000375 
                            [295]	train-rmse:0.240283+0.000131	test-rmse:0.240819+0.000376 
                            [296]	train-rmse:0.240277+0.000128	test-rmse:0.240814+0.000378 
                            [297]	train-rmse:0.240266+0.000129	test-rmse:0.240805+0.000376 
                            [298]	train-rmse:0.240255+0.000130	test-rmse:0.240795+0.000374 
                            [299]	train-rmse:0.240249+0.000130	test-rmse:0.240789+0.000375 
                            [300]	train-rmse:0.240240+0.000130	test-rmse:0.240782+0.000374 
                            [301]	train-rmse:0.240231+0.000128	test-rmse:0.240774+0.000377 
                            [302]	train-rmse:0.240220+0.000129	test-rmse:0.240765+0.000374 
                            [303]	train-rmse:0.240214+0.000130	test-rmse:0.240760+0.000374 
                            [304]	train-rmse:0.240206+0.000131	test-rmse:0.240754+0.000375 
                            [305]	train-rmse:0.240196+0.000131	test-rmse:0.240745+0.000376 
                            [306]	train-rmse:0.240187+0.000131	test-rmse:0.240737+0.000375 
                            [307]	train-rmse:0.240181+0.000130	test-rmse:0.240732+0.000375 
                            [308]	train-rmse:0.240173+0.000131	test-rmse:0.240725+0.000374 
                            [309]	train-rmse:0.240163+0.000135	test-rmse:0.240717+0.000372 
                            [310]	train-rmse:0.240157+0.000134	test-rmse:0.240712+0.000372 
                            [311]	train-rmse:0.240149+0.000132	test-rmse:0.240705+0.000374 
                            [312]	train-rmse:0.240140+0.000132	test-rmse:0.240697+0.000375 
                            [313]	train-rmse:0.240131+0.000129	test-rmse:0.240690+0.000377 
                            [314]	train-rmse:0.240122+0.000131	test-rmse:0.240681+0.000375 
                            [315]	train-rmse:0.240114+0.000130	test-rmse:0.240674+0.000374 
                            [316]	train-rmse:0.240106+0.000132	test-rmse:0.240667+0.000374 
                            [317]	train-rmse:0.240098+0.000130	test-rmse:0.240660+0.000375 
                            [318]	train-rmse:0.240088+0.000132	test-rmse:0.240652+0.000375 
                            [319]	train-rmse:0.240079+0.000133	test-rmse:0.240644+0.000374 
                            [320]	train-rmse:0.240070+0.000133	test-rmse:0.240636+0.000375 
                            [321]	train-rmse:0.240064+0.000133	test-rmse:0.240632+0.000374 
                            [322]	train-rmse:0.240057+0.000133	test-rmse:0.240626+0.000375 
                            [323]	train-rmse:0.240049+0.000131	test-rmse:0.240619+0.000377 
                            [324]	train-rmse:0.240038+0.000131	test-rmse:0.240609+0.000375 
                            [325]	train-rmse:0.240031+0.000132	test-rmse:0.240602+0.000376 
                            [326]	train-rmse:0.240021+0.000135	test-rmse:0.240594+0.000373 
                            [327]	train-rmse:0.240010+0.000136	test-rmse:0.240584+0.000374 
                            [328]	train-rmse:0.240004+0.000135	test-rmse:0.240580+0.000374 
                            [329]	train-rmse:0.239997+0.000134	test-rmse:0.240574+0.000375 
                            [330]	train-rmse:0.239989+0.000137	test-rmse:0.240567+0.000374 
                            [331]	train-rmse:0.239983+0.000137	test-rmse:0.240562+0.000373 
                            [332]	train-rmse:0.239973+0.000137	test-rmse:0.240553+0.000372 
                            [333]	train-rmse:0.239966+0.000138	test-rmse:0.240548+0.000372 
                            [334]	train-rmse:0.239960+0.000137	test-rmse:0.240543+0.000374 
                            [335]	train-rmse:0.239952+0.000137	test-rmse:0.240536+0.000373 
                            [336]	train-rmse:0.239946+0.000135	test-rmse:0.240532+0.000374 
                            [337]	train-rmse:0.239939+0.000136	test-rmse:0.240526+0.000375 
                            [338]	train-rmse:0.239932+0.000136	test-rmse:0.240520+0.000375 
                            [339]	train-rmse:0.239926+0.000136	test-rmse:0.240515+0.000374 
                            [340]	train-rmse:0.239917+0.000138	test-rmse:0.240508+0.000372 
                            [341]	train-rmse:0.239909+0.000137	test-rmse:0.240501+0.000372 
                            [342]	train-rmse:0.239904+0.000139	test-rmse:0.240497+0.000372 
                            [343]	train-rmse:0.239899+0.000138	test-rmse:0.240493+0.000372 
                            [344]	train-rmse:0.239894+0.000138	test-rmse:0.240489+0.000373 
                            [345]	train-rmse:0.239887+0.000137	test-rmse:0.240484+0.000373 
                            [346]	train-rmse:0.239881+0.000139	test-rmse:0.240479+0.000372 
                            [347]	train-rmse:0.239876+0.000139	test-rmse:0.240474+0.000373 
                            [348]	train-rmse:0.239868+0.000137	test-rmse:0.240467+0.000375 
                            [349]	train-rmse:0.239863+0.000136	test-rmse:0.240462+0.000374 
                            [350]	train-rmse:0.239857+0.000135	test-rmse:0.240458+0.000376 
                            [351]	train-rmse:0.239850+0.000134	test-rmse:0.240453+0.000376 
                            [352]	train-rmse:0.239843+0.000134	test-rmse:0.240447+0.000376 
                            [353]	train-rmse:0.239836+0.000134	test-rmse:0.240441+0.000374 
                            [354]	train-rmse:0.239831+0.000134	test-rmse:0.240437+0.000374 
                            [355]	train-rmse:0.239826+0.000134	test-rmse:0.240432+0.000373 
                            [356]	train-rmse:0.239819+0.000135	test-rmse:0.240427+0.000373 
                            [357]	train-rmse:0.239812+0.000135	test-rmse:0.240421+0.000373 
                            [358]	train-rmse:0.239805+0.000135	test-rmse:0.240416+0.000372 
                            [359]	train-rmse:0.239800+0.000137	test-rmse:0.240411+0.000372 
                            [360]	train-rmse:0.239794+0.000137	test-rmse:0.240407+0.000372 
                            [361]	train-rmse:0.239788+0.000136	test-rmse:0.240402+0.000373 
                            [362]	train-rmse:0.239782+0.000137	test-rmse:0.240397+0.000372 
                            [363]	train-rmse:0.239776+0.000136	test-rmse:0.240392+0.000373 
                            [364]	train-rmse:0.239769+0.000135	test-rmse:0.240387+0.000374 
                            [365]	train-rmse:0.239762+0.000136	test-rmse:0.240381+0.000372 
                            [366]	train-rmse:0.239754+0.000136	test-rmse:0.240375+0.000373 
                            [367]	train-rmse:0.239749+0.000136	test-rmse:0.240370+0.000373 
                            [368]	train-rmse:0.239744+0.000136	test-rmse:0.240366+0.000373 
                            [369]	train-rmse:0.239737+0.000136	test-rmse:0.240361+0.000371 
                            [370]	train-rmse:0.239732+0.000135	test-rmse:0.240356+0.000371 
                            [371]	train-rmse:0.239725+0.000135	test-rmse:0.240351+0.000372 
                            [372]	train-rmse:0.239718+0.000134	test-rmse:0.240345+0.000372 
                            [373]	train-rmse:0.239708+0.000129	test-rmse:0.240335+0.000377 
                            [374]	train-rmse:0.239703+0.000129	test-rmse:0.240331+0.000377 
                            [375]	train-rmse:0.239698+0.000128	test-rmse:0.240327+0.000379 
                            [376]	train-rmse:0.239693+0.000128	test-rmse:0.240324+0.000379 
                            [377]	train-rmse:0.239687+0.000128	test-rmse:0.240319+0.000380 
                            [378]	train-rmse:0.239682+0.000127	test-rmse:0.240315+0.000380 
                            [379]	train-rmse:0.239676+0.000127	test-rmse:0.240310+0.000379 
                            [380]	train-rmse:0.239671+0.000126	test-rmse:0.240306+0.000379 
                            [381]	train-rmse:0.239666+0.000126	test-rmse:0.240302+0.000379 
                            [382]	train-rmse:0.239660+0.000124	test-rmse:0.240298+0.000379 
                            [383]	train-rmse:0.239653+0.000124	test-rmse:0.240292+0.000379 
                            [384]	train-rmse:0.239647+0.000124	test-rmse:0.240286+0.000379 
                            [385]	train-rmse:0.239641+0.000123	test-rmse:0.240282+0.000379 
                            [386]	train-rmse:0.239637+0.000123	test-rmse:0.240279+0.000379 
                            [387]	train-rmse:0.239632+0.000124	test-rmse:0.240275+0.000379 
                            [388]	train-rmse:0.239627+0.000123	test-rmse:0.240271+0.000378 
                            [389]	train-rmse:0.239620+0.000124	test-rmse:0.240266+0.000378 
                            [390]	train-rmse:0.239615+0.000125	test-rmse:0.240262+0.000377 
                            [391]	train-rmse:0.239608+0.000125	test-rmse:0.240256+0.000377 
                            [392]	train-rmse:0.239602+0.000127	test-rmse:0.240251+0.000375 
                            [393]	train-rmse:0.239595+0.000130	test-rmse:0.240245+0.000372 
                            [394]	train-rmse:0.239590+0.000131	test-rmse:0.240241+0.000371 
                            [395]	train-rmse:0.239582+0.000131	test-rmse:0.240234+0.000371 
                            [396]	train-rmse:0.239575+0.000131	test-rmse:0.240229+0.000373 
                            [397]	train-rmse:0.239563+0.000129	test-rmse:0.240219+0.000375 
                            [398]	train-rmse:0.239559+0.000130	test-rmse:0.240216+0.000374 
                            [399]	train-rmse:0.239554+0.000131	test-rmse:0.240211+0.000372 
                            [400]	train-rmse:0.239548+0.000131	test-rmse:0.240206+0.000372 
                            [401]	train-rmse:0.239544+0.000131	test-rmse:0.240203+0.000371 
                            [402]	train-rmse:0.239537+0.000129	test-rmse:0.240198+0.000372 
                            [403]	train-rmse:0.239531+0.000128	test-rmse:0.240194+0.000373 
                            [404]	train-rmse:0.239525+0.000129	test-rmse:0.240188+0.000373 
                            [405]	train-rmse:0.239519+0.000128	test-rmse:0.240184+0.000374 
                            [406]	train-rmse:0.239512+0.000129	test-rmse:0.240178+0.000373 
                            [407]	train-rmse:0.239507+0.000130	test-rmse:0.240174+0.000372 
                            [408]	train-rmse:0.239503+0.000131	test-rmse:0.240170+0.000371 
                            [409]	train-rmse:0.239497+0.000131	test-rmse:0.240165+0.000370 
                            [410]	train-rmse:0.239489+0.000133	test-rmse:0.240159+0.000369 
                            [411]	train-rmse:0.239485+0.000133	test-rmse:0.240157+0.000368 
                            [412]	train-rmse:0.239478+0.000135	test-rmse:0.240151+0.000366 
                            [413]	train-rmse:0.239472+0.000136	test-rmse:0.240147+0.000364 
                            [414]	train-rmse:0.239469+0.000135	test-rmse:0.240144+0.000366 
                            [415]	train-rmse:0.239463+0.000135	test-rmse:0.240140+0.000366 
                            [416]	train-rmse:0.239458+0.000135	test-rmse:0.240136+0.000367 
                            [417]	train-rmse:0.239451+0.000131	test-rmse:0.240130+0.000370 
                            [418]	train-rmse:0.239444+0.000134	test-rmse:0.240123+0.000366 
                            [419]	train-rmse:0.239439+0.000134	test-rmse:0.240119+0.000365 
                            [420]	train-rmse:0.239435+0.000135	test-rmse:0.240116+0.000365 
                            [421]	train-rmse:0.239428+0.000134	test-rmse:0.240111+0.000365 
                            [422]	train-rmse:0.239422+0.000134	test-rmse:0.240106+0.000366 
                            [423]	train-rmse:0.239415+0.000132	test-rmse:0.240101+0.000368 
                            [424]	train-rmse:0.239408+0.000128	test-rmse:0.240095+0.000370 
                            [425]	train-rmse:0.239404+0.000128	test-rmse:0.240092+0.000370 
                            [426]	train-rmse:0.239399+0.000128	test-rmse:0.240089+0.000371 
                            [427]	train-rmse:0.239393+0.000128	test-rmse:0.240083+0.000371 
                            [428]	train-rmse:0.239387+0.000127	test-rmse:0.240079+0.000371 
                            [429]	train-rmse:0.239378+0.000125	test-rmse:0.240071+0.000373 
                            [430]	train-rmse:0.239372+0.000126	test-rmse:0.240067+0.000373 
                            [431]	train-rmse:0.239366+0.000127	test-rmse:0.240062+0.000372 
                            [432]	train-rmse:0.239359+0.000127	test-rmse:0.240057+0.000373 
                            [433]	train-rmse:0.239354+0.000124	test-rmse:0.240052+0.000376 
                            [434]	train-rmse:0.239348+0.000125	test-rmse:0.240048+0.000375 
                            [435]	train-rmse:0.239343+0.000125	test-rmse:0.240044+0.000376 
                            [436]	train-rmse:0.239337+0.000125	test-rmse:0.240039+0.000376 
                            [437]	train-rmse:0.239333+0.000126	test-rmse:0.240036+0.000375 
                            [438]	train-rmse:0.239327+0.000127	test-rmse:0.240032+0.000375 
                            [439]	train-rmse:0.239323+0.000126	test-rmse:0.240028+0.000376 
                            [440]	train-rmse:0.239318+0.000127	test-rmse:0.240024+0.000375 
                            [441]	train-rmse:0.239313+0.000126	test-rmse:0.240021+0.000377 
                            [442]	train-rmse:0.239307+0.000126	test-rmse:0.240016+0.000377 
                            [443]	train-rmse:0.239302+0.000124	test-rmse:0.240012+0.000378 
                            [444]	train-rmse:0.239297+0.000125	test-rmse:0.240008+0.000378 
                            [445]	train-rmse:0.239292+0.000124	test-rmse:0.240004+0.000378 
                            [446]	train-rmse:0.239285+0.000124	test-rmse:0.239999+0.000379 
                            [447]	train-rmse:0.239280+0.000124	test-rmse:0.239994+0.000379 
                            [448]	train-rmse:0.239273+0.000124	test-rmse:0.239988+0.000379 
                            [449]	train-rmse:0.239267+0.000124	test-rmse:0.239983+0.000380 
                            [450]	train-rmse:0.239260+0.000122	test-rmse:0.239978+0.000381 
                            [451]	train-rmse:0.239256+0.000122	test-rmse:0.239974+0.000381 
                            [452]	train-rmse:0.239251+0.000123	test-rmse:0.239970+0.000381 
                            [453]	train-rmse:0.239246+0.000123	test-rmse:0.239966+0.000382 
                            [454]	train-rmse:0.239239+0.000123	test-rmse:0.239961+0.000381 
                            [455]	train-rmse:0.239236+0.000123	test-rmse:0.239958+0.000381 
                            [456]	train-rmse:0.239232+0.000123	test-rmse:0.239955+0.000380 
                            [457]	train-rmse:0.239228+0.000122	test-rmse:0.239952+0.000381 
                            [458]	train-rmse:0.239224+0.000123	test-rmse:0.239949+0.000381 
                            [459]	train-rmse:0.239220+0.000123	test-rmse:0.239946+0.000381 
                            [460]	train-rmse:0.239213+0.000121	test-rmse:0.239940+0.000382 
                            [461]	train-rmse:0.239209+0.000122	test-rmse:0.239937+0.000382 
                            [462]	train-rmse:0.239204+0.000122	test-rmse:0.239933+0.000382 
                            [463]	train-rmse:0.239201+0.000124	test-rmse:0.239931+0.000381 
                            [464]	train-rmse:0.239197+0.000123	test-rmse:0.239928+0.000382 
                            [465]	train-rmse:0.239192+0.000121	test-rmse:0.239924+0.000383 
                            [466]	train-rmse:0.239186+0.000123	test-rmse:0.239920+0.000381 
                            [467]	train-rmse:0.239182+0.000124	test-rmse:0.239917+0.000381 
                            [468]	train-rmse:0.239179+0.000123	test-rmse:0.239914+0.000381 
                            [469]	train-rmse:0.239175+0.000124	test-rmse:0.239911+0.000380 
                            [470]	train-rmse:0.239170+0.000125	test-rmse:0.239907+0.000380 
                            [471]	train-rmse:0.239165+0.000124	test-rmse:0.239904+0.000381 
                            [472]	train-rmse:0.239160+0.000125	test-rmse:0.239900+0.000381 
                            [473]	train-rmse:0.239154+0.000123	test-rmse:0.239896+0.000381 
                            [474]	train-rmse:0.239149+0.000122	test-rmse:0.239892+0.000383 
                            [475]	train-rmse:0.239143+0.000123	test-rmse:0.239886+0.000382 
                            [476]	train-rmse:0.239137+0.000123	test-rmse:0.239882+0.000382 
                            [477]	train-rmse:0.239131+0.000122	test-rmse:0.239877+0.000384 
                            [478]	train-rmse:0.239125+0.000121	test-rmse:0.239872+0.000384 
                            [479]	train-rmse:0.239121+0.000123	test-rmse:0.239868+0.000384 
                            [480]	train-rmse:0.239116+0.000123	test-rmse:0.239865+0.000383 
                            [481]	train-rmse:0.239113+0.000123	test-rmse:0.239863+0.000384 
                            [482]	train-rmse:0.239108+0.000123	test-rmse:0.239859+0.000384 
                            [483]	train-rmse:0.239103+0.000123	test-rmse:0.239855+0.000383 
                            [484]	train-rmse:0.239099+0.000121	test-rmse:0.239852+0.000384 
                            [485]	train-rmse:0.239094+0.000122	test-rmse:0.239850+0.000383 
                            [486]	train-rmse:0.239091+0.000122	test-rmse:0.239848+0.000383 
                            [487]	train-rmse:0.239086+0.000123	test-rmse:0.239845+0.000381 
                            [488]	train-rmse:0.239080+0.000123	test-rmse:0.239840+0.000380 
                            [489]	train-rmse:0.239075+0.000123	test-rmse:0.239836+0.000381 
                            [490]	train-rmse:0.239069+0.000124	test-rmse:0.239831+0.000380 
                            [491]	train-rmse:0.239063+0.000124	test-rmse:0.239827+0.000381 
                            [492]	train-rmse:0.239060+0.000124	test-rmse:0.239824+0.000381 
                            [493]	train-rmse:0.239053+0.000126	test-rmse:0.239819+0.000380 
                            [494]	train-rmse:0.239045+0.000123	test-rmse:0.239812+0.000382 
                            [495]	train-rmse:0.239042+0.000123	test-rmse:0.239810+0.000382 
                            [496]	train-rmse:0.239036+0.000123	test-rmse:0.239806+0.000382 
                            [497]	train-rmse:0.239030+0.000120	test-rmse:0.239801+0.000383 
                            [498]	train-rmse:0.239025+0.000121	test-rmse:0.239797+0.000382 
                            [499]	train-rmse:0.239019+0.000120	test-rmse:0.239792+0.000383 
                            [500]	train-rmse:0.239013+0.000120	test-rmse:0.239788+0.000382 
                            [501]	train-rmse:0.239007+0.000119	test-rmse:0.239783+0.000383 
                            [502]	train-rmse:0.239001+0.000118	test-rmse:0.239779+0.000384 
                            [503]	train-rmse:0.238995+0.000117	test-rmse:0.239774+0.000384 
                            [504]	train-rmse:0.238991+0.000118	test-rmse:0.239771+0.000383 
                            [505]	train-rmse:0.238987+0.000118	test-rmse:0.239768+0.000384 
                            [506]	train-rmse:0.238982+0.000118	test-rmse:0.239765+0.000384 
                            [507]	train-rmse:0.238978+0.000117	test-rmse:0.239762+0.000386 
                            [508]	train-rmse:0.238974+0.000117	test-rmse:0.239759+0.000386 
                            [509]	train-rmse:0.238970+0.000117	test-rmse:0.239757+0.000385 
                            [510]	train-rmse:0.238964+0.000118	test-rmse:0.239752+0.000384 
                            [511]	train-rmse:0.238960+0.000118	test-rmse:0.239749+0.000384 
                            [512]	train-rmse:0.238955+0.000116	test-rmse:0.239744+0.000385 
                            [513]	train-rmse:0.238950+0.000118	test-rmse:0.239740+0.000385 
                            [514]	train-rmse:0.238944+0.000116	test-rmse:0.239736+0.000386 
                            [515]	train-rmse:0.238941+0.000116	test-rmse:0.239733+0.000386 
                            [516]	train-rmse:0.238936+0.000116	test-rmse:0.239729+0.000386 
                            [517]	train-rmse:0.238931+0.000117	test-rmse:0.239725+0.000385 
                            [518]	train-rmse:0.238925+0.000115	test-rmse:0.239721+0.000386 
                            [519]	train-rmse:0.238921+0.000115	test-rmse:0.239718+0.000387 
                            [520]	train-rmse:0.238914+0.000111	test-rmse:0.239713+0.000391 
                            [521]	train-rmse:0.238910+0.000110	test-rmse:0.239710+0.000391 
                            [522]	train-rmse:0.238904+0.000110	test-rmse:0.239705+0.000392 
                            [523]	train-rmse:0.238899+0.000110	test-rmse:0.239702+0.000391 
                            [524]	train-rmse:0.238895+0.000111	test-rmse:0.239699+0.000391 
                            [525]	train-rmse:0.238890+0.000111	test-rmse:0.239696+0.000391 
                            [526]	train-rmse:0.238885+0.000109	test-rmse:0.239692+0.000393 
                            [527]	train-rmse:0.238881+0.000108	test-rmse:0.239689+0.000393 
                            [528]	train-rmse:0.238877+0.000109	test-rmse:0.239687+0.000393 
                            [529]	train-rmse:0.238872+0.000109	test-rmse:0.239684+0.000393 
                            [530]	train-rmse:0.238867+0.000109	test-rmse:0.239680+0.000393 
                            [531]	train-rmse:0.238861+0.000108	test-rmse:0.239675+0.000393 
                            [532]	train-rmse:0.238857+0.000110	test-rmse:0.239671+0.000393 
                            [533]	train-rmse:0.238854+0.000111	test-rmse:0.239669+0.000393 
                            [534]	train-rmse:0.238850+0.000110	test-rmse:0.239666+0.000393 
                            [535]	train-rmse:0.238844+0.000111	test-rmse:0.239662+0.000394 
                            [536]	train-rmse:0.238839+0.000111	test-rmse:0.239659+0.000395 
                            [537]	train-rmse:0.238834+0.000111	test-rmse:0.239655+0.000393 
                            [538]	train-rmse:0.238831+0.000112	test-rmse:0.239653+0.000393 
                            [539]	train-rmse:0.238827+0.000113	test-rmse:0.239650+0.000393 
                            [540]	train-rmse:0.238823+0.000114	test-rmse:0.239647+0.000393 
                            [541]	train-rmse:0.238820+0.000113	test-rmse:0.239645+0.000393 
                            [542]	train-rmse:0.238815+0.000113	test-rmse:0.239642+0.000393 
                            [543]	train-rmse:0.238811+0.000113	test-rmse:0.239638+0.000393 
                            [544]	train-rmse:0.238807+0.000112	test-rmse:0.239636+0.000393 
                            [545]	train-rmse:0.238803+0.000114	test-rmse:0.239633+0.000392 
                            [546]	train-rmse:0.238797+0.000113	test-rmse:0.239628+0.000391 
                            [547]	train-rmse:0.238794+0.000112	test-rmse:0.239626+0.000391 
                            [548]	train-rmse:0.238790+0.000112	test-rmse:0.239623+0.000392 
                            [549]	train-rmse:0.238785+0.000114	test-rmse:0.239620+0.000391 
                            [550]	train-rmse:0.238781+0.000113	test-rmse:0.239616+0.000392 
                            [551]	train-rmse:0.238775+0.000111	test-rmse:0.239612+0.000393 
                            [552]	train-rmse:0.238770+0.000111	test-rmse:0.239608+0.000394 
                            [553]	train-rmse:0.238766+0.000111	test-rmse:0.239605+0.000394 
                            [554]	train-rmse:0.238761+0.000112	test-rmse:0.239602+0.000394 
                            [555]	train-rmse:0.238755+0.000111	test-rmse:0.239597+0.000395 
                            [556]	train-rmse:0.238750+0.000112	test-rmse:0.239594+0.000393 
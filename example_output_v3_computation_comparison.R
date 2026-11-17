# > cat("Training XGBoost V3 model (with temporal and geographic features)...\n")
# Training XGBoost V3 model (with temporal and geographic features)...
# > xgb_cv_v3 <- xgb.cv(
#   +   params = xgb_params_v3,
#   +   data = dtrain_v3,
#   +   nrounds = 1000,
#   +   nfold = 5,
#   +   early_stopping_rounds = 50,
#   +   verbose = FALSE
#   + )
# > 
#   > best_nrounds_v3 <- xgb_cv_v3$best_iteration
# > cat("Optimal rounds V3:", best_nrounds_v3, "Best CV RMSE:", min(xgb_cv_v3$evaluation_log$test_rmse_mean), "\n")
# Optimal rounds V3: 1000 Best CV RMSE: 0.1126056 
# > 
#   > # Train final model V3
#   > xgb_model_v3 <- xgb.train(
#     +   params = xgb_params_v3,
#     +   data = dtrain_v3,
#     +   nrounds = best_nrounds_v3,
#     +   watchlist = list(train = dtrain_v3, test = dtest_v3),
#     +   verbose = FALSE
#     + )
# Warning message:
#   In xgb.train(params = xgb_params_v3, data = dtrain_v3, nrounds = best_nrounds_v3,  :
#                  xgb.train: `seed` is ignored in R package.  Use `set.seed()` instead.
#                > 
#                  > # Evaluate model performance V3
#                  > train_pred_lz_v3 <- predict(xgb_model_v3, dtrain_v3)
#                > test_pred_lz_v3 <- predict(xgb_model_v3, dtest_v3)
#                > 
#                  > # Compare against actual LZ wages
#                  > train_rmse_lz_v3 <- sqrt(mean((train_pred_lz_v3 - y_train_v3)^2))
#                > test_rmse_lz_v3 <- sqrt(mean((test_pred_lz_v3 - y_test_v3)^2))
#                > train_r2_lz_v3 <- cor(train_pred_lz_v3, y_train_v3)^2
#                > test_r2_lz_v3 <- cor(test_pred_lz_v3, y_test_v3)^2
#                > 
#                  > # Compare against BG wages
#                  > train_rmse_bg_v3 <- sqrt(mean((bg_train_v3 - y_train_v3)^2))
#                > test_rmse_bg_v3 <- sqrt(mean((bg_test_v3 - y_test_v3)^2))
#                > train_r2_bg_v3 <- cor(bg_train_v3, y_train_v3)^2
#                > test_r2_bg_v3 <- cor(bg_test_v3, y_test_v3)^2
#                > 
#                  > cat("\n=== Model V3 Performance: Predicting True Wage (LZ) ===\n")
#                
#                === Model V3 Performance: Predicting True Wage (LZ) ===
#                  > cat("XGBoost V3 vs LZ True Wage:\n")
#                XGBoost V3 vs LZ True Wage:
#                  > cat("  Training RMSE:", round(train_rmse_lz_v3, 4), "R²:", round(train_r2_lz_v3, 4), "\n")
#                Training RMSE: 0.0997 R²: 0.9737 
#                > cat("  Test RMSE:", round(test_rmse_lz_v3, 4), "R²:", round(test_r2_lz_v3, 4), "\n")
#                Test RMSE: 0.112 R²: 0.9668 
#                > 
#                  > cat("\nBG vs LZ True Wage (current method):\n")
#                
#                BG vs LZ True Wage (current method):
#                  > cat("  Training RMSE:", round(train_rmse_bg_v3, 4), "R²:", round(train_r2_bg_v3, 4), "\n")
#                Training RMSE: 0.283 R²: 0.7837 
#                > cat("  Test RMSE:", round(test_rmse_bg_v3, 4), "R²:", round(test_r2_bg_v3, 4), "\n")
#                Test RMSE: 0.2797 R²: 0.7891 
#                > 
#                  > cat("\nImprovement from XGBoost V3:\n")
#                
#                Improvement from XGBoost V3:
#                  > cat("  RMSE reduction:", round(test_rmse_bg_v3 - test_rmse_lz_v3, 4), "\n")
#                RMSE reduction: 0.1677 
#                > cat("  R² improvement:", round(test_r2_lz_v3 - test_r2_bg_v3, 4), "\n")
#                R² improvement: 0.1776 
#                > 
#                  > # Compare V2 vs V3
#                  > cat("\n=== Comparison: V2 vs V3 ===\n")
#                
#                === Comparison: V2 vs V3 ===
#                  > cat("V2 Test RMSE:", round(test_rmse_lz_v2, 4), "R²:", round(test_r2_lz_v2, 4), "\n")
#                V2 Test RMSE: 0.1121 R²: 0.9667 
#                > cat("V3 Test RMSE:", round(test_rmse_lz_v3, 4), "R²:", round(test_r2_lz_v3, 4), "\n")
#                V3 Test RMSE: 0.112 R²: 0.9668 
#                > cat("Improvement V3 over V2:\n")
#                Improvement V3 over V2:
#                  > cat("  RMSE reduction:", round(test_rmse_lz_v2 - test_rmse_lz_v3, 4), "\n")
#                RMSE reduction: 1e-04 
#                > cat("  R² improvement:", round(test_r2_lz_v3 - test_r2_lz_v2, 4), "\n")
#                R² improvement: 1e-04 
#                > 
#                  > if(test_rmse_lz_v3 < test_rmse_lz_v2) {
#                    +   cat("✓ V3 with temporal/geographic features performs BETTER\n")
#                    +   improvement_pct <- round((test_rmse_lz_v2 - test_rmse_lz_v3) / test_rmse_lz_v2 * 100, 2)
#                    +   cat("  Improvement: ", improvement_pct, "%\n", sep = "")
#                    + } else {
#                      +   cat("✗ V2 performs BETTER\n")
#                      + }
#                ✓ V3 with temporal/geographic features performs BETTER
#                Improvement: 0.13%
#                > 
#                  > # Feature importance V3
#                  > importance_matrix_v3 <- xgb.importance(feature_names = colnames(X_features_v3), model = xgb_model_v3)
#                > cat("\n=== Top 20 Features for Predicting True Wage (V3) ===\n")
#                
#                === Top 20 Features for Predicting True Wage (V3) ===
#                  > print(head(importance_matrix_v3, 20))
#                Feature         Gain        Cover    Frequency
#                <char>        <num>        <num>        <num>
#                  1:          interval_upper 5.826974e-01 9.398445e-02 0.0488418028
#                2:          interval_lower 2.725564e-01 1.444871e-01 0.1165379781
#                3:               mean_wage 4.481965e-02 2.583326e-01 0.2190025139
#                4:     mean_wage_coworkers 3.181168e-02 1.951055e-01 0.1384225175
#                5:                     age 1.654777e-02 4.228814e-02 0.1378389298
#                6:               firm_size 1.314925e-02 4.160147e-02 0.0727240079
#                7:           mean_censored 1.062464e-02 5.115708e-02 0.0381801041
#                8:               num_years 1.024038e-02 8.658237e-02 0.0911743581
#                9:               contryear 4.031786e-03 2.282473e-02 0.0454525049
#                10:              bundesland 3.476799e-03 1.788700e-02 0.0310423774
#                11:        firm_size_square 2.987140e-03 1.186304e-02 0.0194155145
#                12: mean_censored_coworkers 2.191614e-03 1.174352e-02 0.0120084396
#                13:        bg_censored_flag 1.879375e-03 3.986987e-04 0.0003591309
#                14:                    stsl 1.701344e-03 1.646294e-02 0.0187645897
#                15:                    frau 1.198269e-03 5.275008e-03 0.0096291973
#                16:         smallfirm_dummy 8.644602e-05 6.297874e-06 0.0006060334
#                > 
#                  > # Highlight importance of new features
#                  > new_features_importance <- importance_matrix_v3 %>%
#                  +   filter(Feature %in% c("contryear", "bundesland", "stsl"))
#                > cat("\n=== Importance of NEW V3 Features ===\n")
#                
#                === Importance of NEW V3 Features ===
#                  > print(new_features_importance)
#                Feature        Gain      Cover  Frequency
#                <char>       <num>      <num>      <num>
#                  1:  contryear 0.004031786 0.02282473 0.04545250
#                2: bundesland 0.003476799 0.01788700 0.03104238
#                3:       stsl 0.001701344 0.01646294 0.01876459
#                > 
#                  > # Analysis of predictions V3
#                  > censored_v3 <- training_data_v3$bg_censored_flag[-train_indices_v3]
#                > 
#                  > prediction_comparison_v3 <- data.frame(
#                    +   lz_true = y_test_v3,
#                    +   bg_observed = bg_test_v3,
#                    +   xgb_predicted_v3 = test_pred_lz_v3,
#                    +   censored = censored_v3,
#                    +   
#                      +   bg_error = abs(bg_test_v3 - y_test_v3),
#                    +   xgb_error_v3 = abs(test_pred_lz_v3 - y_test_v3),
#                    +   improvement_v3 = abs(bg_test_v3 - y_test_v3) - abs(test_pred_lz_v3 - y_test_v3)
#                    + ) %>%
#                  +   mutate(
#                    +     bg_better = improvement_v3 < 0,
#                    +     xgb_better = improvement_v3 > 0,
#                    +     bg_pct_error = abs((bg_observed - lz_true) / lz_true) * 100,
#                    +     xgb_pct_error_v3 = abs((xgb_predicted_v3 - lz_true) / lz_true) * 100
#                    +   )
#                > 
#                  > cat("\n=== Prediction Quality Comparison V3 ===\n")
#                
#                === Prediction Quality Comparison V3 ===
#                  > cat("Mean Absolute Error:\n")
#                Mean Absolute Error:
#                  > cat("  BG method:", round(mean(prediction_comparison_v3$bg_error), 4), "\n")
#                BG method: 0.0809 
#                > cat("  XGBoost V3 method:", round(mean(prediction_comparison_v3$xgb_error_v3), 4), "\n")
#                XGBoost V3 method: 0.0469 
#                > cat("  Improvement:", round(mean(prediction_comparison_v3$improvement_v3), 4), "\n")
#                Improvement: 0.034 
#                > 
#                  > cat("Relative Improvement:", round(1 - mean(prediction_comparison_v3$xgb_error_v3) / mean(prediction_comparison_v3$bg_error), 4), "\n")
#                Relative Improvement: 0.4204 
#                > 
#                  > cat("\nPercentage of cases where XGBoost V3 is better:", 
#                        +     round(mean(prediction_comparison_v3$xgb_better) * 100, 1), "%\n")
#                
#                Percentage of cases where XGBoost V3 is better: 39.4 %
#                > 
#                  > # Compare by censoring status
#                  > cat("\n=== V3 Performance by Censoring Status ===\n")
#                
#                === V3 Performance by Censoring Status ===
#                  > v3_by_censoring <- prediction_comparison_v3 %>%
#                  +   group_by(censored) %>%
#                  +   summarise(
#                    +     n_obs = n(),
#                    +     mae_bg = round(mean(bg_error), 4),
#                    +     mae_xgb_v3 = round(mean(xgb_error_v3), 4),
#                    +     improvement = round(mean(improvement_v3), 4),
#                    +     pct_better = round(mean(xgb_better) * 100, 1),
#                    +     .groups = "drop"
#                    +   ) %>%
#                  +   mutate(censoring_status = ifelse(censored == 1, "Censored", "Uncensored"))
#                > 
#                  > print(v3_by_censoring)
#                # A tibble: 2 × 7
#                censored n_obs mae_bg mae_xgb_v3 improvement pct_better censoring_status
#                <dbl> <int>  <dbl>      <dbl>       <dbl>      <dbl> <chr>           
#                  1        0 93804 0.0595     0.0457      0.0138       35.7 Uncensored      
#                2        1  6610 0.384      0.0635      0.320        91.1 Censored        
#                > 
#                  > #########################################
#                > # PLOTS FOR V3 MODEL
#                  > #########################################
#                > 
#                  > cat("\n=== Creating V3 Plots ===\n")
#                
#                === Creating V3 Plots ===
#                  > 
#                  > # Prepare data for plotting
#                  > combined_data_xgb_v3 <- bind_rows(
#                    +   data.frame(
#                      +     dailywage_final_orig = bg_train_v3,
#                      +     dailywage_final_pred = train_pred_lz_v3,
#                      +     lz_true = y_train_v3,
#                      +     censored = training_data_v3$bg_censored_flag[train_indices_v3]
#                      +   ),
#                    +   data.frame(
#                      +     dailywage_final_orig = prediction_comparison_v3$bg_observed,
#                      +     dailywage_final_pred = prediction_comparison_v3$xgb_predicted_v3,
#                      +     lz_true = prediction_comparison_v3$lz_true,
#                      +     censored = censored_v3
#                      +   )
#                    + ) %>%
#                  +   mutate(was_imputed = censored == 1) %>%
#                  +   filter(is.finite(dailywage_final_orig), 
#                             +          is.finite(dailywage_final_pred),
#                             +          is.finite(lz_true))
#                > 
#                  > # P1_xgb_v3: BG Original vs XGBoost V3 Predicted
#                  > wage_comparison_xgb_v3 <- combined_data_xgb_v3 %>%
#                  +   pivot_longer(cols = c(dailywage_final_orig, dailywage_final_pred),
#                                   +                names_to = "wage_type", 
#                                   +                values_to = "log_wage") %>%
#                  +   mutate(wage_type = ifelse(wage_type == "dailywage_final_orig", 
#                                                +                             "BG Original", "XGBoost V3 Predicted"))
#                > 
#                  > p1_xgb_v3 <- ggplot(wage_comparison_xgb_v3, aes(x = log_wage, fill = wage_type)) +
#                  +   geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
#                  +   scale_fill_manual(values = c("BG Original" = "blue", "XGBoost V3 Predicted" = "red")) +
#                  +   labs(title = "XGBoost V3: Distribution Comparison (BG Original vs XGBoost V3)",
#                           +        subtitle = "Model with temporal and geographic features",
#                           +        x = "ln Daily Wage (2000 Euros)",
#                           +        y = "Frequency",
#                           +        fill = "Wage Source") +
#                  +   theme_minimal() +
#                  +   theme(legend.position = "bottom") +
#                  +   xlim(0, 7)
#                > 
#                  > print(p1_xgb_v3)
#                Warning messages:
#                  1: Removed 168 rows containing non-finite values (stat_bin). 
#                2: Removed 4 rows containing missing values (geom_bar). 
#                > 
#                  > # P2_xgb_v3: By censoring status
#                  > p2_xgb_v3 <- ggplot(wage_comparison_xgb_v3, aes(x = log_wage, fill = wage_type)) +
#                  +   geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
#                  +   scale_fill_manual(values = c("BG Original" = "blue", "XGBoost V3 Predicted" = "red")) +
#                  +   facet_wrap(~ifelse(was_imputed, "Censored Observations", "Uncensored Observations"), 
#                                 +              scales = "free_y") +
#                  +   labs(title = "XGBoost V3: Wage Distribution by Censoring Status",
#                           +        x = "ln Daily Wage (2000 Euros)",
#                           +        y = "Frequency",
#                           +        fill = "Wage Source") +
#                  +   theme_minimal() +
#                  +   theme(legend.position = "bottom") +
#                  +   xlim(0, 7)
#                > 
#                  > print(p2_xgb_v3)
#                Warning messages:
#                  1: Removed 168 rows containing non-finite values (stat_bin). 
#                2: Removed 8 rows containing missing values (geom_bar). 
#                > 
#                  > # P11_xgb_v3: Three-way comparison
#                  > combined_wage_xgb_v3 <- combined_data_xgb_v3 %>%
#                  +   pivot_longer(cols = c(dailywage_final_orig, dailywage_final_pred, lz_true),
#                                   +                names_to = "wage_source", 
#                                   +                values_to = "daily_wage") %>%
#                  +   mutate(wage_source = case_when(
#                    +     wage_source == "dailywage_final_orig" ~ "BG Original",
#                    +     wage_source == "dailywage_final_pred" ~ "XGBoost V3 Predicted", 
#                    +     wage_source == "lz_true" ~ "LZ Uncensored"
#                    +   ))
#                > 
#                  > p11_xgb_v3 <- ggplot(combined_wage_xgb_v3, aes(x = daily_wage, fill = wage_source)) +
#                  +   geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
#                  +   scale_fill_manual(values = c("BG Original" = "blue", 
#                                                   +                                "XGBoost V3 Predicted" = "red", 
#                                                   +                                "LZ Uncensored" = "darkgreen")) +
#                  +   labs(title = "XGBoost V3: BG Original vs XGBoost V3 vs LZ Uncensored",
#                           +        subtitle = "Model with temporal and geographic features",
#                           +        x = "ln Daily Wage (2000 Euros)",
#                           +        y = "Frequency",
#                           +        fill = "Wage Source") +
#                  +   theme_minimal() +
#                  +   theme(legend.position = "bottom") +
#                  +   xlim(0, 7)
#                > 
#                  > print(p11_xgb_v3)
#                Warning messages:
#                  1: Removed 333 rows containing non-finite values (stat_bin). 
#                2: Removed 6 rows containing missing values (geom_bar). 
#                > 
#                  > # P12_xgb_v3: Three-way by censoring status
#                  > p12_xgb_v3 <- ggplot(combined_wage_xgb_v3, aes(x = daily_wage, fill = wage_source)) +
#                  +   geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
#                  +   scale_fill_manual(values = c("BG Original" = "blue", 
#                                                   +                                "XGBoost V3 Predicted" = "red", 
#                                                   +                                "LZ Uncensored" = "darkgreen")) +
#                  +   facet_wrap(~ifelse(was_imputed, "Censored Observations", "Uncensored Observations"), 
#                                 +              scales = "free_y") +
#                  +   labs(title = "XGBoost V3: Daily Wage Distribution by Censoring Status",
#                           +        x = "ln Daily Wage (2000 Euros)",
#                           +        y = "Frequency",
#                           +        fill = "Wage Source") +
#                  +   theme_minimal() +
#                  +   theme(legend.position = "bottom") +
#                  +   xlim(0, 7)
#                > 
#                  > print(p12_xgb_v3)
#                Warning messages:
#                  1: Removed 333 rows containing non-finite values (stat_bin). 
#                2: Removed 12 rows containing missing values (geom_bar). 
#                > 
#                  > # P13_xgb_v3: Density overlay
#                  > p13_xgb_v3 <- ggplot(combined_wage_xgb_v3, aes(x = daily_wage, color = wage_source)) +
#                  +   geom_density(size = 1.2, alpha = 0.7) +
#                  +   scale_color_manual(values = c("BG Original" = "blue", 
#                                                    +                                 "XGBoost V3 Predicted" = "red", 
#                                                    +                                 "LZ Uncensored" = "darkgreen")) +
#                  +   labs(title = "XGBoost V3: Density Overlay (BG vs XGBoost V3 vs LZ)",
#                           +        subtitle = "Model with temporal and geographic features",
#                           +        x = "ln Daily Wage (2000 Euros)",
#                           +        y = "Density",
#                           +        color = "Wage Source") +
#                  +   theme_minimal() +
#                  +   theme(legend.position = "bottom") +
#                  +   xlim(0, 7)
#                > 
#                  > print(p13_xgb_v3)
#                Warning message:
#                  Removed 333 rows containing non-finite values (stat_density). 
#                > 
#                  > #########################################
#                > # V3 ERROR DISTRIBUTION AND COMPARISON
#                  > #########################################
#                > 
#                  > cat("\n=== Creating V3 Error Comparison ===\n")
#                
#                === Creating V3 Error Comparison ===
#                  > 
#                  > # V3 errors (all observations)
#                  > xgb_v3_errors_all <- prediction_comparison_v3 %>%
#                  +   mutate(
#                    +     error = xgb_predicted_v3 - lz_true,
#                    +     method = "XGBoost V3"
#                    +   ) %>%
#                  +   select(error, method, censored) %>%
#                  +   mutate(was_imputed = (censored == 1))
#                > 
#                  > # V3 errors (censored only)
#                  > xgb_v3_errors_censored <- xgb_v3_errors_all %>%
#                  +   filter(was_imputed)
#                > 
#                  > # Add V3 to comprehensive comparison (censored only)
#                  > censored_errors_with_v3 <- bind_rows(
#                    +   censored_errors_corrected,
#                    +   xgb_v3_errors_censored %>% select(error, method, was_imputed)
#                    + ) %>%
#                  +   filter(is.finite(error))
#                > 
#                  > # Plot: Censored observations with V3
#                  > p14_with_v3_censored <- ggplot(censored_errors_with_v3, aes(x = error, color = method, fill = method)) +
#                  +   geom_density(alpha = 0.3, size = 1.2) +
#                  +   geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
#                  +   scale_color_manual(values = c("Tobit" = "purple", 
#                                                    +                                 "XGBoost V1" = "red", 
#                                                    +                                 "XGBoost V2" = "orange",
#                                                    +                                 "XGBoost V3" = "darkblue")) +
#                  +   scale_fill_manual(values = c("Tobit" = "purple", 
#                                                   +                                "XGBoost V1" = "red", 
#                                                   +                                "XGBoost V2" = "orange",
#                                                   +                                "XGBoost V3" = "darkblue")) +
#                  +   labs(title = "Error Distribution: All Methods Including V3",
#                           +        subtitle = "CENSORED observations only - Error = Predicted - LZ True Wage",
#                           +        x = "Prediction Error (ln wage units)",
#                           +        y = "Density",
#                           +        color = "Method",
#                           +        fill = "Method") +
#                  +   theme_minimal() +
#                  +   theme(legend.position = "bottom",
#                            +         plot.title = element_text(face = "bold")) +
#                  +   xlim(-2, 2)
#                > 
#                  > print(p14_with_v3_censored)
#                Warning message:
#                  Removed 306 rows containing non-finite values (stat_density). 
#                > 
#                  > # Add V3 to comprehensive comparison (all observations)
#                  > all_errors_with_v3 <- bind_rows(
#                    +   all_errors_corrected,
#                    +   xgb_v3_errors_all %>% select(error, method)
#                    + ) %>%
#                  +   filter(is.finite(error))
#                > 
#                  > # Plot: All observations with V3
#                  > p14_with_v3_all <- ggplot(all_errors_with_v3, aes(x = error, color = method, fill = method)) +
#                  +   geom_density(alpha = 0.3, size = 1.2) +
#                  +   geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
#                  +   scale_color_manual(values = c("Tobit" = "purple", 
#                                                    +                                 "XGBoost V1" = "red", 
#                                                    +                                 "XGBoost V2" = "orange",
#                                                    +                                 "XGBoost V3" = "darkblue")) +
#                  +   scale_fill_manual(values = c("Tobit" = "purple", 
#                                                   +                                "XGBoost V1" = "red", 
#                                                   +                                "XGBoost V2" = "orange",
#                                                   +                                "XGBoost V3" = "darkblue")) +
#                  +   labs(title = "Error Distribution: All Methods Including V3",
#                           +        subtitle = "ALL observations - Error = Predicted - LZ True Wage",
#                           +        x = "Prediction Error (ln wage units)",
#                           +        y = "Density",
#                           +        color = "Method",
#                           +        fill = "Method") +
#                  +   theme_minimal() +
#                  +   theme(legend.position = "bottom",
#                            +         plot.title = element_text(face = "bold")) +
#                  +   xlim(-2, 2)
#                > 
#                  > print(p14_with_v3_all)
#                Warning message:
#                  Removed 2117 rows containing non-finite values (stat_density). 
#                > 
#                  > # Summary statistics with V3 (censored only)
#                  > error_summary_with_v3_censored <- censored_errors_with_v3 %>%
#                  +   group_by(method) %>%
#                  +   summarise(
#                    +     n_obs = n(),
#                    +     mean_error = round(mean(error), 4),
#                    +     median_error = round(median(error), 4),
#                    +     sd_error = round(sd(error), 4),
#                    +     mae = round(mean(abs(error)), 4),
#                    +     rmse = round(sqrt(mean(error^2)), 4),
#                    +     .groups = "drop"
#                    +   )
#                > 
#                  > cat("\n=== Error Summary WITH V3 (CENSORED ONLY) ===\n")
#                
#                === Error Summary WITH V3 (CENSORED ONLY) ===
#                  > print(error_summary_with_v3_censored)
#                # A tibble: 4 × 7
#                method     n_obs mean_error median_error sd_error    mae   rmse
#                <chr>      <int>      <dbl>        <dbl>    <dbl>  <dbl>  <dbl>
#                  1 Tobit      33447     0.0042      -0.0066   0.511  0.287  0.511 
#                2 XGBoost V1  6610    -0.0046       0.0016   0.126  0.0843 0.126 
#                3 XGBoost V2  6610    -0.0008       0.0041   0.0927 0.0638 0.0927
#                4 XGBoost V3  6610    -0.0006       0.0035   0.0963 0.0635 0.0963
#                > 
#                  > # Summary statistics with V3 (all observations)
#                  > error_summary_with_v3_all <- all_errors_with_v3 %>%
#                  +   group_by(method) %>%
#                  +   summarise(
#                    +     n_obs = n(),
#                    +     mean_error = round(mean(error), 4),
#                    +     median_error = round(median(error), 4),
#                    +     sd_error = round(sd(error), 4),
#                    +     mae = round(mean(abs(error)), 4),
#                    +     rmse = round(sqrt(mean(error^2)), 4),
#                    +     .groups = "drop"
#                    +   )
#                > 
#                  > cat("\n=== Error Summary WITH V3 (ALL OBSERVATIONS) ===\n")
#                
#                === Error Summary WITH V3 (ALL OBSERVATIONS) ===
#                  > print(error_summary_with_v3_all)
#                # A tibble: 4 × 7
#                method      n_obs mean_error median_error sd_error    mae  rmse
#                <chr>       <int>      <dbl>        <dbl>    <dbl>  <dbl> <dbl>
#                  1 Tobit      502023     0.137        0.101     0.292 0.173  0.322
#                2 XGBoost V1 100414     0.0002      -0.0113    0.151 0.07   0.151
#                3 XGBoost V2 100414     0           -0.0089    0.112 0.047  0.112
#                4 XGBoost V3 100414     0.0001      -0.0086    0.112 0.0469 0.112
#                > 
#                  > # Performance ranking
#                  > cat("\n=== FINAL PERFORMANCE RANKING (CENSORED OBSERVATIONS) ===\n")
#                
#                === FINAL PERFORMANCE RANKING (CENSORED OBSERVATIONS) ===
#                  > ranking_censored <- error_summary_with_v3_censored %>%
#                  +   arrange(rmse) %>%
#                  +   mutate(
#                    +     rank = row_number(),
#                    +     improvement_vs_worst = round((max(rmse) - rmse) / max(rmse) * 100, 1)
#                    +   )
#                > print(ranking_censored)
#                # A tibble: 4 × 9
#                method     n_obs mean_error median_error sd_error    mae   rmse  rank improvement_vs_worst
#                <chr>      <int>      <dbl>        <dbl>    <dbl>  <dbl>  <dbl> <int>                <dbl>
#                  1 XGBoost V2  6610    -0.0008       0.0041   0.0927 0.0638 0.0927     1                 81.9
#                2 XGBoost V3  6610    -0.0006       0.0035   0.0963 0.0635 0.0963     2                 81.2
#                3 XGBoost V1  6610    -0.0046       0.0016   0.126  0.0843 0.126      3                 75.4
#                4 Tobit      33447     0.0042      -0.0066   0.511  0.287  0.511      4                  0  
#                > 
#                  > cat("\n=== FINAL PERFORMANCE RANKING (ALL OBSERVATIONS) ===\n")
#                
#                === FINAL PERFORMANCE RANKING (ALL OBSERVATIONS) ===
#                  > ranking_all <- error_summary_with_v3_all %>%
#                  +   arrange(rmse) %>%
#                  +   mutate(
#                    +     rank = row_number(),
#                    +     improvement_vs_worst = round((max(rmse) - rmse) / max(rmse) * 100, 1)
#                    +   )
#                > print(ranking_all)
#                # A tibble: 4 × 9
#                method      n_obs mean_error median_error sd_error    mae  rmse  rank improvement_vs_worst
#                <chr>       <int>      <dbl>        <dbl>    <dbl>  <dbl> <dbl> <int>                <dbl>
#                  1 XGBoost V3 100414     0.0001      -0.0086    0.112 0.0469 0.112     1                 65.3
#                2 XGBoost V2 100414     0           -0.0089    0.112 0.047  0.112     2                 65.2
#                3 XGBoost V1 100414     0.0002      -0.0113    0.151 0.07   0.151     3                 53.3
#                4 Tobit      502023     0.137        0.101     0.292 0.173  0.322     4                  0  
#                > 
#                  > cat("\n=== XGBoost V3 Model with Temporal and Geographic Features Complete ===\n")
#                
#                === XGBoost V3 Model with Temporal and Geographic Features Complete ===
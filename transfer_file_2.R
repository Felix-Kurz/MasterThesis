# > dim(tobit_data)
# [1] 529735     34
# > dim(training_data)
# [1] 509006     41
# 
# 
# 
# 
# > tobit_summary_old
# # A tibble: 2 × 8
# was_imputed method       n_obs mean_error median_error sd_error   mae  rmse
# <lgl>       <chr>        <int>      <dbl>        <dbl>    <dbl> <dbl> <dbl>
#   1 FALSE       Tobit (OLD) 470743      0.130       0.0985    0.126 0.130 0.181
# 2 TRUE        Tobit (OLD)  36997      0.311       0.291     0.210 0.311 0.375
# > tobit_summary_new
# Error: object 'tobit_summary_new' not found
# > tobit_summary_corrected
# # A tibble: 2 × 8
# was_imputed method  n_obs mean_error median_error sd_error   mae  rmse
# <lgl>       <chr>   <int>      <dbl>        <dbl>    <dbl> <dbl> <dbl>
#   1 FALSE       Tobit  470743     0.146        0.106     0.276 0.168 0.313
# 2 TRUE        Tobit   36997    -0.0431      -0.0294    0.485 0.292 0.486
# > error_summary_all
# # A tibble: 3 × 7
# method      n_obs mean_error median_error sd_error    mae  rmse
# <chr>       <int>      <dbl>        <dbl>    <dbl>  <dbl> <dbl>
#   1 Tobit      529109     0.144        0.105     0.143 0.144  0.203
# 2 XGBoost V1 101556     0.0004      -0.0088    0.137 0.0514 0.137
# 3 XGBoost V2 101556     0.0004      -0.0081    0.123 0.048  0.123
# > error_summary_censored
# # A tibble: 3 × 7
# method     n_obs mean_error median_error sd_error    mae   rmse
# <chr>      <int>      <dbl>        <dbl>    <dbl>  <dbl>  <dbl>
#   1 Tobit      38984     0.310        0.289    0.211  0.310  0.374 
# 2 XGBoost V1  7438     0.0016       0.0041   0.119  0.0741 0.119 
# 3 XGBoost V2  7438     0.0016       0.0034   0.0998 0.0636 0.0998
# > error_summary_corrected_all
# # A tibble: 3 × 7
# method      n_obs mean_error median_error sd_error    mae  rmse
# <chr>       <int>      <dbl>        <dbl>    <dbl>  <dbl> <dbl>
#   1 Tobit      507740     0.132        0.102     0.301 0.177  0.328
# 2 XGBoost V1 101556     0.0004      -0.0088    0.137 0.0514 0.137
# 3 XGBoost V2 101556     0.0004      -0.0081    0.123 0.048  0.123
# > (181*470743+0.375*36997)/507740
# [1] 167.8386
# > (.181*470743+0.375*36997)/507740
# [1] 0.195136
# > (.313*470743+0.486*36997)/507740
# [1] 0.3256058
# > 
#   
#   
#   
#   > #########################################
# > # CORRECTED COMPARISON: ALL METHODS vs LZ TRUE WAGES
#   > #########################################
# > 
#   > cat("\n=== Creating Corrected Comparison Against LZ Ground Truth ===\n")
# 
# === Creating Corrected Comparison Against LZ Ground Truth ===
#   > 
#   > # Step 1: Get LZ data (same processing as XGBoost training)
#   > lz_comparison_data <- lohnzettel %>%
#   +   filter(penr %in% hauptverband_2022_1994_until_2012_penr_sample_vec) %>%
#   +   filter(year >= 1994, year <= 2012) %>%
#   +   rename(contryear = year) %>%
#   +   left_join(vpi_1986 %>% select(contryear, cpi_2000), by = "contryear") %>%
#   +   mutate(
#     +     lz_dwage_log = log(pmax(lz_dwage * cpi_2000, 1))
#     +   ) %>%
#   +   select(penr, benr, contryear, lz_dwage_log) %>%
#   +   filter(!is.na(lz_dwage_log), is.finite(lz_dwage_log))
# > 
#   > cat("LZ data available:", nrow(lz_comparison_data), "observations\n")
# LZ data available: 124698 observations
# > 
#   > # Step 2: Merge Tobit results with LZ ground truth
#   > tobit_corrected <- final_imputed_data %>%
#   +   inner_join(lz_comparison_data, by = c("penr", "benr", "contryear")) %>%
#   +   mutate(
#     +     error_vs_lz = dailywage_final_pred - lz_dwage_log,
#     +     error_vs_censored = dailywage_final_pred - dailywage_final_orig,
#     +     was_imputed = (dailywage_final != dailywage_final_orig)
#     +   ) %>%
#   +   filter(is.finite(error_vs_lz))
# > 
#   > cat("Tobit observations with LZ match:", nrow(tobit_corrected), "\n")
# Tobit observations with LZ match: 507740 
# > 
#   > # Step 3: Summary statistics - Tobit vs LZ (corrected)
#   > tobit_summary_corrected <- tobit_corrected %>%
#   +   group_by(was_imputed) %>%
#   +   summarise(
#     +     method = "Tobit",
#     +     n_obs = n(),
#     +     mean_error = round(mean(error_vs_lz), 4),
#     +     median_error = round(median(error_vs_lz), 4),
#     +     sd_error = round(sd(error_vs_lz), 4),
#     +     mae = round(mean(abs(error_vs_lz)), 4),
#     +     rmse = round(sqrt(mean(error_vs_lz^2)), 4),
#     +     .groups = "drop"
#     +   )
# > 
#   > cat("\n=== CORRECTED Tobit Error Summary (vs LZ True Wages) ===\n")
# 
# === CORRECTED Tobit Error Summary (vs LZ True Wages) ===
#   > print(tobit_summary_corrected)
# # A tibble: 2 × 8
# was_imputed method  n_obs mean_error median_error sd_error   mae  rmse
# <lgl>       <chr>   <int>      <dbl>        <dbl>    <dbl> <dbl> <dbl>
#   1 FALSE       Tobit  470743     0.146        0.106     0.276 0.168 0.313
# 2 TRUE        Tobit   36997    -0.0431      -0.0294    0.485 0.292 0.486
# > 
#   > # Step 4: Compare old vs new Tobit RMSE
#   > tobit_summary_old <- tobit_corrected %>%
#   +   group_by(was_imputed) %>%
#   +   summarise(
#     +     method = "Tobit (OLD)",
#     +     n_obs = n(),
#     +     mean_error = round(mean(error_vs_censored), 4),
#     +     median_error = round(median(error_vs_censored), 4),
#     +     sd_error = round(sd(error_vs_censored), 4),
#     +     mae = round(mean(abs(error_vs_censored)), 4),
#     +     rmse = round(sqrt(mean(error_vs_censored^2)), 4),
#     +     .groups = "drop"
#     +   )
# > 
#   > cat("\n=== OLD Tobit Error Summary (vs Censored BG - WRONG!) ===\n")
# 
# === OLD Tobit Error Summary (vs Censored BG - WRONG!) ===
#   > print(tobit_summary_old)
# # A tibble: 2 × 8
# was_imputed method       n_obs mean_error median_error sd_error   mae  rmse
# <lgl>       <chr>        <int>      <dbl>        <dbl>    <dbl> <dbl> <dbl>
#   1 FALSE       Tobit (OLD) 470743      0.130       0.0985    0.126 0.130 0.181
# 2 TRUE        Tobit (OLD)  36997      0.311       0.291     0.210 0.311 0.375
# > 
#   > # Step 5: Prepare corrected error data for all methods
#   > 
#   > # Tobit errors (corrected - all observations with LZ)
#   > tobit_errors_corrected_all <- tobit_corrected %>%
#   +   mutate(method = "Tobit") %>%
#   +   select(error = error_vs_lz, method, was_imputed)
# > 
#   > # Tobit errors (corrected - censored only)
#   > tobit_errors_corrected_censored <- tobit_corrected %>%
#   +   filter(was_imputed) %>%
#   +   mutate(method = "Tobit") %>%
#   +   select(error = error_vs_lz, method, was_imputed)
# > 
#   > # XGBoost V1 errors (all)
#   > xgb_v1_errors_corrected_all <- prediction_comparison %>%
#   +   mutate(
#     +     error = xgb_predicted - lz_true,
#     +     method = "XGBoost V1",
#     +     was_imputed = NA
#     +   ) %>%
#   +   select(error, method, was_imputed)
# > 
#   > # XGBoost V1 errors (censored only)
#   > censored_v1 <- training_data$bg_censored_flag[-train_indices]
# > xgb_v1_errors_corrected_censored <- prediction_comparison %>%
#   +   mutate(
#     +     censored = censored_v1,
#     +     error = xgb_predicted - lz_true,
#     +     method = "XGBoost V1",
#     +     was_imputed = (censored == 1)
#     +   ) %>%
#   +   filter(was_imputed) %>%
#   +   select(error, method, was_imputed)
# > 
#   > # XGBoost V2 errors (all)
#   > xgb_v2_errors_corrected_all <- prediction_comparison_v2 %>%
#   +   mutate(
#     +     error = xgb_predicted_v2 - lz_true,
#     +     method = "XGBoost V2",
#     +     was_imputed = NA
#     +   ) %>%
#   +   select(error, method, was_imputed)
# > 
#   > # XGBoost V2 errors (censored only)
#   > censored_v2 <- training_data$bg_censored_flag[-train_indices_v2]
# > xgb_v2_errors_corrected_censored <- prediction_comparison_v2 %>%
#   +   mutate(
#     +     censored = censored_v2,
#     +     error = xgb_predicted_v2 - lz_true,
#     +     method = "XGBoost V2",
#     +     was_imputed = (censored == 1)
#     +   ) %>%
#   +   filter(was_imputed) %>%
#   +   select(error, method, was_imputed)
# > 
#   > # Step 6: Combine all corrected errors
#   > 
#   > # All observations
#   > all_errors_corrected <- bind_rows(
#     +   tobit_errors_corrected_all,
#     +   xgb_v1_errors_corrected_all,
#     +   xgb_v2_errors_corrected_all
#     + ) %>%
#   +   filter(is.finite(error))
# > 
#   > # Censored only
#   > censored_errors_corrected <- bind_rows(
#     +   tobit_errors_corrected_censored,
#     +   xgb_v1_errors_corrected_censored,
#     +   xgb_v2_errors_corrected_censored
#     + ) %>%
#   +   filter(is.finite(error))
# > 
#   > # Step 7: Summary statistics - ALL METHODS (corrected, all observations)
#   > cat("\n=== CORRECTED Error Summary:  ALL OBSERVATIONS ===\n")
# 
# === CORRECTED Error Summary:  ALL OBSERVATIONS ===
#   > error_summary_corrected_all <- all_errors_corrected %>%
#   +   group_by(method) %>%
#   +   summarise(
#     +     n_obs = n(),
#     +     mean_error = round(mean(error), 4),
#     +     median_error = round(median(error), 4),
#     +     sd_error = round(sd(error), 4),
#     +     mae = round(mean(abs(error)), 4),
#     +     rmse = round(sqrt(mean(error^2)), 4),
#     +     .groups = "drop"
#     +   )
# > print(error_summary_corrected_all)
# # A tibble: 3 × 7
# method      n_obs mean_error median_error sd_error    mae  rmse
# <chr>       <int>      <dbl>        <dbl>    <dbl>  <dbl> <dbl>
#   1 Tobit      507740     0.132        0.102     0.301 0.177  0.328
# 2 XGBoost V1 101556     0.0004      -0.0088    0.137 0.0514 0.137
# 3 XGBoost V2 101556     0.0004      -0.0081    0.123 0.048  0.123
# > 
#   > # Step 8: Summary statistics - ALL METHODS (corrected, censored only)
#   > cat("\n=== CORRECTED Error Summary:  CENSORED OBSERVATIONS ONLY ===\n")
# 
# === CORRECTED Error Summary:  CENSORED OBSERVATIONS ONLY ===
#   > error_summary_corrected_censored <- censored_errors_corrected %>%
#   +   group_by(method) %>%
#   +   summarise(
#     +     n_obs = n(),
#     +     mean_error = round(mean(error), 4),
#     +     median_error = round(median(error), 4),
#     +     sd_error = round(sd(error), 4),
#     +     mae = round(mean(abs(error)), 4),
#     +     rmse = round(sqrt(mean(error^2)), 4),
#     +     .groups = "drop"
#     +   )
# > print(error_summary_corrected_censored)
# # A tibble: 3 × 7
# method     n_obs mean_error median_error sd_error    mae   rmse
# <chr>      <int>      <dbl>        <dbl>    <dbl>  <dbl>  <dbl>
#   1 Tobit      36997    -0.0431      -0.0294   0.485  0.292  0.486 
# 2 XGBoost V1  7438     0.0016       0.0041   0.119  0.0741 0.119 
# 3 XGBoost V2  7438     0.0016       0.0034   0.0998 0.0636 0.0998
# > 
#   > # Step 9: Create corrected plots
#   > 
#   > # Plot 1: All observations
#   > p14_corrected_all <- ggplot(all_errors_corrected, aes(x = error, color = method, fill = method)) +
#   +   geom_density(alpha = 0.3, size = 1.2) +
#   +   geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
#   +   scale_color_manual(values = c("Tobit" = "purple", 
#                                     +                                 "XGBoost V1" = "red", 
#                                     +                                 "XGBoost V2" = "orange")) +
#   +   scale_fill_manual(values = c("Tobit" = "purple", 
#                                    +                                "XGBoost V1" = "red", 
#                                    +                                "XGBoost V2" = "orange")) +
#   +   labs(title = "CORRECTED Error Distribution:  Tobit vs XGBoost V1 vs XGBoost V2",
#            +        subtitle = "ALL observations - Error = Predicted - LZ True Wage",
#            +        x = "Prediction Error (ln wage units)",
#            +        y = "Density",
#            +        color = "Method",
#            +        fill = "Method") +
#   +   theme_minimal() +
#   +   theme(legend.position = "bottom",
#             +         plot.title = element_text(face = "bold")) +
#   +   xlim(-2, 2)
# > 
#   > print(p14_corrected_all)
# Warning message:
#   Removed 2242 rows containing non-finite values (stat_density). 
# > 
#   > # Plot 2: Censored observations only
#   > p14_corrected_censored <- ggplot(censored_errors_corrected, aes(x = error, color = method, fill = method)) +
#   +   geom_density(alpha = 0.3, size = 1.2) +
#   +   geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
#   +   scale_color_manual(values = c("Tobit" = "purple", 
#                                     +                                 "XGBoost V1" = "red", 
#                                     +                                 "XGBoost V2" = "orange")) +
#   +   scale_fill_manual(values = c("Tobit" = "purple", 
#                                    +                                "XGBoost V1" = "red", 
#                                    +                                "XGBoost V2" = "orange")) +
#   +   labs(title = "CORRECTED Error Distribution:  Tobit vs XGBoost V1 vs XGBoost V2",
#            +        subtitle = "CENSORED observations only - Error = Predicted - LZ True Wage",
#            +        x = "Prediction Error (ln wage units)",
#            +        y = "Density",
#            +        color = "Method",
#            +        fill = "Method") +
#   +   theme_minimal() +
#   +   theme(legend.position = "bottom",
#             +         plot.title = element_text(face = "bold")) +
#   +   xlim(-2, 2)
# > 
#   > print(p14_corrected_censored)
# Warning message:
#   Removed 323 rows containing non-finite values (stat_density). 
# > 
#   > # Step 10: Calculate improvement percentages
#   > cat("\n=== CORRECTED Performance Comparison ===\n")
# 
# === CORRECTED Performance Comparison ===
#   > 
#   > improvement_all <- error_summary_corrected_all %>%
#   +   arrange(method) %>%
#   +   mutate(
#     +     rmse_improvement_vs_tobit = round((rmse[method == "Tobit"] - rmse) / rmse[method == "Tobit"] * 100, 1),
#     +     mae_improvement_vs_tobit = round((mae[method == "Tobit"] - mae) / mae[method == "Tobit"] * 100, 1)
#     +   )
# > 
#   > cat("\nAll Observations - RMSE Improvement vs Tobit:\n")
# 
# All Observations - RMSE Improvement vs Tobit:
#   > print(improvement_all %>% select(method, rmse, rmse_improvement_vs_tobit, mae, mae_improvement_vs_tobit))
# # A tibble: 3 × 5
# method      rmse rmse_improvement_vs_tobit    mae mae_improvement_vs_tobit
# <chr>      <dbl>                     <dbl>  <dbl>                    <dbl>
#   1 Tobit      0.328                       0   0.177                       0  
# 2 XGBoost V1 0.137                      58.3 0.0514                     71  
# 3 XGBoost V2 0.123                      62.6 0.048                      72.9
# > 
#   > improvement_censored <- error_summary_corrected_censored %>%
#   +   arrange(method) %>%
#   +   mutate(
#     +     rmse_improvement_vs_tobit = round((rmse[method == "Tobit"] - rmse) / rmse[method == "Tobit"] * 100, 1),
#     +     mae_improvement_vs_tobit = round((mae[method == "Tobit"] - mae) / mae[method == "Tobit"] * 100, 1)
#     +   )
# > 
#   > cat("\nCensored Observations - RMSE Improvement vs Tobit:\n")
# 
# Censored Observations - RMSE Improvement vs Tobit:
#   > print(improvement_censored %>% select(method, rmse, rmse_improvement_vs_tobit, mae, mae_improvement_vs_tobit))
# # A tibble: 3 × 5
# method       rmse rmse_improvement_vs_tobit    mae mae_improvement_vs_tobit
# <chr>       <dbl>                     <dbl>  <dbl>                    <dbl>
#   1 Tobit      0.486                        0   0.292                       0  
# 2 XGBoost V1 0.119                       75.6 0.0741                     74.7
# 3 XGBoost V2 0.0998                      79.5 0.0636                     78.2
# > 
#   > cat("\n=== Corrected Comparison Complete ===\n")
# 
# === Corrected Comparison Complete ===
#   > cat("Key Finding: Now all methods are evaluated against LZ ground truth!\n")
# Key Finding: Now all methods are evaluated against LZ ground truth!
#   >
#   
#   
#   
#   
# 
# 
#   
#   
#   
#   
#   
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# > print(head(importance_matrix_v2, 20))
# Feature         Gain        Cover    Frequency
# <char>        <num>        <num>        <num>
#   1:          interval_lower 5.558089e-01 0.1685539906 0.1084506405
# 2:          interval_upper 2.240447e-01 0.0846884531 0.0434979405
# 3:               mean_wage 1.271766e-01 0.3094414159 0.2226723397
# 4:     mean_wage_coworkers 2.585615e-02 0.1989312282 0.1380527769
# 5:                     age 2.200100e-02 0.0460381874 0.1674285973
# 6:               firm_size 1.277641e-02 0.0372350826 0.0903227267
# 7:               num_years 9.868197e-03 0.0538307401 0.0690942833
# 8:               birthyear 8.815926e-03 0.0429635018 0.0845516679
# 9:           mean_censored 7.679362e-03 0.0190496915 0.0152989635
# 10:        firm_size_square 2.652261e-03 0.0105415956 0.0228579188
# 11: mean_censored_coworkers 1.667799e-03 0.0246085537 0.0157063323
# 12:                    frau 1.503134e-03 0.0033954878 0.0203231793
# 13:        bg_censored_flag 1.054055e-04 0.0005003342 0.0001810528
# 14:         smallfirm_dummy 4.414797e-05 0.0002217375 0.0015615806
# > 
#   
#   
#  
# 
# # Merge wage information for each year
# full_data <- employment_data %>%
#   filter(contryear >= firstyear) %>%
#   left_join(all_processed_wages, by = c("penr", "benr", "contryear")) %>%
#   mutate(dailywage_final = wage_bg / duration) %>%
#   filter(!is.na(dailywage_final), !is.na(censored))
# 
# 
# 
# # Merge wage information for each year
# full_data <- employment_data %>%
#   filter(contryear >= firstyear) %>%
#   left_join(all_processed_wages, by = c("penr", "benr", "contryear")) %>%
#   mutate(dailywage_final = wage_bg / duration) %>%
#   filter(!is.na(dailywage_final), !is.na(censored))
# 
# 
# 
# 
# # Table: code -> label for subcat
# subcat_labels <- attr(qual_cat_step3_dta$subcat, "labels")
# 
# subcat_label_table <- tibble::tibble(
#   code = as.numeric(subcat_labels),
#   label = names(subcat_labels)
# ) %>%
#   dplyr::arrange(code)
# 
# print(subcat_label_table, n = Inf)
# unique(qual_cat_step3_dta$subcat)
# 
# 
# 
# 
# # Table: code -> label for cat
# cat_labels <- attr(qual_cat_step3_dta$cat, "labels")
# 
# cat_label_table <- tibble::tibble(
#   code = as.numeric(cat_labels),
#   label = names(cat_labels)
# ) %>%
#   dplyr::arrange(code)
# 
# print(cat_label_table, n = Inf)
# unique(qual_cat_step3_dta$cat)
# 
# 
# 
# # Table: code -> label for qual
# qual_labels <- attr(qualifikationen_data$qu_qual, "labels")
# 
# qual_label_table <- tibble::tibble(
#   code = as.numeric(qual_labels),
#   label = names(qual_labels)
# ) %>%
#   dplyr::arrange(code)
# 
# print(qual_label_table, n = Inf)
# unique_qual <- unique(qualifikationen_data$qu_qual)
# any(unique_qual == "49")
# 
# 
# sum(qualifikationen_data$qu_qual == "49", na.rm = TRUE)
# summary(qualifikationen_data$qu_vdat)
# 
# > 
#   > #########################################
# > # V3 ERROR DISTRIBUTION AND COMPARISON
#   > #########################################
# > 
#   > cat("\n=== Creating V3 Error Comparison ===\n")
# 
# === Creating V3 Error Comparison ===
#   > 
#   > # V3 errors (all observations)
#   > xgb_v3_errors_all <- prediction_comparison_v3 %>%
#   +   mutate(
#     +     error = xgb_predicted_v3 - lz_true,
#     +     method = "XGBoost V3"
#     +   ) %>%
#   +   select(error, method, censored) %>%
#   +   mutate(was_imputed = (censored == 1))
# > 
#   > # V3 errors (censored only)
#   > xgb_v3_errors_censored <- xgb_v3_errors_all %>%
#   +   filter(was_imputed)
# > 
#   > # Add V3 to comprehensive comparison (censored only)
#   > censored_errors_with_v3 <- bind_rows(
#     +   censored_errors_corrected,
#     +   xgb_v3_errors_censored %>% select(error, method, was_imputed)
#     + ) %>%
#   +   filter(is.finite(error))
# > 
#   > # Plot: Censored observations with V3
#   > p14_with_v3_censored <- ggplot(censored_errors_with_v3, aes(x = error, color = method, fill = method)) +
#   +   geom_density(alpha = 0.3, size = 1.2) +
#   +   geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
#   +   scale_color_manual(values = c("Tobit" = "purple", 
#                                     +                                 "XGBoost V1" = "red", 
#                                     +                                 "XGBoost V2" = "orange",
#                                     +                                 "XGBoost V3" = "darkblue")) +
#   +   scale_fill_manual(values = c("Tobit" = "purple", 
#                                    +                                "XGBoost V1" = "red", 
#                                    +                                "XGBoost V2" = "orange",
#                                    +                                "XGBoost V3" = "darkblue")) +
#   +   labs(title = "Error Distribution: All Methods Including V3",
#            +        subtitle = "CENSORED observations only - Error = Predicted - LZ True Wage",
#            +        x = "Prediction Error (ln wage units)",
#            +        y = "Density",
#            +        color = "Method",
#            +        fill = "Method") +
#   +   theme_minimal() +
#   +   theme(legend.position = "bottom",
#             +         plot.title = element_text(face = "bold")) +
#   +   xlim(-2, 2)
# > 
#   > print(p14_with_v3_censored)
# Warning message:
#   Removed 247 rows containing non-finite values (stat_density). 
# > 
#   > # Add V3 to comprehensive comparison (all observations)
#   > all_errors_with_v3 <- bind_rows(
#     +   all_errors_corrected,
#     +   xgb_v3_errors_all %>% select(error, method)
#     + ) %>%
#   +   filter(is.finite(error))
# > 
#   > # Plot: All observations with V3
#   > p14_with_v3_all <- ggplot(all_errors_with_v3, aes(x = error, color = method, fill = method)) +
#   +   geom_density(alpha = 0.3, size = 1.2) +
#   +   geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
#   +   scale_color_manual(values = c("Tobit" = "purple", 
#                                     +                                 "XGBoost V1" = "red", 
#                                     +                                 "XGBoost V2" = "orange",
#                                     +                                 "XGBoost V3" = "darkblue")) +
#   +   scale_fill_manual(values = c("Tobit" = "purple", 
#                                    +                                "XGBoost V1" = "red", 
#                                    +                                "XGBoost V2" = "orange",
#                                    +                                "XGBoost V3" = "darkblue")) +
#   +   labs(title = "Error Distribution: All Methods Including V3",
#            +        subtitle = "ALL observations - Error = Predicted - LZ True Wage",
#            +        x = "Prediction Error (ln wage units)",
#            +        y = "Density",
#            +        color = "Method",
#            +        fill = "Method") +
#   +   theme_minimal() +
#   +   theme(legend.position = "bottom",
#             +         plot.title = element_text(face = "bold")) +
#   +   xlim(-2, 2)
# > 
#   > print(p14_with_v3_all)
# Warning message:
#   Removed 2406 rows containing non-finite values (stat_density). 
# > 
#   > # Summary statistics with V3 (censored only)
#   > error_summary_with_v3_censored <- censored_errors_with_v3 %>%
#   +   group_by(method) %>%
#   +   summarise(
#     +     n_obs = n(),
#     +     mean_error = round(mean(error), 4),
#     +     median_error = round(median(error), 4),
#     +     sd_error = round(sd(error), 4),
#     +     mae = round(mean(abs(error)), 4),
#     +     rmse = round(sqrt(mean(error^2)), 4),
#     +     .groups = "drop"
#     +   )
# > 
#   > cat("\n=== Error Summary WITH V3 (CENSORED ONLY) ===\n")
# 
# === Error Summary WITH V3 (CENSORED ONLY) ===
#   > print(error_summary_with_v3_censored)
# # A tibble: 4 × 7
# method     n_obs mean_error median_error sd_error    mae   rmse
# <chr>      <int>      <dbl>        <dbl>    <dbl>  <dbl>  <dbl>
#   1 Tobit      34972    -0.05        -0.0442   0.465  0.281  0.467 
# 2 XGBoost V1  6941    -0.0031       0.001    0.145  0.091  0.145 
# 3 XGBoost V2  6941     0.0021       0.004    0.109  0.0666 0.109 
# 4 XGBoost V3  6941     0.0016       0.0034   0.0995 0.0636 0.0995
# > 
#   > # Summary statistics with V3 (all observations)
#   > error_summary_with_v3_all <- all_errors_with_v3 %>%
#   +   group_by(method) %>%
#   +   summarise(
#     +     n_obs = n(),
#     +     mean_error = round(mean(error), 4),
#     +     median_error = round(median(error), 4),
#     +     sd_error = round(sd(error), 4),
#     +     mae = round(mean(abs(error)), 4),
#     +     rmse = round(sqrt(mean(error^2)), 4),
#     +     .groups = "drop"
#     +   )
# > 
#   > cat("\n=== Error Summary WITH V3 (ALL OBSERVATIONS) ===\n")
# 
# === Error Summary WITH V3 (ALL OBSERVATIONS) ===
#   > print(error_summary_with_v3_all)
# # A tibble: 4 × 7
# method      n_obs mean_error median_error sd_error    mae  rmse
# <chr>       <int>      <dbl>        <dbl>    <dbl>  <dbl> <dbl>
#   1 Tobit      509143     0.135        0.102     0.299 0.177  0.328
# 2 XGBoost V1 101833    -0.0001      -0.0113    0.158 0.0739 0.158
# 3 XGBoost V2 101833     0.0003      -0.0095    0.120 0.0486 0.120
# 4 XGBoost V3 101833     0.0004      -0.0091    0.117 0.0481 0.117
# > 
#   > # Performance ranking
#   > cat("\n=== FINAL PERFORMANCE RANKING (CENSORED OBSERVATIONS) ===\n")
# 
# === FINAL PERFORMANCE RANKING (CENSORED OBSERVATIONS) ===
#   > ranking_censored <- error_summary_with_v3_censored %>%
#   +   arrange(rmse) %>%
#   +   mutate(
#     +     rank = row_number(),
#     +     improvement_vs_worst = round((max(rmse) - rmse) / max(rmse) * 100, 1)
#     +   )
# > print(ranking_censored)
# # A tibble: 4 × 9
# method     n_obs mean_error median_error sd_error    mae   rmse  rank improvement_vs_worst
# <chr>      <int>      <dbl>        <dbl>    <dbl>  <dbl>  <dbl> <int>                <dbl>
#   1 XGBoost V3  6941     0.0016       0.0034   0.0995 0.0636 0.0995     1                 78.7
# 2 XGBoost V2  6941     0.0021       0.004    0.109  0.0666 0.109      2                 76.7
# 3 XGBoost V1  6941    -0.0031       0.001    0.145  0.091  0.145      3                 69  
# 4 Tobit      34972    -0.05        -0.0442   0.465  0.281  0.467      4                  0  
# > 
#   > cat("\n=== FINAL PERFORMANCE RANKING (ALL OBSERVATIONS) ===\n")
# 
# === FINAL PERFORMANCE RANKING (ALL OBSERVATIONS) ===
#   > ranking_all <- error_summary_with_v3_all %>%
#   +   arrange(rmse) %>%
#   +   mutate(
#     +     rank = row_number(),
#     +     improvement_vs_worst = round((max(rmse) - rmse) / max(rmse) * 100, 1)
#     +   )
# > print(ranking_all)
# # A tibble: 4 × 9
# method      n_obs mean_error median_error sd_error    mae  rmse  rank improvement_vs_worst
# <chr>       <int>      <dbl>        <dbl>    <dbl>  <dbl> <dbl> <int>                <dbl>
#   1 XGBoost V3 101833     0.0004      -0.0091    0.117 0.0481 0.117     1                 64.3
# 2 XGBoost V2 101833     0.0003      -0.0095    0.120 0.0486 0.120     2                 63.3
# 3 XGBoost V1 101833    -0.0001      -0.0113    0.158 0.0739 0.158     3                 51.9
# 4 Tobit      509143     0.135        0.102     0.299 0.177  0.328     4                  0  
# > 
#   > cat("\n=== XGBoost V3 Model with Temporal and Geographic Features Complete ===\n")
# 
# === XGBoost V3 Model with Temporal and Geographic Features Complete ===
#   > print(head(importance_matrix_v1, 20))
# Error in head(importance_matrix_v1, 20) : 
#   object 'importance_matrix_v1' not found
# > print(head(importance_matrix, 20))
# Feature         Gain        Cover    Frequency
# <char>        <num>        <num>        <num>
#   1:     mean_wage_coworkers 5.690980e-01 3.199257e-01 0.2376460327
# 2:               mean_wage 2.826604e-01 4.274348e-01 0.3296380454
# 3:           mean_censored 6.891361e-02 7.451335e-02 0.0613509605
# 4:                     age 2.775155e-02 4.299093e-02 0.1813445340
# 5:               firm_size 2.379457e-02 7.869764e-02 0.1298859281
# 6: mean_censored_coworkers 2.010496e-02 3.692400e-02 0.0316968487
# 7:        firm_size_square 7.603690e-03 1.948055e-02 0.0276572793
# 8:         smallfirm_dummy 7.319948e-05 3.307786e-05 0.0007803714
# > print(head(importance_matrix_v2, 20))
# Feature         Gain        Cover    Frequency
# <char>        <num>        <num>        <num>
#   1:          interval_lower 0.5144620280 1.786391e-01 0.1250169952
# 2:          interval_upper 0.3500172992 8.586178e-02 0.0539768865
# 3:               mean_wage 0.0456294422 2.523478e-01 0.2332426920
# 4:     mean_wage_coworkers 0.0248415050 1.811124e-01 0.1446181736
# 5:                     age 0.0147164923 4.381841e-02 0.1255381826
# 6:               firm_size 0.0139573158 4.558823e-02 0.0786313166
# 7:               num_years 0.0099939568 7.141901e-02 0.0867890324
# 8:           mean_censored 0.0096458751 5.254374e-02 0.0393836392
# 9:               birthyear 0.0075133802 5.598374e-02 0.0672784954
# 10:        bg_censored_flag 0.0035294226 3.704498e-04 0.0005438477
# 11:        firm_size_square 0.0024907973 1.254828e-02 0.0187854068
# 12:                    frau 0.0017378350 4.631815e-03 0.0109902561
# 13: mean_censored_coworkers 0.0012763998 1.512862e-02 0.0147065488
# 14:         smallfirm_dummy 0.0001882507 6.602733e-06 0.0004985271
# > print(head(importance_matrix_v3, 20))
# Feature         Gain        Cover    Frequency
# <char>        <num>        <num>        <num>
#   1:          interval_lower 6.641108e-01 1.690332e-01 0.1230258615
# 2:          interval_upper 1.991263e-01 7.588669e-02 0.0464660415
# 3:               mean_wage 4.156294e-02 2.460478e-01 0.2152228633
# 4:     mean_wage_coworkers 2.545312e-02 1.897218e-01 0.1427171274
# 5:                     age 1.673167e-02 4.019088e-02 0.1349876373
# 6:               firm_size 1.324089e-02 5.034015e-02 0.0707905464
# 7:           mean_censored 1.159518e-02 5.519554e-02 0.0394716325
# 8:               num_years 9.993637e-03 8.053911e-02 0.0891898514
# 9:              bundesland 4.816760e-03 2.096708e-02 0.0334127815
# 10:               contryear 4.326560e-03 2.302920e-02 0.0476020760
# 11:        firm_size_square 2.907276e-03 1.484223e-02 0.0176419486
# 12:        bg_censored_flag 2.166944e-03 4.330051e-04 0.0002227519
# 13:                    frau 1.357902e-03 4.589633e-03 0.0108702916
# 14:                    stsl 1.327065e-03 1.418990e-02 0.0145902479
# 15: mean_censored_coworkers 1.251736e-03 1.491102e-02 0.0134987637
# 16:         smallfirm_dummy 3.125325e-05 8.278943e-05 0.0002895774
# > 
#   
#   
#   
#   
#   > load("pre_model_output/pre_model_full_history.RData")   # object: pre_model_full_history
# > load("lohnzettel_dta.RData")
# > cat("Loaded pre_model_full_history rows:", nrow(pre_model_full_history), "\n")
# Loaded pre_model_full_history rows: 6911360 
# > cat("Loaded lohnzettel rows:", nrow(lohnzettel), "\n")
# Loaded lohnzettel rows: 113481460 
# > topcoding <- c(
#   +   632, 686, 763, 850, 959, 1090, 1220, 1351, 1417, 1482, 1569, 1656,
#   +   1744, 1787, 1874, 1918, 2005, 2049, 2092, 2180, 2311, 2441, 2616,
#   +   2747, 2834, 2965, 3052, 3095, 3139, 3226, 3270, 3360, 3450, 3630,
#   +   3750, 3840, 3930, 4020, 4110, 4200, 4230, 4440, 4530, 4650, 4860,
#   +   4980, 5130, 5220, 5370
#   + )
# > vpi_1986 <- data.frame(
#   +   contryear = 1986:2024,
#   +   year_inflation = c(
#     +     0, 1.4, 2, 2.5, 3.3, 3.3, 4.1, 3.6, 3.0, 2.2, 1.9, 1.3, 0.9, 0.6,
#     +     2.3, 2.7, 1.8, 1.3, 2.1, 2.3, 1.5, 2.2, 3.2, 0.5, 1.9, 3.3, 2.4,
#     +     2.0, 1.7, 0.9, 0.9, 2.1, 2.0, 1.5, 1.4, 2.8, 8.6, 7.8, 2.9
#     +   )
#   + ) %>%
#   +   mutate(
#     +     vpi = year_inflation / 100 + 1,
#     +     reverse_index = cumprod(vpi),
#     +     cpi_2000 = reverse_index[contryear == 2000] / reverse_index,
#     +     cpi_2012 = reverse_index[contryear == 2012] / reverse_index
#     +   )
# > analysis_data <- pre_model_full_history %>%
#   +   filter(contryear >= firstyear, contryear <= lastyear)
# > all_penr <- analysis_data %>%
#   +   distinct(penr)
# > if (nrow(all_penr) == 0) {
#   +   stop("No persons available in analysis_data after year restriction.")
#   + }
# > if (use_debug_sample) {
#   +   set.seed(sample_seed)
#   +   
#     +   n_available_penr <- nrow(all_penr)
#     +   n_to_sample <- min(sample_n_penr, n_available_penr)
#     +   
#       +   sampled_penr <- all_penr %>%
#         +     slice_sample(n = n_to_sample) %>%
#         +     pull(penr)
#       +   
#         +   analysis_data <- analysis_data %>%
#           +     filter(penr %in% sampled_penr)
#         +   
#           +   cat("DEBUG SAMPLE ACTIVE:", length(sampled_penr), "persons\n")
#         + } else {
#           +   sampled_penr <- all_penr %>%
#             +     pull(penr)
#           +   
#             +   cat("USING FULL PRE-MODEL DATASET CONTENT:", length(sampled_penr), "persons\n")
#           + }
# USING FULL PRE-MODEL DATASET CONTENT: 44105 persons
# > cat("Analysis data rows after time/sample restriction:", nrow(analysis_data), "\n")
# Analysis data rows after time/sample restriction: 3350211 
# > cat("Analysis year range:", min(analysis_data$contryear), "to", max(analysis_data$contryear), "\n")
# Analysis year range: 1994 to 2012 
# > lz_comparison_data <- lohnzettel %>%
#   +   filter(penr %in% sampled_penr) %>%
#   +   filter(year >= firstyear, year <= lastyear) %>%
#   +   rename(contryear = year) %>%
#   +   left_join(vpi_1986 %>% select(contryear, cpi_2000), by = "contryear") %>%
#   +   mutate(
#     +     lz_ywage_log = log(pmax(lz_ywage * cpi_2000, 1)),
#     +     lz_dwage_log = log(pmax(lz_dwage * cpi_2000, 1)),
#     +     lz_golden_log = log(pmax(lz_golden_p67_2_6_10 * cpi_2000 + 1, 1))
#     +   ) %>%
#   +   select(penr, benr, contryear, lz_ywage_log, lz_dwage_log, lz_golden_log) %>%
#   +   filter(is.finite(lz_dwage_log))
# > cat("LZ comparison rows:", nrow(lz_comparison_data), "\n")
# LZ comparison rows: 664980 
# > tobit_data <- analysis_data %>%
#   +   group_by(penr) %>%
#   +   mutate(
#     +     num_years = n(),
#     +     mean_wage = mean(dailywage_final, na.rm = TRUE),
#     +     mean_censored = mean(censored, na.rm = TRUE)
#     +   ) %>%
#   +   mutate(
#     +     mean_wage = ifelse(
#       +       num_years > 1,
#       +       (mean_wage - dailywage_final / num_years) * (num_years / (num_years - 1)),
#       +       mean_wage
#       +     ),
#     +     mean_censored = ifelse(
#       +       num_years > 1,
#       +       (mean_censored - censored / num_years) * (num_years / (num_years - 1)),
#       +       mean_censored
#       +     )
#     +   ) %>%
#   +   ungroup() %>%
#   +   group_by(benr, contryear) %>%
#   +   mutate(
#     +     firm_size = n(),
#     +     mean_wage_coworkers = mean(dailywage_final, na.rm = TRUE),
#     +     mean_censored_coworkers = mean(censored, na.rm = TRUE)
#     +   ) %>%
#   +   mutate(
#     +     mean_wage_coworkers = ifelse(
#       +       firm_size > 1,
#       +       (mean_wage_coworkers - dailywage_final / firm_size) * (firm_size / (firm_size - 1)),
#       +       mean_wage_coworkers
#       +     ),
#     +     mean_censored_coworkers = ifelse(
#       +       firm_size > 1,
#       +       (mean_censored_coworkers - censored / firm_size) * (firm_size / (firm_size - 1)),
#       +       mean_censored_coworkers
#       +     )
#     +   ) %>%
#   +   ungroup() %>%
#   +   left_join(vpi_1986 %>% select(contryear, cpi_2000), by = "contryear") %>%
#   +   mutate(
#     +     topcode_threshold = topcoding[contryear - 1971],
#     +     daily_topcode = topcode_threshold * 14 / 30,
#     +     log_daily_topcode = log(daily_topcode * cpi_2000),
#     +     firm_size_square = firm_size^2 / 100,
#     +     oneyear_dummy = ifelse(num_years == 1, 1, 0),
#     +     oneperson_dummy = ifelse(firm_size == 1, 1, 0),
#     +     smallfirm_dummy = ifelse(firm_size < 10, 1, 0),
#     +     agegroup = case_when(
#       +       age <= 30 ~ 1L,
#       +       age > 30 & age <= 40 ~ 2L,
#       +       age > 40 & age <= 50 ~ 3L,
#       +       age > 50 & age <= 60 ~ 4L,
#       +       TRUE ~ 5L
#       +     ),
#     +     bg_censored_flag = censored,
#     +     interval_upper = ifelse(censored == 0, dailywage_final, log_daily_topcode),
#     +     interval_lower = dailywage_final
#     +   ) %>%
#   +   select(-cpi_2000, -topcode_threshold, -daily_topcode, -log_daily_topcode)
# > cat("Tobit data rows:", nrow(tobit_data), "\n")
# Tobit data rows: 3350211 
# > #########################################
# > # 6) TOBIT IMPUTATION
#   > #########################################
# > 
#   > imputed_data_list <- list()
# > 
#   > min_year <- min(tobit_data$contryear, na.rm = TRUE)
# > max_year <- max(tobit_data$contryear, na.rm = TRUE)
# > 
#   > for (year in min_year:max_year) {
#     +   cat("Processing Tobit year:", year, "\n")
#     + 
#       +   year_data <- tobit_data %>% filter(contryear == year)
#       + 
#         +   if (nrow(year_data) == 0) next
#       + 
#         +   year_results <- year_data %>%
#           +     group_by(frau, agegroup) %>%
#           +     do({
#             +       grp <- .
#             + 
#               +       if (nrow(grp) <= 10) {
#                 +         out <- grp %>%
#                   +           mutate(
#                     +             predict = dailywage_final,
#                     +             dailywage_final_orig = dailywage_final,
#                     +             dailywage_final_pred = dailywage_final,
#                     +             sigma_hat = 1
#                     +           )
#                 +         return(out)
#                 +       }
#             + 
#               +       formula_str <- paste(
#                 +         "survival::Surv(interval_lower, interval_upper, type = 'interval2') ~",
#                 +         "age + mean_wage + mean_censored + firm_size + firm_size_square +",
#                 +         "oneyear_dummy + oneperson_dummy + smallfirm_dummy +",
#                 +         "mean_wage_coworkers + mean_censored_coworkers"
#                 +       )
#               + 
#                 +       tryCatch({
#                   +         model <- survival::survreg(as.formula(formula_str), data = grp, dist = "gaussian")
#                   +         predictions <- predict(model, newdata = grp, type = "response")
#                   +         sigma_hat <- model$scale
#                   + 
#                     +         set.seed(123)
#                   +         k <- pnorm((grp$dailywage_final - predictions) / sigma_hat)
#                   +         replace_vals <- predictions + sigma_hat * qnorm(k + runif(length(k)) * (1 - k))
#                   + 
#                     +         grp %>%
#                     +           mutate(
#                       +             predict = predictions,
#                       +             dailywage_final_orig = dailywage_final,
#                       +             dailywage_final_pred = replace_vals,
#                       +             sigma_hat = sigma_hat
#                       +           )
#                   +       }, error = function(e) {
#                     +         grp %>%
#                       +           mutate(
#                         +             predict = dailywage_final,
#                         +             dailywage_final_orig = dailywage_final,
#                         +             dailywage_final_pred = dailywage_final,
#                         +             sigma_hat = 1
#                         +           )
#                     +       })
#               +     }) %>%
#           +     ungroup() %>%
#           +     mutate(
#             +       dailywage_final = ifelse(censored == 1, dailywage_final_pred, dailywage_final),
#             +       educ_proxy = case_when(
#               +         age_entry < 18 ~ 1L,
#               +         age_entry >= 18 & age_entry <= 21 ~ 2L,
#               +         age_entry > 21 ~ 3L,
#               +         TRUE ~ NA_integer_
#               +       )
#             +     )
#         + 
#           +   save(year_results, file = paste0("model_output_v4/results_by_year/tobit_year_", year, ".RData"))
#         +   imputed_data_list[[as.character(year)]] <- year_results
#         + }
# Processing Tobit year: 1994 
# |=======================================================================|100% ~0 s remaining     Processing Tobit year: 1995 
# |=======================================================================|100% ~0 s remaining     Processing Tobit year: 1996 
# |=======================================================================|100% ~0 s remaining     Processing Tobit year: 1997 
# |=======================================================================|100% ~0 s remaining     Processing Tobit year: 1998 
# |=======================================================================|100% ~0 s remaining     Processing Tobit year: 1999 
# |=======================================================================|100% ~0 s remaining     Processing Tobit year: 2000 
# |=======================================================================|100% ~0 s remaining     Processing Tobit year: 2001 
# |=======================================================================|100% ~0 s remaining     Processing Tobit year: 2002 
# |=======================================================================|100% ~0 s remaining     Processing Tobit year: 2003 
# |=======================================================================|100% ~0 s remaining     Processing Tobit year: 2004 
# |=======================================================================|100% ~0 s remaining     Processing Tobit year: 2005 
# |=======================================================================|100% ~0 s remaining     Processing Tobit year: 2006 
# |=======================================================================|100% ~0 s remaining     Processing Tobit year: 2007 
# |=======================================================================|100% ~0 s remaining     Processing Tobit year: 2008 
# |=======================================================================|100% ~0 s remaining     Processing Tobit year: 2009 
# |=======================================================================|100% ~0 s remaining     Processing Tobit year: 2010 
# |=======================================================================|100% ~0 s remaining     Processing Tobit year: 2011 
# |=======================================================================|100% ~0 s remaining     Processing Tobit year: 2012 
# |=======================================================================|100% ~0 s remaining     > 
#   > final_imputed_data <- bind_rows(imputed_data_list)
# > 
#   > save(final_imputed_data, file = "model_output_v4/final_imputed_data_v4.RData")
# > 
#   > #########################################
# > # 7) TOBIT EVALUATION
#   > #########################################
# > 
#   > imputation_eval <- final_imputed_data %>%
#   +   filter(!is.na(dailywage_final_orig), !is.na(dailywage_final_pred)) %>%
#   +   mutate(
#     +     was_imputed = (dailywage_final != dailywage_final_orig),
#     +     residual = dailywage_final_pred - dailywage_final_orig,
#     +     squared_error = residual^2
#     +   )
# > 
#   > rmse_by_censoring <- imputation_eval %>%
#   +   group_by(was_imputed) %>%
#   +   summarise(
#     +     rmse = sqrt(mean(squared_error, na.rm = TRUE)),
#     +     mae = mean(abs(residual), na.rm = TRUE),
#     +     n_obs = n(),
#     +     mean_residual = mean(residual, na.rm = TRUE),
#     +     median_residual = median(residual, na.rm = TRUE),
#     +     sd_residual = sd(residual, na.rm = TRUE)
#     +   ) %>%
#   +   mutate(
#     +     censoring_status = ifelse(was_imputed, "Censored (Imputed)", "Uncensored")
#     +   )
# > 
#   > rmse_overall <- imputation_eval %>%
#   +   summarise(
#     +     was_imputed = NA,
#     +     rmse = sqrt(mean(squared_error, na.rm = TRUE)),
#     +     mae = mean(abs(residual), na.rm = TRUE),
#     +     n_obs = n(),
#     +     mean_residual = mean(residual, na.rm = TRUE),
#     +     median_residual = median(residual, na.rm = TRUE),
#     +     sd_residual = sd(residual, na.rm = TRUE),
#     +     censoring_status = "Overall"
#     +   )
# > 
#   > complete_rmse_summary <- bind_rows(rmse_by_censoring, rmse_overall)
# > 
#   > print(complete_rmse_summary)
# # A tibble: 3 × 8
# was_imputed  rmse   mae   n_obs mean_residual median_residual sd_residual censoring_status  
# <lgl>       <dbl> <dbl>   <int>         <dbl>           <dbl>       <dbl> <chr>             
#   1 FALSE         Inf   Inf 3108297           Inf           0.132         NaN Uncensored        
# 2 TRUE          Inf   Inf  241914           Inf           0.278         NaN Censored (Imputed)
# 3 NA            Inf   Inf 3350211           Inf           0.138         NaN Overall           
# > 
#   > #########################################
# > # 8) TRAINING DATA FOR XGBOOST
#   > #########################################
# > 
#   > training_data <- tobit_data %>%
#   +   left_join(
#     +     lz_comparison_data,
#     +     by = c("penr", "benr", "contryear")
#     +   ) %>%
#   +   filter(!is.na(lz_dwage_log)) %>%
#   +   mutate(
#     +     bg_dwage_log = dailywage_final
#     +   )
# > 
#   > cat("Training rows with BG and LZ:", nrow(training_data), "\n")
# Training rows with BG and LZ: 3218020 
# > 
#   > #########################################
# > # 9) SHARED XGBOOST HELPERS
#   > #########################################
# > 
#   > xgb_params <- list(
#     +   objective = "reg:squarederror",
#     +   eval_metric = "rmse",
#     +   eta = 0.1,
#     +   max_depth = 6,
#     +   min_child_weight = 1,
#     +   subsample = 0.8,
#     +   colsample_bytree = 0.8,
#     +   reg_alpha = 0.1,
#     +   reg_lambda = 1
#     + )
# > 
#   > prepare_matrix <- function(df, feature_cols) {
#     +   df %>%
#       +     select(all_of(feature_cols)) %>%
#       +     mutate(across(everything(), ~ ifelse(is.na(.) | is.infinite(.), 0, .))) %>%
#       +     as.matrix()
#     + }
# > 
#   > run_xgb_model <- function(data, feature_cols, model_name, seed = 123) {
#     +   X_features <- prepare_matrix(data, feature_cols)
#     +   y_target <- data$lz_dwage_log
#     + 
#       +   set.seed(seed)
#     +   n_obs <- length(y_target)
#     +   train_indices <- sample(seq_len(n_obs), size = floor(0.8 * n_obs))
#     + 
#       +   X_train <- X_features[train_indices, , drop = FALSE]
#       +   X_test  <- X_features[-train_indices, , drop = FALSE]
#       +   y_train <- y_target[train_indices]
#       +   y_test  <- y_target[-train_indices]
#       + 
#         +   bg_train <- data$bg_dwage_log[train_indices]
#         +   bg_test  <- data$bg_dwage_log[-train_indices]
#         + 
#           +   dtrain <- xgb.DMatrix(data = X_train, label = y_train)
#           +   dtest  <- xgb.DMatrix(data = X_test, label = y_test)
#           + 
#             +   cat("\nTraining", model_name, "...\n")
#           +   cat("Features:", paste(feature_cols, collapse = ", "), "\n")
#           +   cat("Training rows:", nrow(X_train), "Test rows:", nrow(X_test), "\n")
#           + 
#             +   set.seed(seed)
#           +   xgb_cv <- xgb.cv(
#             +     params = xgb_params,
#             +     data = dtrain,
#             +     nrounds = 1000,
#             +     nfold = 5,
#             +     early_stopping_rounds = 50,
#             +     verbose = FALSE
#             +   )
#           + 
#             +   best_nrounds <- xgb_cv$best_iteration
#             + 
#               +   set.seed(seed)
#             +   xgb_model <- xgb.train(
#               +     params = xgb_params,
#               +     data = dtrain,
#               +     nrounds = best_nrounds,
#               +     watchlist = list(train = dtrain, test = dtest),
#               +     verbose = FALSE
#               +   )
#             + 
#               +   train_pred <- predict(xgb_model, dtrain)
#               +   test_pred  <- predict(xgb_model, dtest)
#               + 
#                 +   results <- list(
#                   +     model_name = model_name,
#                   +     feature_cols = feature_cols,
#                   +     train_indices = train_indices,
#                   +     X_features = X_features,
#                   +     model = xgb_model,
#                   +     cv = xgb_cv,
#                   +     best_nrounds = best_nrounds,
#                   +     train_pred = train_pred,
#                   +     test_pred = test_pred,
#                   +     y_train = y_train,
#                   +     y_test = y_test,
#                   +     bg_train = bg_train,
#                   +     bg_test = bg_test,
#                   +     censored_test = data$bg_censored_flag[-train_indices],
#                   +     censored_train = data$bg_censored_flag[train_indices],
#                   +     train_rmse_lz = sqrt(mean((train_pred - y_train)^2)),
#                   +     test_rmse_lz = sqrt(mean((test_pred - y_test)^2)),
#                   +     train_r2_lz = cor(train_pred, y_train)^2,
#                   +     test_r2_lz = cor(test_pred, y_test)^2,
#                   +     train_rmse_bg = sqrt(mean((bg_train - y_train)^2)),
#                   +     test_rmse_bg = sqrt(mean((bg_test - y_test)^2)),
#                   +     train_r2_bg = cor(bg_train, y_train)^2,
#                   +     test_r2_bg = cor(bg_test, y_test)^2
#                   +   )
#                 + 
#                   +   results$prediction_comparison <- data.frame(
#                     +     lz_true = results$y_test,
#                     +     bg_observed = results$bg_test,
#                     +     xgb_predicted = results$test_pred,
#                     +     censored = results$censored_test,
#                     +     bg_error = abs(results$bg_test - results$y_test),
#                     +     xgb_error = abs(results$test_pred - results$y_test),
#                     +     improvement = abs(results$bg_test - results$y_test) - abs(results$test_pred - results$y_test)
#                     +   ) %>%
#                     +     mutate(
#                       +       bg_better = improvement < 0,
#                       +       xgb_better = improvement > 0,
#                       +       bg_pct_error = abs((bg_observed - lz_true) / lz_true) * 100,
#                       +       xgb_pct_error = abs((xgb_predicted - lz_true) / lz_true) * 100
#                       +     )
#                   + 
#                     +   results$importance <- xgb.importance(
#                       +     feature_names = colnames(X_features),
#                       +     model = xgb_model
#                       +   )
#                     + 
#                       +   cat("Optimal rounds:", best_nrounds,
#                               +       "Best CV RMSE:", min(xgb_cv$evaluation_log$test_rmse_mean), "\n")
#                     + 
#                       +   cat("\n===", model_name, "Performance ===\n")
#                     +   cat("Train RMSE vs LZ:", round(results$train_rmse_lz, 4),
#                             +       "R²:", round(results$train_r2_lz, 4), "\n")
#                     +   cat("Test RMSE vs LZ:", round(results$test_rmse_lz, 4),
#                             +       "R²:", round(results$test_r2_lz, 4), "\n")
#                     +   cat("BG baseline Test RMSE:", round(results$test_rmse_bg, 4),
#                             +       "R²:", round(results$test_r2_bg, 4), "\n")
#                     +   cat("RMSE reduction vs BG:", round(results$test_rmse_bg - results$test_rmse_lz, 4), "\n")
#                     + 
#                       +   results
#                     + }
# > 
#   > #########################################
# > # 10) XGBOOST V1
#   > #########################################
# > 
#   > feature_cols_v1 <- c(
#     +   "age",
#     +   "mean_wage",
#     +   "mean_censored",
#     +   "firm_size",
#     +   "firm_size_square",
#     +   "oneyear_dummy",
#     +   "oneperson_dummy",
#     +   "smallfirm_dummy",
#     +   "mean_wage_coworkers",
#     +   "mean_censored_coworkers"
#     + )
# > 
#   > xgb_v1 <- run_xgb_model(
#     +   data = training_data,
#     +   feature_cols = feature_cols_v1,
#     +   model_name = "XGBoost V1",
#     +   seed = 123
#     + )
# 
# Training XGBoost V1 ...
# Features: age, mean_wage, mean_censored, firm_size, firm_size_square, oneyear_dummy, oneperson_dummy, smallfirm_dummy, mean_wage_coworkers, mean_censored_coworkers 
# Training rows: 2574416 Test rows: 643604 
# Optimal rounds: 1000 Best CV RMSE: 0.2590894 
# 
# === XGBoost V1 Performance ===
#   Train RMSE vs LZ: 0.2539 R²: 0.8321 
# Test RMSE vs LZ: 0.2561 R²: 0.8285 
# BG baseline Test RMSE: 0.2854 R²: 0.7874 
# RMSE reduction vs BG: 0.0293 
# > 
#   > #########################################
# > # 11) XGBOOST V2
#   > #########################################
# > 
#   > feature_cols_v2 <- c(
#     +   "age",
#     +   "mean_wage",
#     +   "mean_censored",
#     +   "firm_size",
#     +   "firm_size_square",
#     +   "oneyear_dummy",
#     +   "oneperson_dummy",
#     +   "smallfirm_dummy",
#     +   "mean_wage_coworkers",
#     +   "mean_censored_coworkers",
#     +   "frau",
#     +   "birthyear",
#     +   "num_years",
#     +   "interval_lower",
#     +   "interval_upper",
#     +   "bg_censored_flag"
#     + )
# > 
#   > xgb_v2 <- run_xgb_model(
#     +   data = training_data,
#     +   feature_cols = feature_cols_v2,
#     +   model_name = "XGBoost V2",
#     +   seed = 123
#     + )
# 
# Training XGBoost V2 ...
# Features: age, mean_wage, mean_censored, firm_size, firm_size_square, oneyear_dummy, oneperson_dummy, smallfirm_dummy, mean_wage_coworkers, mean_censored_coworkers, frau, birthyear, num_years, interval_lower, interval_upper, bg_censored_flag 
# Training rows: 2574416 Test rows: 643604 
# Optimal rounds: 1000 Best CV RMSE: 0.2101442 
# 
# === XGBoost V2 Performance ===
#   Train RMSE vs LZ: 0.2036 R²: 0.8921 
# Test RMSE vs LZ: 0.2076 R²: 0.8874 
# BG baseline Test RMSE: 0.2854 R²: 0.7874 
# RMSE reduction vs BG: 0.0778 
# > 
#   > #########################################
# > # 12) XGBOOST V4 / LATEST ACTIVE MODEL
#   > #########################################
# > # This is the post-split successor to the old V3.
#   > # It uses the richer pre-model dataset variables.
#   > 
#   > feature_cols_v4 <- c(
#     +   # Core wage / censoring structure
#       +   "age",
#     +   "exper",
#     +   "tenure",
#     +   "mean_wage",
#     +   "mean_censored",
#     +   "firm_size",
#     +   "firm_size_square",
#     +   "oneyear_dummy",
#     +   "oneperson_dummy",
#     +   "smallfirm_dummy",
#     +   "mean_wage_coworkers",
#     +   "mean_censored_coworkers",
#     +   "interval_lower",
#     +   "interval_upper",
#     +   "bg_censored_flag",
#     + 
#       +   # Demographics
#       +   "frau",
#     +   "stsl_missing",
#     +   "austrian_dummy",
#     +   "germany_dummy",
#     +   "eu_dummy",
#     + 
#       +   # Education
#       +   "educ_cat",
#     +   "educ_missing",
#     +   "educ_imputed",
#     + 
#       +   # Job / collar / transitions
#       +   "whitecollar_dummy",
#     +   "bluecollar_dummy",
#     +   "othercollar_dummy",
#     +   "subcat",
#     +   "juj",
#     +   "return",
#     + 
#       +   # Career path
#       +   "year_entry",
#     +   "age_entry",
#     +   "num_years",
#     + 
#       +   # Family
#       +   "live_birth_event",
#     +   "n_children",
#     + 
#       +   # Time / geography / industry
#       +   "contryear",
#     +   "bundesland",
#     +   "nace08_2"
#     + )
# > 
#   > xgb_v4 <- run_xgb_model(
#     +   data = training_data,
#     +   feature_cols = feature_cols_v4,
#     +   model_name = "XGBoost V4 Latest",
#     +   seed = 123
#     + )
# 
# Training XGBoost V4 Latest ...
# Features: age, exper, tenure, mean_wage, mean_censored, firm_size, firm_size_square, oneyear_dummy, oneperson_dummy, smallfirm_dummy, mean_wage_coworkers, mean_censored_coworkers, interval_lower, interval_upper, bg_censored_flag, frau, stsl_missing, austrian_dummy, germany_dummy, eu_dummy, educ_cat, educ_missing, educ_imputed, whitecollar_dummy, bluecollar_dummy, othercollar_dummy, subcat, juj, return, year_entry, age_entry, num_years, live_birth_event, n_children, contryear, bundesland, nace08_2 
# Training rows: 2574416 Test rows: 643604 
# Optimal rounds: 1000 Best CV RMSE: 0.200181 
# 
# === XGBoost V4 Latest Performance ===
#   Train RMSE vs LZ: 0.1933 R²: 0.9028 
# Test RMSE vs LZ: 0.1972 R²: 0.8985 
# BG baseline Test RMSE: 0.2854 R²: 0.7874 
# RMSE reduction vs BG: 0.0882 
# > 
#   > cat("\n=== Comparison: V2 vs V4 Latest ===\n")
# 
# === Comparison: V2 vs V4 Latest ===
#   > cat("V2 Test RMSE:", round(xgb_v2$test_rmse_lz, 4), "R²:", round(xgb_v2$test_r2_lz, 4), "\n")
# V2 Test RMSE: 0.2076 R²: 0.8874 
# > cat("V4 Test RMSE:", round(xgb_v4$test_rmse_lz, 4), "R²:", round(xgb_v4$test_r2_lz, 4), "\n")
# V4 Test RMSE: 0.1972 R²: 0.8985 
# > cat("RMSE reduction V4 over V2:", round(xgb_v2$test_rmse_lz - xgb_v4$test_rmse_lz, 4), "\n")
# RMSE reduction V4 over V2: 0.0104 
# > cat("R² improvement V4 over V2:", round(xgb_v4$test_r2_lz - xgb_v2$test_r2_lz, 4), "\n")
# R² improvement V4 over V2: 0.0111 
# > 
#   > #########################################
# > # 13) TOBIT CORRECTED AGAINST LZ
#   > #########################################
# > 
#   > tobit_corrected <- final_imputed_data %>%
#   +   inner_join(lz_comparison_data %>% select(penr, benr, contryear, lz_dwage_log),
#                  +              by = c("penr", "benr", "contryear")) %>%
#   +   mutate(
#     +     error_vs_lz = dailywage_final_pred - lz_dwage_log,
#     +     error_vs_censored = dailywage_final_pred - dailywage_final_orig,
#     +     was_imputed = (dailywage_final != dailywage_final_orig)
#     +   ) %>%
#   +   filter(is.finite(error_vs_lz))
# > 
#   > #########################################
# > # 14) COMPARATIVE ERROR SUMMARIES
#   > #########################################
# > 
#   > tobit_errors_corrected_all <- tobit_corrected %>%
#   +   transmute(error = error_vs_lz, method = "Tobit", was_imputed = was_imputed)
# > 
#   > tobit_errors_corrected_censored <- tobit_corrected %>%
#   +   filter(was_imputed) %>%
#   +   transmute(error = error_vs_lz, method = "Tobit", was_imputed = was_imputed)
# > 
#   > xgb_v1_errors_all <- xgb_v1$prediction_comparison %>%
#   +   transmute(error = xgb_predicted - lz_true, method = "XGBoost V1", was_imputed = (censored == 1))
# > 
#   > xgb_v2_errors_all <- xgb_v2$prediction_comparison %>%
#   +   transmute(error = xgb_predicted - lz_true, method = "XGBoost V2", was_imputed = (censored == 1))
# > 
#   > xgb_v4_errors_all <- xgb_v4$prediction_comparison %>%
#   +   transmute(error = xgb_predicted - lz_true, method = "XGBoost V4", was_imputed = (censored == 1))
# > 
#   > all_errors_corrected <- bind_rows(
#     +   tobit_errors_corrected_all,
#     +   xgb_v1_errors_all,
#     +   xgb_v2_errors_all,
#     +   xgb_v4_errors_all
#     + ) %>%
#   +   filter(is.finite(error))
# > 
#   > censored_errors_corrected <- bind_rows(
#     +   tobit_errors_corrected_censored,
#     +   xgb_v1_errors_all %>% filter(was_imputed),
#     +   xgb_v2_errors_all %>% filter(was_imputed),
#     +   xgb_v4_errors_all %>% filter(was_imputed)
#     + ) %>%
#   +   filter(is.finite(error))
# > 
#   > error_summary_corrected_all <- all_errors_corrected %>%
#   +   group_by(method) %>%
#   +   summarise(
#     +     n_obs = n(),
#     +     mean_error = round(mean(error), 4),
#     +     median_error = round(median(error), 4),
#     +     sd_error = round(sd(error), 4),
#     +     mae = round(mean(abs(error)), 4),
#     +     rmse = round(sqrt(mean(error^2)), 4)
#     +   ) %>%
#   +   arrange(rmse)
# > 
#   > error_summary_corrected_censored <- censored_errors_corrected %>%
#   +   group_by(method) %>%
#   +   summarise(
#     +     n_obs = n(),
#     +     mean_error = round(mean(error), 4),
#     +     median_error = round(median(error), 4),
#     +     sd_error = round(sd(error), 4),
#     +     mae = round(mean(abs(error)), 4),
#     +     rmse = round(sqrt(mean(error^2)), 4)
#     +   ) %>%
#   +   arrange(rmse)
# > 
#   > cat("\n=== Error Summary: ALL OBSERVATIONS ===\n")
# 
# === Error Summary: ALL OBSERVATIONS ===
#   > print(error_summary_corrected_all)
# # A tibble: 4 × 7
# method       n_obs mean_error median_error sd_error    mae  rmse
# <chr>        <int>      <dbl>        <dbl>    <dbl>  <dbl> <dbl>
#   1 XGBoost V4  643604     0.0004      -0.012     0.197 0.0671 0.197
# 2 XGBoost V2  643604     0.0003      -0.0155    0.208 0.0706 0.208
# 3 XGBoost V1  643604     0.0004      -0.0157    0.256 0.122  0.256
# 4 Tobit      3217965     0.176        0.135     0.313 0.214  0.359
# > 
#   > cat("\n=== Error Summary: CENSORED OBSERVATIONS ONLY ===\n")
# 
# === Error Summary: CENSORED OBSERVATIONS ONLY ===
#   > print(error_summary_corrected_censored)
# # A tibble: 4 × 7
# method      n_obs mean_error median_error sd_error   mae  rmse
# <chr>       <int>      <dbl>        <dbl>    <dbl> <dbl> <dbl>
#   1 XGBoost V4  46235     0.002        0.0131    0.219 0.126 0.219
# 2 XGBoost V2  46235     0.0023       0.0165    0.238 0.140 0.238
# 3 XGBoost V1  46235    -0.0143       0.0014    0.282 0.172 0.282
# 4 Tobit      231101    -0.0218      -0.0118    0.473 0.282 0.473
# > 
#   > #########################################
# > # 15) COMPREHENSIVE RMSE TABLE
#   > #########################################
# > 
#   > tobit_vs_bg_all <- tobit_corrected %>%
#   +   summarise(
#     +     Method = "Tobit",
#     +     Target = "BG (censored data)",
#     +     Observation_Type = "All",
#     +     n_obs = n(),
#     +     RMSE = round(sqrt(mean(error_vs_censored^2)), 4),
#     +     MAE = round(mean(abs(error_vs_censored)), 4),
#     +     Mean_Error = round(mean(error_vs_censored), 4),
#     +     SD_Error = round(sd(error_vs_censored), 4)
#     +   )
# > 
#   > tobit_vs_bg_uncensored <- tobit_corrected %>%
#   +   filter(!was_imputed) %>%
#   +   summarise(
#     +     Method = "Tobit",
#     +     Target = "BG (censored data)",
#     +     Observation_Type = "Uncensored",
#     +     n_obs = n(),
#     +     RMSE = round(sqrt(mean(error_vs_censored^2)), 4),
#     +     MAE = round(mean(abs(error_vs_censored)), 4),
#     +     Mean_Error = round(mean(error_vs_censored), 4),
#     +     SD_Error = round(sd(error_vs_censored), 4)
#     +   )
# > 
#   > tobit_vs_bg_censored <- tobit_corrected %>%
#   +   filter(was_imputed) %>%
#   +   summarise(
#     +     Method = "Tobit",
#     +     Target = "BG (censored data)",
#     +     Observation_Type = "Censored",
#     +     n_obs = n(),
#     +     RMSE = round(sqrt(mean(error_vs_censored^2)), 4),
#     +     MAE = round(mean(abs(error_vs_censored)), 4),
#     +     Mean_Error = round(mean(error_vs_censored), 4),
#     +     SD_Error = round(sd(error_vs_censored), 4)
#     +   )
# > 
#   > tobit_vs_lz_all <- tobit_corrected %>%
#   +   summarise(
#     +     Method = "Tobit",
#     +     Target = "LZ (true wages)",
#     +     Observation_Type = "All",
#     +     n_obs = n(),
#     +     RMSE = round(sqrt(mean(error_vs_lz^2)), 4),
#     +     MAE = round(mean(abs(error_vs_lz)), 4),
#     +     Mean_Error = round(mean(error_vs_lz), 4),
#     +     SD_Error = round(sd(error_vs_lz), 4)
#     +   )
# > 
#   > tobit_vs_lz_uncensored <- tobit_corrected %>%
#   +   filter(!was_imputed) %>%
#   +   summarise(
#     +     Method = "Tobit",
#     +     Target = "LZ (true wages)",
#     +     Observation_Type = "Uncensored",
#     +     n_obs = n(),
#     +     RMSE = round(sqrt(mean(error_vs_lz^2)), 4),
#     +     MAE = round(mean(abs(error_vs_lz)), 4),
#     +     Mean_Error = round(mean(error_vs_lz), 4),
#     +     SD_Error = round(sd(error_vs_lz), 4)
#     +   )
# > 
#   > tobit_vs_lz_censored <- tobit_corrected %>%
#   +   filter(was_imputed) %>%
#   +   summarise(
#     +     Method = "Tobit",
#     +     Target = "LZ (true wages)",
#     +     Observation_Type = "Censored",
#     +     n_obs = n(),
#     +     RMSE = round(sqrt(mean(error_vs_lz^2)), 4),
#     +     MAE = round(mean(abs(error_vs_lz)), 4),
#     +     Mean_Error = round(mean(error_vs_lz), 4),
#     +     SD_Error = round(sd(error_vs_lz), 4)
#     +   )
# > 
#   > xgb_summary_block <- function(obj, method_name) {
#     +   bind_rows(
#       +     obj$prediction_comparison %>%
#         +       summarise(
#           +         Method = method_name,
#           +         Target = "LZ (true wages)",
#           +         Observation_Type = "All",
#           +         n_obs = n(),
#           +         RMSE = round(sqrt(mean((xgb_predicted - lz_true)^2)), 4),
#           +         MAE = round(mean(abs(xgb_predicted - lz_true)), 4),
#           +         Mean_Error = round(mean(xgb_predicted - lz_true), 4),
#           +         SD_Error = round(sd(xgb_predicted - lz_true), 4)
#           +       ),
#       +     obj$prediction_comparison %>%
#         +       filter(censored == 0) %>%
#         +       summarise(
#           +         Method = method_name,
#           +         Target = "LZ (true wages)",
#           +         Observation_Type = "Uncensored",
#           +         n_obs = n(),
#           +         RMSE = round(sqrt(mean((xgb_predicted - lz_true)^2)), 4),
#           +         MAE = round(mean(abs(xgb_predicted - lz_true)), 4),
#           +         Mean_Error = round(mean(xgb_predicted - lz_true), 4),
#           +         SD_Error = round(sd(xgb_predicted - lz_true), 4)
#           +       ),
#       +     obj$prediction_comparison %>%
#         +       filter(censored == 1) %>%
#         +       summarise(
#           +         Method = method_name,
#           +         Target = "LZ (true wages)",
#           +         Observation_Type = "Censored",
#           +         n_obs = n(),
#           +         RMSE = round(sqrt(mean((xgb_predicted - lz_true)^2)), 4),
#           +         MAE = round(mean(abs(xgb_predicted - lz_true)), 4),
#           +         Mean_Error = round(mean(xgb_predicted - lz_true), 4),
#           +         SD_Error = round(sd(xgb_predicted - lz_true), 4)
#           +       )
#       +   )
#     + }
# > 
#   > comprehensive_rmse_table <- bind_rows(
#     +   tobit_vs_bg_all,
#     +   tobit_vs_bg_uncensored,
#     +   tobit_vs_bg_censored,
#     +   tobit_vs_lz_all,
#     +   tobit_vs_lz_uncensored,
#     +   tobit_vs_lz_censored,
#     +   xgb_summary_block(xgb_v1, "XGBoost V1"),
#     +   xgb_summary_block(xgb_v2, "XGBoost V2"),
#     +   xgb_summary_block(xgb_v4, "XGBoost V4")
#     + ) %>%
#   +   arrange(Observation_Type, Method, Target)
# > 
#   > print(comprehensive_rmse_table, n = Inf)
# # A tibble: 15 × 8
# Method     Target             Observation_Type   n_obs  RMSE    MAE Mean_Error SD_Error
# <chr>      <chr>              <chr>              <int> <dbl>  <dbl>      <dbl>    <dbl>
#   1 Tobit      BG (censored data) All              3217965 0.252 0.183      0.183     0.174
# 2 Tobit      LZ (true wages)    All              3217965 0.359 0.214      0.176     0.313
# 3 XGBoost V1 LZ (true wages)    All               643604 0.256 0.122      0.0004    0.256
# 4 XGBoost V2 LZ (true wages)    All               643604 0.208 0.0706     0.0003    0.208
# 5 XGBoost V4 LZ (true wages)    All               643604 0.197 0.0671     0.0004    0.197
# 6 Tobit      BG (censored data) Censored          231101 0.372 0.305      0.305     0.213
# 7 Tobit      LZ (true wages)    Censored          231101 0.473 0.282     -0.0218    0.473
# 8 XGBoost V1 LZ (true wages)    Censored           46235 0.282 0.172     -0.0143    0.282
# 9 XGBoost V2 LZ (true wages)    Censored           46235 0.238 0.140      0.0023    0.238
# 10 XGBoost V4 LZ (true wages)    Censored           46235 0.219 0.126      0.002     0.219
# 11 Tobit      BG (censored data) Uncensored       2986864 0.240 0.173      0.173     0.166
# 12 Tobit      LZ (true wages)    Uncensored       2986864 0.349 0.209      0.191     0.292
# 13 XGBoost V1 LZ (true wages)    Uncensored        597369 0.254 0.118      0.0015    0.254
# 14 XGBoost V2 LZ (true wages)    Uncensored        597369 0.205 0.0653     0.0001    0.205
# 15 XGBoost V4 LZ (true wages)    Uncensored        597369 0.195 0.0625     0.0003    0.195
# > write.csv(comprehensive_rmse_table,
#             +           "model_output_v4/comprehensive_rmse_comparison_v4.csv",
#             +           row.names = FALSE)
# > 
#   > #########################################
# > # 16) FEATURE IMPORTANCE OUTPUTS
#   > #########################################
# > 
#   > cat("\n=== Top Features: XGBoost V1 ===\n")
# 
# === Top Features: XGBoost V1 ===
#   > print(head(xgb_v1$importance, 15))
# Feature        Gain        Cover   Frequency
# <char>       <num>        <num>       <num>
#   1:               mean_wage 0.499936940 0.3432976461 0.280765802
# 2:     mean_wage_coworkers 0.358267913 0.3153101534 0.239673982
# 3:           mean_censored 0.083282062 0.1057835385 0.088211572
# 4:               firm_size 0.020985091 0.0949471220 0.137411385
# 5:                     age 0.018699528 0.0341658613 0.163581950
# 6: mean_censored_coworkers 0.015301089 0.0903188056 0.060916076
# 7:        firm_size_square 0.002683561 0.0159265753 0.028696354
# 8:         smallfirm_dummy 0.000843816 0.0002502979 0.000742879
# > 
#   > cat("\n=== Top Features: XGBoost V2 ===\n")
# 
# === Top Features: XGBoost V2 ===
#   > print(head(xgb_v2$importance, 15))
# Feature         Gain        Cover    Frequency
# <char>        <num>        <num>        <num>
#   1:          interval_lower 0.4694866179 1.922940e-01 0.1225660864
# 2:          interval_upper 0.4577928675 1.167577e-01 0.0642381259
# 3:               mean_wage 0.0201109022 2.160448e-01 0.1978938319
# 4:     mean_wage_coworkers 0.0119140586 1.696806e-01 0.1301740812
# 5:           mean_censored 0.0100152684 6.576363e-02 0.0575112830
# 6:                     age 0.0076058491 2.967606e-02 0.1139264990
# 7:               num_years 0.0062526651 6.727980e-02 0.0919406834
# 8:               firm_size 0.0059309278 4.667332e-02 0.0837954008
# 9:               birthyear 0.0052690011 4.403343e-02 0.0689232753
# 10: mean_censored_coworkers 0.0018556635 2.941665e-02 0.0301096067
# 11:        firm_size_square 0.0018198973 1.595023e-02 0.0235332044
# 12:                    frau 0.0013367989 5.086237e-03 0.0143778208
# 13:        bg_censored_flag 0.0005766276 1.320137e-03 0.0006017623
# 14:         smallfirm_dummy 0.0000328547 2.345108e-05 0.0004083387
# > 
#   > cat("\n=== Top Features: XGBoost V4 Latest ===\n")
# 
# === Top Features: XGBoost V4 Latest ===
#   > print(head(xgb_v4$importance, 25))
# Feature         Gain       Cover    Frequency
# <char>        <num>       <num>        <num>
#   1:          interval_lower 0.6951404954 0.160952450 0.0994186639
# 2:          interval_upper 0.1646791950 0.086932933 0.0450790413
# 3:               mean_wage 0.0502412277 0.181732203 0.1515553289
# 4:     mean_wage_coworkers 0.0203395845 0.150679862 0.0902804691
# 5:           mean_censored 0.0093328155 0.064408824 0.0408771035
# 6:        bg_censored_flag 0.0086614689 0.002419218 0.0007751147
# 7:                     age 0.0078340977 0.020192675 0.0891177970
# 8:               firm_size 0.0060676262 0.049801824 0.0620703723
# 9:                  tenure 0.0054080818 0.013673868 0.0453850076
# 10:               num_years 0.0047851835 0.058057880 0.0633554309
# 11:                   exper 0.0045369399 0.020166097 0.0501988781
# 12:                nace08_2 0.0041385050 0.046913343 0.0484650688
# 13:              year_entry 0.0029703902 0.022560617 0.0341458440
# 14:               age_entry 0.0026334972 0.014505003 0.0282916879
# 15:               contryear 0.0019269460 0.017159560 0.0246608873
# 16:              bundesland 0.0016462199 0.014842273 0.0227231005
# 17:                educ_cat 0.0015635070 0.007146308 0.0165425803
# 18:                  return 0.0015292180 0.007733114 0.0097501275
# 19: mean_censored_coworkers 0.0013606934 0.023538460 0.0205609383
# 20:        firm_size_square 0.0012465646 0.010257367 0.0139316675
# 21:                    frau 0.0011206360 0.005507506 0.0093421724
# 22:                     juj 0.0008127780 0.004700898 0.0100356961
# 23:                eu_dummy 0.0004249819 0.001078738 0.0029576747
# 24:       whitecollar_dummy 0.0003724965 0.001520984 0.0044875064
# 25:            educ_imputed 0.0003399990 0.003410114 0.0048138705
# Feature         Gain       Cover    Frequency
# > 
#   > save(
#     +   xgb_v1, xgb_v2, xgb_v4,
#     +   file = "model_output_v4/xgboost_models_v4.RData"
#     + )
# > 
#   > #########################################
# > # 17) SIMPLE ERROR PLOTS
#   > #########################################
# > 
#   > plot_error_density <- function(df, title, out_path) {
#     +   p <- ggplot(df, aes(x = error, color = method, fill = method)) +
#       +     geom_density(alpha = 0.25, size = 1.1) +
#       +     geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
#       +     theme_minimal() +
#       +     theme(legend.position = "bottom") +
#       +     labs(
#         +       title = title,
#         +       x = "Prediction Error (ln wage units)",
#         +       y = "Density",
#         +       color = "Method",
#         +       fill = "Method"
#         +     ) +
#       +     xlim(-2, 2)
#     + 
#       +   print(p)
#     +   ggsave(out_path, plot = p, width = 10, height = 6)
#     + }
# > 
#   > plot_error_density(
#     +   all_errors_corrected,
#     +   "Prediction Error Distribution - All Observations",
#     +   "model_output_v4/plots/error_density_all.png"
#     + )
# Warning messages:
#   1: Removed 18597 rows containing non-finite values (stat_density). 
# 2: Removed 18597 rows containing non-finite values (stat_density). 
# > 
#   > plot_error_density(
#     +   censored_errors_corrected,
#     +   "Prediction Error Distribution - Censored Observations",
#     +   "model_output_v4/plots/error_density_censored.png"
#     + )
# Warning messages:
#   1: Removed 1778 rows containing non-finite values (stat_density). 
# 2: Removed 1778 rows containing non-finite values (stat_density). 
# > 
#   > #########################################
# > # 18) PREDICTION FUNCTION FOR LATEST MODEL
#   > #########################################
# > 
#   > predict_true_wage_v4 <- function(bg_data,
#                                      +                                  model_obj = xgb_v4,
#                                      +                                  feature_columns = xgb_v4$feature_cols) {
#     +   prediction_features <- bg_data %>%
#       +     select(all_of(feature_columns)) %>%
#       +     mutate(across(everything(), ~ ifelse(is.na(.) | is.infinite(.), 0, .))) %>%
#       +     as.matrix()
#     + 
#       +   predict(model_obj$model, xgb.DMatrix(data = prediction_features))
#     + }
# > 
#   > save(
#     +   predict_true_wage_v4,
#     +   file = "model_output_v4/predict_true_wage_v4_function.RData"
#     + )
# > 
#   > cat("\n=== V4 modeling pipeline complete ===\n")
# 
# === V4 modeling pipeline complete ===
#   > cat("Latest active model: XGBoost V4\n")
# Latest active model: XGBoost V4
# > cat("Output folder: model_output_v4/\n")
# Output folder: model_output_v4/
#   >
#   
#   
#   


> gc()
used     (Mb)  gc trigger     (Mb)    max used     (Mb)
Ncells     3858891    206.1    25252031   1348.7    39456297   2107.2
Vcells 16264947623 124091.8 24482603705 186787.5 20200631408 154118.6



> print(run_summary)
source_object          model_label                sample_label n_rows_total n_rows_shap n_features    top_feature top_feature_mean_abs_shap
1          xgb_v1 XGBoost V1 In-Sample In-sample common comparison      1034250        5000         10      mean_wage                 0.2277120
2 temporal_xgb_v1  XGBoost V1 Temporal    Temporal train 1994-2011       972483        5000         10      mean_wage                 0.2653351
3 temporal_xgb_v1  XGBoost V1 Temporal          Temporal test 2012        61767        5000         10      mean_wage                 0.2617876
4          xgb_v4 XGBoost V4 In-Sample In-sample common comparison      1034250        5000         37 interval_lower                 0.2554167
5 temporal_xgb_v4  XGBoost V4 Temporal    Temporal train 1994-2011       972483        5000         37 interval_lower                 0.2935251
6 temporal_xgb_v4  XGBoost V4 Temporal          Temporal test 2012        61767        5000         37 interval_lower                 0.2985124

#########################################
# CORRECTED ERROR SUMMARY WITH BG BASELINE
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
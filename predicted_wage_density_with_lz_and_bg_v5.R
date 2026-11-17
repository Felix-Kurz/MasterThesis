#########################################
# PREDICTED WAGE DISTRIBUTION PLOTS:
# BG BASELINE + 4 MODELS + LZ TRUE WAGE
#########################################

predicted_wages_full <- bind_rows(
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
    ),
  tobit_v1$data %>%
    transmute(
      method = "Tobit V1",
      predicted_wage = model_prediction,
      censored = bg_censored_flag
    ),
  tobit_v4$data %>%
    transmute(
      method = "Tobit V4",
      predicted_wage = model_prediction,
      censored = bg_censored_flag
    ),
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
  "LZ True Wage" = "darkgreen",
  "Tobit V1" = "purple",
  "Tobit V4" = "brown",
  "XGBoost V1" = "red",
  "XGBoost V4" = "darkblue"
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
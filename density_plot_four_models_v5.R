#########################################
# DENSITY PLOTS: 4 MODELS ONLY
#########################################

errors_4models_all <- bind_rows(
  tobit_v1$evaluation %>% select(method, error, censored),
  tobit_v4$evaluation %>% select(method, error, censored),
  xgb_v1$evaluation %>% select(method, error, censored),
  xgb_v4$evaluation %>% select(method, error, censored)
) %>%
  filter(is.finite(error))

errors_4models_censored <- errors_4models_all %>%
  filter(censored == 1)

errors_4models_uncensored <- errors_4models_all %>%
  filter(censored == 0)

color_scheme_4models <- c(
  "Tobit V1"   = "purple",
  "Tobit V4"   = "brown",
  "XGBoost V1" = "red",
  "XGBoost V4" = "darkblue"
)

plot_model_error_density <- function(df, title_text, subtitle_text, out_path) {
  p <- ggplot(df, aes(x = error, color = method, fill = method)) +
    geom_density(alpha = 0.20, size = 1.1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
    scale_color_manual(values = color_scheme_4models) +
    scale_fill_manual(values = color_scheme_4models) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = "Prediction Error (Predicted - LZ True Wage, ln units)",
      y = "Density",
      color = "Method",
      fill = "Method"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    ) +
    xlim(-2, 2)

  print(p)
  ggsave(out_path, plot = p, width = 10, height = 6)
}

# All observations
plot_model_error_density(
  errors_4models_all,
  "Error Distribution: Tobit V1 vs Tobit V4 vs XGBoost V1 vs XGBoost V4",
  "All observations",
  file.path(plots_dir, "error_density_4models_all.png")
)

# Censored only
plot_model_error_density(
  errors_4models_censored,
  "Error Distribution: Tobit V1 vs Tobit V4 vs XGBoost V1 vs XGBoost V4",
  "Censored observations only",
  file.path(plots_dir, "error_density_4models_censored.png")
)

# Uncensored only
plot_model_error_density(
  errors_4models_uncensored,
  "Error Distribution: Tobit V1 vs Tobit V4 vs XGBoost V1 vs XGBoost V4",
  "Uncensored observations only",
  file.path(plots_dir, "error_density_4models_uncensored.png")
)
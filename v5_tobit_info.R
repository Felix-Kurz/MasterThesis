library(dplyr)

out <- "model_output_v5_temporal_250k_data_seed123"

load(file.path(out, "model_objects_v5.RData"))
# gives: tobit_v1, tobit_v4, xgb_v1, xgb_v4

tobit_importance <- function(obj, label) {
  tab <- as.data.frame(summary(obj$model)$table)
  tab$feature <- rownames(tab)
  
  tab %>%
    rename(
      estimate = Value,
      std_error = `Std. Error`,
      z_value = z,
      p_value = p
    ) %>%
    filter(!feature %in% c("(Intercept)", "Log(scale)")) %>%
    mutate(
      model = label,
      abs_z = abs(z_value),
      abs_estimate = abs(estimate),
      .before = 1
    ) %>%
    arrange(desc(abs_z))
}

tobit_v1_importance <- tobit_importance(tobit_v1, "Tobit V1")
tobit_v4_importance <- tobit_importance(tobit_v4, "Tobit V4")

View(tobit_v1_importance)
View(tobit_v4_importance)

write.csv(tobit_v1_importance, file.path(out, "tobit_v1_importance_v5.csv"), row.names = FALSE)
write.csv(tobit_v4_importance, file.path(out, "tobit_v4_importance_v5.csv"), row.names = FALSE)
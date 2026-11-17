library(data.table)
library(dplyr)
library(ggplot2)
library(xgboost)

options(dplyr.summarise.inform = FALSE)

#########################################
# 0) CONFIG
#########################################

base_dir   <- "model_output_v4_10k"
model_file <- file.path(base_dir, "xgboost_models_v4.RData")
output_dir <- file.path(base_dir, "shap_output")

# Use a manageable sample first for memory safety
use_sample <- TRUE
sample_n <- 5000
sample_seed <- 123

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

#########################################
# 1) LOAD SAVED MODEL OBJECTS
#########################################

load(model_file)   # loads xgb_v1, xgb_v2, xgb_v4

if (!exists("xgb_v4")) {
  stop("xgb_v4 not found in model file.")
}

if (is.null(xgb_v4$model)) {
  stop("xgb_v4$model is missing.")
}

if (is.null(xgb_v4$X_features)) {
  stop("xgb_v4$X_features is missing.")
}

if (is.null(xgb_v4$feature_cols)) {
  stop("xgb_v4$feature_cols is missing.")
}

X_all <- xgb_v4$X_features
model <- xgb_v4$model
feature_cols <- xgb_v4$feature_cols

cat("Loaded xgb_v4 successfully.\n")
cat("Rows in X_all:", nrow(X_all), "\n")
cat("Columns in X_all:", ncol(X_all), "\n")
cat("Number of features:", length(feature_cols), "\n")

#########################################
# 2) OPTIONAL SAMPLE
#########################################

if (use_sample) {
  set.seed(sample_seed)
  n_use <- min(sample_n, nrow(X_all))
  sample_idx <- sample(seq_len(nrow(X_all)), n_use)
  X_use <- X_all[sample_idx, , drop = FALSE]
  cat("Using SHAP sample of", nrow(X_use), "rows.\n")
} else {
  X_use <- X_all
  cat("Using full feature matrix for SHAP.\n")
}

# Make sure column names exist
if (is.null(colnames(X_use))) {
  colnames(X_use) <- feature_cols
}

#########################################
# 3) COMPUTE NATIVE XGBOOST SHAP VALUES
#########################################

cat("Computing SHAP contributions with predcontrib = TRUE ...\n")
shap_values <- predict(model, X_use, predcontrib = TRUE)

cat("SHAP matrix dimensions:", dim(shap_values)[1], "x", dim(shap_values)[2], "\n")

# Convert to data.frame for saving
shap_df <- as.data.frame(shap_values)

write.csv(
  shap_df,
  file.path(output_dir, "shap_values_v4_sample.csv"),
  row.names = FALSE
)

#########################################
# 4) SHAP IMPORTANCE TABLE
#########################################

# Exclude BIAS column if present
feature_names_shap <- colnames(shap_df)
bias_candidates <- c("BIAS", "BiasTerm")

feature_cols_only <- setdiff(feature_names_shap, bias_candidates)

shap_importance <- data.frame(
  Feature = feature_cols_only,
  MeanAbsSHAP = sapply(feature_cols_only, function(f) mean(abs(shap_df[[f]]), na.rm = TRUE)),
  stringsAsFactors = FALSE
) %>%
  arrange(desc(MeanAbsSHAP))

write.csv(
  shap_importance,
  file.path(output_dir, "shap_importance_v4_sample.csv"),
  row.names = FALSE
)

cat("\nTop 20 SHAP features:\n")
print(head(shap_importance, 20))

#########################################
# 5) BAR PLOT OF TOP SHAP FEATURES
#########################################

top_n_bar <- min(20, nrow(shap_importance))

p_bar <- shap_importance %>%
  slice_head(n = top_n_bar) %>%
  mutate(Feature = factor(Feature, levels = rev(Feature))) %>%
  ggplot(aes(x = Feature, y = MeanAbsSHAP)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal(base_size = 12) +
  labs(
    title = "SHAP Feature Importance - XGBoost V4",
    subtitle = paste0("Native XGBoost SHAP, n = ", nrow(X_use), " observations"),
    x = "Feature",
    y = "Mean absolute SHAP value"
  )

ggsave(
  filename = file.path(output_dir, "shap_importance_bar_v4_sample.png"),
  plot = p_bar,
  width = 10,
  height = 7,
  dpi = 300
)

#########################################
# 6) BEESWARM-LIKE SUMMARY PLOT FOR TOP FEATURES
#########################################

top_n_summary <- min(10, nrow(shap_importance))
top_features <- shap_importance$Feature[1:top_n_summary]

X_use_df <- as.data.frame(X_use)

plot_list <- lapply(top_features, function(feat) {
  data.frame(
    Feature = feat,
    SHAP = shap_df[[feat]],
    FeatureValue = X_use_df[[feat]]
  )
})

summary_long <- bind_rows(plot_list)

# Cap extreme SHAP values for prettier plotting if needed
summary_long <- summary_long %>%
  filter(is.finite(SHAP), is.finite(FeatureValue))

p_summary <- ggplot(summary_long, aes(x = SHAP, y = Feature, color = FeatureValue)) +
  geom_point(alpha = 0.5, size = 1, position = position_jitter(height = 0.2, width = 0)) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal(base_size = 12) +
  labs(
    title = "SHAP Summary Plot - XGBoost V4",
    subtitle = paste0("Top ", top_n_summary, " features"),
    x = "SHAP value",
    y = "Feature",
    color = "Feature value"
  )

ggsave(
  filename = file.path(output_dir, "shap_summary_v4_sample.png"),
  plot = p_summary,
  width = 11,
  height = 7,
  dpi = 300
)

#########################################
# 7) OPTIONAL CHECK: SHAP SUMS MATCH PREDICTIONS
#########################################

pred_regular <- predict(model, X_use)
pred_from_shap <- rowSums(shap_values)

check_diff <- pred_regular - pred_from_shap

cat("\nPrediction reconstruction check:\n")
print(summary(check_diff))

write.csv(
  data.frame(
    pred_regular = pred_regular,
    pred_from_shap = pred_from_shap,
    diff = check_diff
  ),
  file.path(output_dir, "shap_prediction_check_v4_sample.csv"),
  row.names = FALSE
)

cat("\nDone.\n")
cat("Output written to:", output_dir, "\n")


#########################################
# 8) SHAP INTERACTION ANALYSIS - SAFE FULL-MATRIX VERSION
#########################################

run_interactions <- TRUE
interaction_top_n <- 8
interaction_sample_n <- 500

if (run_interactions) {
  cat("\nStarting SHAP interaction analysis...\n")
  
  top_interaction_features <- shap_importance %>%
    slice_head(n = min(interaction_top_n, nrow(shap_importance))) %>%
    pull(Feature)
  
  cat("Top features used for interaction summaries:\n")
  print(top_interaction_features)
  
  if (nrow(X_use) > interaction_sample_n) {
    set.seed(sample_seed)
    idx_inter <- sample(seq_len(nrow(X_use)), interaction_sample_n)
    X_inter_full <- X_use[idx_inter, , drop = FALSE]
    cat("Using reduced row sample for interactions:", nrow(X_inter_full), "rows.\n")
  } else {
    X_inter_full <- X_use
    cat("Using current SHAP sample for interactions with", nrow(X_inter_full), "rows.\n")
  }
  
  cat("Computing SHAP interaction values with full feature matrix...\n")
  interaction_values <- predict(model, X_inter_full, predinteraction = TRUE)
  
  print(dim(interaction_values))
  
  if (length(dim(interaction_values)) != 3) {
    stop("Unexpected shape returned by predinteraction = TRUE.")
  }
  
  full_feature_names <- dimnames(interaction_values)[[2]]
  
  if (is.null(full_feature_names)) {
    full_feature_names <- colnames(X_inter_full)
  }
  
  keep_idx <- which(full_feature_names %in% top_interaction_features)
  
  interaction_sub <- interaction_values[, keep_idx, keep_idx, drop = FALSE]
  
  kept_names <- full_feature_names[keep_idx]
  
  interaction_mean <- matrix(
    NA_real_,
    nrow = length(keep_idx),
    ncol = length(keep_idx),
    dimnames = list(kept_names, kept_names)
  )
  
  for (i in seq_along(keep_idx)) {
    for (j in seq_along(keep_idx)) {
      interaction_mean[i, j] <- mean(abs(interaction_sub[, i, j]), na.rm = TRUE)
    }
  }
  
  interaction_df <- as.data.frame(as.table(interaction_mean)) %>%
    rename(Feature1 = Var1, Feature2 = Var2, MeanAbsInteraction = Freq) %>%
    arrange(desc(MeanAbsInteraction))
  
  write.csv(
    interaction_df,
    file.path(output_dir, "shap_interaction_importance_v4_sample.csv"),
    row.names = FALSE
  )
  
  p_interaction <- ggplot(interaction_df,
                          aes(x = Feature1, y = Feature2, fill = MeanAbsInteraction)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "darkred") +
    theme_minimal(base_size = 12) +
    labs(
      title = "SHAP Interaction Heatmap - XGBoost V4",
      subtitle = paste0("Top ", length(kept_names), " features"),
      x = "Feature",
      y = "Feature",
      fill = "Mean |interaction|"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(
    filename = file.path(output_dir, "shap_interaction_heatmap_v4_sample.png"),
    plot = p_interaction,
    width = 9,
    height = 7,
    dpi = 300
  )
  
  cat("\nTop SHAP interactions:\n")
  print(head(interaction_df, 20))
}
library(dplyr)
library(ggplot2)
library(xgboost)

options(dplyr.summarise.inform = FALSE)

#########################################
# 0) CONFIG
#########################################

# Latest v5 folder with log-RMSE and wage-level outputs.
# The heavy .RData model files must also be present here when this script runs:
#   model_objects_v5.RData
#   temporal_split_models_v5.RData
base_dir <- "model_output_v5_temporal_250k_data_seed123"
output_dir <- file.path(base_dir, "shap_output")

# SHAP is computed for all saved XGBoost models:
#   - in-sample xgb_v1 and xgb_v4
#   - temporal xgb_v1 and xgb_v4, explained on train and test samples
use_sample <- FALSE
sample_n <- 5000
sample_seed <- 123

# Native XGBoost interactions are memory-heavy. Keep FALSE unless needed.
run_interactions <- TRUE
interaction_top_n <- 8
interaction_sample_n <- 500

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

#########################################
# 1) HELPERS
#########################################

prepare_matrix <- function(df, feature_cols) {
  missing_cols <- setdiff(feature_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing feature columns: ", paste(missing_cols, collapse = ", "))
  }

  df %>%
    select(all_of(feature_cols)) %>%
    mutate(across(everything(), ~ ifelse(is.na(.) | is.infinite(.), 0, .))) %>%
    as.matrix()
}

load_required <- function(file) {
  if (!file.exists(file)) {
    stop(
      "Required model file not found: ", file, "\n",
      "Copy this .RData file from the MACH/HPC run into base_dir, or change base_dir."
    )
  }

  env <- new.env(parent = globalenv())
  load(file, envir = env)
  env
}

get_required_object <- function(env, object_name, source_file) {
  if (!exists(object_name, envir = env, inherits = FALSE)) {
    stop(object_name, " not found in ", source_file)
  }

  get(object_name, envir = env, inherits = FALSE)
}

validate_xgb_object <- function(xgb_obj, object_name) {
  if (is.null(xgb_obj$model)) {
    stop(object_name, "$model is missing.")
  }

  if (is.null(xgb_obj$feature_cols)) {
    stop(object_name, "$feature_cols is missing.")
  }

  invisible(TRUE)
}

build_in_sample_job <- function(xgb_obj, object_name, model_label, source_file) {
  validate_xgb_object(xgb_obj, object_name)

  if (!is.null(xgb_obj$X_features)) {
    X_all <- xgb_obj$X_features
  } else if (!is.null(xgb_obj$data)) {
    X_all <- prepare_matrix(xgb_obj$data, xgb_obj$feature_cols)
  } else {
    stop(object_name, " must contain X_features or data.")
  }

  list(
    model_label = paste(model_label, "In-Sample"),
    sample_label = "In-sample common comparison",
    file_tag = paste0(object_name, "_in_sample_v5"),
    model = xgb_obj$model,
    feature_cols = xgb_obj$feature_cols,
    X_all = X_all,
    source_file = source_file,
    source_object = object_name
  )
}

build_temporal_job <- function(xgb_obj,
                               object_name,
                               model_label,
                               source_file,
                               sample_name,
                               sample_label,
                               data_name) {
  validate_xgb_object(xgb_obj, object_name)

  explain_data <- xgb_obj[[data_name]]
  if (is.null(explain_data)) {
    stop(object_name, "$", data_name, " is missing.")
  }

  list(
    model_label = paste(model_label, "Temporal"),
    sample_label = sample_label,
    file_tag = paste0(object_name, "_", sample_name, "_v5"),
    model = xgb_obj$model,
    feature_cols = xgb_obj$feature_cols,
    X_all = prepare_matrix(explain_data, xgb_obj$feature_cols),
    source_file = source_file,
    source_object = object_name
  )
}

build_shap_jobs <- function(base_dir) {
  in_sample_file <- file.path(base_dir, "model_objects_v5.RData")
  temporal_file <- file.path(base_dir, "temporal_split_models_v5.RData")

  in_sample_env <- load_required(in_sample_file)
  temporal_env <- load_required(temporal_file)

  xgb_specs <- data.frame(
    in_sample_object = c("xgb_v1", "xgb_v4"),
    temporal_object = c("temporal_xgb_v1", "temporal_xgb_v4"),
    model_label = c("XGBoost V1", "XGBoost V4"),
    stringsAsFactors = FALSE
  )

  jobs <- list()

  for (i in seq_len(nrow(xgb_specs))) {
    in_sample_obj <- get_required_object(
      in_sample_env,
      xgb_specs$in_sample_object[i],
      in_sample_file
    )

    temporal_obj <- get_required_object(
      temporal_env,
      xgb_specs$temporal_object[i],
      temporal_file
    )

    jobs[[length(jobs) + 1]] <- build_in_sample_job(
      xgb_obj = in_sample_obj,
      object_name = xgb_specs$in_sample_object[i],
      model_label = xgb_specs$model_label[i],
      source_file = in_sample_file
    )

    jobs[[length(jobs) + 1]] <- build_temporal_job(
      xgb_obj = temporal_obj,
      object_name = xgb_specs$temporal_object[i],
      model_label = xgb_specs$model_label[i],
      source_file = temporal_file,
      sample_name = "temporal_train",
      sample_label = "Temporal train 1994-2011",
      data_name = "train_data"
    )

    jobs[[length(jobs) + 1]] <- build_temporal_job(
      xgb_obj = temporal_obj,
      object_name = xgb_specs$temporal_object[i],
      model_label = xgb_specs$model_label[i],
      source_file = temporal_file,
      sample_name = "temporal_test",
      sample_label = "Temporal test 2012",
      data_name = "test_data"
    )
  }

  jobs
}

run_shap_job <- function(job) {
  X_all <- job$X_all
  model <- job$model
  feature_cols <- job$feature_cols

  if (is.null(colnames(X_all))) {
    colnames(X_all) <- feature_cols
  }

  cat("\n========================================\n")
  cat("SHAP job:", job$model_label, "-", job$sample_label, "\n")
  cat("Source file:", job$source_file, "\n")
  cat("Source object:", job$source_object, "\n")
  cat("Rows in X_all:", nrow(X_all), "\n")
  cat("Columns in X_all:", ncol(X_all), "\n")
  cat("Number of features:", length(feature_cols), "\n")

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

  if (is.null(colnames(X_use))) {
    colnames(X_use) <- feature_cols
  }

  d_use <- xgb.DMatrix(data = X_use)
  file_tag <- paste0(job$file_tag, ifelse(use_sample, "_sample", "_full"))

  #########################################
  # SHAP VALUES
  #########################################

  cat("Computing SHAP contributions with predcontrib = TRUE ...\n")
  shap_values <- predict(model, d_use, predcontrib = TRUE)

  cat("SHAP matrix dimensions:", dim(shap_values)[1], "x", dim(shap_values)[2], "\n")

  shap_df <- as.data.frame(shap_values)

  write.csv(
    shap_df,
    file.path(output_dir, paste0("shap_values_", file_tag, ".csv")),
    row.names = FALSE
  )

  #########################################
  # SHAP IMPORTANCE TABLE
  #########################################

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
    file.path(output_dir, paste0("shap_importance_", file_tag, ".csv")),
    row.names = FALSE
  )

  metadata <- data.frame(
    base_dir = base_dir,
    source_file = job$source_file,
    source_object = job$source_object,
    model_label = job$model_label,
    sample_label = job$sample_label,
    used_sample = use_sample,
    sample_n_requested = sample_n,
    sample_n_used = nrow(X_use),
    sample_seed = sample_seed,
    n_features = length(feature_cols),
    stringsAsFactors = FALSE
  )

  write.csv(
    metadata,
    file.path(output_dir, paste0("shap_run_metadata_", file_tag, ".csv")),
    row.names = FALSE
  )

  cat("\nTop 20 SHAP features:\n")
  print(head(shap_importance, 20))

  #########################################
  # BAR PLOT OF TOP SHAP FEATURES
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
      title = paste("SHAP Feature Importance -", job$model_label),
      subtitle = paste0(job$sample_label, ", n = ", nrow(X_use), " observations"),
      x = "Feature",
      y = "Mean absolute SHAP value"
    )

  ggsave(
    filename = file.path(output_dir, paste0("shap_importance_bar_", file_tag, ".png")),
    plot = p_bar,
    width = 10,
    height = 7,
    dpi = 300
  )

  #########################################
  # BEESWARM-LIKE SUMMARY PLOT
  #########################################

  top_n_summary <- min(10, nrow(shap_importance))
  top_features <- shap_importance$Feature[1:top_n_summary]

  X_use_df <- as.data.frame(X_use)

  summary_long <- bind_rows(lapply(top_features, function(feat) {
    data.frame(
      Feature = feat,
      SHAP = shap_df[[feat]],
      FeatureValue = X_use_df[[feat]]
    )
  }))

  summary_long <- summary_long %>%
    filter(is.finite(SHAP), is.finite(FeatureValue)) %>%
    group_by(Feature) %>%
    mutate(
      FeatureValueRelative = if (n_distinct(FeatureValue) > 1) {
        percent_rank(FeatureValue)
      } else {
        rep(0.5, n())
      }
    ) %>%
    ungroup()

  p_summary <- ggplot(summary_long, aes(x = SHAP, y = Feature, color = FeatureValueRelative)) +
    geom_point(alpha = 0.5, size = 1, position = position_jitter(height = 0.2, width = 0)) +
    scale_color_gradient(
      low = "blue",
      high = "red",
      limits = c(0, 1),
      breaks = c(0, 1),
      labels = c("Low", "High")
    ) +
    theme_minimal(base_size = 12) +
    labs(
      title = paste("SHAP Summary Plot -", job$model_label),
      subtitle = paste0(job$sample_label, ", top ", top_n_summary, " features"),
      x = "SHAP value",
      y = "Feature",
      color = "Relative feature value"
    )

  ggsave(
    filename = file.path(output_dir, paste0("shap_summary_", file_tag, ".png")),
    plot = p_summary,
    width = 11,
    height = 7,
    dpi = 300
  )

  #########################################
  # CHECK: SHAP SUMS MATCH PREDICTIONS
  #########################################

  pred_regular <- predict(model, d_use)
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
    file.path(output_dir, paste0("shap_prediction_check_", file_tag, ".csv")),
    row.names = FALSE
  )

  #########################################
  # OPTIONAL SHAP INTERACTION ANALYSIS
  #########################################

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

    d_inter <- xgb.DMatrix(data = X_inter_full)

    cat("Computing SHAP interaction values with full feature matrix...\n")
    interaction_values <- predict(model, d_inter, predinteraction = TRUE)

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
      file.path(output_dir, paste0("shap_interaction_importance_", file_tag, ".csv")),
      row.names = FALSE
    )

    p_interaction <- ggplot(
      interaction_df,
      aes(x = Feature1, y = Feature2, fill = MeanAbsInteraction)
    ) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "darkred") +
      theme_minimal(base_size = 12) +
      labs(
        title = paste("SHAP Interaction Heatmap -", job$model_label),
        subtitle = paste0(job$sample_label, ", top ", length(kept_names), " features"),
        x = "Feature",
        y = "Feature",
        fill = "Mean |interaction|"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggsave(
      filename = file.path(output_dir, paste0("shap_interaction_heatmap_", file_tag, ".png")),
      plot = p_interaction,
      width = 9,
      height = 7,
      dpi = 300
    )

    cat("\nTop SHAP interactions:\n")
    print(head(interaction_df, 20))
  }

  data.frame(
    source_object = job$source_object,
    model_label = job$model_label,
    sample_label = job$sample_label,
    n_rows_total = nrow(X_all),
    n_rows_shap = nrow(X_use),
    n_features = length(feature_cols),
    top_feature = shap_importance$Feature[1],
    top_feature_mean_abs_shap = shap_importance$MeanAbsSHAP[1],
    stringsAsFactors = FALSE
  )
}

#########################################
# 2) RUN ALL SHAP JOBS
#########################################

shap_jobs <- build_shap_jobs(base_dir)

cat("Prepared", length(shap_jobs), "SHAP jobs.\n")

run_summary <- bind_rows(lapply(shap_jobs, run_shap_job))

write.csv(
  run_summary,
  file.path(output_dir, "shap_all_models_run_summary_v5.csv"),
  row.names = FALSE
)

cat("\n========================================\n")
cat("All SHAP jobs complete.\n")
cat("Output written to:", output_dir, "\n")
cat("\nRun summary:\n")
print(run_summary)

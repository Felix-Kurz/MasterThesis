# Rebuild the result tables without refitting any model.
# Run this in the still-open R session after the V2 modelling script finished:
#   source("rebuild_finite_tobit_tables_v5.R")

suppressPackageStartupMessages(library(dplyr))

required_objects <- c(
  "output_dir", "tobit_v1", "temporal_tobit_v1",
  "comprehensive_rmse_table", "temporal_rmse_table", "summarise_eval"
)
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1), inherits = TRUE)]
if (length(missing_objects) > 0) {
  stop(
    "Required objects are not loaded: ", paste(missing_objects, collapse = ", "),
    ". Run this script in the R session that completed the V2 modelling run."
  )
}

clean_output_dir <- file.path(output_dir, "finite_tobit_tables")
dir.create(clean_output_dir, recursive = TRUE, showWarnings = FALSE)

# Copy the complete root-level CSV result set; the two affected RMSE files are
# overwritten below with finite-only Tobit summaries.
source_csvs <- list.files(output_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(source_csvs) > 0) {
  copied <- file.copy(source_csvs, clean_output_dir, overwrite = TRUE)
  if (!all(copied)) stop("Could not copy every existing CSV result table.")
}

finite_eval <- function(eval_df) {
  eval_df %>%
    filter(
      is.finite(lz_true), is.finite(pred), is.finite(error),
      is.finite(abs_error), is.finite(squared_error)
    )
}

count_nonfinite <- function(eval_df, evaluation_sample) {
  tibble(
    Evaluation_Sample = evaluation_sample,
    Total_Rows = nrow(eval_df),
    Finite_Rows = sum(is.finite(eval_df$error)),
    Positive_Inf = sum(is.infinite(eval_df$error) & eval_df$error > 0, na.rm = TRUE),
    Negative_Inf = sum(is.infinite(eval_df$error) & eval_df$error < 0, na.rm = TRUE),
    NA_or_NaN = sum(is.na(eval_df$error)),
    Nonfinite_Total = sum(!is.finite(eval_df$error)),
    Nonfinite_Censored = sum(!is.finite(eval_df$error) & eval_df$censored == 1, na.rm = TRUE),
    Nonfinite_Uncensored = sum(!is.finite(eval_df$error) & eval_df$censored == 0, na.rm = TRUE)
  )
}

tobit_nonfinite_counts <- bind_rows(
  count_nonfinite(tobit_v1$evaluation, "In-sample common comparison"),
  count_nonfinite(temporal_tobit_v1$train_evaluation, "Train 1994-2011"),
  count_nonfinite(temporal_tobit_v1$test_evaluation, "Test 2012")
)

tobit_v1_summary_finite <- summarise_eval(finite_eval(tobit_v1$evaluation)) %>%
  mutate(Method = "Tobit V1", Target = "LZ (true wages)") %>%
  select(Method, Target, Observation_Type, n_obs, RMSE, MAE, Mean_Error, SD_Error)

comprehensive_rmse_table_finite <- comprehensive_rmse_table %>%
  filter(Method != "Tobit V1") %>%
  bind_rows(tobit_v1_summary_finite) %>%
  arrange(Observation_Type, RMSE)

temporal_tobit_summary_finite <- bind_rows(
  summarise_eval(finite_eval(temporal_tobit_v1$train_evaluation)) %>%
    mutate(Evaluation_Sample = "Train 1994-2011", .before = 1),
  summarise_eval(finite_eval(temporal_tobit_v1$test_evaluation)) %>%
    mutate(Evaluation_Sample = "Test 2012", .before = 1)
) %>%
  mutate(Method = "Tobit V1 Temporal", Target = "LZ (true wages)") %>%
  select(
    Evaluation_Sample, Method, Target, Observation_Type,
    n_obs, RMSE, MAE, Mean_Error, SD_Error
  )

temporal_rmse_table_finite <- temporal_rmse_table %>%
  filter(Method != "Tobit V1 Temporal") %>%
  bind_rows(temporal_tobit_summary_finite) %>%
  arrange(Evaluation_Sample, Observation_Type, RMSE)

write.csv(
  comprehensive_rmse_table_finite,
  file.path(clean_output_dir, "comprehensive_rmse_comparison_v5.csv"),
  row.names = FALSE
)
write.csv(
  temporal_rmse_table_finite,
  file.path(clean_output_dir, "temporal_split_rmse_v5.csv"),
  row.names = FALSE
)
write.csv(
  tobit_nonfinite_counts,
  file.path(clean_output_dir, "tobit_nonfinite_counts_v5.csv"),
  row.names = FALSE
)

# Final safety check across every CSV saved in the new subfolder.
saved_csvs <- list.files(clean_output_dir, pattern = "\\.csv$", full.names = TRUE)
inf_by_file <- vapply(saved_csvs, function(csv_file) {
  tab <- read.csv(csv_file, check.names = FALSE)
  sum(vapply(tab, function(column) {
    if (is.numeric(column)) sum(is.infinite(column)) else 0L
  }, integer(1)))
}, integer(1))

if (any(inf_by_file > 0L)) {
  stop(
    "Infinite values remain in: ",
    paste(basename(names(inf_by_file)[inf_by_file > 0L]), collapse = ", ")
  )
}

cat("\n=== TOBIT NON-FINITE ERROR COUNTS ===\n")
print(tobit_nonfinite_counts, n = Inf)
cat("\nClean tables written to:\n", normalizePath(clean_output_dir), "\n", sep = "")
cat("Verified:", length(saved_csvs), "CSV files contain zero Inf values.\n")

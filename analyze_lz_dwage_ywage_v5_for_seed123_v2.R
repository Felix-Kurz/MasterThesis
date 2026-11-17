# ==========================================
# Analyze Lohnzettel daily vs yearly wages
# for the new v5 250k seed123 comparison sample drawn from persons
# observed in HV during the 1994-2012 Lohnzettel/model window
# ==========================================

library(dplyr)
library(readr)
library(lubridate)

options(dplyr.summarise.inform = FALSE)

# ----------------------------
# 0) CONFIG
# ----------------------------

firstyear <- 1994
lastyear  <- 2012

# Sample-specific paths. These match the V4V2 dataset builder and V2 model run.
pre_model_file <- file.path(
  "pre_model_output_250k_seed123_hv_1994_2012_v4v2",
  "pre_model_full_history_250k_seed123_hv_1994_2012_v4v2.RData"
)
lohnzettel_file <- "lohnzettel_dta.RData"
model_output_dir <- "model_output_v5_temporal_250k_data_seed123_hv_1994_2012_v4v2"
output_dir <- file.path(model_output_dir, "lz_exploratory_analysis")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ----------------------------
# 1) LOAD INPUTS
# ----------------------------

missing_input_files <- c(pre_model_file, lohnzettel_file)[
  !file.exists(c(pre_model_file, lohnzettel_file))
]

if (length(missing_input_files) > 0) {
  stop(
    "Missing required input file(s): ",
    paste(missing_input_files, collapse = ", ")
  )
}

loaded_pre_model_objects <- load(pre_model_file)
if (!("pre_model_full_history" %in% loaded_pre_model_objects)) {
  stop("Input file does not contain the expected object: pre_model_full_history")
}

loaded_lohnzettel_objects <- load(lohnzettel_file)
if (!("lohnzettel" %in% loaded_lohnzettel_objects)) {
  stop("Input file does not contain the expected object: lohnzettel")
}

required_pre_model_columns <- c(
  "penr", "benr", "contryear", "dailywage_final", "censored"
)
missing_pre_model_columns <- setdiff(
  required_pre_model_columns,
  names(pre_model_full_history)
)
if (length(missing_pre_model_columns) > 0) {
  stop(
    "pre_model_full_history is missing required column(s): ",
    paste(missing_pre_model_columns, collapse = ", ")
  )
}

required_lohnzettel_columns <- c(
  "penr", "benr", "year", "lz_ywage", "lz_dwage"
)
missing_lohnzettel_columns <- setdiff(
  required_lohnzettel_columns,
  names(lohnzettel)
)
if (length(missing_lohnzettel_columns) > 0) {
  stop(
    "lohnzettel is missing required column(s): ",
    paste(missing_lohnzettel_columns, collapse = ", ")
  )
}

cat("Loaded pre-model file:", pre_model_file, "\n")
cat("Loaded Lohnzettel file:", lohnzettel_file, "\n")
cat("Loaded pre_model_full_history rows:", nrow(pre_model_full_history), "\n")
cat("Loaded lohnzettel rows:", nrow(lohnzettel), "\n")

# ----------------------------
# 2) CPI
# ----------------------------

vpi_1986 <- data.frame(
  contryear = 1986:2024,
  year_inflation = c(
    0, 1.4, 2, 2.5, 3.3, 3.3, 4.1, 3.6, 3.0, 2.2, 1.9, 1.3, 0.9, 0.6,
    2.3, 2.7, 1.8, 1.3, 2.1, 2.3, 1.5, 2.2, 3.2, 0.5, 1.9, 3.3, 2.4,
    2.0, 1.7, 0.9, 0.9, 2.1, 2.0, 1.5, 1.4, 2.8, 8.6, 7.8, 2.9
  )
) %>%
  mutate(
    vpi = year_inflation / 100 + 1,
    reverse_index = cumprod(vpi),
    cpi_2000 = reverse_index[contryear == 2000] / reverse_index
  )

# ----------------------------
# 3) REBUILD COMPARISON DATA
# ----------------------------

analysis_data <- pre_model_full_history %>%
  filter(contryear >= firstyear, contryear <= lastyear)

if (nrow(analysis_data) == 0) {
  stop("No pre-model observations remain after the analysis-year restriction.")
}

missing_analysis_years <- setdiff(
  firstyear:lastyear,
  sort(unique(analysis_data$contryear))
)
if (length(missing_analysis_years) > 0) {
  stop(
    "Analysis data is missing required year(s): ",
    paste(missing_analysis_years, collapse = ", ")
  )
}

analysis_duplicate_row <- anyDuplicated(
  as.data.frame(analysis_data[c("penr", "benr", "contryear")])
)
if (analysis_duplicate_row > 0) {
  stop(
    "Duplicate penr-benr-contryear key detected in analysis_data at row ",
    analysis_duplicate_row, "."
  )
}

nonfinite_analysis_wages <- sum(!is.finite(analysis_data$dailywage_final))
if (nonfinite_analysis_wages > 0) {
  stop(
    "analysis_data contains ", nonfinite_analysis_wages,
    " non-finite dailywage_final value(s)."
  )
}

sampled_penr <- analysis_data %>%
  distinct(penr) %>%
  pull(penr)

lz_comparison_data <- lohnzettel %>%
  filter(penr %in% sampled_penr) %>%
  filter(year >= firstyear, year <= lastyear) %>%
  rename(contryear = year) %>%
  left_join(vpi_1986 %>% select(contryear, cpi_2000), by = "contryear") %>%
  mutate(
    lz_ywage_2000 = lz_ywage * cpi_2000,
    lz_dwage_2000 = lz_dwage * cpi_2000,
    lz_ywage_log = log(pmax(lz_ywage_2000, 1)),
    lz_dwage_log = log(pmax(lz_dwage_2000, 1)),
    implied_lz_days = ifelse(lz_dwage > 0, lz_ywage / lz_dwage, NA_real_),
    log_y_minus_log_d = lz_ywage_log - lz_dwage_log
  ) %>%
  select(
    penr, benr, contryear,
    lz_ywage, lz_dwage,
    lz_ywage_2000, lz_dwage_2000,
    lz_ywage_log, lz_dwage_log,
    implied_lz_days, log_y_minus_log_d
  ) %>%
  filter(is.finite(lz_dwage_log))

if (nrow(lz_comparison_data) == 0) {
  stop("No valid Lohnzettel rows remain for the selected persons and years.")
}

lz_duplicate_row <- anyDuplicated(
  as.data.frame(lz_comparison_data[c("penr", "benr", "contryear")])
)
if (lz_duplicate_row > 0) {
  stop(
    "Duplicate penr-benr-contryear key detected in lz_comparison_data at row ",
    lz_duplicate_row,
    ". Resolve this before the BG-LZ join to prevent row multiplication."
  )
}

comparison_data <- analysis_data %>%
  left_join(
    lz_comparison_data,
    by = c("penr", "benr", "contryear")
  ) %>%
  filter(!is.na(lz_dwage_log)) %>%
  mutate(
    bg_dwage_log = dailywage_final,
    wage_group = ifelse(censored == 1, "Censored", "Uncensored")
  )

if (nrow(comparison_data) == 0) {
  stop("The BG-LZ comparison sample is empty after matching.")
}

comparison_duplicate_row <- anyDuplicated(
  as.data.frame(comparison_data[c("penr", "benr", "contryear")])
)
if (comparison_duplicate_row > 0) {
  stop(
    "Duplicate penr-benr-contryear key detected in comparison_data at row ",
    comparison_duplicate_row, "."
  )
}

sample_flow <- tibble(
  Stage = c(
    "Loaded pre-model full history",
    paste0("Pre-model analysis window ", firstyear, "-", lastyear),
    paste0("Valid LZ rows for selected persons ", firstyear, "-", lastyear),
    "Final matched BG-LZ comparison sample"
  ),
  Observations = c(
    nrow(pre_model_full_history),
    nrow(analysis_data),
    nrow(lz_comparison_data),
    nrow(comparison_data)
  ),
  Persons = c(
    n_distinct(pre_model_full_history$penr),
    n_distinct(analysis_data$penr),
    n_distinct(lz_comparison_data$penr),
    n_distinct(comparison_data$penr)
  )
)

cat("Analysis data rows:", nrow(analysis_data), "\n")
cat("Analysis data persons:", n_distinct(analysis_data$penr), "\n")
cat("Analysis duplicate keys:", analysis_duplicate_row, "\n")
cat("Analysis non-finite wages:", nonfinite_analysis_wages, "\n")
cat("LZ comparison rows:", nrow(lz_comparison_data), "\n")
cat("LZ comparison persons:", n_distinct(lz_comparison_data$penr), "\n")
cat("LZ duplicate keys:", lz_duplicate_row, "\n")
cat("Comparison rows:", nrow(comparison_data), "\n")
cat("Comparison persons:", n_distinct(comparison_data$penr), "\n")
cat("Comparison duplicate keys:", comparison_duplicate_row, "\n")

cat("\n=== Sample flow ===\n")
print(sample_flow, n = Inf)

# ----------------------------
# 4) SUMMARY HELPERS
# ----------------------------

summarise_numeric <- function(df, var) {
  x <- df[[var]]
  tibble(
    variable = var,
    n = sum(!is.na(x)),
    n_missing = sum(is.na(x)),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    p01 = quantile(x, 0.01, na.rm = TRUE, names = FALSE),
    p05 = quantile(x, 0.05, na.rm = TRUE, names = FALSE),
    p25 = quantile(x, 0.25, na.rm = TRUE, names = FALSE),
    median = median(x, na.rm = TRUE),
    p75 = quantile(x, 0.75, na.rm = TRUE, names = FALSE),
    p90 = quantile(x, 0.90, na.rm = TRUE, names = FALSE),
    p95 = quantile(x, 0.95, na.rm = TRUE, names = FALSE),
    p99 = quantile(x, 0.99, na.rm = TRUE, names = FALSE),
    max = max(x, na.rm = TRUE)
  )
}

vars_to_summarise <- c(
  "lz_ywage", "lz_dwage",
  "lz_ywage_2000", "lz_dwage_2000",
  "lz_ywage_log", "lz_dwage_log",
  "implied_lz_days", "log_y_minus_log_d",
  "bg_dwage_log"
)

# ----------------------------
# 5) OVERALL AND GROUP SUMMARIES
# ----------------------------

overall_summary <- bind_rows(
  lapply(vars_to_summarise, function(v) summarise_numeric(comparison_data, v))
)

by_censor_summary <- comparison_data %>%
  group_split(wage_group) %>%
  lapply(function(df) {
    bind_rows(lapply(vars_to_summarise, function(v) summarise_numeric(df, v))) %>%
      mutate(wage_group = unique(df$wage_group), .before = 1)
  }) %>%
  bind_rows()

by_year_summary <- comparison_data %>%
  group_by(contryear) %>%
  summarise(
    n = n(),
    mean_lz_ywage = mean(lz_ywage, na.rm = TRUE),
    mean_lz_dwage = mean(lz_dwage, na.rm = TRUE),
    mean_lz_ywage_log = mean(lz_ywage_log, na.rm = TRUE),
    mean_lz_dwage_log = mean(lz_dwage_log, na.rm = TRUE),
    median_implied_lz_days = median(implied_lz_days, na.rm = TRUE),
    mean_implied_lz_days = mean(implied_lz_days, na.rm = TRUE),
    cor_ywage_dwage = cor(lz_ywage, lz_dwage, use = "complete.obs"),
    cor_ylog_dlog = cor(lz_ywage_log, lz_dwage_log, use = "complete.obs"),
    .groups = "drop"
  )

relationship_summary <- comparison_data %>%
  summarise(
    n = n(),
    cor_ywage_dwage = cor(lz_ywage, lz_dwage, use = "complete.obs"),
    cor_ylog_dlog = cor(lz_ywage_log, lz_dwage_log, use = "complete.obs"),
    mean_implied_lz_days = mean(implied_lz_days, na.rm = TRUE),
    median_implied_lz_days = median(implied_lz_days, na.rm = TRUE),
    p05_implied_lz_days = quantile(implied_lz_days, 0.05, na.rm = TRUE, names = FALSE),
    p90_implied_lz_days = quantile(implied_lz_days, 0.90, na.rm = TRUE, names = FALSE),
    p95_implied_lz_days = quantile(implied_lz_days, 0.95, na.rm = TRUE, names = FALSE),
    share_ywage_le_0 = mean(lz_ywage <= 0, na.rm = TRUE),
    share_dwage_le_0 = mean(lz_dwage <= 0, na.rm = TRUE),
    share_implied_days_lt_30 = mean(implied_lz_days < 30, na.rm = TRUE),
    share_implied_days_gt_366 = mean(implied_lz_days > 366, na.rm = TRUE)
  )

# ----------------------------
# 6) WRITE OUTPUTS
# ----------------------------

write_csv(overall_summary, file.path(output_dir, "lz_wage_summary_overall.csv"))
write_csv(by_censor_summary, file.path(output_dir, "lz_wage_summary_by_censor.csv"))
write_csv(by_year_summary, file.path(output_dir, "lz_wage_summary_by_year.csv"))
write_csv(relationship_summary, file.path(output_dir, "lz_wage_relationship_summary.csv"))
write_csv(sample_flow, file.path(output_dir, "lz_wage_sample_flow.csv"))

cat("\n=== Relationship summary ===\n")
print(relationship_summary)

cat("\nWrote diagnostics to:", output_dir, "\n")




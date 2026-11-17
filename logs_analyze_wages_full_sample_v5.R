> # ==========================================
> # Analyze Lohnzettel daily vs yearly wages
  > # on the complete v4 pre-model population observed in HV during the
  > # 1994-2012 Lohnzettel/model window.
  > #
  > # Despite the historical "seed123" filename, this script performs no sampling.
  > # It is memory-conscious because the full pre-model input has more than
  > # 112 million rows: inputs are loaded sequentially, only required columns are
  > # retained, transformations are performed by reference, and only aggregate
  > # diagnostics are written.
  > # ==========================================
> 
  > library(data.table)
data.table 1.17.0 using 4 threads (see ?getDTthreads).  Latest news: r-datatable.com

Attaching package: ‘data.table’

The following objects are masked from ‘package:lubridate’:
  
  hour, isoweek, mday, minute, month, quarter, second, wday, week, yday, year

The following objects are masked from ‘package:dplyr’:
  
  between, first, last

> 
  > # ----------------------------
> # 0) CONFIG
  > # ----------------------------
> 
  > firstyear <- 1994L
> lastyear  <- 2012L
> 
  > # These paths match the temporal full-sample v5 model pipeline.
  > pre_model_file <- file.path(
    +   "pre_model_output_full_v4",
    +   "pre_model_full_history_full_v4.RData"
    + )
> lohnzettel_file <- "lohnzettel_dta.RData"
> model_output_dir <- "model_output_v5_temporal_full_sample_v4"
> output_dir <- file.path(model_output_dir, "lz_exploratory_analysis")
> 
  > # Leave headroom inside the standard 512 GB XXL capsule used for full modeling.
  > memory_budget_gib <- 430
> 
  > dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
> 
  > # ----------------------------
> # 1) MEMORY AND VALIDATION HELPERS
  > # ----------------------------
> 
  > rss_gib <- function() {
    +   status_file <- "/proc/self/status"
    +   if (!file.exists(status_file)) return(NA_real_)
    + 
      +   status <- readLines(status_file, warn = FALSE)
      +   rss_line <- grep("^VmRSS:", status, value = TRUE)
      +   if (length(rss_line) != 1L) return(NA_real_)
      + 
        +   rss_kib <- suppressWarnings(as.numeric(
          +     sub("^VmRSS:[[:space:]]*([0-9]+).*$", "\\1", rss_line)
          +   ))
        +   rss_kib / 1024^2
        + }
> 
  > gib <- function(bytes) bytes / 1024^3
> 
  > r_heap_gib <- function() {
    +   g <- gc(verbose = FALSE, reset = FALSE)
    +   sum(g[, "(Mb)"], na.rm = TRUE) / 1024
    + }
> 
  > memory_checkpoint <- function(stage, collect = TRUE) {
    +   if (collect) gc(verbose = FALSE)
    +   rss <- rss_gib()
    + 
      +   if (is.finite(rss)) {
        +     cat(sprintf(
          +       "[MEMORY] %-40s RSS=%.1f GiB | budget=%.0f GiB\n",
          +       stage, rss, memory_budget_gib
          +     ))
        + 
          +     if (rss > memory_budget_gib) {
            +       stop(
              +         "Memory budget exceeded at '", stage, "': ",
              +         round(rss, 1), " GiB RSS > ", memory_budget_gib, " GiB."
              +       )
            +     }
        +   } else {
          +     cat("[MEMORY]", stage, "- RSS unavailable on this platform\n")
          +   }
    + 
      +   invisible(rss)
    + }
> 
  > missing_input_files <- c(pre_model_file, lohnzettel_file)[
    +   !file.exists(c(pre_model_file, lohnzettel_file))
    + ]
> if (length(missing_input_files) > 0L) {
  +   stop(
    +     "Missing required input file(s): ",
    +     paste(missing_input_files, collapse = ", ")
    +   )
  + }
> 
  > required_pre_model_columns <- c(
    +   "penr", "benr", "contryear", "dailywage_final", "censored"
    + )
> required_lohnzettel_columns <- c(
  +   "penr", "benr", "year", "lz_ywage", "lz_dwage"
  + )
> comparison_key <- c("penr", "benr", "contryear")
> 
  > # ----------------------------
> # 2) CPI
  > # ----------------------------
> 
  > vpi_1986 <- data.table(
    +   contryear = 1986:2024,
    +   year_inflation = c(
      +     0, 1.4, 2, 2.5, 3.3, 3.3, 4.1, 3.6, 3.0, 2.2, 1.9, 1.3, 0.9, 0.6,
      +     2.3, 2.7, 1.8, 1.3, 2.1, 2.3, 1.5, 2.2, 3.2, 0.5, 1.9, 3.3, 2.4,
      +     2.0, 1.7, 0.9, 0.9, 2.1, 2.0, 1.5, 1.4, 2.8, 8.6, 7.8, 2.9
      +   )
    + )
> vpi_1986[, vpi := year_inflation / 100 + 1]
> vpi_1986[, reverse_index := cumprod(vpi)]
> vpi_1986[, cpi_2000 :=
             +   reverse_index[contryear == 2000L] / reverse_index]
> vpi_1986 <- vpi_1986[, .(contryear, cpi_2000)]
> 
  > # ----------------------------
> # 3) LOAD AND TRIM FULL PRE-MODEL DATA
  > # ----------------------------
> 
  > loaded_pre_model_objects <- load(pre_model_file)
> if (!("pre_model_full_history" %in% loaded_pre_model_objects)) {
  +   stop("Input file does not contain the expected object: pre_model_full_history")
  + }
> 
  > missing_pre_model_columns <- setdiff(
    +   required_pre_model_columns,
    +   names(pre_model_full_history)
    + )
> if (length(missing_pre_model_columns) > 0L) {
  +   stop(
    +     "pre_model_full_history is missing required column(s): ",
    +     paste(missing_pre_model_columns, collapse = ", ")
    +   )
  + }
> 
  > setDT(pre_model_full_history)
> pre_model_rows_loaded <- nrow(pre_model_full_history)
> pre_model_persons_loaded <- uniqueN(pre_model_full_history$penr)
> 
  > # This is the only large subset copy from the compressed full-history input.
  > # It immediately reduces the object to the analysis years and five needed fields.
  > analysis_data <- pre_model_full_history[
    +   contryear >= firstyear & contryear <= lastyear,
    +   required_pre_model_columns,
    +   with = FALSE
    + ]
> rm(pre_model_full_history)
> memory_checkpoint("pre-model load and trim")
[MEMORY] pre-model load and trim                  RSS=7.8 GiB | budget=430 GiB
> 
  > if (nrow(analysis_data) == 0L) {
    +   stop("No pre-model observations remain after the analysis-year restriction.")
    + }
> 
  > missing_analysis_years <- setdiff(
    +   firstyear:lastyear,
    +   sort(unique(analysis_data$contryear))
    + )
> if (length(missing_analysis_years) > 0L) {
  +   stop(
    +     "Analysis data is missing required year(s): ",
    +     paste(missing_analysis_years, collapse = ", ")
    +   )
  + }
> 
  > analysis_duplicate_row <- anyDuplicated(analysis_data, by = comparison_key)
> if (analysis_duplicate_row > 0L) {
  +   stop(
    +     "Duplicate penr-benr-contryear key detected in analysis_data at row ",
    +     analysis_duplicate_row, "."
    +   )
  + }
> 
  > nonfinite_analysis_wages <- sum(!is.finite(analysis_data$dailywage_final))
> if (nonfinite_analysis_wages > 0L) {
  +   stop(
    +     "analysis_data contains ", nonfinite_analysis_wages,
    +     " non-finite dailywage_final value(s)."
    +   )
  + }
> 
  > analysis_rows <- nrow(analysis_data)
> analysis_penr <- unique(analysis_data$penr)
> analysis_persons <- length(analysis_penr)
> 
  > cat("Loaded full-sample pre-model file:", pre_model_file, "\n")
Loaded full-sample pre-model file: pre_model_output_full_v4/pre_model_full_history_full_v4.RData 
> cat("Loaded pre-model rows:", pre_model_rows_loaded, "\n")
Loaded pre-model rows: 112238803 
> cat("Loaded pre-model persons:", pre_model_persons_loaded, "\n")
Loaded pre-model persons: 8220895 
> cat("Analysis rows", firstyear, "-", lastyear, ":", analysis_rows, "\n")
Analysis rows 1994 - 2012 : 52182986 
> cat("Analysis persons", firstyear, "-", lastyear, ":", analysis_persons, "\n")
Analysis persons 1994 - 2012 : 5335451 
> cat("Analysis duplicate keys:", analysis_duplicate_row, "\n")
Analysis duplicate keys: 0 
> cat("Analysis non-finite wages:", nonfinite_analysis_wages, "\n")
Analysis non-finite wages: 0 
> 
  > # ----------------------------
> # 4) LOAD AND TRIM LOHNZETTEL DATA
  > # ----------------------------
> 
  > # Load Lohnzettel only after the wide raw pre-model object has been removed.
  > loaded_lohnzettel_objects <- load(lohnzettel_file)
> if (!("lohnzettel" %in% loaded_lohnzettel_objects)) {
  +   stop("Input file does not contain the expected object: lohnzettel")
  + }
> 
  > missing_lohnzettel_columns <- setdiff(
    +   required_lohnzettel_columns,
    +   names(lohnzettel)
    + )
> if (length(missing_lohnzettel_columns) > 0L) {
  +   stop(
    +     "lohnzettel is missing required column(s): ",
    +     paste(missing_lohnzettel_columns, collapse = ", ")
    +   )
  + }
> 
  > setDT(lohnzettel)
> lohnzettel_rows_loaded <- nrow(lohnzettel)
> 
  > # analysis_penr contains every person in the full 1994-2012 pre-model window;
  > # there is deliberately no random or fixed-size subsampling step here.
  > lz_comparison_data <- lohnzettel[
    +   year >= firstyear & year <= lastyear & penr %in% analysis_penr,
    +   required_lohnzettel_columns,
    +   with = FALSE
    + ]
> rm(lohnzettel, analysis_penr)
> gc(verbose = FALSE)
used   (Mb) gc trigger    (Mb)   max used    (Mb)
Ncells    938389   50.2    2734060   146.1   13037010   696.3
Vcells 666233746 5083.0 2416154769 18433.8 3716036161 28351.2
> setnames(lz_comparison_data, "year", "contryear")
> 
  > # Update join avoids constructing a full-length character CPI lookup vector.
  > lz_comparison_data[
    +   vpi_1986,
    +   cpi_2000 := i.cpi_2000,
    +   on = "contryear"
    + ]
> 
  > if (anyNA(lz_comparison_data$cpi_2000)) {
    +   stop("Missing CPI value(s) in the selected Lohnzettel analysis years.")
    + }
> 
  > lz_comparison_data[, `:=`(
    +   lz_ywage_2000 = lz_ywage * cpi_2000,
    +   lz_dwage_2000 = lz_dwage * cpi_2000
    + )]
> lz_comparison_data[, `:=`(
  +   lz_ywage_log = log(pmax(lz_ywage_2000, 1)),
  +   lz_dwage_log = log(pmax(lz_dwage_2000, 1)),
  +   implied_lz_days = fifelse(lz_dwage > 0, lz_ywage / lz_dwage, NA_real_)
  + )]
> lz_comparison_data[, log_y_minus_log_d := lz_ywage_log - lz_dwage_log]
> lz_comparison_data[, cpi_2000 := NULL]
> 
  > # Match the v2 diagnostic's valid-target rule.
  > lz_comparison_data <- lz_comparison_data[is.finite(lz_dwage_log)]
> memory_checkpoint("Lohnzettel load and trim")
[MEMORY] Lohnzettel load and trim                 RSS=12.6 GiB | budget=430 GiB
> 
  > if (nrow(lz_comparison_data) == 0L) {
    +   stop("No valid Lohnzettel rows remain for the full-sample persons and years.")
    + }
> 
  > lz_duplicate_row <- anyDuplicated(lz_comparison_data, by = comparison_key)
> if (lz_duplicate_row > 0L) {
  +   stop(
    +     "Duplicate penr-benr-contryear key detected in lz_comparison_data at row ",
    +     lz_duplicate_row,
    +     ". Resolve this before the BG-LZ join to prevent row multiplication."
    +   )
  + }
> 
  > lz_rows <- nrow(lz_comparison_data)
> lz_persons <- uniqueN(lz_comparison_data$penr)
> 
  > cat("Loaded Lohnzettel file:", lohnzettel_file, "\n")
Loaded Lohnzettel file: lohnzettel_dta.RData 
> cat("Loaded Lohnzettel rows:", lohnzettel_rows_loaded, "\n")
Loaded Lohnzettel rows: 113481460 
> cat("Valid full-sample LZ rows:", lz_rows, "\n")
Valid full-sample LZ rows: 80740627 
> cat("Valid full-sample LZ persons:", lz_persons, "\n")
Valid full-sample LZ persons: 5293552 
> cat("LZ duplicate keys:", lz_duplicate_row, "\n")
LZ duplicate keys: 0 
> 
  > # ----------------------------
> # 5) BUILD MATCHED BG-LZ COMPARISON DATA
  > # ----------------------------
> 
  > # Both inputs have been checked for unique keys, so this inner join cannot
  > # multiply rows. It is equivalent to the v2 left join followed by dropping
  > # unmatched LZ targets, but does not retain the unmatched full BG population.
  > # Apply the same keyed-join pattern and conservative preflight used by the
  > # dedicated full-sample model script.
  > analysis_bytes_per_row <- as.numeric(object.size(analysis_data)) / nrow(analysis_data)
> lz_bytes_per_row <- as.numeric(object.size(lz_comparison_data)) / nrow(lz_comparison_data)
> join_output_gib <- gib(
  +   as.double(lz_rows) * (analysis_bytes_per_row + lz_bytes_per_row)
  + )
> join_base_gib <- rss_gib()
> if (!is.finite(join_base_gib)) join_base_gib <- r_heap_gib()
> join_peak_gib <- join_base_gib + 1.8 * join_output_gib + 12
> 
  > cat(sprintf(
    +   "[PREFLIGHT] BG-LZ join conservative peak=%.1f GiB\n",
    +   join_peak_gib
    + ))
[PREFLIGHT] BG-LZ join conservative peak=42.0 GiB
> if (join_peak_gib > memory_budget_gib) {
  +   stop(
    +     "Refusing BG-LZ join: conservative peak estimate is ",
    +     round(join_peak_gib, 1), " GiB, above the ", memory_budget_gib,
    +     " GiB application budget."
    +   )
  + }
> 
  > setkeyv(analysis_data, comparison_key)
> setkeyv(lz_comparison_data, comparison_key)
> comparison_data <- analysis_data[
  +   lz_comparison_data,
  +   on = comparison_key,
  +   nomatch = 0L
  + ]
> 
  > comparison_rows <- nrow(comparison_data)
> comparison_persons <- uniqueN(comparison_data$penr)
> rm(analysis_data, lz_comparison_data)
> gc(verbose = FALSE)
used   (Mb) gc trigger    (Mb)   max used    (Mb)
Ncells    944529   50.5    2734060   146.1   13037010   696.3
Vcells 652936508 4981.6 1932923816 14747.1 3716036161 28351.2
> 
  > if (comparison_rows == 0L) {
    +   stop("The full-sample BG-LZ comparison sample is empty after matching.")
    + }
> 
  > comparison_data[, bg_dwage_log := dailywage_final]
> 
  > comparison_duplicate_row <- anyDuplicated(comparison_data, by = comparison_key)
> if (comparison_duplicate_row > 0L) {
  +   stop(
    +     "Duplicate penr-benr-contryear key detected in comparison_data at row ",
    +     comparison_duplicate_row, "."
    +   )
  + }
> memory_checkpoint("matched BG-LZ comparison data")
[MEMORY] matched BG-LZ comparison data            RSS=10.0 GiB | budget=430 GiB
> 
  > sample_flow <- data.table(
    +   Stage = c(
      +     "Loaded full pre-model history",
      +     paste0("Full pre-model analysis window ", firstyear, "-", lastyear),
      +     paste0("Valid LZ rows for full-sample persons ", firstyear, "-", lastyear),
      +     "Final matched full-sample BG-LZ comparison sample"
      +   ),
    +   Observations = c(
      +     pre_model_rows_loaded,
      +     analysis_rows,
      +     lz_rows,
      +     comparison_rows
      +   ),
    +   Persons = c(
      +     pre_model_persons_loaded,
      +     analysis_persons,
      +     lz_persons,
      +     comparison_persons
      +   )
    + )
> 
  > cat("Comparison rows:", comparison_rows, "\n")
Comparison rows: 50100141 
> cat("Comparison persons:", comparison_persons, "\n")
Comparison persons: 5250104 
> cat("Comparison duplicate keys:", comparison_duplicate_row, "\n")
Comparison duplicate keys: 0 
> cat("\n=== Full-sample flow ===\n")

=== Full-sample flow ===
  > print(sample_flow)
Stage Observations Persons
<char>        <int>   <int>
  1:                     Loaded full pre-model history    112238803 8220895
2:          Full pre-model analysis window 1994-2012     52182986 5335451
3:   Valid LZ rows for full-sample persons 1994-2012     80740627 5293552
4: Final matched full-sample BG-LZ comparison sample     50100141 5250104
> 
  > # ----------------------------
> # 6) SUMMARY HELPERS
  > # ----------------------------
> 
  > summarise_numeric <- function(dt, var, rows = NULL) {
    +   x <- dt[[var]]
    +   if (!is.null(rows)) x <- x[rows]
    + 
        +   n_nonmissing <- sum(!is.na(x))
        +   if (n_nonmissing == 0L) {
          +     return(data.table(
            +       variable = var,
            +       n = 0,
            +       n_missing = length(x),
            +       mean = NA_real_,
            +       sd = NA_real_,
            +       min = NA_real_,
            +       p01 = NA_real_,
            +       p05 = NA_real_,
            +       p25 = NA_real_,
            +       median = NA_real_,
            +       p75 = NA_real_,
            +       p90 = NA_real_,
            +       p95 = NA_real_,
            +       p99 = NA_real_,
            +       max = NA_real_
            +     ))
          +   }
        + 
          +   q <- quantile(
            +     x,
            +     probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99),
            +     na.rm = TRUE,
            +     names = FALSE
            +   )
          + 
            +   data.table(
              +     variable = var,
              +     n = n_nonmissing,
              +     n_missing = sum(is.na(x)),
              +     mean = mean(x, na.rm = TRUE),
              +     sd = sd(x, na.rm = TRUE),
              +     min = min(x, na.rm = TRUE),
              +     p01 = q[1L],
              +     p05 = q[2L],
              +     p25 = q[3L],
              +     median = q[4L],
              +     p75 = q[5L],
              +     p90 = q[6L],
              +     p95 = q[7L],
              +     p99 = q[8L],
              +     max = max(x, na.rm = TRUE)
              +   )
          + }
> 
  > vars_to_summarise <- c(
    +   "lz_ywage", "lz_dwage",
    +   "lz_ywage_2000", "lz_dwage_2000",
    +   "lz_ywage_log", "lz_dwage_log",
    +   "implied_lz_days", "log_y_minus_log_d",
    +   "bg_dwage_log"
    + )
> 
  > # ----------------------------
> # 7) OVERALL AND GROUP SUMMARIES
  > # ----------------------------
> 
  > overall_summary <- rbindlist(
    +   lapply(vars_to_summarise, function(v) summarise_numeric(comparison_data, v))
    + )
> 
  > # Reuse one integer index per group rather than copying the entire comparison
  > # table into censored and uncensored data frames.
  > by_censor_summary <- rbindlist(lapply(
    +   list(Censored = 1, Uncensored = 0),
    +   function(group_code) {
      +     group_rows <- which(comparison_data$censored == group_code)
      +     group_name <- if (group_code == 1) "Censored" else "Uncensored"
      + 
        +     group_summary <- rbindlist(lapply(
          +       vars_to_summarise,
          +       function(v) summarise_numeric(comparison_data, v, group_rows)
          +     ))
        +     group_summary[, wage_group := group_name]
        +     setcolorder(group_summary, c("wage_group", "variable"))
        +     group_summary
        +   }
    + ), use.names = TRUE)
> 
  > by_year_summary <- comparison_data[, .(
    +   n = .N,
    +   mean_lz_ywage = mean(lz_ywage, na.rm = TRUE),
    +   mean_lz_dwage = mean(lz_dwage, na.rm = TRUE),
    +   mean_lz_ywage_log = mean(lz_ywage_log, na.rm = TRUE),
    +   mean_lz_dwage_log = mean(lz_dwage_log, na.rm = TRUE),
    +   median_implied_lz_days = median(implied_lz_days, na.rm = TRUE),
    +   mean_implied_lz_days = mean(implied_lz_days, na.rm = TRUE),
    +   cor_ywage_dwage = cor(lz_ywage, lz_dwage, use = "complete.obs"),
    +   cor_ylog_dlog = cor(lz_ywage_log, lz_dwage_log, use = "complete.obs")
    + ), by = contryear]
Processed 19 groups out of 19. 100% done. Time elapsed: 10s. ETA: 0s.
> setorder(by_year_summary, contryear)
> 
  > relationship_summary <- comparison_data[, .(
    +   n = .N,
    +   cor_ywage_dwage = cor(lz_ywage, lz_dwage, use = "complete.obs"),
    +   cor_ylog_dlog = cor(lz_ywage_log, lz_dwage_log, use = "complete.obs"),
    +   mean_implied_lz_days = mean(implied_lz_days, na.rm = TRUE),
    +   median_implied_lz_days = median(implied_lz_days, na.rm = TRUE),
    +   p05_implied_lz_days = quantile(
      +     implied_lz_days, 0.05, na.rm = TRUE, names = FALSE
      +   ),
    +   p90_implied_lz_days = quantile(
      +     implied_lz_days, 0.90, na.rm = TRUE, names = FALSE
      +   ),
    +   p95_implied_lz_days = quantile(
      +     implied_lz_days, 0.95, na.rm = TRUE, names = FALSE
      +   ),
    +   share_ywage_le_0 = mean(lz_ywage <= 0, na.rm = TRUE),
    +   share_dwage_le_0 = mean(lz_dwage <= 0, na.rm = TRUE),
    +   share_implied_days_lt_30 = mean(implied_lz_days < 30, na.rm = TRUE),
    +   share_implied_days_gt_366 = mean(implied_lz_days > 366, na.rm = TRUE)
    + )]
> 
  > # ----------------------------
> # 8) WRITE AGGREGATE OUTPUTS ONLY
  > # ----------------------------
> 
  > fwrite(
    +   overall_summary,
    +   file.path(output_dir, "lz_wage_summary_overall.csv"),
    +   na = "NA"
    + )
> fwrite(
  +   by_censor_summary,
  +   file.path(output_dir, "lz_wage_summary_by_censor.csv"),
  +   na = "NA"
  + )
> fwrite(
  +   by_year_summary,
  +   file.path(output_dir, "lz_wage_summary_by_year.csv"),
  +   na = "NA"
  + )
> fwrite(
  +   relationship_summary,
  +   file.path(output_dir, "lz_wage_relationship_summary.csv"),
  +   na = "NA"
  + )
> fwrite(
  +   sample_flow,
  +   file.path(output_dir, "lz_wage_sample_flow.csv"),
  +   na = "NA"
  + )
> 
  > cat("\n=== Relationship summary ===\n")

=== Relationship summary ===
  > print(relationship_summary)
n cor_ywage_dwage cor_ylog_dlog mean_implied_lz_days median_implied_lz_days p05_implied_lz_days p90_implied_lz_days
<int>           <num>         <num>                <num>                  <num>               <num>               <num>
  1: 50100141       0.2879206     0.8161763             316.5225                    365                  99                 366
p95_implied_lz_days share_ywage_le_0 share_dwage_le_0 share_implied_days_lt_30 share_implied_days_gt_366
<num>            <num>            <num>                    <num>                     <num>
  1:                 366     0.0003153484     0.0003153484              0.002217839               0.007585904
> cat("\nWrote full-sample aggregate diagnostics to:", output_dir, "\n")

Wrote full-sample aggregate diagnostics to: model_output_v5_temporal_full_sample_v4/lz_exploratory_analysis 
> memory_checkpoint("diagnostics complete")
[MEMORY] diagnostics complete                     RSS=10.0 GiB | budget=430 GiB
>
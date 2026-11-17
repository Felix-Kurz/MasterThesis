> # ==========================================
> # 01_build_pre_model_dataset_Version4.R
  > # Full-history pre-model dataset construction
  > # with consistent sample-specific paths + diagnostics
  > # ==========================================
> 
  > library(haven)
> library(dplyr)
> library(tidyr)
> library(readr)
> library(lubridate)
> library(purrr)
> library(data.table)
> library(stringr)
> 
  > options(dplyr.summarise.inform = FALSE)
> 
  > # ----------------------------
> # 0) CONFIG
  > # ----------------------------
> 
  > use_debug_sample <- FALSE
  > sample_n_penr <- 250000
  > sample_seed <- 123
  > 
    > # sample_suffix examples:
    > # ""        for default/full baseline naming
    > # "_10k"
    > # "_200k"
    > # "_250k"
    > # "_full_v4"
    > sample_suffix <- "_full_v4"
    > 
      > bg_prep_dir <- "bg_prep_full_v4"
      > pre_model_output_dir <- "pre_model_output_full_v4"
      > 
        > dir.create(bg_prep_dir, showWarnings = FALSE, recursive = TRUE)
      > dir.create(pre_model_output_dir, showWarnings = FALSE, recursive = TRUE)
      > 
        > qual_data_file <- file.path(pre_model_output_dir, paste0("qual_data_full_history", sample_suffix, ".RData"))
        > pre_model_file <- file.path(pre_model_output_dir, paste0("pre_model_full_history", sample_suffix, ".RData"))
        > 
          > # ----------------------------
        > # 1) LOAD INPUT DATA
          > # ----------------------------
        > 
          > load("qual_cat_step3_dta.RData")      # qual_cat_step3_dta
        > load("hauptverband_2022_dta.RData")   # hauptverband_2022
        > load("stammdaten_uniqe_2022.RData")   # stammdaten_uniqe_2022
        > load("dg_full_merge.RData")           # dg_full_merge
        > load("educ_imp.RData")                # educ_imp_data
        > load("qualifikationen_data.RData")    # qualifikationen_data
        > 
          > # ----------------------------
        > # 2) HELPERS
          > # ----------------------------
        > 
          > log_rows <- function(name, df) {
            +   cat(sprintf("%-40s %15s rows\n", paste0(name, ":"), format(nrow(df), big.mark = ",")))
            + }
          > 
            > log_distinct_penr <- function(name, df) {
              +   if ("penr" %in% names(df)) {
                +     cat(sprintf("%-40s %15s distinct penr\n",
                                  +                 paste0(name, ":"), format(dplyr::n_distinct(df$penr), big.mark = ",")))
                +   }
              + }
            > 
              > check_duplicates <- function(df, keys, label, top_n = 10) {
                +   dup <- df %>%
                  +     count(across(all_of(keys)), name = "n") %>%
                  +     filter(n > 1) %>%
                  +     arrange(desc(n))
                +   
                  +   cat("\n----------------------------------------\n")
                +   cat("Duplicate check:", label, "\n")
                +   cat("Keys:", paste(keys, collapse = ", "), "\n")
                +   cat("Duplicate key rows:", nrow(dup), "\n")
                +   if (nrow(dup) > 0) {
                  +     print(head(dup, top_n))
                  +   }
                +   cat("----------------------------------------\n")
                +   
                  +   invisible(dup)
                + }
              > 
                > safe_object_save <- function(object, file) {
                  +   save(list = object, file = file)
                  + }
                > 
                  > # ----------------------------
                > # 3) FULL-HISTORY INPUTS
                  > # ----------------------------
                > 
                  > hv_full <- hauptverband_2022
                  > qual_full <- qual_cat_step3_dta
                  > quali_full <- qualifikationen_data
                  > 
                    > log_rows("hv_full original", hv_full)
                  hv_full original:                            285,834,659 rows
                  > log_rows("qual_full original", qual_full)
                  qual_full original:                          143,650,596 rows
                  > log_rows("quali_full original", quali_full)
                  quali_full original:                         289,913,868 rows
                  > 
                    > if (use_debug_sample) {
                      +   set.seed(sample_seed)
                      +   
                        +   n_available_penr <- hv_full %>%
                          +     distinct(penr) %>%
                          +     nrow()
                        +   
                          +   n_to_sample <- min(sample_n_penr, n_available_penr)
                          +   
                            +   debug_penr <- hv_full %>%
                              +     distinct(penr) %>%
                              +     slice_sample(n = n_to_sample) %>%
                              +     pull(penr)
                            +   
                              +   hv_full <- hv_full %>% filter(penr %in% debug_penr)
                              +   qual_full <- qual_full %>% filter(penr %in% debug_penr)
                              +   quali_full <- quali_full %>% filter(penr %in% debug_penr)
                              +   
                                +   cat("\nDEBUG MODE ACTIVE:", length(debug_penr), "penr\n")
                              +   cat("Sample suffix:", sample_suffix, "\n")
                              +   cat("Distinct penr in hv_full after sampling:", dplyr::n_distinct(hv_full$penr), "\n\n")
                              + }else {
                                +   cat("\nFULL DATA MODE ACTIVE\n")
                                +   cat("Sample suffix:", sample_suffix, "\n\n")
                                + }
                  
                  FULL DATA MODE ACTIVE
                  Sample suffix: _full_v4 
                  
                  > 
                    > log_rows("hv_full sampled", hv_full)
                  hv_full sampled:                             285,834,659 rows
                  > log_distinct_penr("hv_full sampled", hv_full)
                  hv_full sampled:                              12,100,355 distinct penr
                  > log_rows("qual_full sampled", qual_full)
                  qual_full sampled:                           143,650,596 rows
                  > log_distinct_penr("qual_full sampled", qual_full)
                  qual_full sampled:                            12,749,343 distinct penr
                  > log_rows("quali_full sampled", quali_full)
                  quali_full sampled:                          289,913,868 rows
                  > log_distinct_penr("quali_full sampled", quali_full)
                  quali_full sampled:                           13,426,998 distinct penr
                  > 
                    > t_step3_to_end <- Sys.time()
                  > 
                    > # ----------------------------
                  > # 4) QUAL SPELL PREP
                    > # ----------------------------
                  > 
                    > qual_data <- qual_full %>%
                    +   select(penr, benr, vdat, bdat, subcat) %>%
                    +   rename(start = vdat, end = bdat) %>%
                    +   mutate(
                      +     dur = end - start + 1,
                      +     year_end = year(end),
                      +     year_start = year(start)
                      +   ) %>%
                    +   filter(!(subcat == 0 | (subcat > 16 & subcat < 1112)))
                  > 
                    > log_rows("qual_data after initial filter", qual_data)
                  qual_data after initial filter:               90,356,739 rows
                  > 
                    > qual_data <- qual_data %>%
                    +   mutate(years = year_end - year_start + 1) %>%
                    +   group_by(penr) %>%
                    +   mutate(spellindex = row_number()) %>%
                    +   uncount(years, .id = "year_seq") %>%
                    +   group_by(penr, spellindex) %>%
                    +   mutate(
                      +     contryear = year_start + year_seq - 1,
                      +     end = ifelse(
                        +       year_seq != max(year_seq),
                        +       as.Date(paste0(contryear + 1, "-01-01")) - 1,
                        +       end
                        +     ),
                      +     start = ifelse(
                        +       year_seq > 1,
                        +       as.Date(paste0(contryear, "-01-01")),
                        +       start
                        +     )
                      +   ) %>%
                    +   select(-spellindex, -year_end, -year_start, -year_seq) %>%
                    +   ungroup()
                  Adding missing grouping variables: `spellindex`
                  > 
                    > log_rows("qual_data after year expansion", qual_data)
                  qual_data after year expansion:              235,341,343 rows
                  > 
                    > qual_data <- qual_data %>%
                    +   mutate(dur_year = end - start + 1) %>%
                    +   group_by(penr, benr, contryear) %>%
                    +   arrange(start, .by_group = TRUE) %>%
                    +   mutate(
                      +     totdur = sum(dur_year),
                      +     durfrac = dur_year / totdur,
                      +     end2 = end,
                      +     end2 = ifelse(!is.na(lead(start)) & end2 >= lead(start), lead(start) - 1, end2),
                      +     dur2 = end2 - start + 1,
                      +     dur_nooverlap = sum(dur2, na.rm = TRUE)
                      +   ) %>%
                    +   select(-end2, -dur2, -totdur) %>%
                    +   ungroup()
                  > 
                    > log_rows("qual_data final", qual_data)
                  qual_data final:                             235,341,343 rows
                  > check_duplicates(qual_data, c("penr", "benr", "contryear"), "qual_data key duplicates")
                  
                  ----------------------------------------
                    Duplicate check: qual_data key duplicates 
                  Keys: penr, benr, contryear 
                  Duplicate key rows: 14975070 
                  # A tibble: 10 × 4
                  penr          benr contryear     n
                  <dbl>         <dbl>     <dbl> <int>
                    1  845890 1210028312000      1972    27
                  2 6139060 1511407013000      1974    27
                  3 6139060 1511407013000      1975    27
                  4 6270608 1414030191100      1999    27
                  5  359368 1500105080000      1983    26
                  6  359368 1500105080000      1984    26
                  7  759361 1210028312000      1972    26
                  8  759361 1210028312000      1973    26
                  9  825792 1500105080000      1991    26
                  10  845890 1210028312000      1973    26
                  ----------------------------------------
                    > 
                    > save(qual_data, file = qual_data_file)
                  > cat("Saved qual_data to:", qual_data_file, "\n")
                  Saved qual_data to: pre_model_output_full_v4/qual_data_full_history_full_v4.RData 
                  > 
                    > # ----------------------------
                  > # 5) BG PREP BY YEAR
                    > # ----------------------------
                  > 
                    > topcoding <- c(
                      +   632, 686, 763, 850, 959, 1090, 1220, 1351, 1417, 1482, 1569, 1656,
                      +   1744, 1787, 1874, 1918, 2005, 2049, 2092, 2180, 2311, 2441, 2616,
                      +   2747, 2834, 2965, 3052, 3095, 3139, 3226, 3270, 3360, 3450, 3630,
                      +   3750, 3840, 3930, 4020, 4110, 4200, 4230, 4440, 4530, 4650, 4860,
                      +   4980, 5130, 5220, 5370
                      + )
                  > 
                    > min_year <- max(min(hv_full$contryear, na.rm = TRUE), 1972)
                  > max_year <- min(max(hv_full$contryear, na.rm = TRUE), 2020)
                  > 
                    > cat("\nBG prep year range:", min_year, "to", max_year, "\n")
                  
                  BG prep year range: 1972 to 2020 
                  > 
                    > saved_bg_years <- integer()
                  > 
                    > for (year in min_year:max_year) {
                      +   cat("Processing BG prep year:", year, "\n")
                      +   
                        +   year_data <- qual_data %>% filter(contryear == year)
                        +   bg_data <- hv_full %>%
                          +     filter(contryear == year) %>%
                          +     select(benr, penr, bg_euro, sz_euro, duration, dailywage)
                        +   
                          +   if (nrow(bg_data) > 0 && nrow(year_data) > 0) {
                            +     year_data <- year_data %>%
                              +       left_join(bg_data, by = c("benr", "penr")) %>%
                              +       mutate(
                                +         duration = ifelse(is.na(duration) | duration == 0, dur_nooverlap, duration),
                                +         wage_bg = round((bg_euro + sz_euro) * durfrac),
                                +         censored = 0
                                +       ) %>%
                              +       filter(!is.na(bg_euro), !is.na(sz_euro))
                            +     
                              +     if (year >= 1972 && year <= 2020) {
                                +       topcode_threshold <- topcoding[year - 1971]
                                +       
                                  +       year_data <- year_data %>%
                                    +         mutate(
                                      +           censored = ifelse((bg_euro + sz_euro) * 30 / duration >= topcode_threshold * 7/6 & !is.na(wage_bg), 1, censored),
                                      +           censored = ifelse((bg_euro + sz_euro) * 30 / dur_nooverlap >= topcode_threshold * 7/6 & !is.na(wage_bg), 1, censored),
                                      +           censored = ifelse((bg_euro + sz_euro) >= topcode_threshold * 14, 1, censored),
                                      +           censored = ifelse(bg_euro >= topcode_threshold * 12, 1, censored),
                                      +           wage_bg = ifelse((bg_euro + sz_euro) > topcode_threshold * 14, topcode_threshold * 14, wage_bg)
                                      +         )
                                  +     }
                            +     
                              +     year_data <- year_data %>%
                                +       mutate(dailywage = (bg_euro + sz_euro) / duration) %>%
                                +       group_by(contryear, censored) %>%
                                +       mutate(dailywage_mode = as.numeric(names(sort(table(dailywage), decreasing = TRUE))[1])) %>%
                                +       ungroup() %>%
                                +       mutate(dailywage = ifelse(censored == 1, dailywage_mode, dailywage))
                              +     
                                +     max_dailywage <- suppressWarnings(
                                  +       year_data %>% filter(censored == 1) %>% pull(dailywage_mode) %>% max(na.rm = TRUE)
                                  +     )
                                +     
                                  +     if (is.finite(max_dailywage)) {
                                    +       year_data <- year_data %>%
                                      +         mutate(
                                        +           censored = ifelse(censored == 0 & dailywage >= max_dailywage, 1, censored),
                                        +           dailywage = ifelse(dailywage >= max_dailywage, max_dailywage, dailywage)
                                        +         )
                                    +     }
                                +     
                                  +     year_data <- year_data %>%
                                    +       select(-dailywage_mode, -bg_euro, -sz_euro, -durfrac, -dur_nooverlap, -duration) %>%
                                    +       rename(duration = dur_year) %>%
                                    +       group_by(penr, benr, contryear) %>%
                                    +       arrange(desc(duration), .by_group = TRUE) %>%
                                    +       filter(dplyr::row_number() == 1) %>%
                                    +       ungroup()
                                  +     
                                    +     out_file <- file.path(bg_prep_dir, paste0("bg_prep", sample_suffix, "_", year, ".RData"))
                                    +     save(year_data, file = out_file)
                                    +     saved_bg_years <- c(saved_bg_years, year)
                                    +   }
                        + }
                  Processing BG prep year: 1972 
                  Processing BG prep year: 1973 
                  Processing BG prep year: 1974 
                  Processing BG prep year: 1975 
                  Processing BG prep year: 1976 
                  Processing BG prep year: 1977 
                  Processing BG prep year: 1978 
                  Processing BG prep year: 1979 
                  Processing BG prep year: 1980 
                  Processing BG prep year: 1981 
                  Processing BG prep year: 1982 
                  Processing BG prep year: 1983 
                  Processing BG prep year: 1984 
                  Processing BG prep year: 1985 
                  Processing BG prep year: 1986 
                  Processing BG prep year: 1987 
                  Processing BG prep year: 1988 
                  Processing BG prep year: 1989 
                  Processing BG prep year: 1990 
                  Processing BG prep year: 1991 
                  Processing BG prep year: 1992 
                  Processing BG prep year: 1993 
                  Processing BG prep year: 1994 
                  Processing BG prep year: 1995 
                  Processing BG prep year: 1996 
                  Processing BG prep year: 1997 
                  Processing BG prep year: 1998 
                  Processing BG prep year: 1999 
                  Processing BG prep year: 2000 
                  Processing BG prep year: 2001 
                  Processing BG prep year: 2002 
                  Processing BG prep year: 2003 
                  Processing BG prep year: 2004 
                  Processing BG prep year: 2005 
                  Processing BG prep year: 2006 
                  Processing BG prep year: 2007 
                  Processing BG prep year: 2008 
                  Processing BG prep year: 2009 
                  Processing BG prep year: 2010 
                  Processing BG prep year: 2011 
                  Processing BG prep year: 2012 
                  Processing BG prep year: 2013 
                  Processing BG prep year: 2014 
                  Processing BG prep year: 2015 
                  Processing BG prep year: 2016 
                  Processing BG prep year: 2017 
                  Processing BG prep year: 2018 
                  Processing BG prep year: 2019 
                  Processing BG prep year: 2020 
                  > 
                    > cat("Saved BG prep files for", length(saved_bg_years), "years\n")
                  Saved BG prep files for 49 years
                  > if (length(saved_bg_years) == 0) {
                    +   stop("No BG prep files were created. Check the BG prep loop before continuing.")
                    + }
                  > 
                    > # ----------------------------
                  > # 6) EMPLOYMENT PANEL
                    > # ----------------------------
                  > 
                    > employment_data <- qual_full %>%
                    +   select(penr, benr, vdat, bdat, subcat) %>%
                    +   filter(
                      +     subcat %in% c(11, 12, 14) |
                        +       (subcat == 21 & (bdat - vdat) >= 27) |
                        +       (subcat == 22 & (bdat - vdat) >= 27)
                      +   ) %>%
                    +   rename(start = vdat, end = bdat) %>%
                    +   mutate(
                      +     dur = end - start,
                      +     year_end = year(end),
                      +     year_start = year(start)
                      +   )
                  > 
                    > log_rows("employment_data initial", employment_data)
                  employment_data initial:                     103,968,035 rows
                  > 
                    > employment_data <- employment_data %>%
                    +   group_by(penr) %>%
                    +   mutate(spellindex = row_number()) %>%
                    +   mutate(years = ifelse(subcat %in% c(21, 22), 1, year_end - year_start + 1)) %>%
                    +   uncount(years, .id = "year_seq") %>%
                    +   group_by(penr, spellindex) %>%
                    +   mutate(
                      +     contryear = ifelse(!(subcat %in% c(21, 22)), year_start + year_seq - 1, year_start),
                      +     start = ifelse(year_seq > 1 & !(subcat %in% c(21, 22)), as.Date(paste0(contryear, "-01-01")), start),
                      +     end = ifelse(year_seq != max(year_seq) & !(subcat %in% c(21, 22)), as.Date(paste0(contryear + 1, "-01-01")) - 1, end)
                      +   ) %>%
                    +   select(-dur, -spellindex, -year_seq) %>%
                    +   ungroup()
                  Adding missing grouping variables: `spellindex`
                  > 
                    > log_rows("employment_data after expansion", employment_data)
                  employment_data after expansion:             226,624,922 rows
                  > 
                    > employment_data <- employment_data %>%
                    +   mutate(appren = ifelse(subcat == 14, 1, 0)) %>%
                    +   group_by(penr, benr, contryear) %>%
                    +   mutate(appren_max = max(appren)) %>%
                    +   filter(appren_max != 1) %>%
                    +   select(-appren, -appren_max) %>%
                    +   ungroup()
                  > 
                    > log_rows("employment_data after apprenticeship removal", employment_data)
                  employment_data after apprenticeship removal:     214,921,056 rows
                  > 
                    > employment_data <- employment_data %>%
                    +   filter(!(subcat %in% c(11, 12)) | (end - start > 29)) %>%
                    +   arrange(penr, start) %>%
                    +   group_by(penr) %>%
                    +   mutate(
                      +     job_unemp = ifelse(lead(subcat) %in% c(21, 22) & lead(end) > end, 1, 0),
                      +     unemp_job = ifelse(lag(subcat) %in% c(21, 22), 1, 0)
                      +   ) %>%
                    +   ungroup() %>%
                    +   replace_na(list(job_unemp = 0, unemp_job = 0)) %>%
                    +   filter(!(subcat %in% c(21, 22, 14))) %>%
                    +   mutate(dur = end - start + 1) %>%
                    +   group_by(penr, benr, contryear) %>%
                    +   summarise(
                      +     totdur = sum(dur),
                      +     minstart = min(start),
                      +     maxend = max(end),
                      +     subcat = first(subcat),
                      +     job_unemp = first(job_unemp),
                      +     unemp_job = first(unemp_job),
                      +     .groups = "drop"
                      +   ) %>%
                    +   mutate(start = minstart, end = maxend, dur = totdur) %>%
                    +   select(-minstart, -maxend, -totdur)
                  > 
                    > log_rows("employment_data final", employment_data)
                  employment_data final:                       158,266,535 rows
                  > check_duplicates(employment_data, c("penr", "benr", "contryear"), "employment_data key duplicates")
                  
                  ----------------------------------------
                    Duplicate check: employment_data key duplicates 
                  Keys: penr, benr, contryear 
                  Duplicate key rows: 0 
                  ----------------------------------------
                    > 
                    > # ----------------------------
                  > # 7) PERSON-LEVEL COVARIATES
                    > # ----------------------------
                  > 
                    > eu_stsl_codes_strict_wo_A_G <- c(3,5,13,14,15,16,17,18,19,30,32,34,35,37,40,41,42,43,44,46,47,48,51,52)
                  > 
                    > stamm_data <- stammdaten_uniqe_2022 %>%
                    +   select(penr, birthyear, frau, stsl) %>%
                    +   left_join(
                      +     educ_imp_data %>%
                        +       select(penr, educ, imputed) %>%
                        +       distinct(penr, .keep_all = TRUE),
                      +     by = "penr"
                      +   ) %>%
                    +   mutate(
                      +     stsl_missing = ifelse(is.na(stsl) | stsl == 999, 1L, 0L),
                      +     austrian_dummy = ifelse(!is.na(stsl) & stsl == 1, 1L, 0L),
                      +     germany_dummy = ifelse(!is.na(stsl) & stsl %in% c(2, 12), 1L, 0L),
                      +     eu_dummy = ifelse(!is.na(stsl) & stsl %in% eu_stsl_codes_strict_wo_A_G, 1L, 0L),
                      +     educ_missing = ifelse(is.na(educ), 1L, 0L),
                      +     educ_cat = case_when(
                        +       is.na(educ) ~ 0L,
                        +       TRUE ~ as.integer(educ)
                        +     ),
                      +     educ_imputed = ifelse(is.na(imputed), 0L, as.integer(imputed))
                      +   )
                  > 
                    > employment_data <- employment_data %>%
                    +   left_join(stamm_data, by = "penr") %>%
                    +   mutate(age = contryear - birthyear) %>%
                    +   filter(!is.na(age), !is.na(frau), age >= 20, age <= 60)
                  > 
                    > log_rows("employment_data after person covariates", employment_data)
                  employment_data after person covariates:     149,454,042 rows
                  > 
                    > # ----------------------------
                  > # 8) MERGE YEARLY BG PREP OUTPUTS
                    > # ----------------------------
                  > 
                    > processed_wage_files <- list()
                  > 
                    > for (year in min_year:max_year) {
                      +   file_path <- file.path(bg_prep_dir, paste0("bg_prep", sample_suffix, "_", year, ".RData"))
                      +   if (file.exists(file_path)) {
                        +     load(file_path)
                        +     processed_wage_files[[as.character(year)]] <- year_data %>%
                          +       select(penr, benr, contryear, wage_bg, duration, censored)
                        +   }
                      + }
                  > 
                    > all_processed_wages <- bind_rows(processed_wage_files)
                  > 
                    > log_rows("all_processed_wages", all_processed_wages)
                  all_processed_wages:                         190,582,906 rows
                  > check_duplicates(all_processed_wages, c("penr", "benr", "contryear"), "all_processed_wages key duplicates")
                  
                  ----------------------------------------
                    Duplicate check: all_processed_wages key duplicates 
                  Keys: penr, benr, contryear 
                  Duplicate key rows: 0 
                  ----------------------------------------
                    > 
                    > full_data <- employment_data %>%
                    +   left_join(all_processed_wages, by = c("penr", "benr", "contryear")) %>%
                    +   mutate(dailywage_final = wage_bg / duration) %>%
                    +   filter(!is.na(dailywage_final), !is.na(censored))
                  > 
                    > log_rows("full_data after wage join", full_data)
                  full_data after wage join:                   145,556,412 rows
                  > 
                    > full_data <- full_data %>%
                    +   arrange(penr, start) %>%
                    +   group_by(penr) %>%
                    +   mutate(
                      +     cumulative_days = cumsum(end - start + 1),
                      +     stable_career = cumulative_days >= 90
                      +   ) %>%
                    +   filter(stable_career) %>%
                    +   select(-cumulative_days, -stable_career) %>%
                    +   ungroup()
                  > 
                    > log_rows("full_data after stable career filter", full_data)
                  full_data after stable career filter:        142,964,357 rows
                  > 
                    > full_data <- full_data %>%
                    +   group_by(penr, contryear) %>%
                    +   arrange(desc(wage_bg), .by_group = TRUE) %>%
                    +   filter(dplyr::row_number() == 1) %>%
                    +   ungroup()
                  > 
                    > log_rows("full_data after top job per person-year", full_data)
                  full_data after top job per person-year:     127,031,172 rows
                  > 
                    > # ----------------------------
                  > # 9) FEATURES
                    > # ----------------------------
                  > 
                    > full_data <- full_data %>%
                    +   group_by(penr) %>%
                    +   arrange(contryear, .by_group = TRUE) %>%
                    +   mutate(
                      +     juj = ifelse(job_unemp == 1 | lead(unemp_job) == 1, 1, 0),
                      +     juj = replace_na(juj, 0)
                      +   ) %>%
                    +   mutate(
                      +     age_entry = first(contryear) - first(birthyear),
                      +     exper = row_number() - 1,
                      +     cohort = first(contryear) - 1994,
                      +     year_entry = first(contryear),
                      +     return = ifelse(contryear > lag(contryear) + 1, 1, 0),
                      +     return = replace_na(return, 0)
                      +   ) %>%
                    +   ungroup() %>%
                    +   mutate(
                      +     exper = pmin(exper, 60),
                      +     experdummy = ifelse(birthyear < 1957, 1, 0)
                      +   )
                  > 
                    > full_data <- full_data %>%
                    +   arrange(penr, benr, contryear) %>%
                    +   group_by(penr, benr) %>%
                    +   mutate(
                      +     matchid = cur_group_id(),
                      +     tenure = row_number()
                      +   ) %>%
                    +   ungroup() %>%
                    +   mutate(
                      +     tenure = pmin(tenure, 60),
                      +     tenuredummy = ifelse(contryear == 1994, 1, 0),
                      +     dailywage_final = pmax(dailywage_final, 1)
                      +   )
                  > 
                    > log_rows("full_data after career features", full_data)
                  full_data after career features:             127,031,172 rows
                  > 
                    > # ----------------------------
                  > # 10) FIRM DATA JOIN
                    > # ----------------------------
                  > 
                    > firm_data_raw <- dg_full_merge %>%
                    +   select(benr, contryear, bundesland, nace08_2)
                  > 
                    > log_rows("firm_data_raw", firm_data_raw)
                  firm_data_raw:                               758,000,768 rows
                  > check_duplicates(firm_data_raw, c("benr", "contryear"), "firm_data_raw key duplicates")
                  
                  ----------------------------------------
                    Duplicate check: firm_data_raw key duplicates 
                  Keys: benr, contryear 
                  Duplicate key rows: 107013271 
                  # A tibble: 10 × 3
                  benr contryear     n
                  <dbl>     <dbl> <int>
                    1 1211621500600      1978   168
                  2 1211621500600      1979   168
                  3 1211621500600      1980   168
                  4 1211621500600      1981   168
                  5 1211621500600      1982   168
                  6 1211621500600      1983   168
                  7 1211621500600      1984   168
                  8 1211621500600      1985   168
                  9 1211621500600      1986   168
                  10 1211621500600      1987   168
                  ----------------------------------------
                    > 
                    > firm_data <- firm_data_raw %>%
                    +   distinct(benr, contryear, .keep_all = TRUE)
                  > 
                    > log_rows("firm_data deduplicated", firm_data)
                  firm_data deduplicated:                      242,029,494 rows
                  > check_duplicates(firm_data, c("benr", "contryear"), "firm_data deduplicated key duplicates")
                  
                  ----------------------------------------
                    Duplicate check: firm_data deduplicated key duplicates 
                  Keys: benr, contryear 
                  Duplicate key rows: 0 
                  ----------------------------------------
                    > 
                    > full_data <- full_data %>%
                    +   left_join(firm_data, by = c("benr", "contryear")) %>%
                    +   filter(!is.na(nace08_2))
                  > 
                    > log_rows("full_data after firm join", full_data)
                  full_data after firm join:                   112,238,803 rows
                  > 
                    > full_data <- full_data %>%
                    +   mutate(
                      +     whitecollar_dummy = ifelse(subcat == 11, 1L, 0L),
                      +     bluecollar_dummy  = ifelse(subcat == 12, 1L, 0L),
                      +     othercollar_dummy = ifelse(!(subcat %in% c(11, 12)) & !is.na(subcat), 1L, 0L)
                      +   )
                  > 
                    > full_data <- full_data %>%
                    +   mutate(
                      +     subcat = ifelse(subcat %in% c(12, 14), 1, 0),
                      +     subcat = ifelse(subcat == 11, 0, subcat)
                      +   ) %>%
                    +   group_by(penr) %>%
                    +   arrange(contryear, .by_group = TRUE) %>%
                    +   mutate(
                      +     benr_lagged = lag(benr),
                      +     benr_lagged = ifelse(is.na(benr_lagged), -9, benr_lagged),
                      +     benr_lagged = ifelse(benr_lagged == benr, -9, benr_lagged)
                      +   ) %>%
                    +   ungroup()
                  > 
                    > # ----------------------------
                  > # 11) LIVE BIRTH / CHILDREN
                    > # ----------------------------
                  > 
                    > birth_events <- quali_full %>%
                    +   filter(qu_qual == "49") %>%
                    +   transmute(
                      +     penr,
                      +     contryear = lubridate::year(qu_vdat)
                      +   ) %>%
                    +   filter(!is.na(contryear)) %>%
                    +   distinct(penr, contryear) %>%
                    +   mutate(live_birth_event = 1L)
                  > 
                    > full_data <- full_data %>%
                    +   left_join(birth_events, by = c("penr", "contryear")) %>%
                    +   mutate(
                      +     live_birth_event = ifelse(is.na(live_birth_event), 0L, live_birth_event)
                      +   ) %>%
                    +   arrange(penr, contryear) %>%
                    +   group_by(penr) %>%
                    +   mutate(
                      +     n_children = cumsum(live_birth_event),
                      +     n_children = ifelse(frau == 1, n_children, 0L)
                      +   ) %>%
                    +   ungroup()
                  > 
                    > # ----------------------------
                  > # 12) CPI ADJUSTMENT
                    > # ----------------------------
                  > 
                    > vpi_1986 <- data.frame(
                      +   contryear = 1986:2024,
                      +   year_inflation = c(
                        +     0, 1.4, 2, 2.5, 3.3, 3.3, 4.1, 3.6, 3.0, 2.2, 1.9, 1.3, 0.9, 0.6,
                        +     2.3, 2.7, 1.8, 1.3, 2.1, 2.3, 1.5, 2.2, 3.2, 0.5, 1.9, 3.3, 2.4,
                        +     2.0, 1.7, 0.9, 0.9, 2.1, 2.0, 1.5, 1.4, 2.8, 8.6, 7.8, 2.9
                        +   )
                      + )
                  > 
                    > vpi_1986 <- vpi_1986 %>%
                    +   mutate(
                      +     vpi = year_inflation / 100 + 1,
                      +     reverse_index = cumprod(vpi),
                      +     cpi_2000 = reverse_index[contryear == 2000] / reverse_index
                      +   )
                  > 
                    > full_data <- full_data %>%
                    +   left_join(vpi_1986 %>% select(contryear, cpi_2000), by = "contryear") %>%
                    +   mutate(
                      +     dailywage_final = dailywage_final * cpi_2000,
                      +     dailywage_final = log(dailywage_final)
                      +   ) %>%
                    +   select(-cpi_2000)
                  > 
                    > # ----------------------------
                  > # 13) FINAL SELECTION
                    > # ----------------------------
                  > 
                    > pre_model_full_history <- full_data %>%
                    +   select(
                      +     penr, benr, contryear, dailywage_final, benr_lagged, tenure, birthyear,
                      +     age, exper, experdummy, frau, stsl, stsl_missing, austrian_dummy, germany_dummy, eu_dummy,
                      +     educ_cat, educ_missing, educ_imputed,
                      +     whitecollar_dummy, bluecollar_dummy, othercollar_dummy,
                      +     live_birth_event, n_children,
                      +     subcat, age_entry, year_entry, juj, censored, cohort, bundesland, nace08_2, return
                      +   )
                  > 
                    > log_rows("pre_model_full_history final", pre_model_full_history)
                  pre_model_full_history final:                112,238,803 rows
                  > log_distinct_penr("pre_model_full_history final", pre_model_full_history)
                  pre_model_full_history final:                  8,220,895 distinct penr
                  > check_duplicates(pre_model_full_history, c("penr", "benr", "contryear"), "pre_model_full_history key duplicates")
                  
                  ----------------------------------------
                    Duplicate check: pre_model_full_history key duplicates 
                  Keys: penr, benr, contryear 
                  Duplicate key rows: 0 
                  ----------------------------------------
                    > 
                    > save(pre_model_full_history, file = pre_model_file)
                  > 
                    > cat("\nDone.\n")
                  
                  Done.
                  > cat("Saved pre_model_full_history to:", pre_model_file, "\n")
                  Saved pre_model_full_history to: pre_model_output_full_v4/pre_model_full_history_full_v4.RData 
                  > cat("Rows pre_model_full_history:", nrow(pre_model_full_history), "\n")
                  Rows pre_model_full_history: 112238803 
                  > cat("Year range:", min(pre_model_full_history$contryear, na.rm = TRUE), "to",
                        +     max(pre_model_full_history$contryear, na.rm = TRUE), "\n")
                  Year range: 1978 to 2020 
                  > 
                    > elapsed <- Sys.time() - t_step3_to_end
                  > cat("\n=== Runtime Step 3 -> End ===\n")
                  
                  === Runtime Step 3 -> End ===
                    > print(elapsed)
                  Time difference of 4.697172 days
                  > cat("Seconds:", round(as.numeric(elapsed, units = "secs"), 2), "\n")
                  Seconds: 405835.7 
                  > cat("Minutes:", round(as.numeric(elapsed, units = "mins"), 2), "\n")
                  Minutes: 6763.93 
                  > cat("Hours:", round(as.numeric(elapsed, units = "hours"), 2), "\n")
                  Hours: 112.73 
                  >
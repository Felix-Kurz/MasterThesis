
R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
Copyright (C) 2020 The R Foundation for Statistical Computing
Platform: x86_64-pc-linux-gnu (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> load("qualifikationen_data.RData")
> sum(qualifikationen_data$qu_qual == "49", na.rm = TRUE)
[1] 3098655
> summary(qualifikationen_data$qu_vdat)
Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
"1972-01-01" "1990-12-19" "2002-09-09" "1999-09-20" "2010-05-25" "2016-12-31" 
> str(qualifikationen_data)
tibble [289,913,868 × 7] (S3: tbl_df/tbl/data.frame)
$ penr     : num [1:289913868] 1 1 1 1 1 2 2 2 2 2 ...
..- attr(*, "label")= chr "(ID) Person"
..- attr(*, "format.stata")= chr "%8.0f"
$ benr     : num [1:289913868] 1.20e+12 1.72e+12 1.72e+12 1.72e+12 1.72e+12 ...
..- attr(*, "label")= chr "(ID) Dienstgeber"
..- attr(*, "format.stata")= chr "%13.0f"
$ qu_vdat  : Date[1:289913868], format: "1975-06-02" "1972-01-01" "1973-04-28" "1973-05-02" ...
$ qu_bdat  : Date[1:289913868], format: "2003-02-10" "1973-04-27" "1973-05-01" "1975-06-30" ...
$ qu_qual  : chr [1:289913868] "09" "10" "36" "10" ...
..- attr(*, "label")= chr "(QU) Qualifikation"
..- attr(*, "format.stata")= chr "%2s"
$ qu_cat   : 'haven_labelled' num [1:289913868] 3 1 14 1 10 2 3 10 1 14 ...
..- attr(*, "label")= chr "(QU) Kategorie"
..- attr(*, "format.stata")= chr "%40.0g"
..- attr(*, "labels")= Named num [1:12] 1 2 3 4 5 6 7 8 10 12 ...
.. ..- attr(*, "names")= chr [1:12] "employed (01)" "unemployed (02)" "retired (03)" "maternity benefit, parental leave (04)" ...
$ qu_subcat: 'haven_labelled' num [1:289913868] 31 12 141 12 0 21 33 0 12 141 ...
..- attr(*, "label")= chr "(QU) Unterkategorie"
..- attr(*, "format.stata")= chr "%39.0g"
..- attr(*, "labels")= Named num [1:18] 0 11 12 13 14 15 16 21 22 23 ...
.. ..- attr(*, "names")= chr [1:18] "no subcat (000)" "white collar (011)" "blue collar (012)" "civil servant (013)" ...
- attr(*, "label")= chr "(QU) Qualifikationen"
> =========================
  Error: unexpected '==' in "=="
> 
  > library(haven)
> library(dplyr)

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:
  
  filter, lag

The following objects are masked from ‘package:base’:
  
  intersect, setdiff, setequal, union

> library(tidyr)
> library(readr)
> library(lubridate)

Attaching package: ‘lubridate’

The following objects are masked from ‘package:base’:
  
  date, intersect, setdiff, union

> library(purrr)
> library(data.table)
data.table 1.17.0 using 4 threads (see ?getDTthreads).  Latest news: r-datatable.com

Attaching package: ‘data.table’

The following object is masked from ‘package:purrr’:
  
  transpose

The following objects are masked from ‘package:lubridate’:
  
  hour, isoweek, mday, minute, month, quarter, second, wday, week, yday, year

The following objects are masked from ‘package:dplyr’:
  
  between, first, last

> library(stringr)
> 
  > # ----------------------------
> # 0) OUTPUT PATHS
  > # ----------------------------
> dir.create("bg_prep", showWarnings = FALSE, recursive = TRUE)
> dir.create("pre_model_output", showWarnings = FALSE, recursive = TRUE)
> load("qual_cat_step3_dta.RData")      # qual_cat_step3_dta
> load("hauptverband_2022_dta.RData")   # hauptverband_2022
> load("stammdaten_uniqe_2022.RData")   # stammdaten_uniqe_2022
> load("dg_full_merge.RData")           # dg_full_merge
> load("educ_imp.RData")
> load("qualifikationen_data.RData")
> # ----------------------------
> # 2) FULL-HISTORY INPUTS (no sampling)
  > # ----------------------------
> hv_full <- hauptverband_2022
> qual_full <- qual_cat_step3_dta
> # --- TEMP DEBUG SAMPLE: keep only 1000 penr (comment out after test) ---
  > set.seed(123)
> debug_penr <- hv_full %>%
  +   distinct(penr) %>%
  +   slice_sample(n = 1000) %>%
  +   pull(penr)
> 
  > hv_full <- hv_full %>% filter(penr %in% debug_penr)
> qual_full <- qual_full %>% filter(penr %in% debug_penr)
> qualifikationen_data <- qualifikationen_data %>% filter(penr %in% debug_penr)
> 
  > cat("DEBUG MODE ACTIVE: using", length(debug_penr), "penr\n")
DEBUG MODE ACTIVE: using 1000 penr
> # --- END TEMP DEBUG SAMPLE ---
  > 
  > # --- TIMER: start (place directly before STEP 3) ---
  > t_step3_to_end <- Sys.time()
> 
  > 
  > 
  > # ----------------------------
> # 3) QUAL SPELL PREP (full history)
  > # ----------------------------
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
  > qual_data <- qual_data %>%
  +   mutate(years = year_end - year_start + 1) %>%
  +   group_by(penr) %>%
  +   mutate(spellindex = row_number()) %>%
  +   uncount(years, .id = "year_seq") %>%
  +   group_by(penr, spellindex) %>%
  +   mutate(
    +     contryear = year_start + year_seq - 1,
    +     end = ifelse(year_seq != max(year_seq),
                       +                  as.Date(paste0(contryear + 1, "-01-01")) - 1,
                       +                  end),
    +     start = ifelse(year_seq > 1,
                         +                    as.Date(paste0(contryear, "-01-01")) - 1,
                         +                    start)
    +   ) %>%
  +   select(-spellindex, -year_end, -year_start, -year_seq) %>%
  +   ungroup()
Adding missing grouping variables: `spellindex`
> 
  > qual_data <- qual_data %>%
  +   mutate(dur_year = end - start + 1) %>%
  +   group_by(penr, benr, contryear) %>%
  +   arrange(start) %>%
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
  > save(qual_data, file = "pre_model_output/qual_data_full_history.RData")
> 
  > # ----------------------------
> # 4) BG PREP BY YEAR (full available range)
  > # ----------------------------
> topcoding <- c(632, 686, 763, 850, 959, 1090, 1220, 1351, 1417, 1482, 1569, 1656,
                 +                1744, 1787, 1874, 1918, 2005, 2049, 2092, 2180, 2311, 2441, 2616,
                 +                2747, 2834, 2965, 3052, 3095, 3139, 3226, 3270, 3360, 3450, 3630,
                 +                3750, 3840, 3930, 4020, 4110, 4200, 4230, 4440, 4530, 4650, 4860,
                 +                4980, 5130, 5220, 5370)
> 
  > min_year <- max(min(hv_full$contryear, na.rm = TRUE), 1972)
> max_year <- min(max(hv_full$contryear, na.rm = TRUE), 2020)
> 
  > for (year in min_year:max_year) {
    +   cat("Processing BG prep year:", year, "\n")
    + 
      +   year_data <- qual_data %>% filter(contryear == year)
      +   bg_data <- hv_full %>%
        +     filter(contryear == year) %>%
        +     select(benr, penr, bg_euro, sz_euro, duration, dailywage)
      + 
        +   if (NROW(bg_data) > 0 && NROW(year_data) > 0) {
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
                  +       arrange(desc(duration)) %>%
                  +       slice(1) %>%
                  +       ungroup()
                + 
                  +     save(year_data, file = paste0("bg_prep/bg_prep_", year, ".RData"))
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
  > # ----------------------------
> # 5) EMPLOYMENT PANEL (full history)
  > # ----------------------------
> employment_data <- qual_full %>%
  +   select(penr, benr, vdat, bdat, subcat) %>%
  +   filter(subcat %in% c(11, 12, 14) |
               +            (subcat == 21 & (bdat - vdat) >= 27) |
               +            (subcat == 22 & (bdat - vdat) >= 27)) %>%
  +   rename(start = vdat, end = bdat) %>%
  +   mutate(
    +     dur = end - start,
    +     year_end = year(end),
    +     year_start = year(start)
    +   )
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
  > employment_data <- employment_data %>%
  +   mutate(appren = ifelse(subcat == 14, 1, 0)) %>%
  +   group_by(penr, benr, contryear) %>%
  +   mutate(appren_max = max(appren)) %>%
  +   filter(appren_max != 1) %>%
  +   select(-appren, -appren_max) %>%
  +   ungroup()
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
  > #stamm_data <- stammdaten_uniqe_2022 %>% select(penr, birthyear, frau, stsl)
  > 
  > # --- Person-level covariates incl. nationality + education imputation ---
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
    +     # nationality
      +     stsl_missing = ifelse(is.na(stsl) | stsl == 999, 1L, 0L),
    +     austrian_dummy = ifelse(!is.na(stsl) & stsl == 1, 1L, 0L),
    +     germany_dummy = ifelse(!is.na(stsl) & stsl == 2, 1L, 0L),
    +     eu_dummy = ifelse(!is.na(stsl) & stsl %in% eu_stsl_codes_strict, 1L, 0L),
    +     
      +     # education (explicit missing category)
      +     educ_missing = ifelse(is.na(educ), 1L, 0L),
    +     educ_cat = case_when(
      +       is.na(educ) ~ 0L,  # missing category
      +       TRUE ~ as.integer(educ)
      +     ),
    +     educ_imputed = ifelse(is.na(imputed), 0L, as.integer(imputed))
    +   )
Error in `mutate()`:
  ℹ In argument: `eu_dummy = ifelse(!is.na(stsl) & stsl %in% eu_stsl_codes_strict, 1L, 0L)`.
Caused by error in `stsl %in% eu_stsl_codes_strict`:
  ! object 'eu_stsl_codes_strict' not found
Run `rlang::last_trace()` to see where the error occurred.
> 
  > 
  > employment_data <- employment_data %>%
  +   left_join(stamm_data, by = "penr") %>%
  +   mutate(age = contryear - birthyear) %>%
  +   filter(!is.na(age), !is.na(frau), age >= 20, age <= 60)
Error in is.data.frame(y) : object 'stamm_data' not found
> 
  > # ----------------------------
> # 6) MERGE YEARLY BG PREP OUTPUTS
  > # ----------------------------
> processed_wage_files <- list()
> for (year in min_year:max_year) {
  +   file_path <- paste0("bg_prep/bg_prep_", year, ".RData")
  +   if (file.exists(file_path)) {
    +     load(file_path)
    +     processed_wage_files[[as.character(year)]] <- year_data %>%
      +       select(penr, benr, contryear, wage_bg, duration, censored)
    +   }
  + }
> all_processed_wages <- bind_rows(processed_wage_files)
> 
  > full_data <- employment_data %>%
  +   left_join(all_processed_wages, by = c("penr", "benr", "contryear")) %>%
  +   mutate(dailywage_final = wage_bg / duration) %>%
  +   filter(!is.na(dailywage_final), !is.na(censored))
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
  > full_data <- full_data %>%
  +   group_by(penr, contryear) %>%
  +   arrange(desc(wage_bg)) %>%
  +   slice(1) %>%
  +   ungroup()
> 
  > # ----------------------------
> # 7) FEATURES (full-history based)
  > # ----------------------------
> full_data <- full_data %>%
  +   mutate(
    +     juj = ifelse(job_unemp == 1 | lead(unemp_job) == 1, 1, 0),
    +     juj = replace_na(juj, 0)
    +   ) %>%
  +   group_by(penr) %>%
  +   arrange(contryear) %>%
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
    +     exper = pmin(exper, 40),
    +     experdummy = ifelse(birthyear < 1957, 1, 0)
    +   )
Error in `mutate()`:
  ℹ In argument: `age_entry = first(contryear) - first(birthyear)`.
ℹ In group 1: `penr = 23815`.
Caused by error in `first()`:
  ! object 'birthyear' not found
Run `rlang::last_trace()` to see where the error occurred.
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
    +     tenure = pmin(tenure, 40),
    +     tenuredummy = ifelse(contryear == 1994, 1, 0),
    +     dailywage_final = pmax(dailywage_final, 1)
    +   )
> 
  > firm_data <- dg_full_merge %>%
  +   select(benr, contryear, bundesland, nace08_2)
> 
  > full_data <- full_data %>%
  +   left_join(firm_data, by = c("benr", "contryear")) %>%
  +   filter(!is.na(nace08_2))
Warning message:
  In left_join(., firm_data, by = c("benr", "contryear")) :
  Detected an unexpected many-to-many relationship between `x` and `y`.
ℹ Row 1 of `x` matches multiple rows in `y`.
ℹ Row 350101930 of `y` matches multiple rows in `x`.
ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
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
  +   arrange(contryear) %>%
  +   mutate(
    +     benr_lagged = lag(benr),
    +     benr_lagged = ifelse(is.na(benr_lagged), -9, benr_lagged),
    +     benr_lagged = ifelse(benr_lagged == benr, -9, benr_lagged)
    +   ) %>%
  +   ungroup()
> 
  > 
  > 
  > # --- Live-birth indicator from raw QU data (qu_qual == "49") ---
  > # place this after full_data is created and after stamm_data/frau is already merged
  > 
  > birth_events <- qualifikationen_data %>%
  +   filter(qu_qual == "49") %>%
  +   transmute(
    +     penr,
    +     contryear = lubridate::year(qu_vdat)
    +   ) %>%
  +   filter(!is.na(contryear)) %>%
  +   distinct(penr, contryear) %>%   # one increment per person-year
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
Error in `mutate()`:
  ℹ In argument: `n_children = ifelse(frau == 1, n_children, 0L)`.
ℹ In group 1: `penr = 23815`.
Caused by error in `ifelse()`:
  ! object 'frau' not found
Run `rlang::last_trace()` to see where the error occurred.
> 
  > 
  > 
  > vpi_1986 <- data.frame(
    +   contryear = 1986:2024,
    +   year_inflation = c(0, 1.4, 2, 2.5, 3.3, 3.3, 4.1, 3.6, 3.0, 2.2, 1.9, 1.3, 0.9, 0.6, 2.3,
                           +                      2.7, 1.8, 1.3, 2.1, 2.3, 1.5, 2.2, 3.2, 0.5, 1.9, 3.3, 2.4, 2.0, 1.7,
                           +                      0.9, 0.9, 2.1, 2.0, 1.5, 1.4, 2.8, 8.6, 7.8, 2.9)
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
  > # pre_model_full_history <- full_data %>%
  > #   select(
  > #     penr, benr, contryear, dailywage_final, benr_lagged, tenure, birthyear,
  > #     age, exper, experdummy, frau, stsl, subcat, age_entry, year_entry,
  > #     juj, censored, cohort, bundesland, nace08_2, return
  > #   )
  > 
  > 
  > # pre_model_full_history <- full_data %>%
  > #   select(
  > #     penr, benr, contryear, dailywage_final, benr_lagged, tenure, birthyear,
  > #     age, exper, experdummy, frau, stsl, stsl_missing, austrian_dummy, eu_dummy,
  > #     educ_cat, educ_missing, educ_imputed,
  > #     subcat, age_entry, year_entry, juj, censored, cohort, bundesland, nace08_2, return
  > #   )
  > 
  > 
  > # pre_model_full_history <- full_data %>%
  > #   select(
  > #     penr, benr, contryear, dailywage_final, benr_lagged, tenure, birthyear,
  > #     age, exper, experdummy, frau, stsl, stsl_missing, austrian_dummy, eu_dummy,
  > #     educ_cat, educ_missing, educ_imputed,
  > #     whitecollar_dummy, bluecollar_dummy, othercollar_dummy,
  > #     subcat, age_entry, year_entry, juj, censored, cohort, bundesland, nace08_2, return
  > #   )
  > 
  > 
  > pre_model_full_history <- full_data %>%
  +   select(
    +     penr, benr, contryear, dailywage_final, benr_lagged, tenure, birthyear,
    +     age, exper, experdummy, frau, stsl, stsl_missing, austrian_dummy, eu_dummy,
    +     educ_cat, educ_missing, educ_imputed,
    +     whitecollar_dummy, bluecollar_dummy, othercollar_dummy,
    +     live_birth_event, n_children,
    +     subcat, age_entry, year_entry, juj, censored, cohort, bundesland, nace08_2, return
    +   )
Error in `select()`:
  ! Can't select columns that don't exist.
✖ Column `birthyear` doesn't exist.
Run `rlang::last_trace()` to see where the error occurred.
> 
> 
> testnames <- c("penr", "benr", "contryear", "dailywage_final", "benr_lagged", "tenure", "birthyear",
+   "age", "exper", "experdummy", "frau", "stsl", "stsl_missing", "austrian_dummy", "eu_dummy",
+   "educ_cat", "educ_missing", "educ_imputed",
+   "whitecollar_dummy", "bluecollar_dummy", "othercollar_dummy",
+   "subcat", "age_entry", "year_entry", "juj", "censored", "cohort", "bundesland", "nace08_2", "return"
+ )
> 
> 
> intersect(testnames, names(full_data))
[1] "penr"              "benr"              "contryear"         "dailywage_final"   "benr_lagged"       "tenure"           
[7] "whitecollar_dummy" "bluecollar_dummy"  "othercollar_dummy" "subcat"            "censored"          "bundesland"       
[13] "nace08_2"         
> 
> 
> save(pre_model_full_history, file = "pre_model_output/pre_model_full_history.RData")
Error in save(pre_model_full_history, file = "pre_model_output/pre_model_full_history.RData") : 
object ‘pre_model_full_history’ not found
> 
> cat("Done.\n")
Done.
> cat("Rows pre_model_full_history:", nrow(pre_model_full_history), "\n")
Error in nrow(pre_model_full_history) : 
object 'pre_model_full_history' not found
> cat("Year range:", min(pre_model_full_history$contryear, na.rm = TRUE), "to",
+     max(pre_model_full_history$contryear, na.rm = TRUE), "\n")
Error in cat("Year range:", min(pre_model_full_history$contryear, na.rm = TRUE),  : 
object 'pre_model_full_history' not found
> 
> 
> # --- TIMER: end (place at very end of file) ---
> elapsed <- Sys.time() - t_step3_to_end
> cat("\n=== Runtime Step 3 -> End ===\n")

=== Runtime Step 3 -> End ===
> print(elapsed)
Time difference of 6.652147 mins
> cat("Seconds:", round(as.numeric(elapsed, units = "secs"), 2), "\n")
Seconds: 399.13 
> cat("Minutes:", round(as.numeric(elapsed, units = "mins"), 2), "\n")
Minutes: 6.65 
> cat("Hours:", round(as.numeric(elapsed, units = "hours"), 2), "\n")
Hours: 0.11 
> names(full_data)
[1] "penr"              "benr"              "contryear"         "subcat"            "job_unemp"         "unemp_job"        
[7] "start"             "end"               "dur"               "wage_bg"           "duration"          "censored"         
[13] "dailywage_final"   "matchid"           "tenure"            "tenuredummy"       "bundesland"        "nace08_2"         
[19] "whitecollar_dummy" "bluecollar_dummy"  "othercollar_dummy" "benr_lagged"      

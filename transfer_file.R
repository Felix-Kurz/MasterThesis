# # # Check if the upper bounds themselves are reasonable
# # upper_bound_check <- tobit_data %>%
# #   summarise(
# #     min_upper = min(interval_upper, na.rm = TRUE),
# #     max_upper = max(interval_upper, na.rm = TRUE),
# #     any_inf_upper = any(is.infinite(interval_upper)),
# #     any_na_upper = any(is.na(interval_upper))
# #   )
# # print(upper_bound_check)
# #
# # # Check for problematic predictions in the tobit results
# # pred_check <- final_imputed_data %>%
# #   summarise(
# #     min_pred = min(dailywage_final_pred, na.rm = TRUE),
# #     max_pred = max(dailywage_final_pred, na.rm = TRUE),
# #     any_inf_pred = any(is.infinite(dailywage_final_pred)),
# #     any_na_pred = any(is.na(dailywage_final_pred))
# #   )
# # print(pred_check)
# #
# # # Find the specific problematic observations
# # problematic_obs <- final_imputed_data %>%
# #   filter(is.infinite(dailywage_final_pred)) %>%
# #   select(penr, contryear, frau, agegroup, dailywage_final_orig, dailywage_final_pred)
# # print(problematic_obs)
# 
# 
# 
# 
# 
# > str(lohnzettel)
# tibble [113,481,460 × 6] (S3: tbl_df/tbl/data.frame)
# $ penr                : num [1:113481460] 1 2 3 5 6 6 6 9 10 10 ...
# ..- attr(*, "label")= chr "PENR der mitversicherten Person"
# ..- attr(*, "format.stata")= chr "%16.0f"
# $ benr                : num [1:113481460] 1.20e+12 1.20e+12 1.20e+12 1.21e+12 1.11e+12 ...
# ..- attr(*, "label")= chr "firm ID"
# ..- attr(*, "format.stata")= chr "%13.0f"
# $ year                : num [1:113481460] 1994 1994 1994 1994 1994 ...
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ lz_ywage            : num [1:113481460] 8137 7616 2539 46169 2702 ...
# ..- attr(*, "format.stata")= chr "%10.0g"
# $ lz_dwage            : num [1:113481460] 22.29 20.86 6.96 126.49 35.55 ...
# ..- attr(*, "format.stata")= chr "%10.0g"
# $ lz_golden_p67_2_6_10: num [1:113481460] 0 0 0 1398 144 ...
# ..- attr(*, "format.stata")= chr "%10.0g"
# > str(qual_cat_step3_dta)
# tibble [143,650,596 × 18] (S3: tbl_df/tbl/data.frame)
# $ svnr    : num [1:143650596] -13306755 -13306750 -13306732 -13306713 -13306702 ...
# ..- attr(*, "label")= chr "person ID"
# ..- attr(*, "format.stata")= chr "%15.0f"
# $ benr    : num [1:143650596] 1.52e+12 1.97e+12 1.12e+12 1.97e+12 1.97e+12 ...
# ..- attr(*, "label")= chr "firm ID"
# ..- attr(*, "format.stata")= chr "%13.0f"
# $ qual    : chr [1:143650596] "G8" "39" "G9" "39" ...
# ..- attr(*, "label")= chr "qualification"
# ..- attr(*, "format.stata")= chr "%9s"
# $ vdat    : Date[1:143650596], format: "2021-06-21" "2021-03-17" "2021-01-01" "2021-06-14" ...
# $ bdat    : Date[1:143650596], format: "2021-06-30" "2021-07-31" "2021-07-31" "2021-07-31" ...
# $ penr    : num [1:143650596] 17102663 17102658 17102640 17102621 17102610 ...
# ..- attr(*, "label")= chr "PENR der mitversicherten Person"
# ..- attr(*, "format.stata")= chr "%16.0f"
# $ cat     : 'haven_labelled' num [1:143650596] 1 2 1 2 2 2 1 1 1 1 ...
# ..- attr(*, "label")= chr "category"
# ..- attr(*, "format.stata")= chr "%51.0g"
# ..- attr(*, "labels")= Named num [1:12] 1 2 3 4 5 6 7 8 10 12 ...
# .. ..- attr(*, "names")= chr [1:12] "employed" "unemployed" "retired" "maternity benefit, parental leave" ...
# $ subcat  : 'haven_labelled' num [1:143650596] 15 22 15 22 22 22 15 15 15 15 ...
# ..- attr(*, "label")= chr "subcategory"
# ..- attr(*, "format.stata")= chr "%36.0g"
# ..- attr(*, "labels")= Named num [1:18] 11 12 13 14 15 16 21 22 23 31 ...
# .. ..- attr(*, "names")= chr [1:18] "E - white collar" "E - blue collar" "E - civil servant" "E - apprenticeship" ...
# $ class   : 'haven_labelled' num [1:143650596] 1 0 2 0 0 0 1 1 1 1 ...
# ..- attr(*, "label")= chr "class of worker"
# ..- attr(*, "format.stata")= chr "%19.0g"
# ..- attr(*, "labels")= Named num [1:4] 1 2 3 4
# .. ..- attr(*, "names")= chr [1:4] "blue collar worker" "white collar worker" "civil servant" "undefined"
# $ type    : 'haven_labelled' num [1:143650596] 3 0 3 0 0 0 3 3 3 3 ...
# ..- attr(*, "label")= chr "type of contract"
# ..- attr(*, "format.stata")= chr "%20.0g"
# ..- attr(*, "labels")= Named num [1:4] 2 3 4 5
# .. ..- attr(*, "names")= chr [1:4] "appentriceship" "marginal worker" "werkvertrag" "freier dienstvertrag"
# $ last_e  : num [1:143650596] NA NA NA NA NA NA NA NA NA NA ...
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ next_e  : num [1:143650596] NA NA NA NA NA NA NA NA NA NA ...
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ last_u  : num [1:143650596] NA NA NA NA NA NA NA NA NA NA ...
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ next_u  : num [1:143650596] NA NA NA NA NA NA NA NA NA NA ...
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ last90_e: num [1:143650596] NA NA NA NA NA NA NA NA NA NA ...
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ next90_e: num [1:143650596] NA NA NA NA NA NA NA NA NA NA ...
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ last90_r: num [1:143650596] NA NA NA NA NA NA NA NA NA NA ...
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ next90_r: num [1:143650596] NA NA NA NA NA NA NA NA NA NA ...
# ..- attr(*, "format.stata")= chr "%8.0g"
# > str(hauptverband_2022)
# tibble [285,834,659 × 16] (S3: tbl_df/tbl/data.frame)
# $ penr      : num [1:285834659] 17318057 17318044 17318026 17318004 17318004 ...
# ..- attr(*, "label")= chr "PENR"
# ..- attr(*, "format.stata")= chr "%16.0f"
# $ svnr      : num [1:285834659] -13530300 -13530287 -13530269 -13530247 -13530247 ...
# ..- attr(*, "format.stata")= chr "%16.0f"
# $ benr      : num [1:285834659] 1.21e+12 1.12e+12 1.62e+12 1.71e+12 1.71e+12 ...
# ..- attr(*, "label")= chr "BENR"
# ..- attr(*, "format.stata")= chr "%16.0f"
# $ contryear : num [1:285834659] 2022 2022 2022 2021 2022 ...
# ..- attr(*, "label")= chr "JAHR"
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ bg_euro   : num [1:285834659] 497 84 2646 227 2454 ...
# ..- attr(*, "label")= chr "(sum) bg_euro"
# ..- attr(*, "format.stata")= chr "%9.0g"
# $ sz_euro   : num [1:285834659] 0 0 0 0 0 27 0 0 0 42 ...
# ..- attr(*, "label")= chr "(sum) sz_euro"
# ..- attr(*, "format.stata")= chr "%9.0g"
# $ tetg      : num [1:285834659] 0 0 0 0 0 0 0 0 0 0 ...
# ..- attr(*, "label")= chr "(sum) tetg"
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ duration  : num [1:285834659] 8 0 14 4 31 6 15 7 9 5 ...
# ..- attr(*, "label")= chr "(sum) duration"
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ dailywage : num [1:285834659] 62.1 NA 189 56.8 79.2 ...
# ..- attr(*, "format.stata")= chr "%9.0g"
# $ von_mon   : num [1:285834659] 2 2 2 12 1 11 2 2 2 2 ...
# ..- attr(*, "label")= chr "(min) von_mon"
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ bis_mon   : num [1:285834659] 2 2 2 12 1 11 2 2 2 2 ...
# ..- attr(*, "label")= chr "(max) bis_mon"
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ new       : Date[1:285834659], format: "2022-07-19" "2022-07-19" "2022-07-19" "2022-07-19" ...
# $ old_benr  : chr [1:285834659] "" "" "" "" ...
# ..- attr(*, "format.stata")= chr "%13s"
# $ ktkz      : chr [1:285834659] "" "" "" "" ...
# ..- attr(*, "format.stata")= chr "%9s"
# $ svnrklasse: num [1:285834659] NA NA NA NA NA NA NA NA NA NA ...
# ..- attr(*, "format.stata")= chr "%10.0g"
# $ benr_str  : chr [1:285834659] "" "" "" "" ...
# ..- attr(*, "format.stata")= chr "%13s"
# > str(stammdaten_uniqe_2022)
# tibble [16,624,328 × 36] (S3: tbl_df/tbl/data.frame)
# $ penr           : num [1:16624328] 1 2 3 4 5 6 7 8 9 10 ...
# ..- attr(*, "format.stata")= chr "%16.0f"
# $ svnr           : num [1:16624328] 5.05e+08 7.74e+08 1.04e+09 1.95e+09 2.22e+09 ...
# ..- attr(*, "format.stata")= chr "%16.0f"
# $ frau           : num [1:16624328] 1 1 1 1 0 1 1 1 1 1 ...
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ birthyear      : num [1:16624328] 1915 1923 1931 1958 1966 ...
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ birthmonth     : num [1:16624328] 1 1 1 1 1 1 1 1 2 2 ...
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ akgr           : num [1:16624328] 0 0 0 NA 9 0 0 0 0 0 ...
# ..- attr(*, "label")= chr "akad. Grad NUR IN KOMBINATION MIT akgr_new valide!!!"
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ akgr_new       : chr [1:16624328] "(kein Titel)" "(kein Titel)" "(kein Titel)" "" ...
# ..- attr(*, "label")= chr "akad. Grad NUR IN KOMBINATION MIT akgr valide!!!"
# ..- attr(*, "format.stata")= chr "%30s"
# $ akgr_2012      : num [1:16624328] NA NA NA NA NA NA 0 0 NA NA ...
# ..- attr(*, "label")= chr "ACHTUNG! akad. Grad aus neuer Qu. -> viele Fehlen!!!! akgr+akgr_new verwenden!!!"
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ stsl           : num [1:16624328] 999 999 999 1 1 1 NA NA 999 1 ...
# ..- attr(*, "format.stata")= chr "%10.0g"
# $ stsl_former    : num [1:16624328] NA NA NA NA NA NA NA NA NA NA ...
# ..- attr(*, "format.stata")= chr "%9.0g"
# $ deathday       : num [1:16624328] 10 7 10 NA NA NA NA NA 15 NA ...
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ deathmonth     : num [1:16624328] 2 5 6 NA NA NA 11 8 6 NA ...
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ deathyear      : num [1:16624328] 2003 2003 1997 NA NA ...
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ stma           : 'haven_labelled' num [1:16624328] 1 1 1 NA NA NA 2 2 1 NA ...
# ..- attr(*, "format.stata")= chr "%43.0g"
# ..- attr(*, "labels")= Named num [1:5] 1 2 3 4 5
# .. ..- attr(*, "names")= chr [1:5] "1 - verstorben (MMJJ=Sterbemonat)" "2 -verstorben (MMJJ=Meldemonat)" "3 - verstorben, Datum unbekannt (MMJJ=0000)" "B - nicht dokumentiert" ...
# $ old_frau       : num [1:16624328] 1 1 1 1 0 1 NA NA 1 1 ...
# ..- attr(*, "format.stata")= chr "%10.0g"
# $ old_birthyear  : num [1:16624328] 1915 1923 1931 1958 1966 ...
# ..- attr(*, "format.stata")= chr "%10.0g"
# $ old_birthmonth : num [1:16624328] 1 1 1 1 1 1 NA NA 2 2 ...
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ old_akgr       : num [1:16624328] 0 0 0 0 9 0 NA NA 0 0 ...
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ old_stsl       : num [1:16624328] NA NA NA NA 1 1 NA NA NA 1 ...
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ old_stsl_former: num [1:16624328] NA NA NA NA 1 1 NA NA NA NA ...
# ..- attr(*, "format.stata")= chr "%9.0g"
# $ old_deathday   : num [1:16624328] 10 7 10 NA NA NA NA NA 15 NA ...
# ..- attr(*, "format.stata")= chr "%10.0g"
# $ old_deathmonth : num [1:16624328] 2 5 6 NA NA NA NA NA 6 NA ...
# ..- attr(*, "format.stata")= chr "%10.0g"
# $ old_deathyear  : num [1:16624328] 2003 2003 1997 NA NA ...
# ..- attr(*, "format.stata")= chr "%10.0g"
# $ old_stma       : num [1:16624328] 1 1 1 NA NA NA NA NA 1 NA ...
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ stmedatc       : Date[1:16624328], format: "2003-02-10" "2003-05-07" "1997-06-10" NA ...
# $ zeitstempel    : Date[1:16624328], format: "2011-06-30" "2011-06-30" "2006-02-28" "2020-07-31" ...
# $ st_type        : 'haven_labelled' num [1:16624328] 0 0 0 0 0 0 1 1 0 0 ...
# ..- attr(*, "format.stata")= chr "%19.0g"
# ..- attr(*, "labels")= Named num [1:4] 0 1 2 3
# .. ..- attr(*, "names")= chr [1:4] "in both sets" "only ZURICH" "only in AMDB" "Mitversicherte AMDB"
# $ ams_geschst    : chr [1:16624328] "" "" "" "" ...
# ..- attr(*, "label")= chr "Geschaeftsstelle"
# ..- attr(*, "format.stata")= chr "%9s"
# $ beruf          : chr [1:16624328] "" "" "" "" ...
# ..- attr(*, "label")= chr "Berufsart"
# ..- attr(*, "format.stata")= chr "%9s"
# $ nation         : chr [1:16624328] "" "" "" "" ...
# ..- attr(*, "label")= chr "Nationalitaet"
# ..- attr(*, "format.stata")= chr "%9s"
# $ ausbildung     : chr [1:16624328] "" "" "" "" ...
# ..- attr(*, "label")= chr "Ausbildung"
# ..- attr(*, "format.stata")= chr "%9s"
# $ nace95         : chr [1:16624328] "" "" "" "" ...
# ..- attr(*, "label")= chr "Nace95"
# ..- attr(*, "format.stata")= chr "%9s"
# $ nace           : num [1:16624328] NA NA NA NA NA NA NA NA NA NA ...
# ..- attr(*, "label")= chr "Nace"
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ familienstand  : chr [1:16624328] "" "" "" "" ...
# ..- attr(*, "label")= chr "Familienstand lt AMS"
# ..- attr(*, "format.stata")= chr "%9s"
# $ _addons        : num [1:16624328] 1 1 1 1 1 1 1 1 1 1 ...
# ..- attr(*, "format.stata")= chr "%8.0g"
# $ _m_old_akgr    : 'haven_labelled' num [1:16624328] 3 3 3 1 3 3 1 1 3 3 ...
# ..- attr(*, "format.stata")= chr "%23.0g"
# ..- attr(*, "labels")= Named num [1:5] 1 2 3 4 5
# .. ..- attr(*, "names")= chr [1:5] "master only (1)" "using only (2)" "matched (3)" "missing updated (4)" ...
# >
# 
# 
# 


library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(survival)
library(censReg)
library(VGAM)
library(purrr)
library(data.table)
library(purrr)
library(stringr)

load("lohnzettel_dta.RData")
load("qual_cat_step3_dta.RData")
load("hauptverband_2022_dta.RData")
load("stammdaten_uniqe_2022.RData")

str(lohnzettel)
str(qual_cat_step3_dta)
str(hauptverband_2022)
str(stammdaten_uniqe_2022)

hauptverband_2022_1994_until_2012 <- hauptverband_2022 %>%
  filter(contryear >= 1994 & contryear <= 2012)

hauptverband_2022_1994_until_2012_penr_sample_vec <- sample(unique(hauptverband_2022_1994_until_2012$penr), size = 10000)
summary(hauptverband_2022_1994_until_2012_penr_sample_vec)

save(hauptverband_2022_1994_until_2012_penr_sample_vec,
     file = "hauptverband_2022_1994_until_2012_penr_sample_vec.RData")

hv_1994_until_2012_penr_sample <- hauptverband_2022_1994_until_2012 %>%
  filter(hauptverband_2022_1994_until_2012$penr %in% hauptverband_2022_1994_until_2012_penr_sample_vec)

save(hv_1994_until_2012_penr_sample, file = "hv_1994_until_2012_penr_sample.RData")

qual_penr_sample <- qual_cat_step3_dta %>%
  filter(qual_cat_step3_dta$penr %in% hauptverband_2022_1994_until_2012_penr_sample_vec)

save(qual_penr_sample, file = "qual_penr_sample.RData")

qual_data <- qual_penr_sample %>%
  select(penr, benr, vdat, bdat, subcat) %>%
  rename(start = vdat, end = bdat) %>%
  mutate(
    dur = end - start + 1,
    year_end = year(end),
    year_start = year(start)
  ) %>%
  filter(!(subcat == 0 | (subcat > 16 & subcat < 1112)))

qual_data <- qual_data %>%
  mutate(years = year_end - year_start + 1) %>%
  group_by(penr) %>%
  mutate(spellindex = row_number()) %>%
  uncount(years, .id = "year_seq") %>%
  group_by(penr, spellindex) %>%
  mutate(
    contryear = year_start + year_seq - 1,
    end = ifelse(year_seq != max(year_seq),
                 as.Date(paste0(contryear + 1, "-01-01")) - 1,
                 end),
    start = ifelse(year_seq > 1,
                   as.Date(paste0(contryear, "-01-01")) - 1,
                   start)
  ) %>%
  select(-spellindex, -year_end, -year_start, -year_seq) %>%
  ungroup()

qual_data <- qual_data %>%
  mutate(dur_year = end - start + 1) %>%
  group_by(penr, benr, contryear) %>%
  arrange(start) %>%
  mutate(
    totdur = sum(dur_year),
    durfrac = dur_year / totdur,
    end2 = end,
    end2 = ifelse(!is.na(lead(start)) & end2 >= lead(start),
                  lead(start) - 1, end2),
    dur2 = end2 - start + 1,
    dur_nooverlap = sum(dur2, na.rm = TRUE)
  ) %>%
  select(-end2, -dur2, -totdur) %>%
  ungroup()

save(qual_data, file = "qual_data.RData")

topcoding_test <- c(632, 686, 763, 850, 959, 1090, 1220, 1351, 1417, 1482, 1569, 1656)

topcoding <- c(632, 686, 763, 850, 959, 1090, 1220, 1351, 1417, 1482, 1569, 1656,
               1744, 1787, 1874, 1918, 2005, 2049, 2092, 2180, 2311, 2441, 2616,
               2747, 2834, 2965, 3052, 3095, 3139, 3226, 3270, 3360, 3450, 3630,
               3750, 3840, 3930, 4020, 4110, 4200, 4230, 4440, 4530, 4650, 4860,
               4980, 5130, 5220, 5370)
tctest <- c(1972:2020)

for(year in 1994:2012) {
  cat("Processing year: ", year, "\n")
  
  year_data <- qual_data %>%
    filter(contryear == year)
  
  bg_data <- hv_1994_until_2012_penr_sample %>%
    filter(contryear == year) %>%
    select(benr, penr, bg_euro, sz_euro, duration, dailywage)
  
  if(NROW(bg_data) > 0) {
    year_data <- year_data %>%
      left_join(bg_data, by = c("benr", "penr"))
    
    year_data <- year_data %>%
      mutate(
        duration = ifelse(is.na(duration) | duration == 0, dur_nooverlap, duration),
        wage_bg = round((bg_euro + sz_euro) * durfrac),
        censored = 0,
        topcode_threshold = topcoding[year - 1971]
      ) %>%
      filter(!is.na(bg_euro), !is.na(sz_euro))
    
    year_data <- year_data %>%
      mutate(
        censored = ifelse(
          (bg_euro + sz_euro) * 30 / duration >= topcode_threshold * 7/6 &
            !is.na(wage_bg), 1, censored),
        censored = ifelse(
          (bg_euro + sz_euro) * 30 / dur_nooverlap >= topcode_threshold * 7/6 &
            !is.na(wage_bg), 1, censored),
        censored = ifelse(
          (bg_euro + sz_euro) >= topcode_threshold * 14, 1, censored),
        censored = ifelse(
          bg_euro >= topcode_threshold * 12, 1, censored),
        wage_bg = ifelse(
          (bg_euro + sz_euro) > topcode_threshold * 14,
          topcode_threshold * 14, wage_bg)
      )
    
    year_data <- year_data %>%
      mutate(dailywage = (bg_euro + sz_euro) / duration) %>%
      group_by(contryear, censored) %>%
      mutate(
        dailywage_mode = as.numeric(names(sort(table(dailywage), decreasing = TRUE))[1])
      ) %>%
      ungroup() %>%
      mutate(
        dailywage = ifelse(censored == 1, dailywage_mode, dailywage)
      )
    
    max_dailywage <- year_data %>%
      filter(censored == 1) %>%
      pull(dailywage_mode) %>%
      max(na.rm = TRUE)
    
    year_data <- year_data %>%
      mutate(
        censored = ifelse(censored == 0 & dailywage >= max_dailywage, 1, censored),
        dailywage = ifelse(dailywage >= max_dailywage, max_dailywage, dailywage)
      ) %>%
      select(-dailywage_mode, -bg_euro, -sz_euro, -durfrac, -durfrac, -dur_nooverlap, -duration) %>%
      rename(duration = dur_year)
    
    year_data <- year_data %>%
      group_by(penr, benr, contryear) %>%
      arrange(desc(duration)) %>%
      slice(1) %>%
      ungroup()
    
    save(year_data, file = paste("bg_prep/bg_prep_", year,".RData", sep = ""))
  }
}

firstyear <- 1994

employment_data <- qual_penr_sample %>%
  select(penr, benr, vdat, bdat, subcat) %>%
  filter(subcat %in% c(11, 12, 14) |
           (subcat == 21 & (bdat - vdat) >= 27) |
           (subcat == 22 & (bdat - vdat) >= 27)) %>%
  rename(start = vdat, end = bdat) %>%
  mutate(
    dur = end - start,
    year_end = year(end),
    year_start = year(start)
  )

employment_data <- employment_data %>%
  group_by(penr) %>%
  mutate(spellindex = row_number()) %>%
  mutate(years = ifelse(subcat %in% c(21, 22), 1, year_end - year_start + 1)) %>%
  uncount(years, .id = "year_seq") %>%
  group_by(penr, spellindex) %>%
  mutate(
    contryear = ifelse(!(subcat %in% c(21, 22)), year_start + year_seq - 1, year_start),
    start = ifelse(year_seq > 1 & !(subcat %in% c(21, 22)),
                   as.Date(paste0(contryear, "-01-01")), start),
    end = ifelse(year_seq != max(year_seq) & !(subcat %in% c(21, 22)),
                 as.Date(paste0(contryear + 1, "-01-01")) - 1, end)
  ) %>%
  select(-dur, -spellindex, -year_seq) %>%
  ungroup()

# Remove apprenticeship overlaps
employment_data <- employment_data %>%
  mutate(appren = ifelse(subcat == 14, 1, 0)) %>%
  group_by(penr, benr, contryear) %>%
  mutate(appren_max = max(appren)) %>%
  filter(appren_max != 1) %>%
  select(-appren, -appren_max) %>%
  ungroup()

# Count initial observations
N_matrix <- matrix(0, nrow = 10, ncol = 1)
N_matrix[1 ,1] <- employment_data %>%
  filter(subcat %in% c(11, 12), contryear >= firstyear) %>%
  NROW()

# Restrict to spells lasting at least 30 days
employment_data <- employment_data %>%
  filter(!(subcat %in% c(11, 12)) | (end - start > 29))

N_matrix[2, 1] <- employment_data %>%
  filter(subcat %in% c(11, 12), contryear >= firstyear) %>%
  NROW()

# Generate transition indicators
employment_data <- employment_data %>%
  arrange(penr, start) %>%
  group_by(penr) %>%
  mutate(
    job_unemp = ifelse(lead(subcat) %in% c(21, 22) & lead(end) > end, 1, 0),
    unemp_job = ifelse(lag(subcat) %in% c(21, 22), 1, 0)
  ) %>%
  ungroup() %>%
  replace_na(list(job_unemp = 0, unemp_job = 0))

# Remove unemployment and apprenticeship spells
employment_data <- employment_data %>%
  filter(!(subcat %in% c(21, 22, 14)))

#Keep one spell per year and firm
employment_data <- employment_data %>%
  mutate(dur = end - start + 1) %>%
  group_by(penr, benr, contryear) %>%
  summarise(
    totdur = sum(dur),
    minstart = min(start),
    maxend = max(end),
    subcat = first(subcat),
    job_unemp = first(job_unemp),
    unemp_job = first(unemp_job),
    .groups = "drop"
  ) %>%
  mutate(
    start = minstart,
    end = maxend,
    dur = totdur
  ) %>%
  select(-minstart, -maxend, -totdur)

N_matrix[3, 1] <- employment_data %>%
  filter(contryear >= firstyear) %>%
  NROW()

# Merge individual information
stamm_data <- stammdaten_uniqe_2022 %>%
  select(penr, birthyear, frau, stsl)

employment_data <- employment_data %>%
  left_join(stamm_data, by = "penr") %>%
  mutate(age = contryear - birthyear) %>%
  filter(!is.na(age), !is.na(frau))

N_matrix[4, 1] <- employment_data %>%
  filter(contryear >= firstyear) %>%
  NROW()

#Age restrictions
employment_data <- employment_data %>%
  filter(age >= 20, age <= 60)

N_matrix[5, 1] <- employment_data %>%
  filter(contryear >= firstyear) %>%
  NROW()

# Load all processed data that was created earlier
processed_wage_files <- list()
for (year in firstyear:2012) {
  file_path <- paste("bg_prep/bg_prep_", year, ".RData", sep = "")
  if (file.exists(file_path)) {
    load(file_path)
    processed_wage_files[[as.character(year)]] <- year_data %>%
      select(penr, benr, contryear, wage_bg, duration, censored)
  }
}

# Combine all processed wage data
all_processed_wages <- bind_rows(processed_wage_files)

str(all_processed_wages)

# Merge wage information for each year
full_data <- employment_data %>%
  filter(contryear >= firstyear) %>%
  left_join(all_processed_wages, by = c("penr", "benr", "contryear")) %>%
  mutate(dailywage_final = wage_bg / duration) %>%
  filter(!is.na(dailywage_final), !is.na(censored))

N_matrix[6, 1] <- NROW(full_data)

# Define stable career start
full_data <- full_data %>%
  arrange(penr, start) %>%
  group_by(penr) %>%
  mutate(
    startdate = first(start),
    cumulative_days = cumsum(end - start +1),
    stable_career = cumulative_days >= 90
  ) %>%
  filter(stable_career) %>%
  select(-startdate, -cumulative_days, -stable_career) %>%
  ungroup()

N_matrix[7, 1] <- full_data %>%
  filter(contryear >= firstyear) %>%
  NROW()

# Keep highest earning job per year
full_data <- full_data %>%
  group_by(penr, contryear) %>%
  arrange(desc(wage_bg)) %>%
  slice(1) %>%
  ungroup()

N_matrix[8, 1] <- full_data %>%
  filter(contryear >= firstyear) %>%
  NROW()

# Generate additional variables
full_data <- full_data %>%
  mutate(
    juj = ifelse(job_unemp == 1 | lead(unemp_job) == 1, 1, 0),
    juj = replace_na(juj, 0)
  ) %>%
  group_by(penr) %>%
  arrange(contryear) %>%
  mutate(
    age_entry = first(contryear) - first(birthyear),
    exper = row_number() - 1,
    cohort = first(contryear) - 1994,
    year_entry = first(contryear),
    return = ifelse(contryear > lag(contryear) + 1, 1, 0),
    return = replace_na(return, 0)
  ) %>%
  ungroup() %>%
  mutate(
    exper = pmin(exper, 40),
    experdummy = ifelse(birthyear < 1957, 1, 0)
  )

# Generate tenure
full_data <- full_data %>%
  arrange(penr, benr, contryear) %>%
  group_by(penr, benr) %>%
  mutate(
    matchid = cur_group_id(),
    tenure = row_number()
  ) %>%
  ungroup() %>%
  mutate(
    tenure = pmin(tenure, 40),
    tenuredummy = ifelse(contryear == 1994, 1, 0)
  )

# Filter to analysis period (is redundant now with how i generated the testdata but later will be important)
full_data <- full_data %>%
  filter(contryear >= firstyear)

# Trim some outliers
full_data <- full_data %>%
  mutate(dailywage_final = pmax(dailywage_final, 1))

# Merge firm information
load("dg_full_merge.RData") #this will be later inserted in this code when i know where it finaly will come from
#right now it is in loaderfile.R

#table(dg_full_merge$contryear)

firm_data <- dg_full_merge %>%
  select(benr, contryear, bundesland, nace08_2)

full_data <- full_data %>%
  left_join(firm_data, by = c("benr", "contryear")) %>%
  filter(!is.na(nace08_2))

N_matrix[9, 1] <- NROW(full_data)

# Generate additional variables
full_data <- full_data %>%
  mutate(
    subcat = ifelse(subcat %in% c(12, 14), 1, 0),
    subcat = ifelse(subcat == 11, 0, subcat)
  ) %>%
  group_by(penr) %>%
  arrange(contryear) %>%
  mutate(
    benr_lagged = lag(benr),
    benr_lagged = ifelse(is.na(benr_lagged), -9, benr_lagged),
    benr_lagged = ifelse(benr_lagged == benr, -9, benr_lagged)
  ) %>%
  ungroup()

# Convert wages to 2000 Euros using CPI
vpi_1986 <- data.frame(
  contryear = 1986:2024,
  year_inflation = c(0, 1.4, 2, 2.5, 3.3, 3.3, 4.1, 3.6, 3.0, 2.2, 1.9, 1.3, 0.9, 0.6, 2.3,
                     2.7, 1.8, 1.3, 2.1, 2.3, 1.5, 2.2, 3.2, 0.5, 1.9, 3.3, 2.4, 2.0, 1.7,
                     0.9, 0.9, 2.1, 2.0, 1.5, 1.4, 2.8, 8.6, 7.8, 2.9)
)

vpi_1986$vpi <- vpi_1986$year_inflation/100 + 1

vpi_1986 <- vpi_1986 %>%
  mutate(reverse_index = cumprod(vpi)) %>%
  mutate(cpi_2012 = reverse_index[contryear == 2012] / reverse_index,
         cpi_2000 = reverse_index[contryear == 2000] / reverse_index)

# Apply CPI conversions to wages
full_data <- full_data %>%
  left_join(vpi_1986 %>% select(contryear, cpi_2000), by = "contryear") %>%
  mutate(
    dailywage_final = dailywage_final * cpi_2000,
    dailywage_final = log(dailywage_final)
  ) %>%
  select(-cpi_2000)

analysis_data <- full_data %>%
  select(penr, benr, contryear, dailywage_final, benr_lagged, tenure, birthyear,
         age, exper, experdummy, frau, stsl, subcat, age_entry, year_entry,
         juj, censored, cohort, bundesland, nace08_2, return)

#########################################
# TOBIT IMPUTATIONS FOR CENSORED WAGES
#########################################

# Prepare variables for tobit regression
tobit_data <- analysis_data %>%
  group_by(penr) %>%
  mutate(
    num_years = n(),
    mean_wage = mean(dailywage_final),
    mean_censored = mean(censored)
  ) %>%
  mutate(
    mean_wage = ifelse(num_years > 1,
                       (mean_wage - dailywage_final/num_years) * (num_years/(num_years-1)),
                       mean_wage),
    mean_censored = ifelse(num_years > 1,
                           (mean_censored - censored/num_years) * (num_years/(num_years-1)),
                           mean_censored)
  ) %>%
  ungroup() %>%
  group_by(benr, contryear) %>%
  mutate(
    firm_size = n(),
    mean_wage_coworkers = mean(dailywage_final),
    mean_censored_coworkers = mean(censored)
  ) %>%
  mutate(
    mean_wage_coworkers = ifelse(firm_size > 1,
                                 (mean_wage_coworkers - dailywage_final/firm_size) * (firm_size/(firm_size-1)),
                                 mean_wage_coworkers),
    mean_censored_coworkers = ifelse(firm_size > 1,
                                     (mean_censored_coworkers - censored/firm_size) * (firm_size/(firm_size-1)),
                                     mean_censored_coworkers)
  ) %>%
  ungroup() %>%
  # Add topcoding-based upper bounds
  left_join(vpi_1986 %>% select(contryear, cpi_2000), by = "contryear") %>%
  mutate(
    topcode_threshold = topcoding[contryear - 1971],
    daily_topcode = topcode_threshold * 14 / 30,  # Convert monthly*14 to daily
    log_daily_topcode = log(daily_topcode * cpi_2000),  # Apply same CPI conversion and log
    firm_size_square = firm_size^2 / 100,
    oneyear_dummy = ifelse(num_years == 1, 1, 0),
    oneperson_dummy = ifelse(firm_size == 1, 1, 0),
    smallfirm_dummy = ifelse(firm_size < 10, 1, 0),
    agegroup = case_when(
      age <= 30 ~ 1,
      age > 30 & age <= 40 ~ 2,
      age > 40 & age <= 50 ~ 3,
      age > 50 & age <= 60 ~ 4,
      age > 60 ~ 5
    ),
    interval_upper = ifelse(censored == 0, dailywage_final, log_daily_topcode),
    interval_lower = dailywage_final
  ) %>%
  select(-cpi_2000, -topcode_threshold, -daily_topcode, -log_daily_topcode)

save(tobit_data, file = "help.RData")

# Run tobit regression for each year
min_year <- min(tobit_data$contryear)
max_year <- max(tobit_data$contryear)

imputed_data_list <- list()

for (year in min_year:max_year) {
  cat("Processing tobit for year:", year, "\n")
  
  year_data <- tobit_data %>% filter(contryear == year)
  
  if (NROW(year_data) > 0) {
    # Run tobit regression by gender and age group
    year_results <- year_data %>%
      group_by(frau, agegroup) %>%
      do({
        if (NROW(.) > 10) { # Minimum observations for regression
          # Prepare censored regression
          formula_str <- "survival::Surv(interval_lower, interval_upper, type = 'interval2') ~
          age + mean_wage + mean_censored + firm_size + firm_size_square +
          oneyear_dummy + oneperson_dummy + smallfirm_dummy +
          mean_wage_coworkers + mean_censored_coworkers"
          
          tryCatch({
            model <- survival::survreg(as.formula(formula_str), data = ., dist = "gaussian")
            
            # Generate predictions
            predictions <- predict(model, newdata = ., type = "response")
            sigma_hat <- model$scale
            
            # Generate imputations for censored observations
            set.seed(123)
            k <- pnorm((.$dailywage_final - predictions) / sigma_hat)
            replace_vals <- predictions + sigma_hat * qnorm(k + runif(length(k)) * (1 -k))
            
            data.frame(
              .,
              predict = predictions,
              dailywage_final_orig = .$dailywage_final,
              dailywage_final_pred = replace_vals,
              sigma_hat = sigma_hat
            )
          }, error = function(e) {
            # If regression fails, return original data
            data.frame(
              .,
              predict = .$dailywage_final,
              dailywage_final_orig = .$dailywage_final,
              dailywage_final_pred = .$dailywage_final,
              sigma_hat = 1
            )
          })
        } else {
          # If too few observations, return original data
          data.frame(
            .,
            predict = .$dailywage_final,
            dailywage_final_orig = .$dailywage_final,
            dailywage_final_pred = .$dailywage_final,
            sigma_hat = 1
          )
        }
      }) %>%
      ungroup()
    
    # Replace censored wages with imputed values
    year_results <- year_results %>%
      mutate(
        dailywage_final = ifelse(censored == 1, dailywage_final_pred, dailywage_final),
        educ = case_when(
          age_entry < 18 ~ 1,
          age_entry >= 18 & age_entry <= 21 ~ 2,
          age_entry >= 21 ~ 3
        )
      ) %>%
      filter(!is.na(dailywage_final)) %>%
      select(penr, contryear, benr, benr_lagged, dailywage_final, dailywage_final_orig, 
             dailywage_final_pred,  birthyear, educ, firm_size, age, frau)
    
    # Save year data
    save(year_results, file = paste("results_by_year/help", year, ".RData", sep = ""))
    
    imputed_data_list[[as.character(year)]] <- year_results
}
  }

final_imputed_data <- bind_rows(imputed_data_list)

save(final_imputed_data, file = "tu_felix_austria_imputed_wages")

imputation_eval <- final_imputed_data %>%
  filter(!is.na(dailywage_final_orig) & !is.na(dailywage_final_pred)) %>%
  mutate(
    #was_imputed = abs(dailywage_final - dailywage_final_orig) > 1e-10,
    was_imputed = (dailywage_final != dailywage_final_orig),
    
    residual = dailywage_final_pred - dailywage_final_orig,
    squared_error = residual^2
  )

imputation_eval

summary(imputation_eval$squared_error)

rmse_imputed <- imputation_eval %>%
  filter(was_imputed) %>%
  summarise(
    rmse = sqrt(mean(squared_error, na.rm = TRUE)),
    mae = mean(abs(residual), na.rm = TRUE),
    n_imputed = n()
  )

rmse_imputed

extreme_check <- imputation_eval %>%
  filter(was_imputed) %>%
  summarise(
    min_residual = min(residual, na.rm = TRUE),
    max_residual = max(residual, na.rm = TRUE),
    any_infinite = any(is.infinite(residual)),
    any_na = any(is.na(residual))
  )
print(extreme_check)




# cpi_data <- data.frame(
#   contryear = c(1960:2024),
#   year_inflation = c(
#     1.95, 3.54, 4.38, 2.71, 3.87, 4.93, 2.05, 3.97, 2.76, 3.08,  # 1960-1969
#     4.37, 4.7, 6.36, 7.53, 9.52, 8.45, 7.32, 5.49, 3.57, 3.71,   # 1970-1979
#     6.33, 6.8, 5.44, 3.34, 5.66, 3.19, 1.71, 1.4, 1.92, 2.57,    # 1980-1989
#     3.26, 3.34, 4.02, 3.63, 2.95, 2.24, 1.86, 1.31, 0.92, 0.57,  # 1990-1999
#     2.34, 2.65, 1.81, 1.36, 2.06, 2.3, 1.44, 2.17, 3.22, 0.51,   # 2000-2009
#     1.81, 3.29, 2.49, 2, 1.61, 0.9, 0.89, 2.08, 2, 1.53,         # 2010-2019
#     1.38, 2.77, 8.55, 7.81, 2.94                                  # 2020-2024
#   )
# )


library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(survival)
library(data.table)
library(purrr)
library(stringr)

# Load data
load("lohnzettel_dta.RData")
load("qual_cat_step3_dta.RData")
load("hauptverband_2022_dta.RData")
load("stammdaten_uniqe_2022.RData")

# Define parameters
firstyear <- 1980  # Analysis start year (matching Stata)
wage_data_start <- 1994  # When wage data becomes available

# Sample selection - but keep ALL years for these individuals
set.seed(123)  # for reproducibility
sample_penr <- sample(unique(hauptverband_2022$penr), size = 10000)
save(sample_penr, file = "sample_penr.RData")

# Filter ALL data for sampled individuals (not just 1994-2012)
hv_sample <- hauptverband_2022 %>%
  filter(penr %in% sample_penr)

qual_sample <- qual_cat_step3_dta %>%
  filter(penr %in% sample_penr)

# Process qualification data for ALL years
qual_data <- qual_sample %>%
  select(penr, benr, vdat, bdat, subcat) %>%
  rename(start = vdat, end = bdat) %>%
  mutate(
    dur = as.numeric(end - start + 1),
    year_end = year(end),
    year_start = year(start)
  ) %>%
  filter(!(subcat == 0 | (subcat > 16 & subcat < 1112))) %>%
  filter(year_end >= 1972)  # Keep from 1972

# Split into annual spells
qual_data <- qual_data %>%
  mutate(years = year_end - year_start + 1) %>%
  group_by(penr) %>%
  mutate(spellindex = row_number()) %>%
  uncount(years, .id = "year_seq") %>%
  group_by(penr, spellindex) %>%
  mutate(
    contryear = year_start + year_seq - 1,
    end = if_else(year_seq != max(year_seq),
                  as.Date(paste0(contryear + 1, "-01-01")) - 1,
                  end),
    start = if_else(year_seq > 1,
                    as.Date(paste0(contryear, "-01-01")),
                    start)
  ) %>%
  ungroup() %>%
  select(-spellindex, -year_seq) %>%
  filter(contryear >= 1972, contryear <= 2020)

# Calculate duration variables
qual_data <- qual_data %>%
  mutate(dur_year = as.numeric(end - start + 1)) %>%
  group_by(penr, benr, contryear) %>%
  arrange(start) %>%
  mutate(
    totdur = sum(dur_year),
    durfrac = dur_year / totdur,
    # Handle overlapping spells
    end2 = end,
    end2 = if_else(!is.na(lead(start)) & end2 >= lead(start),
                   lead(start) - 1, end2),
    dur2 = as.numeric(end2 - start + 1),
    dur_nooverlap = sum(dur2, na.rm = TRUE)
  ) %>%
  select(-end2, -dur2, -totdur) %>%
  ungroup()

# Topcoding vector for all years
topcoding <- c(632, 686, 763, 850, 959, 1090, 1220, 1351, 1417, 1482, 1569, 1656,
               1744, 1787, 1874, 1918, 2005, 2049, 2092, 2180, 2311, 2441, 2616,
               2747, 2834, 2965, 3052, 3095, 3139, 3226, 3270, 3360, 3450, 3630,
               3750, 3840, 3930, 4020, 4110, 4200, 4230, 4440, 4530, 4650, 4860,
               4980, 5130, 5220, 5370)

# Process wage data for years where available (1994-2012)
for(year in wage_data_start:2012) {
  cat("Processing wages for year:", year, "\n")
  
  year_qual <- qual_data %>%
    filter(contryear == year)
  
  year_hv <- hv_sample %>%
    filter(contryear == year) %>%
    select(benr, penr, bg_euro, sz_euro, duration, dailywage)
  
  if(nrow(year_hv) > 0) {
    year_data <- year_qual %>%
      left_join(year_hv, by = c("benr", "penr"))
    
    year_data <- year_data %>%
      mutate(
        duration = if_else(is.na(duration) | duration == 0, dur_nooverlap, duration),
        wage_bg = round((bg_euro + sz_euro) * durfrac),
        censored = 0,
        topcode_threshold = topcoding[year - 1971]
      ) %>%
      filter(!is.na(bg_euro), !is.na(sz_euro))
    
    # Apply censoring rules
    year_data <- year_data %>%
      mutate(
        censored = case_when(
          (bg_euro + sz_euro) * 30 / duration >= topcode_threshold * 7/6 & !is.na(wage_bg) ~ 1,
          (bg_euro + sz_euro) * 30 / dur_nooverlap >= topcode_threshold * 7/6 & !is.na(wage_bg) ~ 1,
          (bg_euro + sz_euro) >= topcode_threshold * 14 ~ 1,
          bg_euro >= topcode_threshold * 12 ~ 1,
          TRUE ~ 0
        ),
        wage_bg = if_else((bg_euro + sz_euro) > topcode_threshold * 14,
                          topcode_threshold * 14, wage_bg),
        dailywage = (bg_euro + sz_euro) / duration
      )
    
    # Handle mode calculation for censored wages
    if(any(year_data$censored == 1)) {
      mode_calc <- year_data %>%
        filter(censored == 1) %>%
        count(dailywage) %>%
        arrange(desc(n)) %>%
        slice(1) %>%
        pull(dailywage)
      
      year_data <- year_data %>%
        mutate(
          dailywage = if_else(censored == 1, mode_calc, dailywage),
          censored = if_else(censored == 0 & dailywage >= mode_calc, 1, censored),
          dailywage = if_else(dailywage >= mode_calc, mode_calc, dailywage)
        )
    }
    
    # Keep one spell per firm-year
    year_data <- year_data %>%
      select(-bg_euro, -sz_euro, -durfrac, -dur_nooverlap, -duration) %>%
      rename(duration = dur_year) %>%
      group_by(penr, benr, contryear) %>%
      arrange(desc(duration)) %>%
      slice(1) %>%
      ungroup()
    
    save(year_data, file = paste0("bg_prep/bg_prep_", year, ".RData"))
  }
}

# Process employment spells from 1972 onwards
employment_data <- qual_sample %>%
  select(penr, benr, vdat, bdat, subcat) %>%
  filter(subcat %in% c(11, 12, 14) |
           (subcat == 21 & as.numeric(bdat - vdat) >= 27) |
           (subcat == 22 & as.numeric(bdat - vdat) >= 27)) %>%
  rename(start = vdat, end = bdat) %>%
  mutate(
    dur = as.numeric(end - start),
    year_end = year(end),
    year_start = year(start)
  ) %>%
  filter(year_start >= 1972)  # Keep all years from 1972

# Split employment spells by year
employment_data <- employment_data %>%
  group_by(penr) %>%
  mutate(spellindex = row_number()) %>%
  mutate(years = if_else(subcat %in% c(21, 22), 1, year_end - year_start + 1)) %>%
  uncount(years, .id = "year_seq") %>%
  group_by(penr, spellindex) %>%
  mutate(
    contryear = if_else(!(subcat %in% c(21, 22)), 
                        year_start + year_seq - 1, 
                        year_start),
    start = if_else(year_seq > 1 & !(subcat %in% c(21, 22)),
                    as.Date(paste0(contryear, "-01-01")), 
                    start),
    end = if_else(year_seq != max(year_seq) & !(subcat %in% c(21, 22)),
                  as.Date(paste0(contryear + 1, "-01-01")) - 1, 
                  end)
  ) %>%
  ungroup() %>%
  select(-dur, -spellindex, -year_seq)

# Remove apprenticeship overlaps
employment_data <- employment_data %>%
  group_by(penr, benr, contryear) %>%
  mutate(has_appren = any(subcat == 14)) %>%
  filter(!has_appren) %>%
  select(-has_appren) %>%
  ungroup()

# Apply duration restrictions only to employment spells
employment_data <- employment_data %>%
  filter(!(subcat %in% c(11, 12)) | as.numeric(end - start) > 29)

# Generate transition indicators
employment_data <- employment_data %>%
  arrange(penr, start) %>%
  group_by(penr) %>%
  mutate(
    job_unemp = if_else(lead(subcat) %in% c(21, 22) & lead(end) > end, 1, 0, 0),
    unemp_job = if_else(lag(subcat) %in% c(21, 22), 1, 0, 0)
  ) %>%
  ungroup()

# Keep pre-firstyear data for career tracking
pre_analysis_data <- employment_data %>%
  filter(contryear < firstyear | subcat %in% c(21, 22))

# Remove unemployment/apprenticeship for main analysis
employment_data <- employment_data %>%
  filter(!(subcat %in% c(21, 22, 14)))

# Consolidate to one spell per person-firm-year
employment_data <- employment_data %>%
  mutate(dur = as.numeric(end - start + 1)) %>%
  group_by(penr, benr, contryear) %>%
  summarise(
    totdur = sum(dur),
    start = min(start),
    end = max(end),
    subcat = first(subcat),
    job_unemp = max(job_unemp),
    unemp_job = max(unemp_job),
    .groups = "drop"
  ) %>%
  mutate(dur = totdur) %>%
  select(-totdur)

# Merge individual characteristics
employment_data <- employment_data %>%
  left_join(stammdaten_uniqe_2022 %>% 
              select(penr, birthyear, frau, stsl), 
            by = "penr") %>%
  mutate(age = contryear - birthyear) %>%
  filter(!is.na(age), !is.na(frau), age >= 20, age <= 60)

# Merge wage data where available
wage_files <- list.files("bg_prep", pattern = "bg_prep_.*\\.RData", full.names = TRUE)
all_wages <- map_dfr(wage_files, function(f) {
  load(f)
  year_data %>% select(penr, benr, contryear, wage_bg, duration, censored)
})

full_data <- employment_data %>%
  left_join(all_wages, by = c("penr", "benr", "contryear"))

# Separate data with and without wages
data_with_wages <- full_data %>%
  filter(!is.na(wage_bg)) %>%
  mutate(dailywage_final = wage_bg / duration)

data_without_wages <- full_data %>%
  filter(is.na(wage_bg))

# Combine with pre-analysis data to identify career start
all_career_data <- bind_rows(
  pre_analysis_data %>% select(penr, start, end, contryear, subcat),
  employment_data %>% select(penr, start, end, contryear, subcat)
) %>%
  arrange(penr, start)

# Identify stable career start (90+ days of employment)
career_starts <- all_career_data %>%
  filter(!(subcat %in% c(21, 22))) %>%
  arrange(penr, start) %>%
  group_by(penr) %>%
  mutate(
    cum_dur = cumsum(as.numeric(end - start + 1)),
    stable_start = if_else(cum_dur >= 90 & lag(cum_dur, default = 0) < 90, start, as.Date(NA))
  ) %>%
  filter(!is.na(stable_start)) %>%
  summarise(career_start_date = min(stable_start), .groups = "drop")

# Apply career start filter
data_with_wages <- data_with_wages %>%
  left_join(career_starts, by = "penr") %>%
  filter(!is.na(career_start_date), start >= career_start_date)

# Calculate experience and tenure properly
data_with_wages <- data_with_wages %>%
  arrange(penr, contryear) %>%
  group_by(penr) %>%
  mutate(
    # Experience based on years since career start
    years_since_start = contryear - year(career_start_date),
    exper = pmin(years_since_start, 40),
    experdummy = if_else(birthyear < 1957, 1, 0),
    age_entry = year(career_start_date) - birthyear,
    year_entry = year(career_start_date),
    cohort = year_entry - 1972,
    juj = if_else(job_unemp == 1 | lead(unemp_job, default = 0) == 1, 1, 0),
    return = if_else(contryear > lag(contryear) + 1, 1, 0, 0)
  ) %>%
  ungroup()

# Calculate tenure with proper tracking
data_with_wages <- data_with_wages %>%
  arrange(penr, benr, contryear) %>%
  group_by(penr, benr) %>%
  mutate(
    spell_group = cumsum(contryear != lag(contryear, default = 0) + 1),
    tenure = row_number(),
    tenure = pmin(tenure, 40),
    tenuredummy = if_else(min(contryear) <= 1972, 1, 0)
  ) %>%
  ungroup() %>%
  select(-spell_group)

# Keep highest earning job per person-year
data_with_wages <- data_with_wages %>%
  group_by(penr, contryear) %>%
  arrange(desc(wage_bg)) %>%
  slice(1) %>%
  ungroup()

# Load firm data and merge
load("dg_full_merge.RData")
firm_data <- dg_full_merge %>%
  select(benr, contryear, bundesland, nace08_2)

data_with_wages <- data_with_wages %>%
  left_join(firm_data, by = c("benr", "contryear")) %>%
  filter(!is.na(nace08_2))

# Additional variable processing
data_with_wages <- data_with_wages %>%
  mutate(
    subcat = if_else(subcat %in% c(12, 14), 1, 0),
    dailywage_final = pmax(dailywage_final, 1),
    benr_lagged = lag(benr, default = -9),
    benr_lagged = if_else(benr_lagged == benr, -9, benr_lagged)
  ) %>%
  group_by(penr) %>%
  arrange(contryear) %>%
  fill(benr_lagged, .direction = "down") %>%
  ungroup()

# CPI adjustment
vpi_data <- data.frame(
  contryear = 1972:2024,
  cpi_values = c(1.28, 1.377, 1.508, 1.635, 1.755, 1.851, 1.917, 1.988, 2.114, 2.258, 
                 2.381, 2.46, 2.6, 2.683, 2.728, 2.767, 2.82, 2.892, 2.986, 3.086,
                 3.21, 3.327, 3.425, 3.502, 3.567, 3.614, 3.647, 3.668, 3.754, 
                 3.854, 3.923, 3.977, 4.059, 4.152, 4.212, 4.304, 4.442, 4.465, 
                 4.545, 4.693, 4.81, 4.906, 4.985, 5.030, 5.075, 5.181, 5.284, 
                 5.365, 5.443, 5.594, 6.072, rep(6.072, 3))  # Extend with last value
)

# Calculate CPI relative to 2000
base_2000 <- vpi_data$cpi_values[vpi_data$contryear == 2000]
vpi_data$cpi_2000 = base_2000 / vpi_data$cpi_values

data_with_wages <- data_with_wages %>%
  left_join(vpi_data %>% select(contryear, cpi_2000), by = "contryear") %>%
  mutate(
    dailywage_final = dailywage_final * cpi_2000,
    dailywage_final = log(dailywage_final)
  ) %>%
  select(-cpi_2000)

# Filter to analysis period
analysis_data <- data_with_wages %>%
  filter(contryear >= firstyear) %>%
  select(penr, benr, contryear, dailywage_final, benr_lagged, tenure, birthyear,
         age, exper, experdummy, frau, stsl, subcat, age_entry, year_entry,
         juj, censored, cohort, bundesland, nace08_2, return)

save(analysis_data, file = "AKMprep.RData")

#########################################
# TOBIT IMPUTATIONS
#########################################

# Continue with tobit imputation as before...
# [Rest of tobit code remains the same]












> library(ggplot2)
> library(scales)

Attaching package: ‘scales’

The following object is masked from ‘package:purrr’:
  
  discard

The following object is masked from ‘package:readr’:
  
  col_factor

> library(xgboost)

Attaching package: ‘xgboost’

The following object is masked from ‘package:dplyr’:
  
  slice

> 
  > options(dplyr.summarise.inform = FALSE)
> 
  > #########################################
> # 0) CONFIG
  > #########################################
> 
  > firstyear <- 1994
> lastyear  <- 2012
> 
  > use_debug_sample <- TRUE
> sample_n_penr <- 100000
> sample_seed <- 123
> 
  > dir.create("model_output_v4", showWarnings = FALSE, recursive = TRUE)
> dir.create("model_output_v4/results_by_year", showWarnings = FALSE, recursive = TRUE)
> dir.create("model_output_v4/plots", showWarnings = FALSE, recursive = TRUE)
> 
  > #########################################
> # 1) LOAD INPUTS
  > #########################################
> 
  > load("pre_model_output/pre_model_full_history.RData")   # object: pre_model_full_history
> load("lohnzettel_dta.RData")                            # object: lohnzettel
> 
  > cat("Loaded pre_model_full_history rows:", nrow(pre_model_full_history), "\n")
Loaded pre_model_full_history rows: 6911360 
> cat("Loaded lohnzettel rows:", nrow(lohnzettel), "\n")
Loaded lohnzettel rows: 113481460 
> 
  > #########################################
> # 2) SHARED CONSTANTS
  > #########################################
> 
  > topcoding <- c(
    +   632, 686, 763, 850, 959, 1090, 1220, 1351, 1417, 1482, 1569, 1656,
    +   1744, 1787, 1874, 1918, 2005, 2049, 2092, 2180, 2311, 2441, 2616,
    +   2747, 2834, 2965, 3052, 3095, 3139, 3226, 3270, 3360, 3450, 3630,
    +   3750, 3840, 3930, 4020, 4110, 4200, 4230, 4440, 4530, 4650, 4860,
    +   4980, 5130, 5220, 5370
    + )
> 
  > vpi_1986 <- data.frame(
    +   contryear = 1986:2024,
    +   year_inflation = c(
      +     0, 1.4, 2, 2.5, 3.3, 3.3, 4.1, 3.6, 3.0, 2.2, 1.9, 1.3, 0.9, 0.6,
      +     2.3, 2.7, 1.8, 1.3, 2.1, 2.3, 1.5, 2.2, 3.2, 0.5, 1.9, 3.3, 2.4,
      +     2.0, 1.7, 0.9, 0.9, 2.1, 2.0, 1.5, 1.4, 2.8, 8.6, 7.8, 2.9
      +   )
    + ) %>%
  +   mutate(
    +     vpi = year_inflation / 100 + 1,
    +     reverse_index = cumprod(vpi),
    +     cpi_2000 = reverse_index[contryear == 2000] / reverse_index,
    +     cpi_2012 = reverse_index[contryear == 2012] / reverse_index
    +   )
> 
  > #########################################
> # 3) ANALYSIS SAMPLE FROM PRE-MODEL DATA
  > #########################################
> 
  > analysis_data <- pre_model_full_history %>%
  +   filter(contryear >= firstyear, contryear <= lastyear)
> 
  > if (use_debug_sample) {
    +   set.seed(sample_seed)
    +   sampled_penr <- analysis_data %>%
      +     distinct(penr) %>%
      +     slice_sample(n = min(sample_n_penr, n())) %>%
      +     pull(penr)
    + 
      +   analysis_data <- analysis_data %>%
        +     filter(penr %in% sampled_penr)
      + 
        +   cat("DEBUG SAMPLE ACTIVE:", length(sampled_penr), "persons\n")
      + } else {
        +   sampled_penr <- analysis_data %>%
          +     distinct(penr) %>%
          +     pull(penr)
        + }
Error in `slice_sample()`:
  ! `n` must be a constant.
Caused by error in `n()`:
  ! Must only be used inside data-masking verbs like `mutate()`, `filter()`, and
`group_by()`.
Run `rlang::last_trace()` to see where the error occurred.
> 
  > cat("Analysis data rows after time/sample restriction:", nrow(analysis_data), "\n")
Analysis data rows after time/sample restriction: 3350211 
> cat("Analysis year range:", min(analysis_data$contryear), "to", max(analysis_data$contryear), "\n")
Analysis year range: 1994 to 2012 
> 
  > #########################################
> # 4) LZ DATA PREP
  > #########################################
> 
  > lz_comparison_data <- lohnzettel %>%
  +   filter(penr %in% sampled_penr) %>%
  +   filter(year >= firstyear, year <= lastyear) %>%
  +   rename(contryear = year) %>%
  +   left_join(vpi_1986 %>% select(contryear, cpi_2000), by = "contryear") %>%
  +   mutate(
    +     lz_ywage_log = log(pmax(lz_ywage * cpi_2000, 1)),
    +     lz_dwage_log = log(pmax(lz_dwage * cpi_2000, 1)),
    +     lz_golden_log = log(pmax(lz_golden_p67_2_6_10 * cpi_2000 + 1, 1))
    +   ) %>%
  +   select(penr, benr, contryear, lz_ywage_log, lz_dwage_log, lz_golden_log) %>%
  +   filter(is.finite(lz_dwage_log))
Error in `filter()`:
  ℹ In argument: `penr %in% sampled_penr`.
Caused by error in `penr %in% sampled_penr`:
  ! object 'sampled_penr' not found
Run `rlang::last_trace()` to see where the error occurred.
> 
  > cat("LZ comparison rows:", nrow(lz_comparison_data), "\n")
Error in nrow(lz_comparison_data) : object 'lz_comparison_data' not found
> 
  > #########################################





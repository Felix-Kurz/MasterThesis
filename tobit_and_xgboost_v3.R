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
library(ggplot2)
library(scales)  # For comma formatting

load("lohnzettel_dta.RData")
load("qual_cat_step3_dta.RData")
load("hauptverband_2022_dta.RData")
load("stammdaten_uniqe_2022.RData")
load("qualifikationen_data.RData")

str(lohnzettel)
str(qual_cat_step3_dta)
str(hauptverband_2022)
str(stammdaten_uniqe_2022)
str(qualifikationen_data)



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

# RMSE for imputed observations only (your original code)
rmse_imputed <- imputation_eval %>%
  filter(was_imputed) %>%
  summarise(
    rmse = sqrt(mean(squared_error, na.rm = TRUE)),
    mae = mean(abs(residual), na.rm = TRUE),
    n_imputed = n()
  )

rmse_imputed

# RMSE for both censored and uncensored observations
rmse_by_censoring <- (imputation_eval) %>%
  group_by(was_imputed) %>%
  summarise(
    rmse = sqrt(mean(squared_error, na.rm = TRUE)),
    mae = mean(abs(residual), na.rm = TRUE),
    n_obs = n(),
    mean_residual = mean(residual, na.rm = TRUE),
    median_residual = median(residual, na.rm = TRUE),
    sd_residual = sd(residual, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    censoring_status = ifelse(was_imputed, "Censored (Imputed)", "Uncensored")
  )

print("RMSE by Censoring Status:")
print(rmse_by_censoring)

# Overall RMSE (all observations combined)
rmse_overall <- imputation_eval %>%
  summarise(
    was_imputed = NA,
    rmse = sqrt(mean(squared_error, na.rm = TRUE)),
    mae = mean(abs(residual), na.rm = TRUE),
    n_obs = n(),
    mean_residual = mean(residual, na.rm = TRUE),
    median_residual = median(residual, na.rm = TRUE),
    sd_residual = sd(residual, na.rm = TRUE),
    censoring_status = "Overall"
  )

print("Overall RMSE:")
print(rmse_overall)

# Combined summary table
complete_rmse_summary <- bind_rows(rmse_by_censoring, rmse_overall)
print("Complete RMSE Summary:")
print(complete_rmse_summary)

extreme_check <- imputation_eval %>%
  filter(was_imputed) %>%
  summarise(
    min_residual = min(residual, na.rm = TRUE),
    max_residual = max(residual, na.rm = TRUE),
    any_infinite = any(is.infinite(residual)),
    any_na = any(is.na(residual))
  )

print("Extreme Values Check:")
print(extreme_check)





# First, let's check for and handle infinite values in the predicted wages
imputation_eval_clean <- imputation_eval %>%
  filter(is.finite(dailywage_final_pred), 
         is.finite(dailywage_final_orig),
         is.finite(residual))




cat("Observations with infinite predicted values removed:", 
    nrow(imputation_eval) - nrow(imputation_eval_clean), "\n")

# Create a long format for comparison plotting
wage_comparison <- imputation_eval_clean %>%
  select(penr, contryear, was_imputed, dailywage_final_orig, dailywage_final_pred) %>%
  pivot_longer(cols = c(dailywage_final_orig, dailywage_final_pred),
               names_to = "wage_type", 
               values_to = "log_wage") %>%
  mutate(wage_type = case_when(
    wage_type == "dailywage_final_orig" ~ "Original",
    wage_type == "dailywage_final_pred" ~ "Predicted"
  ))




# Modified lohnzettel overlay analysis for DAILY wages
# Process lohnzettel data for the selected sample (no estimation, just as additional data)
lz_overlay_data <- lohnzettel %>%
  filter(penr %in% hauptverband_2022_1994_until_2012_penr_sample_vec) %>%
  filter(year >= 1994, year <= 2012) %>%
  rename(contryear = year) %>%
  # Apply CPI adjustment to get 2000 Euro values and take log (same as bg data)
  left_join(vpi_1986 %>% select(contryear, cpi_2000), by = "contryear") %>%
  mutate(
    lz_dwage_2000 = lz_dwage * cpi_2000,
    lz_dwage_log = log(lz_dwage_2000)
  ) %>%
  select(penr, benr, contryear, lz_dwage_log) %>%
  filter(!is.na(lz_dwage_log), is.finite(lz_dwage_log))

# Merge bg estimation results with lohnzettel data for DAILY wage comparison
combined_wage_data_daily <- imputation_eval_clean %>%
  select(penr, benr, contryear, was_imputed, dailywage_final_orig, dailywage_final_pred) %>%
  left_join(lz_overlay_data, by = c("penr", "benr", "contryear")) %>%
  filter(!is.na(lz_dwage_log)) %>%  # Only keep observations where we have all three wage measures
  mutate(
    # Convert to daily wages for comparison
    daily_bg_orig = dailywage_final_orig,
    daily_bg_pred = dailywage_final_pred,
    daily_lz = lz_dwage_log
  ) %>%
  select(penr, benr, contryear, was_imputed, daily_bg_orig, daily_bg_pred, daily_lz) %>%
  pivot_longer(cols = c(daily_bg_orig, daily_bg_pred, daily_lz),
               names_to = "wage_source", 
               values_to = "daily_wage") %>%
  mutate(wage_source = case_when(
    wage_source == "daily_bg_orig" ~ "BG Original",
    wage_source == "daily_bg_pred" ~ "BG Predicted", 
    wage_source == "daily_lz" ~ "LZ Uncensored"
  ))

cat("Combined daily wage data summary:\n")
cat("Total observations with all three wage measures:", nrow(combined_wage_data_daily)/3, "\n")
cat("Observations by censoring status:\n")
print(table(combined_wage_data_daily$was_imputed[combined_wage_data_daily$wage_source == "BG Original"]))

# P11: Overall comparison of all three wage sources (DAILY wages)
p11 <- ggplot(combined_wage_data_daily, aes(x = daily_wage, fill = wage_source)) +
  geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
  scale_fill_manual(values = c("BG Original" = "blue", 
                               "BG Predicted" = "red", 
                               "LZ Uncensored" = "darkgreen")) +
  labs(title = "Distribution Comparison: BG Original vs BG Predicted vs LZ Uncensored",
       subtitle = "All observations with complete daily wage information",
       x = "ln Daily Wage (2000 Euros)",
       y = "Frequency",
       fill = "Wage Source") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(0, 7)  # Adjusted xlim for daily wages

print(p11)

# P12: Comparison by censoring status (faceted) - DAILY wages
p12 <- ggplot(combined_wage_data_daily, aes(x = daily_wage, fill = wage_source)) +
  geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
  scale_fill_manual(values = c("BG Original" = "blue", 
                               "BG Predicted" = "red", 
                               "LZ Uncensored" = "darkgreen")) +
  facet_wrap(~ifelse(was_imputed, "Censored Observations", "Uncensored Observations"), 
             scales = "free_y") +
  labs(title = "Daily Wage Distribution by Censoring Status",
       subtitle = "Left: uncensored observations, Right: censored observations",
       x = "ln Daily Wage (2000 Euros)",
       y = "Frequency",
       fill = "Wage Source") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(0, 7)  # Adjusted xlim for daily wages

print(p12)

# P13: Density comparison for better overlay visualization - DAILY wages
p13 <- ggplot(combined_wage_data_daily, aes(x = daily_wage, color = wage_source)) +
  geom_density(size = 1.2, alpha = 0.7) +
  scale_color_manual(values = c("BG Original" = "blue", 
                                "BG Predicted" = "red", 
                                "LZ Uncensored" = "darkgreen")) +
  labs(title = "Density Overlay: BG Original vs BG Predicted vs LZ Uncensored",
       subtitle = "Smooth density comparison of all three daily wage measures",
       x = "ln Daily Wage (2000 Euros)",
       y = "Density",
       color = "Wage Source") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(0, 7)  # Adjusted xlim for daily wages

print(p13)

# Summary statistics for all three wage sources (DAILY wages)
wage_comparison_summary_daily <- combined_wage_data_daily %>%
  group_by(wage_source, was_imputed) %>%
  summarise(
    n_obs = n(),
    mean_wage = round(mean(daily_wage, na.rm = TRUE), 2),
    median_wage = round(median(daily_wage, na.rm = TRUE), 2),
    sd_wage = round(sd(daily_wage, na.rm = TRUE), 2),
    min_wage = round(min(daily_wage, na.rm = TRUE), 2),
    max_wage = round(max(daily_wage, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(wage_source, was_imputed)

cat("\n=== SUMMARY STATISTICS BY WAGE SOURCE AND CENSORING STATUS (DAILY WAGES) ===\n")
print(wage_comparison_summary_daily)

# Overall summary (not split by censoring) - DAILY wages
overall_summary_daily <- combined_wage_data_daily %>%
  group_by(wage_source) %>%
  summarise(
    n_obs = n(),
    mean_wage = round(mean(daily_wage, na.rm = TRUE), 2),
    median_wage = round(median(daily_wage, na.rm = TRUE), 2),
    sd_wage = round(sd(daily_wage, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(wage_source)

cat("\n=== OVERALL SUMMARY BY WAGE SOURCE (DAILY WAGES) ===\n")
print(overall_summary_daily)










#########################################
# XGBOOST MODEL: PREDICT BG FROM LZ (GOLD STANDARD)
# Training: LZ features -> BG target
# Future use: BG features -> Estimated true wage
#########################################

library(xgboost)

# Step 1: Create training dataset where we have both LZ and BG
# Use LZ as features, BG as target
training_data <- tobit_data %>%
  left_join(
    lohnzettel %>%
      filter(penr %in% hauptverband_2022_1994_until_2012_penr_sample_vec) %>%
      filter(year >= 1994, year <= 2012) %>%
      rename(contryear = year) %>%
      left_join(vpi_1986 %>% select(contryear, cpi_2000), by = "contryear") %>%
      mutate(
        lz_ywage_log = log(pmax(lz_ywage * cpi_2000, 1)),
        lz_dwage_log = log(pmax(lz_dwage * cpi_2000, 1)),
        lz_golden_log = log(pmax(lz_golden_p67_2_6_10 * cpi_2000 + 1, 1))
      ) %>%
      select(penr, benr, contryear, lz_ywage_log, lz_dwage_log, lz_golden_log),
    by = c("penr", "benr", "contryear")
  ) %>%
  filter(!is.na(lz_dwage_log)) %>% # Only observations where we have both LZ and BG
  # Create features that would be available in future (BG-based features)
  mutate(
    bg_dwage_log = dailywage_final,  # This is our target variable
    bg_censored_flag = censored,
    bg_interval_upper = interval_upper,
    bg_interval_lower = interval_lower
  )

cat("Training data prepared. Observations with both LZ and BG:", nrow(training_data), "\n")
cat("Training set size:", nrow(X_train), "Test set size:", nrow(X_test), "\n")


# Step 2: Features for training (things we'll have in future from BG data)
# These should be features derivable from BG data alone
feature_cols <- c(
  # # Individual characteristics (available from admin data)
  # "age", 
  # "birthyear", 
  # "frau", 
  # 
  # # Firm characteristics (available from BG data)
  # "firm_size", 
  # "firm_size_square", 
  # "oneperson_dummy", 
  # "smallfirm_dummy",
  # 
  # # Person history (available from BG data)
  # "num_years", 
  # "oneyear_dummy", 
  # "mean_censored",
  # 
  # # BG-derived wage information (censored but available)
  # "interval_lower", 
  # "interval_upper", 
  # "bg_censored_flag",
  # 
  # # Coworker characteristics (from BG data)
  # "mean_censored_coworkers"
  
  
  
  "age", 
  "mean_wage", 
  "mean_censored", 
  "firm_size", 
  "firm_size_square",
  "oneyear_dummy", 
  "oneperson_dummy", 
  "smallfirm_dummy",
  "mean_wage_coworkers", 
  "mean_censored_coworkers"
)

# Training features (what we'll have in future from BG data only)
X_features <- training_data %>%
  select(all_of(feature_cols)) %>%
  mutate(across(everything(), ~ifelse(is.na(.) | is.infinite(.), 0, .))) %>%
  as.matrix()

# Target: True wage from LZ data (what we want to predict in future)
y_target <- training_data$lz_dwage_log  # Use LZ as target (true wage)

cat("Features available for training:", ncol(X_features), "\n")
cat("Target variable: LZ daily wage (uncensored true wage)\n")

# Step 3: Train/test split
set.seed(123)
n_obs <- length(y_target)
train_indices <- sample(seq_len(n_obs), size = floor(0.8 * n_obs))

X_train <- X_features[train_indices, ]
X_test <- X_features[-train_indices, ]
y_train <- y_target[train_indices]
y_test <- y_target[-train_indices]

# Also keep BG wages for comparison
bg_train <- training_data$bg_dwage_log[train_indices]
bg_test <- training_data$bg_dwage_log[-train_indices]

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

cat("Training set size:", nrow(X_train), "Test set size:", nrow(X_test), "\n")

# Step 4: Train XGBoost model (BG features -> LZ true wage)
xgb_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  reg_alpha = 0.1,
  reg_lambda = 1,
  seed = 123
)

cat("Training XGBoost model (BG features -> LZ true wages)...\n")
xgb_cv <- xgb.cv(
  params = xgb_params,
  data = dtrain,
  nrounds = 1000,
  nfold = 5,
  early_stopping_rounds = 50,
  verbose = FALSE
)

best_nrounds <- xgb_cv$best_iteration
cat("Optimal rounds:", best_nrounds, "Best CV RMSE:", min(xgb_cv$evaluation_log$test_rmse_mean), "\n")

# Train final model
xgb_model <- xgb.train(
  params = xgb_params,
  data = dtrain,
  nrounds = best_nrounds,
  watchlist = list(train = dtrain, test = dtest),
  verbose = FALSE
)

# Step 5: Evaluate model performance
train_pred_lz <- predict(xgb_model, dtrain)  # Predicted true wage
test_pred_lz <- predict(xgb_model, dtest)    # Predicted true wage

# Compare against actual LZ wages (ground truth)
train_rmse_lz <- sqrt(mean((train_pred_lz - y_train)^2))
test_rmse_lz <- sqrt(mean((test_pred_lz - y_test)^2))
train_r2_lz <- cor(train_pred_lz, y_train)^2
test_r2_lz <- cor(test_pred_lz, y_test)^2

# Compare against BG wages (what we currently use)
train_rmse_bg <- sqrt(mean((bg_train - y_train)^2))
test_rmse_bg <- sqrt(mean((bg_test - y_test)^2))
train_r2_bg <- cor(bg_train, y_train)^2
test_r2_bg <- cor(bg_test, y_test)^2

cat("\n=== Model Performance: Predicting True Wage (LZ) ===\n")
cat("XGBoost vs LZ True Wage:\n")
cat("  Training RMSE:", round(train_rmse_lz, 4), "R²:", round(train_r2_lz, 4), "\n")
cat("  Test RMSE:", round(test_rmse_lz, 4), "R²:", round(test_r2_lz, 4), "\n")

cat("\nBG vs LZ True Wage (current method):\n")
cat("  Training RMSE:", round(train_rmse_bg, 4), "R²:", round(train_r2_bg, 4), "\n")
cat("  Test RMSE:", round(test_rmse_bg, 4), "R²:", round(test_r2_bg, 4), "\n")

cat("\nImprovement from XGBoost:\n")
cat("  RMSE reduction:", round(test_rmse_bg - test_rmse_lz, 4), "\n")
cat("  R² improvement:", round(test_r2_lz - test_r2_bg, 4), "\n")

# Step 6: Feature importance
importance_matrix <- xgb.importance(feature_names = colnames(X_features), model = xgb_model)
cat("\n=== Top 10 Features for Predicting True Wage ===\n")
print(head(importance_matrix, 10))

# Step 7: Analysis of predictions vs alternatives
prediction_comparison <- data.frame(
  lz_true = y_test,           # Ground truth
  bg_observed = bg_test,       # Current method (censored)
  xgb_predicted = test_pred_lz, # Our model prediction
  
  # Errors
  bg_error = abs(bg_test - y_test),
  xgb_error = abs(test_pred_lz - y_test),
  improvement = abs(bg_test - y_test) - abs(test_pred_lz - y_test)
) %>%
  mutate(
    bg_better = improvement < 0,
    xgb_better = improvement > 0,
    bg_pct_error = abs((bg_observed - lz_true) / lz_true) * 100,
    xgb_pct_error = abs((xgb_predicted - lz_true) / lz_true) * 100
  )

cat("\n=== Prediction Quality Comparison ===\n")
cat("Mean Absolute Error:\n")
cat("  BG method:", round(mean(prediction_comparison$bg_error), 4), "\n")
cat("  XGBoost method:", round(mean(prediction_comparison$xgb_error), 4), "\n")
cat("  Improvement:", round(mean(prediction_comparison$improvement), 4), "\n")

cat("Relative  Improvement:", (1 - round(mean(prediction_comparison$xgb_error), 4) / round(mean(prediction_comparison$bg_error), 4)), "\n")

cat("\nPercentage of cases where XGBoost is better:", 
    round(mean(prediction_comparison$xgb_better) * 100, 1), "%\n")

# Step 8: Function for future predictions
predict_true_wage <- function(bg_data, model = xgb_model, feature_columns = feature_cols) {
  # This function can be used on new BG data (without LZ) to predict true wages
  
  prediction_features <- bg_data %>%
    select(all_of(feature_columns)) %>%
    mutate(across(everything(), ~ifelse(is.na(.) | is.infinite(.), 0, .))) %>%
    as.matrix()
  
  predicted_wages <- predict(model, xgb.DMatrix(data = prediction_features))
  
  return(predicted_wages)
}

cat("\n=== Model trained and ready for future predictions ===\n")
cat("Use predict_true_wage(bg_data) on new data without LZ\n")






#########################################
# XGBOOST MODEL EVALUATION PLOTS
#########################################

# Prepare data for plotting (combine train and test)
combined_data_xgb <- bind_rows(
  data.frame(
    dailywage_final_orig = bg_train,
    dailywage_final_pred = train_pred_lz,
    lz_true = y_train,
    censored = training_data$bg_censored_flag[train_indices]
  ),
  data.frame(
    dailywage_final_orig = prediction_comparison$bg_observed,
    dailywage_final_pred = prediction_comparison$xgb_predicted,
    lz_true = prediction_comparison$lz_true,
    censored = training_data$bg_censored_flag[-train_indices]
  )
) %>%
  mutate(was_imputed = censored == 1) %>%
  filter(is.finite(dailywage_final_orig), 
         is.finite(dailywage_final_pred),
         is.finite(lz_true))

# P1_xgb: BG Original vs XGBoost Predicted (overall)
wage_comparison_xgb <- combined_data_xgb %>%
  pivot_longer(cols = c(dailywage_final_orig, dailywage_final_pred),
               names_to = "wage_type", 
               values_to = "log_wage") %>%
  mutate(wage_type = ifelse(wage_type == "dailywage_final_orig", 
                            "BG Original", "XGBoost Predicted"))

p1_xgb <- ggplot(wage_comparison_xgb, aes(x = log_wage, fill = wage_type)) +
  geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
  scale_fill_manual(values = c("BG Original" = "blue", "XGBoost Predicted" = "red")) +
  labs(title = "XGBoost: Distribution Comparison (BG Original vs XGBoost Predicted)",
       x = "ln Daily Wage (2000 Euros)",
       y = "Frequency",
       fill = "Wage Source") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(0, 7)

print(p1_xgb)

# P2_xgb: By censoring status
p2_xgb <- ggplot(wage_comparison_xgb, aes(x = log_wage, fill = wage_type)) +
  geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
  scale_fill_manual(values = c("BG Original" = "blue", "XGBoost Predicted" = "red")) +
  facet_wrap(~ifelse(was_imputed, "Censored Observations", "Uncensored Observations"), 
             scales = "free_y") +
  labs(title = "XGBoost: Wage Distribution by Censoring Status",
       x = "ln Daily Wage (2000 Euros)",
       y = "Frequency",
       fill = "Wage Source") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(0, 7)

print(p2_xgb)

# P11_xgb: Three-way comparison (BG vs XGBoost vs LZ)
combined_wage_xgb <- combined_data_xgb %>%
  pivot_longer(cols = c(dailywage_final_orig, dailywage_final_pred, lz_true),
               names_to = "wage_source", 
               values_to = "daily_wage") %>%
  mutate(wage_source = case_when(
    wage_source == "dailywage_final_orig" ~ "BG Original",
    wage_source == "dailywage_final_pred" ~ "XGBoost Predicted", 
    wage_source == "lz_true" ~ "LZ Uncensored"
  ))

p11_xgb <- ggplot(combined_wage_xgb, aes(x = daily_wage, fill = wage_source)) +
  geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
  scale_fill_manual(values = c("BG Original" = "blue", 
                               "XGBoost Predicted" = "red", 
                               "LZ Uncensored" = "darkgreen")) +
  labs(title = "XGBoost: BG Original vs XGBoost Predicted vs LZ Uncensored",
       x = "ln Daily Wage (2000 Euros)",
       y = "Frequency",
       fill = "Wage Source") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(0, 7)

print(p11_xgb)

# P12_xgb: Three-way by censoring status
p12_xgb <- ggplot(combined_wage_xgb, aes(x = daily_wage, fill = wage_source)) +
  geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
  scale_fill_manual(values = c("BG Original" = "blue", 
                               "XGBoost Predicted" = "red", 
                               "LZ Uncensored" = "darkgreen")) +
  facet_wrap(~ifelse(was_imputed, "Censored Observations", "Uncensored Observations"), 
             scales = "free_y") +
  labs(title = "XGBoost: Daily Wage Distribution by Censoring Status",
       x = "ln Daily Wage (2000 Euros)",
       y = "Frequency",
       fill = "Wage Source") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(0, 7)

print(p12_xgb)

# P13_xgb: Density overlay
p13_xgb <- ggplot(combined_wage_xgb, aes(x = daily_wage, color = wage_source)) +
  geom_density(size = 1.2, alpha = 0.7) +
  scale_color_manual(values = c("BG Original" = "blue", 
                                "XGBoost Predicted" = "red", 
                                "LZ Uncensored" = "darkgreen")) +
  labs(title = "XGBoost: Density Overlay (BG vs XGBoost vs LZ)",
       x = "ln Daily Wage (2000 Euros)",
       y = "Density",
       color = "Wage Source") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(0, 7)

print(p13_xgb)


#########################################
# XGBOOST MODEL V2: WITH EXPANDED FEATURES
# Including all Tobit features + additional XGBoost features
#########################################

cat("\n=== Training XGBoost Model V2 with Expanded Features ===\n")

# New feature set (combining Tobit features + additional features)
feature_cols_v2 <- c(
  # Core features (matching Tobit)
  "age", 
  "mean_wage", 
  "mean_censored", 
  "firm_size", 
  "firm_size_square",
  "oneyear_dummy", 
  "oneperson_dummy", 
  "smallfirm_dummy",
  "mean_wage_coworkers", 
  "mean_censored_coworkers",
  
  # Additional features (XGBoost can handle more complexity)
  "frau",
  "birthyear",
  "num_years",
  "interval_lower", 
  "interval_upper", 
  "bg_censored_flag"
)

# Training features V2
X_features_v2 <- training_data %>%
  select(all_of(feature_cols_v2)) %>%
  mutate(across(everything(), ~ifelse(is.na(.) | is.infinite(.), 0, .))) %>%
  as.matrix()

# Target remains the same (LZ true wage)
y_target_v2 <- training_data$lz_dwage_log

cat("Features available for training V2:", ncol(X_features_v2), "\n")
cat("New features added:", setdiff(feature_cols_v2, feature_cols), "\n")

# Train/test split (same seed for comparison)
set.seed(123)
n_obs_v2 <- length(y_target_v2)
train_indices_v2 <- sample(seq_len(n_obs_v2), size = floor(0.8 * n_obs_v2))

X_train_v2 <- X_features_v2[train_indices_v2, ]
X_test_v2 <- X_features_v2[-train_indices_v2, ]
y_train_v2 <- y_target_v2[train_indices_v2]
y_test_v2 <- y_target_v2[-train_indices_v2]

# Keep BG wages for comparison
bg_train_v2 <- training_data$bg_dwage_log[train_indices_v2]
bg_test_v2 <- training_data$bg_dwage_log[-train_indices_v2]

dtrain_v2 <- xgb.DMatrix(data = X_train_v2, label = y_train_v2)
dtest_v2 <- xgb.DMatrix(data = X_test_v2, label = y_test_v2)

cat("Training set size:", nrow(X_train_v2), "Test set size:", nrow(X_test_v2), "\n")

# Train XGBoost model V2
cat("Training XGBoost V2 model (with expanded features)...\n")
xgb_cv_v2 <- xgb.cv(
  params = xgb_params,
  data = dtrain_v2,
  nrounds = 1000,
  nfold = 5,
  early_stopping_rounds = 50,
  verbose = FALSE
)

best_nrounds_v2 <- xgb_cv_v2$best_iteration
cat("Optimal rounds V2:", best_nrounds_v2, "Best CV RMSE:", min(xgb_cv_v2$evaluation_log$test_rmse_mean), "\n")

# Train final model V2
xgb_model_v2 <- xgb.train(
  params = xgb_params,
  data = dtrain_v2,
  nrounds = best_nrounds_v2,
  watchlist = list(train = dtrain_v2, test = dtest_v2),
  verbose = FALSE
)

# Evaluate model performance V2
train_pred_lz_v2 <- predict(xgb_model_v2, dtrain_v2)
test_pred_lz_v2 <- predict(xgb_model_v2, dtest_v2)

# Compare against actual LZ wages
train_rmse_lz_v2 <- sqrt(mean((train_pred_lz_v2 - y_train_v2)^2))
test_rmse_lz_v2 <- sqrt(mean((test_pred_lz_v2 - y_test_v2)^2))
train_r2_lz_v2 <- cor(train_pred_lz_v2, y_train_v2)^2
test_r2_lz_v2 <- cor(test_pred_lz_v2, y_test_v2)^2

# Compare against BG wages
train_rmse_bg_v2 <- sqrt(mean((bg_train_v2 - y_train_v2)^2))
test_rmse_bg_v2 <- sqrt(mean((bg_test_v2 - y_test_v2)^2))
train_r2_bg_v2 <- cor(bg_train_v2, y_train_v2)^2
test_r2_bg_v2 <- cor(bg_test_v2, y_test_v2)^2

cat("\n=== Model V2 Performance: Predicting True Wage (LZ) ===\n")
cat("XGBoost V2 vs LZ True Wage:\n")
cat("  Training RMSE:", round(train_rmse_lz_v2, 4), "R²:", round(train_r2_lz_v2, 4), "\n")
cat("  Test RMSE:", round(test_rmse_lz_v2, 4), "R²:", round(test_r2_lz_v2, 4), "\n")

cat("\nBG vs LZ True Wage (current method):\n")
cat("  Training RMSE:", round(train_rmse_bg_v2, 4), "R²:", round(train_r2_bg_v2, 4), "\n")
cat("  Test RMSE:", round(test_rmse_bg_v2, 4), "R²:", round(test_r2_bg_v2, 4), "\n")

cat("\nImprovement from XGBoost V2:\n")
cat("  RMSE reduction:", round(test_rmse_bg_v2 - test_rmse_lz_v2, 4), "\n")
cat("  R² improvement:", round(test_r2_lz_v2 - test_r2_bg_v2, 4), "\n")

# Compare V1 vs V2
if(exists("test_rmse_lz")) {
  cat("\n=== Comparison: V1 vs V2 ===\n")
  cat("V1 Test RMSE:", round(test_rmse_lz, 4), "R²:", round(test_r2_lz, 4), "\n")
  cat("V2 Test RMSE:", round(test_rmse_lz_v2, 4), "R²:", round(test_r2_lz_v2, 4), "\n")
  cat("Improvement V2 over V1:\n")
  cat("  RMSE reduction:", round(test_rmse_lz - test_rmse_lz_v2, 4), "\n")
  cat("  R² improvement:", round(test_r2_lz_v2 - test_r2_lz, 4), "\n")
  
  if(test_rmse_lz_v2 < test_rmse_lz) {
    cat("✓ V2 with expanded features performs BETTER\n")
  } else {
    cat("✗ V1 with original features performs BETTER\n")
  }
}

# Feature importance V2
importance_matrix_v2 <- xgb.importance(feature_names = colnames(X_features_v2), model = xgb_model_v2)
cat("\n=== Top 15 Features for Predicting True Wage (V2) ===\n")
print(head(importance_matrix_v2, 15))

# Analysis of predictions V2
prediction_comparison_v2 <- data.frame(
  lz_true = y_test_v2,
  bg_observed = bg_test_v2,
  xgb_predicted_v2 = test_pred_lz_v2,
  
  bg_error = abs(bg_test_v2 - y_test_v2),
  xgb_error_v2 = abs(test_pred_lz_v2 - y_test_v2),
  improvement_v2 = abs(bg_test_v2 - y_test_v2) - abs(test_pred_lz_v2 - y_test_v2)
) %>%
  mutate(
    bg_better = improvement_v2 < 0,
    xgb_better = improvement_v2 > 0,
    bg_pct_error = abs((bg_observed - lz_true) / lz_true) * 100,
    xgb_pct_error_v2 = abs((xgb_predicted_v2 - lz_true) / lz_true) * 100
  )

cat("\n=== Prediction Quality Comparison V2 ===\n")
cat("Mean Absolute Error:\n")
cat("  BG method:", round(mean(prediction_comparison_v2$bg_error), 4), "\n")
cat("  XGBoost V2 method:", round(mean(prediction_comparison_v2$xgb_error_v2), 4), "\n")
cat("  Improvement:", round(mean(prediction_comparison_v2$improvement_v2), 4), "\n")

cat("Relative Improvement:", round(1 - mean(prediction_comparison_v2$xgb_error_v2) / mean(prediction_comparison_v2$bg_error), 4), "\n")

cat("\nPercentage of cases where XGBoost V2 is better:", 
    round(mean(prediction_comparison_v2$xgb_better) * 100, 1), "%\n")

#########################################
# PLOTS FOR V2 MODEL
#########################################

# Prepare data for plotting
combined_data_xgb_v2 <- bind_rows(
  data.frame(
    dailywage_final_orig = bg_train_v2,
    dailywage_final_pred = train_pred_lz_v2,
    lz_true = y_train_v2,
    censored = training_data$bg_censored_flag[train_indices_v2]
  ),
  data.frame(
    dailywage_final_orig = prediction_comparison_v2$bg_observed,
    dailywage_final_pred = prediction_comparison_v2$xgb_predicted_v2,
    lz_true = prediction_comparison_v2$lz_true,
    censored = training_data$bg_censored_flag[-train_indices_v2]
  )
) %>%
  mutate(was_imputed = censored == 1) %>%
  filter(is.finite(dailywage_final_orig), 
         is.finite(dailywage_final_pred),
         is.finite(lz_true))

# P1_xgb_v2: BG Original vs XGBoost V2 Predicted
wage_comparison_xgb_v2 <- combined_data_xgb_v2 %>%
  pivot_longer(cols = c(dailywage_final_orig, dailywage_final_pred),
               names_to = "wage_type", 
               values_to = "log_wage") %>%
  mutate(wage_type = ifelse(wage_type == "dailywage_final_orig", 
                            "BG Original", "XGBoost V2 Predicted"))

p1_xgb_v2 <- ggplot(wage_comparison_xgb_v2, aes(x = log_wage, fill = wage_type)) +
  geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
  scale_fill_manual(values = c("BG Original" = "blue", "XGBoost V2 Predicted" = "red")) +
  labs(title = "XGBoost V2: Distribution Comparison (BG Original vs XGBoost V2)",
       subtitle = "Model with expanded features",
       x = "ln Daily Wage (2000 Euros)",
       y = "Frequency",
       fill = "Wage Source") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(0, 7)

print(p1_xgb_v2)

# P2_xgb_v2: By censoring status
p2_xgb_v2 <- ggplot(wage_comparison_xgb_v2, aes(x = log_wage, fill = wage_type)) +
  geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
  scale_fill_manual(values = c("BG Original" = "blue", "XGBoost V2 Predicted" = "red")) +
  facet_wrap(~ifelse(was_imputed, "Censored Observations", "Uncensored Observations"), 
             scales = "free_y") +
  labs(title = "XGBoost V2: Wage Distribution by Censoring Status",
       x = "ln Daily Wage (2000 Euros)",
       y = "Frequency",
       fill = "Wage Source") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(0, 7)

print(p2_xgb_v2)

# P11_xgb_v2: Three-way comparison
combined_wage_xgb_v2 <- combined_data_xgb_v2 %>%
  pivot_longer(cols = c(dailywage_final_orig, dailywage_final_pred, lz_true),
               names_to = "wage_source", 
               values_to = "daily_wage") %>%
  mutate(wage_source = case_when(
    wage_source == "dailywage_final_orig" ~ "BG Original",
    wage_source == "dailywage_final_pred" ~ "XGBoost V2 Predicted", 
    wage_source == "lz_true" ~ "LZ Uncensored"
  ))

p11_xgb_v2 <- ggplot(combined_wage_xgb_v2, aes(x = daily_wage, fill = wage_source)) +
  geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
  scale_fill_manual(values = c("BG Original" = "blue", 
                               "XGBoost V2 Predicted" = "red", 
                               "LZ Uncensored" = "darkgreen")) +
  labs(title = "XGBoost V2: BG Original vs XGBoost V2 vs LZ Uncensored",
       subtitle = "Model with expanded features",
       x = "ln Daily Wage (2000 Euros)",
       y = "Frequency",
       fill = "Wage Source") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(0, 7)

print(p11_xgb_v2)

# P12_xgb_v2: Three-way by censoring status
p12_xgb_v2 <- ggplot(combined_wage_xgb_v2, aes(x = daily_wage, fill = wage_source)) +
  geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
  scale_fill_manual(values = c("BG Original" = "blue", 
                               "XGBoost V2 Predicted" = "red", 
                               "LZ Uncensored" = "darkgreen")) +
  facet_wrap(~ifelse(was_imputed, "Censored Observations", "Uncensored Observations"), 
             scales = "free_y") +
  labs(title = "XGBoost V2: Daily Wage Distribution by Censoring Status",
       x = "ln Daily Wage (2000 Euros)",
       y = "Frequency",
       fill = "Wage Source") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(0, 7)

print(p12_xgb_v2)

# P13_xgb_v2: Density overlay
p13_xgb_v2 <- ggplot(combined_wage_xgb_v2, aes(x = daily_wage, color = wage_source)) +
  geom_density(size = 1.2, alpha = 0.7) +
  scale_color_manual(values = c("BG Original" = "blue", 
                                "XGBoost V2 Predicted" = "red", 
                                "LZ Uncensored" = "darkgreen")) +
  labs(title = "XGBoost V2: Density Overlay (BG vs XGBoost V2 vs LZ)",
       subtitle = "Model with expanded features",
       x = "ln Daily Wage (2000 Euros)",
       y = "Density",
       color = "Wage Source") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(0, 7)

print(p13_xgb_v2)

cat("\n=== XGBoost V2 Model with Expanded Features Complete ===\n")





#########################################
# REWORKED PLOTS:  DENSITY COMPARISONS FOR ALL METHODS
# Three plots: Uncensored, Censored, All observations
# Including:  Tobit, XGBoost V1, XGBoost V2, and LZ True Wages
#########################################

cat("\n=== Creating Comprehensive Density Comparison Plots ===\n")

# Step 1: Prepare combined dataset with all methods aligned

# Get Tobit predictions (already matched with LZ)
tobit_for_plots <- tobit_corrected %>%
  select(penr, benr, contryear, 
         tobit_pred = dailywage_final_pred,
         lz_true = lz_dwage_log,
         was_imputed) %>%
  mutate(obs_id = paste(penr, benr, contryear, sep = "_"))

# Get XGBoost V1 predictions with identifiers
# We need to reconstruct which observations these are from training_data
xgb_v1_for_plots <- data.frame(
  tobit_pred = NA,  # Not using Tobit here
  xgb_v1_pred = prediction_comparison$xgb_predicted,
  xgb_v2_pred = NA,  # Not using V2 here
  lz_true = prediction_comparison$lz_true,
  censored = censored_v1,
  method = "XGBoost V1"
) %>%
  mutate(was_imputed = (censored == 1))

# Get XGBoost V2 predictions with identifiers  
xgb_v2_for_plots <- data.frame(
  tobit_pred = NA,  # Not using Tobit here
  xgb_v1_pred = NA,  # Not using V1 here
  xgb_v2_pred = prediction_comparison_v2$xgb_predicted_v2,
  lz_true = prediction_comparison_v2$lz_true,
  censored = censored_v2,
  method = "XGBoost V2"
) %>%
  mutate(was_imputed = (censored == 1))

# Step 2: Create long format datasets for plotting

# For Tobit (all observations with LZ match)
tobit_long <- tobit_corrected %>%
  select(tobit_pred = dailywage_final_pred,
         lz_true = lz_dwage_log,
         was_imputed) %>%
  pivot_longer(cols = c(tobit_pred, lz_true),
               names_to = "method",
               values_to = "wage") %>%
  mutate(method = case_when(
    method == "tobit_pred" ~ "Tobit",
    method == "lz_true" ~ "LZ True Wages"
  ))

# For XGBoost V1
xgb_v1_long <- xgb_v1_for_plots %>%
  select(xgb_v1_pred, lz_true, was_imputed) %>%
  pivot_longer(cols = c(xgb_v1_pred, lz_true),
               names_to = "method",
               values_to = "wage") %>%
  mutate(method = case_when(
    method == "xgb_v1_pred" ~ "XGBoost V1",
    method == "lz_true" ~ "LZ True Wages"
  ))

# For XGBoost V2
xgb_v2_long <- xgb_v2_for_plots %>%
  select(xgb_v2_pred, lz_true, was_imputed) %>%
  pivot_longer(cols = c(xgb_v2_pred, lz_true),
               names_to = "method",
               values_to = "wage") %>%
  mutate(method = case_when(
    method == "xgb_v2_pred" ~ "XGBoost V2",
    method == "lz_true" ~ "LZ True Wages"
  ))

# Combine all methods
all_methods_combined <- bind_rows(tobit_long, xgb_v1_long, xgb_v2_long) %>%
  filter(is.finite(wage))

# Remove duplicate LZ entries (since each method adds LZ)
all_methods_combined <- all_methods_combined %>%
  group_by(wage, was_imputed, method) %>%
  slice(1) %>%
  ungroup()

cat("Combined dataset created with", nrow(all_methods_combined), "observations\n")

# Step 3: Create three density plots

# Define consistent color scheme for all plots
color_scheme <- c(
  "Tobit" = "purple",
  "XGBoost V1" = "red", 
  "XGBoost V2" = "orange",
  "LZ True Wages" = "darkgreen"
)

# PLOT 1: Uncensored Observations Only
cat("\nCreating Plot 1: Uncensored Observations\n")

data_uncensored <- all_methods_combined %>%
  filter(! was_imputed)

p_uncensored <- ggplot(data_uncensored, aes(x = wage, color = method, fill = method)) +
  geom_density(alpha = 0, size = 1.2) +
  scale_color_manual(values = color_scheme) +
  scale_fill_manual(values = color_scheme) +
  labs(title = "Wage Prediction Comparison: Uncensored Observations",
       subtitle = "All methods compared against LZ true wages",
       x = "ln Daily Wage (2000 Euros)",
       y = "Density",
       color = "Method",
       fill = "Method") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11)) +
  xlim(0, 7)

print(p_uncensored)

# PLOT 2: Censored Observations Only
cat("Creating Plot 2: Censored Observations\n")

data_censored <- all_methods_combined %>%
  filter(was_imputed)

p_censored <- ggplot(data_censored, aes(x = wage, color = method, fill = method)) +
  geom_density(alpha = 0, size = 1.2) +
  scale_color_manual(values = color_scheme) +
  scale_fill_manual(values = color_scheme) +
  labs(title = "Wage Prediction Comparison: Censored Observations",
       subtitle = "All methods compared against LZ true wages - Focus on imputation quality",
       x = "ln Daily Wage (2000 Euros)",
       y = "Density",
       color = "Method",
       fill = "Method") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11)) +
  xlim(4, 7)

print(p_censored)

# PLOT 3: All Observations
cat("Creating Plot 3: All Observations\n")

p_all <- ggplot(all_methods_combined, aes(x = wage, color = method, fill = method)) +
  geom_density(alpha = 0, size = 1.2) +
  scale_color_manual(values = color_scheme) +
  scale_fill_manual(values = color_scheme) +
  labs(title = "Wage Prediction Comparison: All Observations",
       subtitle = "All methods compared against LZ true wages",
       x = "ln Daily Wage (2000 Euros)",
       y = "Density",
       color = "Method",
       fill = "Method") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11)) +
  xlim(0, 7)

print(p_all)

# Step 4: Summary statistics for each plot
cat("\n=== Summary Statistics by Observation Type ===\n")

summary_by_type <- all_methods_combined %>%
  group_by(method, was_imputed) %>%
  summarise(
    n_obs = n(),
    mean_wage = round(mean(wage, na.rm = TRUE), 4),
    median_wage = round(median(wage, na.rm = TRUE), 4),
    sd_wage = round(sd(wage, na.rm = TRUE), 4),
    .groups = "drop"
  ) %>%
  mutate(obs_type = ifelse(was_imputed, "Censored", "Uncensored")) %>%
  select(method, obs_type, everything(), -was_imputed)

print(summary_by_type)

# Step 5: Save plots
cat("\n=== Saving Plots ===\n")







#########################################
# ERROR DISTRIBUTION COMPARISON:  TOBIT vs XGBoost V1 vs XGBoost V2
#########################################

cat("\n=== Creating Error Distribution Comparison Plot ===\n")

# OPTION 1: Compare only on CENSORED observations (where imputation matters)
cat("\n--- CENSORED OBSERVATIONS ONLY ---\n")

# Get censoring flags for the test sets
censored_v1 <- training_data$bg_censored_flag[-train_indices]
censored_v2 <- training_data$bg_censored_flag[-train_indices_v2]

# Tobit errors (censored only)
tobit_errors_censored <- imputation_eval %>%
  filter(was_imputed) %>%
  mutate(method = "Tobit") %>%
  select(error = residual, method)

# XGBoost V1 errors (censored only)
xgb_v1_errors_censored <- prediction_comparison %>%
  mutate(censored = censored_v1) %>%
  filter(censored == 1) %>%
  mutate(
    error = xgb_predicted - lz_true,
    method = "XGBoost V1"
  ) %>%
  select(error, method)

# XGBoost V2 errors (censored only)
xgb_v2_errors_censored <- prediction_comparison_v2 %>%
  mutate(censored = censored_v2) %>%
  filter(censored == 1) %>%
  mutate(
    error = xgb_predicted_v2 - lz_true,
    method = "XGBoost V2"
  ) %>%
  select(error, method)

# Combine censored-only errors
all_errors_censored <- bind_rows(tobit_errors_censored, xgb_v1_errors_censored, xgb_v2_errors_censored) %>%
  filter(is.finite(error))

# Summary for censored only
error_summary_censored <- all_errors_censored %>%
  group_by(method) %>%
  summarise(
    n_obs = n(),
    mean_error = round(mean(error), 4),
    median_error = round(median(error), 4),
    sd_error = round(sd(error), 4),
    mae = round(mean(abs(error)), 4),
    rmse = round(sqrt(mean(error^2)), 4),
    .groups = "drop"
  )

cat("\n=== Error Summary (CENSORED ONLY) ===\n")
print(error_summary_censored)

# Plot for censored only
p14_censored <- ggplot(all_errors_censored, aes(x = error, color = method, fill = method)) +
  geom_density(alpha = 0.3, size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
  scale_color_manual(values = c("Tobit" = "purple", 
                                "XGBoost V1" = "red", 
                                "XGBoost V2" = "orange")) +
  scale_fill_manual(values = c("Tobit" = "purple", 
                               "XGBoost V1" = "red", 
                               "XGBoost V2" = "orange")) +
  labs(title = "Error Distribution:  Tobit vs XGBoost V1 vs XGBoost V2",
       subtitle = "CENSORED observations only - Error = Predicted - LZ True Wage",
       x = "Prediction Error (ln wage units)",
       y = "Density",
       color = "Method",
       fill = "Method") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(-2, 2)

print(p14_censored)


# OPTION 2: Compare on ALL observations
cat("\n--- ALL OBSERVATIONS (CENSORED + UNCENSORED) ---\n")

# Tobit errors (all)
tobit_errors_all <- imputation_eval %>%
  mutate(method = "Tobit") %>%
  select(error = residual, method)

# XGBoost V1 errors (all)
xgb_v1_errors_all <- prediction_comparison %>%
  mutate(
    error = xgb_predicted - lz_true,
    method = "XGBoost V1"
  ) %>%
  select(error, method)

# XGBoost V2 errors (all)
xgb_v2_errors_all <- prediction_comparison_v2 %>%
  mutate(
    error = xgb_predicted_v2 - lz_true,
    method = "XGBoost V2"
  ) %>%
  select(error, method)

# Combine all errors
all_errors_all <- bind_rows(tobit_errors_all, xgb_v1_errors_all, xgb_v2_errors_all) %>%
  filter(is.finite(error))

# Summary for all
error_summary_all <- all_errors_all %>%
  group_by(method) %>%
  summarise(
    n_obs = n(),
    mean_error = round(mean(error), 4),
    median_error = round(median(error), 4),
    sd_error = round(sd(error), 4),
    mae = round(mean(abs(error)), 4),
    rmse = round(sqrt(mean(error^2)), 4),
    .groups = "drop"
  )

cat("\n=== Error Summary (ALL OBSERVATIONS) ===\n")
print(error_summary_all)

# Plot for all observations
p14_all <- ggplot(all_errors_all, aes(x = error, color = method, fill = method)) +
  geom_density(alpha = 0.3, size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
  scale_color_manual(values = c("Tobit" = "purple", 
                                "XGBoost V1" = "red", 
                                "XGBoost V2" = "orange")) +
  scale_fill_manual(values = c("Tobit" = "purple", 
                               "XGBoost V1" = "red", 
                               "XGBoost V2" = "orange")) +
  labs(title = "Error Distribution: Tobit vs XGBoost V1 vs XGBoost V2",
       subtitle = "ALL observations - Error = Predicted - LZ True Wage",
       x = "Prediction Error (ln wage units)",
       y = "Density",
       color = "Method",
       fill = "Method") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(-2, 2)

print(p14_all)

cat("\n=== Error Distribution Plots Complete ===\n")





#########################################
# CORRECTED COMPARISON: ALL METHODS vs LZ TRUE WAGES
#########################################

cat("\n=== Creating Corrected Comparison Against LZ Ground Truth ===\n")

# Step 1: Get LZ data (same processing as XGBoost training)
lz_comparison_data <- lohnzettel %>%
  filter(penr %in% hauptverband_2022_1994_until_2012_penr_sample_vec) %>%
  filter(year >= 1994, year <= 2012) %>%
  rename(contryear = year) %>%
  left_join(vpi_1986 %>% select(contryear, cpi_2000), by = "contryear") %>%
  mutate(
    lz_dwage_log = log(pmax(lz_dwage * cpi_2000, 1))
  ) %>%
  select(penr, benr, contryear, lz_dwage_log) %>%
  filter(!is.na(lz_dwage_log), is.finite(lz_dwage_log))

cat("LZ data available:", nrow(lz_comparison_data), "observations\n")

# Step 2: Merge Tobit results with LZ ground truth
tobit_corrected <- final_imputed_data %>%
  inner_join(lz_comparison_data, by = c("penr", "benr", "contryear")) %>%
  mutate(
    error_vs_lz = dailywage_final_pred - lz_dwage_log,
    error_vs_censored = dailywage_final_pred - dailywage_final_orig,
    was_imputed = (dailywage_final != dailywage_final_orig)
  ) %>%
  filter(is.finite(error_vs_lz))

cat("Tobit observations with LZ match:", nrow(tobit_corrected), "\n")

# Step 3: Summary statistics - Tobit vs LZ (corrected)
tobit_summary_corrected <- tobit_corrected %>%
  group_by(was_imputed) %>%
  summarise(
    method = "Tobit",
    n_obs = n(),
    mean_error = round(mean(error_vs_lz), 4),
    median_error = round(median(error_vs_lz), 4),
    sd_error = round(sd(error_vs_lz), 4),
    mae = round(mean(abs(error_vs_lz)), 4),
    rmse = round(sqrt(mean(error_vs_lz^2)), 4),
    .groups = "drop"
  )

cat("\n=== CORRECTED Tobit Error Summary (vs LZ True Wages) ===\n")
print(tobit_summary_corrected)

# Step 4: Compare old vs new Tobit RMSE
tobit_summary_old <- tobit_corrected %>%
  group_by(was_imputed) %>%
  summarise(
    method = "Tobit (OLD)",
    n_obs = n(),
    mean_error = round(mean(error_vs_censored), 4),
    median_error = round(median(error_vs_censored), 4),
    sd_error = round(sd(error_vs_censored), 4),
    mae = round(mean(abs(error_vs_censored)), 4),
    rmse = round(sqrt(mean(error_vs_censored^2)), 4),
    .groups = "drop"
  )

cat("\n=== OLD Tobit Error Summary (vs Censored BG - WRONG!) ===\n")
print(tobit_summary_old)

# Step 5: Prepare corrected error data for all methods

# Tobit errors (corrected - all observations with LZ)
tobit_errors_corrected_all <- tobit_corrected %>%
  mutate(method = "Tobit") %>%
  select(error = error_vs_lz, method, was_imputed)

# Tobit errors (corrected - censored only)
tobit_errors_corrected_censored <- tobit_corrected %>%
  filter(was_imputed) %>%
  mutate(method = "Tobit") %>%
  select(error = error_vs_lz, method, was_imputed)

# XGBoost V1 errors (all)
xgb_v1_errors_corrected_all <- prediction_comparison %>%
  mutate(
    error = xgb_predicted - lz_true,
    method = "XGBoost V1",
    was_imputed = NA
  ) %>%
  select(error, method, was_imputed)

# XGBoost V1 errors (censored only)
censored_v1 <- training_data$bg_censored_flag[-train_indices]
xgb_v1_errors_corrected_censored <- prediction_comparison %>%
  mutate(
    censored = censored_v1,
    error = xgb_predicted - lz_true,
    method = "XGBoost V1",
    was_imputed = (censored == 1)
  ) %>%
  filter(was_imputed) %>%
  select(error, method, was_imputed)

# XGBoost V2 errors (all)
xgb_v2_errors_corrected_all <- prediction_comparison_v2 %>%
  mutate(
    error = xgb_predicted_v2 - lz_true,
    method = "XGBoost V2",
    was_imputed = NA
  ) %>%
  select(error, method, was_imputed)

# XGBoost V2 errors (censored only)
censored_v2 <- training_data$bg_censored_flag[-train_indices_v2]
xgb_v2_errors_corrected_censored <- prediction_comparison_v2 %>%
  mutate(
    censored = censored_v2,
    error = xgb_predicted_v2 - lz_true,
    method = "XGBoost V2",
    was_imputed = (censored == 1)
  ) %>%
  filter(was_imputed) %>%
  select(error, method, was_imputed)

# Step 6: Combine all corrected errors

# All observations
all_errors_corrected <- bind_rows(
  tobit_errors_corrected_all,
  xgb_v1_errors_corrected_all,
  xgb_v2_errors_corrected_all
) %>%
  filter(is.finite(error))

# Censored only
censored_errors_corrected <- bind_rows(
  tobit_errors_corrected_censored,
  xgb_v1_errors_corrected_censored,
  xgb_v2_errors_corrected_censored
) %>%
  filter(is.finite(error))

# Step 7: Summary statistics - ALL METHODS (corrected, all observations)
cat("\n=== CORRECTED Error Summary:  ALL OBSERVATIONS ===\n")
error_summary_corrected_all <- all_errors_corrected %>%
  group_by(method) %>%
  summarise(
    n_obs = n(),
    mean_error = round(mean(error), 4),
    median_error = round(median(error), 4),
    sd_error = round(sd(error), 4),
    mae = round(mean(abs(error)), 4),
    rmse = round(sqrt(mean(error^2)), 4),
    .groups = "drop"
  )
print(error_summary_corrected_all)

# Step 8: Summary statistics - ALL METHODS (corrected, censored only)
cat("\n=== CORRECTED Error Summary:  CENSORED OBSERVATIONS ONLY ===\n")
error_summary_corrected_censored <- censored_errors_corrected %>%
  group_by(method) %>%
  summarise(
    n_obs = n(),
    mean_error = round(mean(error), 4),
    median_error = round(median(error), 4),
    sd_error = round(sd(error), 4),
    mae = round(mean(abs(error)), 4),
    rmse = round(sqrt(mean(error^2)), 4),
    .groups = "drop"
  )
print(error_summary_corrected_censored)

# Step 9: Create corrected plots

# Plot 1: All observations
p14_corrected_all <- ggplot(all_errors_corrected, aes(x = error, color = method, fill = method)) +
  geom_density(alpha = 0.3, size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
  scale_color_manual(values = c("Tobit" = "purple", 
                                "XGBoost V1" = "red", 
                                "XGBoost V2" = "orange")) +
  scale_fill_manual(values = c("Tobit" = "purple", 
                               "XGBoost V1" = "red", 
                               "XGBoost V2" = "orange")) +
  labs(title = "CORRECTED Error Distribution:  Tobit vs XGBoost V1 vs XGBoost V2",
       subtitle = "ALL observations - Error = Predicted - LZ True Wage",
       x = "Prediction Error (ln wage units)",
       y = "Density",
       color = "Method",
       fill = "Method") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold")) +
  xlim(-2, 2)

print(p14_corrected_all)

# Plot 2: Censored observations only
p14_corrected_censored <- ggplot(censored_errors_corrected, aes(x = error, color = method, fill = method)) +
  geom_density(alpha = 0.3, size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
  scale_color_manual(values = c("Tobit" = "purple", 
                                "XGBoost V1" = "red", 
                                "XGBoost V2" = "orange")) +
  scale_fill_manual(values = c("Tobit" = "purple", 
                               "XGBoost V1" = "red", 
                               "XGBoost V2" = "orange")) +
  labs(title = "CORRECTED Error Distribution:  Tobit vs XGBoost V1 vs XGBoost V2",
       subtitle = "CENSORED observations only - Error = Predicted - LZ True Wage",
       x = "Prediction Error (ln wage units)",
       y = "Density",
       color = "Method",
       fill = "Method") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold")) +
  xlim(-2, 2)

print(p14_corrected_censored)

# Step 10: Calculate improvement percentages
cat("\n=== CORRECTED Performance Comparison ===\n")

improvement_all <- error_summary_corrected_all %>%
  arrange(method) %>%
  mutate(
    rmse_improvement_vs_tobit = round((rmse[method == "Tobit"] - rmse) / rmse[method == "Tobit"] * 100, 1),
    mae_improvement_vs_tobit = round((mae[method == "Tobit"] - mae) / mae[method == "Tobit"] * 100, 1)
  )

cat("\nAll Observations - RMSE Improvement vs Tobit:\n")
print(improvement_all %>% select(method, rmse, rmse_improvement_vs_tobit, mae, mae_improvement_vs_tobit))

improvement_censored <- error_summary_corrected_censored %>%
  arrange(method) %>%
  mutate(
    rmse_improvement_vs_tobit = round((rmse[method == "Tobit"] - rmse) / rmse[method == "Tobit"] * 100, 1),
    mae_improvement_vs_tobit = round((mae[method == "Tobit"] - mae) / mae[method == "Tobit"] * 100, 1)
  )

cat("\nCensored Observations - RMSE Improvement vs Tobit:\n")
print(improvement_censored %>% select(method, rmse, rmse_improvement_vs_tobit, mae, mae_improvement_vs_tobit))

cat("\n=== Corrected Comparison Complete ===\n")
cat("Key Finding: Now all methods are evaluated against LZ ground truth!\n")





#########################################
# COMPREHENSIVE RMSE COMPARISON TABLE
#########################################

cat("\n=== Creating Comprehensive RMSE Comparison Table ===\n")

# Prepare comprehensive comparison table

# 1. Tobit vs BG (uncensored observations)
tobit_vs_bg_uncensored <- tobit_corrected %>%
  filter(! was_imputed) %>%
  summarise(
    Method = "Tobit",
    Target = "BG (censored data)",
    Observation_Type = "Uncensored",
    n_obs = n(),
    RMSE = round(sqrt(mean(error_vs_censored^2)), 4),
    MAE = round(mean(abs(error_vs_censored)), 4),
    Mean_Error = round(mean(error_vs_censored), 4),
    SD_Error = round(sd(error_vs_censored), 4)
  )

# 2. Tobit vs BG (censored observations)
tobit_vs_bg_censored <- tobit_corrected %>%
  filter(was_imputed) %>%
  summarise(
    Method = "Tobit",
    Target = "BG (censored data)",
    Observation_Type = "Censored",
    n_obs = n(),
    RMSE = round(sqrt(mean(error_vs_censored^2)), 4),
    MAE = round(mean(abs(error_vs_censored)), 4),
    Mean_Error = round(mean(error_vs_censored), 4),
    SD_Error = round(sd(error_vs_censored), 4)
  )

# 3. Tobit vs BG (all observations)
tobit_vs_bg_all <- tobit_corrected %>%
  summarise(
    Method = "Tobit",
    Target = "BG (censored data)",
    Observation_Type = "All",
    n_obs = n(),
    RMSE = round(sqrt(mean(error_vs_censored^2)), 4),
    MAE = round(mean(abs(error_vs_censored)), 4),
    Mean_Error = round(mean(error_vs_censored), 4),
    SD_Error = round(sd(error_vs_censored), 4)
  )

# 4. Tobit vs LZ (uncensored observations)
tobit_vs_lz_uncensored <- tobit_corrected %>%
  filter(!was_imputed) %>%
  summarise(
    Method = "Tobit",
    Target = "LZ (true wages)",
    Observation_Type = "Uncensored",
    n_obs = n(),
    RMSE = round(sqrt(mean(error_vs_lz^2)), 4),
    MAE = round(mean(abs(error_vs_lz)), 4),
    Mean_Error = round(mean(error_vs_lz), 4),
    SD_Error = round(sd(error_vs_lz), 4)
  )

# 5. Tobit vs LZ (censored observations)
tobit_vs_lz_censored <- tobit_corrected %>%
  filter(was_imputed) %>%
  summarise(
    Method = "Tobit",
    Target = "LZ (true wages)",
    Observation_Type = "Censored",
    n_obs = n(),
    RMSE = round(sqrt(mean(error_vs_lz^2)), 4),
    MAE = round(mean(abs(error_vs_lz)), 4),
    Mean_Error = round(mean(error_vs_lz), 4),
    SD_Error = round(sd(error_vs_lz), 4)
  )

# 6. Tobit vs LZ (all observations)
tobit_vs_lz_all <- tobit_corrected %>%
  summarise(
    Method = "Tobit",
    Target = "LZ (true wages)",
    Observation_Type = "All",
    n_obs = n(),
    RMSE = round(sqrt(mean(error_vs_lz^2)), 4),
    MAE = round(mean(abs(error_vs_lz)), 4),
    Mean_Error = round(mean(error_vs_lz), 4),
    SD_Error = round(sd(error_vs_lz), 4)
  )

# 7. XGBoost V1 (uncensored)
xgb_v1_uncensored <- prediction_comparison %>%
  mutate(censored = censored_v1) %>%
  filter(censored == 0) %>%
  summarise(
    Method = "XGBoost V1",
    Target = "LZ (true wages)",
    Observation_Type = "Uncensored",
    n_obs = n(),
    RMSE = round(sqrt(mean((xgb_predicted - lz_true)^2)), 4),
    MAE = round(mean(abs(xgb_predicted - lz_true)), 4),
    Mean_Error = round(mean(xgb_predicted - lz_true), 4),
    SD_Error = round(sd(xgb_predicted - lz_true), 4)
  )

# 8. XGBoost V1 (censored)
xgb_v1_censored <- prediction_comparison %>%
  mutate(censored = censored_v1) %>%
  filter(censored == 1) %>%
  summarise(
    Method = "XGBoost V1",
    Target = "LZ (true wages)",
    Observation_Type = "Censored",
    n_obs = n(),
    RMSE = round(sqrt(mean((xgb_predicted - lz_true)^2)), 4),
    MAE = round(mean(abs(xgb_predicted - lz_true)), 4),
    Mean_Error = round(mean(xgb_predicted - lz_true), 4),
    SD_Error = round(sd(xgb_predicted - lz_true), 4)
  )

# 9. XGBoost V1 (all)
xgb_v1_all <- prediction_comparison %>%
  summarise(
    Method = "XGBoost V1",
    Target = "LZ (true wages)",
    Observation_Type = "All",
    n_obs = n(),
    RMSE = round(sqrt(mean((xgb_predicted - lz_true)^2)), 4),
    MAE = round(mean(abs(xgb_predicted - lz_true)), 4),
    Mean_Error = round(mean(xgb_predicted - lz_true), 4),
    SD_Error = round(sd(xgb_predicted - lz_true), 4)
  )

# 10. XGBoost V2 (uncensored)
xgb_v2_uncensored <- prediction_comparison_v2 %>%
  mutate(censored = censored_v2) %>%
  filter(censored == 0) %>%
  summarise(
    Method = "XGBoost V2",
    Target = "LZ (true wages)",
    Observation_Type = "Uncensored",
    n_obs = n(),
    RMSE = round(sqrt(mean((xgb_predicted_v2 - lz_true)^2)), 4),
    MAE = round(mean(abs(xgb_predicted_v2 - lz_true)), 4),
    Mean_Error = round(mean(xgb_predicted_v2 - lz_true), 4),
    SD_Error = round(sd(xgb_predicted_v2 - lz_true), 4)
  )

# 11. XGBoost V2 (censored)
xgb_v2_censored <- prediction_comparison_v2 %>%
  mutate(censored = censored_v2) %>%
  filter(censored == 1) %>%
  summarise(
    Method = "XGBoost V2",
    Target = "LZ (true wages)",
    Observation_Type = "Censored",
    n_obs = n(),
    RMSE = round(sqrt(mean((xgb_predicted_v2 - lz_true)^2)), 4),
    MAE = round(mean(abs(xgb_predicted_v2 - lz_true)), 4),
    Mean_Error = round(mean(xgb_predicted_v2 - lz_true), 4),
    SD_Error = round(sd(xgb_predicted_v2 - lz_true), 4)
  )

# 12. XGBoost V2 (all)
xgb_v2_all <- prediction_comparison_v2 %>%
  summarise(
    Method = "XGBoost V2",
    Target = "LZ (true wages)",
    Observation_Type = "All",
    n_obs = n(),
    RMSE = round(sqrt(mean((xgb_predicted_v2 - lz_true)^2)), 4),
    MAE = round(mean(abs(xgb_predicted_v2 - lz_true)), 4),
    Mean_Error = round(mean(xgb_predicted_v2 - lz_true), 4),
    SD_Error = round(sd(xgb_predicted_v2 - lz_true), 4)
  )

# Combine all into one comprehensive table
comprehensive_rmse_table <- bind_rows(
  tobit_vs_bg_all,
  tobit_vs_bg_uncensored,
  tobit_vs_bg_censored,
  tobit_vs_lz_all,
  tobit_vs_lz_uncensored,
  tobit_vs_lz_censored,
  xgb_v1_all,
  xgb_v1_uncensored,
  xgb_v1_censored,
  xgb_v2_all,
  xgb_v2_uncensored,
  xgb_v2_censored
) %>%
  arrange(Observation_Type, Method, Target)

print(comprehensive_rmse_table, n = Inf)

write.csv(comprehensive_rmse_table, "comprehensive_rmse_comparison.csv", row.names = FALSE)



#########################################
# XGBOOST MODEL V3: WITH TEMPORAL AND GEOGRAPHIC FEATURES
# Changes from V2:
# - Remove: birthyear (redundant with age)
# - Add: contryear (temporal patterns)
# - Add: bundesland (regional effects)
# - Add: stsl (country of origin effects)
#########################################

cat("\n=== Training XGBoost Model V3 with Temporal and Geographic Features ===\n")

# First, ensure bundesland and stsl are in the training data
training_data_v3 <- training_data %>%
  # bundesland should already be there from the merge, but let's verify
  # stsl should also be there
  select(penr, benr, contryear, lz_dwage_log, bg_dwage_log, bg_censored_flag,
         age, mean_wage, mean_censored, firm_size, firm_size_square,
         oneyear_dummy, oneperson_dummy, smallfirm_dummy,
         mean_wage_coworkers, mean_censored_coworkers,
         frau, num_years, interval_lower, interval_upper,
         bundesland, stsl) %>%
  # Handle any missing values in the new variables
  mutate(
    bundesland = ifelse(is.na(bundesland), 0, bundesland),
    stsl = ifelse(is.na(stsl), 0, stsl)
  )

cat("Training data V3 prepared with bundesland and stsl\n")
cat("Bundesland unique values:", length(unique(training_data_v3$bundesland)), "\n")
cat("STSL unique values:", length(unique(training_data_v3$stsl)), "\n")

# V3 feature set
feature_cols_v3 <- c(
  # Core features (matching Tobit)
  "age",
  "mean_wage",
  "mean_censored",
  "firm_size",
  "firm_size_square",
  "oneyear_dummy",
  "oneperson_dummy",
  "smallfirm_dummy",
  "mean_wage_coworkers",
  "mean_censored_coworkers",
  
  # Additional features from V2
  "frau",
  "num_years",
  "interval_lower",
  "interval_upper",
  "bg_censored_flag",
  
  # NEW V3 features
  "contryear",      # Temporal dimension (replacing birthyear)
  "bundesland",     # Regional effects
  "stsl"            # Country of origin effects
)

cat("\nV3 Feature Set:\n")
cat("Total features:", length(feature_cols_v3), "\n")
cat("New features:", setdiff(feature_cols_v3, feature_cols_v2), "\n")
cat("Removed features:", setdiff(feature_cols_v2, feature_cols_v3), "\n")

# Prepare features for XGBoost
# For categorical variables, we'll let XGBoost handle them
X_features_v3 <- training_data_v3 %>%
  select(all_of(feature_cols_v3)) %>%
  mutate(across(everything(), ~ifelse(is.na(.) | is.infinite(.), 0, .))) %>%
  as.matrix()

# Target remains the same (LZ true wage)
y_target_v3 <- training_data_v3$lz_dwage_log

cat("Features available for training V3:", ncol(X_features_v3), "\n")

# Train/test split (same seed for comparison)
set.seed(123)
n_obs_v3 <- length(y_target_v3)
train_indices_v3 <- sample(seq_len(n_obs_v3), size = floor(0.8 * n_obs_v3))

X_train_v3 <- X_features_v3[train_indices_v3, ]
X_test_v3 <- X_features_v3[-train_indices_v3, ]
y_train_v3 <- y_target_v3[train_indices_v3]
y_test_v3 <- y_target_v3[-train_indices_v3]

# Keep BG wages for comparison
bg_train_v3 <- training_data_v3$bg_dwage_log[train_indices_v3]
bg_test_v3 <- training_data_v3$bg_dwage_log[-train_indices_v3]

# Mark categorical features for XGBoost
# Find column indices for categorical variables
bundesland_col <- which(colnames(X_features_v3) == "bundesland") - 1  # 0-indexed
stsl_col <- which(colnames(X_features_v3) == "stsl") - 1  # 0-indexed

dtrain_v3 <- xgb.DMatrix(data = X_train_v3, label = y_train_v3)
dtest_v3 <- xgb.DMatrix(data = X_test_v3, label = y_test_v3)

cat("Training set size:", nrow(X_train_v3), "Test set size:", nrow(X_test_v3), "\n")

# Train XGBoost model V3 with categorical features
xgb_params_v3 <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  reg_alpha = 0.1,
  reg_lambda = 1,
  seed = 123
)

cat("Training XGBoost V3 model (with temporal and geographic features)...\n")
xgb_cv_v3 <- xgb.cv(
  params = xgb_params_v3,
  data = dtrain_v3,
  nrounds = 1000,
  nfold = 5,
  early_stopping_rounds = 50,
  verbose = FALSE
)

best_nrounds_v3 <- xgb_cv_v3$best_iteration
cat("Optimal rounds V3:", best_nrounds_v3, "Best CV RMSE:", min(xgb_cv_v3$evaluation_log$test_rmse_mean), "\n")

# Train final model V3
xgb_model_v3 <- xgb.train(
  params = xgb_params_v3,
  data = dtrain_v3,
  nrounds = best_nrounds_v3,
  watchlist = list(train = dtrain_v3, test = dtest_v3),
  verbose = FALSE
)

# Evaluate model performance V3
train_pred_lz_v3 <- predict(xgb_model_v3, dtrain_v3)
test_pred_lz_v3 <- predict(xgb_model_v3, dtest_v3)

# Compare against actual LZ wages
train_rmse_lz_v3 <- sqrt(mean((train_pred_lz_v3 - y_train_v3)^2))
test_rmse_lz_v3 <- sqrt(mean((test_pred_lz_v3 - y_test_v3)^2))
train_r2_lz_v3 <- cor(train_pred_lz_v3, y_train_v3)^2
test_r2_lz_v3 <- cor(test_pred_lz_v3, y_test_v3)^2

# Compare against BG wages
train_rmse_bg_v3 <- sqrt(mean((bg_train_v3 - y_train_v3)^2))
test_rmse_bg_v3 <- sqrt(mean((bg_test_v3 - y_test_v3)^2))
train_r2_bg_v3 <- cor(bg_train_v3, y_train_v3)^2
test_r2_bg_v3 <- cor(bg_test_v3, y_test_v3)^2

cat("\n=== Model V3 Performance: Predicting True Wage (LZ) ===\n")
cat("XGBoost V3 vs LZ True Wage:\n")
cat("  Training RMSE:", round(train_rmse_lz_v3, 4), "R²:", round(train_r2_lz_v3, 4), "\n")
cat("  Test RMSE:", round(test_rmse_lz_v3, 4), "R²:", round(test_r2_lz_v3, 4), "\n")

cat("\nBG vs LZ True Wage (current method):\n")
cat("  Training RMSE:", round(train_rmse_bg_v3, 4), "R²:", round(train_r2_bg_v3, 4), "\n")
cat("  Test RMSE:", round(test_rmse_bg_v3, 4), "R²:", round(test_r2_bg_v3, 4), "\n")

cat("\nImprovement from XGBoost V3:\n")
cat("  RMSE reduction:", round(test_rmse_bg_v3 - test_rmse_lz_v3, 4), "\n")
cat("  R² improvement:", round(test_r2_lz_v3 - test_r2_bg_v3, 4), "\n")

# Compare V2 vs V3
cat("\n=== Comparison: V2 vs V3 ===\n")
cat("V2 Test RMSE:", round(test_rmse_lz_v2, 4), "R²:", round(test_r2_lz_v2, 4), "\n")
cat("V3 Test RMSE:", round(test_rmse_lz_v3, 4), "R²:", round(test_r2_lz_v3, 4), "\n")
cat("Improvement V3 over V2:\n")
cat("  RMSE reduction:", round(test_rmse_lz_v2 - test_rmse_lz_v3, 4), "\n")
cat("  R² improvement:", round(test_r2_lz_v3 - test_r2_lz_v2, 4), "\n")

if(test_rmse_lz_v3 < test_rmse_lz_v2) {
  cat("✓ V3 with temporal/geographic features performs BETTER\n")
  improvement_pct <- round((test_rmse_lz_v2 - test_rmse_lz_v3) / test_rmse_lz_v2 * 100, 2)
  cat("  Improvement: ", improvement_pct, "%\n", sep = "")
} else {
  cat("✗ V2 performs BETTER\n")
}

# Feature importance V3
importance_matrix_v3 <- xgb.importance(feature_names = colnames(X_features_v3), model = xgb_model_v3)
cat("\n=== Top 20 Features for Predicting True Wage (V3) ===\n")
print(head(importance_matrix_v3, 20))

# Highlight importance of new features
new_features_importance <- importance_matrix_v3 %>%
  filter(Feature %in% c("contryear", "bundesland", "stsl"))
cat("\n=== Importance of NEW V3 Features ===\n")
print(new_features_importance)

# Analysis of predictions V3
censored_v3 <- training_data_v3$bg_censored_flag[-train_indices_v3]

prediction_comparison_v3 <- data.frame(
  lz_true = y_test_v3,
  bg_observed = bg_test_v3,
  xgb_predicted_v3 = test_pred_lz_v3,
  censored = censored_v3,
  
  bg_error = abs(bg_test_v3 - y_test_v3),
  xgb_error_v3 = abs(test_pred_lz_v3 - y_test_v3),
  improvement_v3 = abs(bg_test_v3 - y_test_v3) - abs(test_pred_lz_v3 - y_test_v3)
) %>%
  mutate(
    bg_better = improvement_v3 < 0,
    xgb_better = improvement_v3 > 0,
    bg_pct_error = abs((bg_observed - lz_true) / lz_true) * 100,
    xgb_pct_error_v3 = abs((xgb_predicted_v3 - lz_true) / lz_true) * 100
  )

cat("\n=== Prediction Quality Comparison V3 ===\n")
cat("Mean Absolute Error:\n")
cat("  BG method:", round(mean(prediction_comparison_v3$bg_error), 4), "\n")
cat("  XGBoost V3 method:", round(mean(prediction_comparison_v3$xgb_error_v3), 4), "\n")
cat("  Improvement:", round(mean(prediction_comparison_v3$improvement_v3), 4), "\n")

cat("Relative Improvement:", round(1 - mean(prediction_comparison_v3$xgb_error_v3) / mean(prediction_comparison_v3$bg_error), 4), "\n")

cat("\nPercentage of cases where XGBoost V3 is better:", 
    round(mean(prediction_comparison_v3$xgb_better) * 100, 1), "%\n")

# Compare by censoring status
cat("\n=== V3 Performance by Censoring Status ===\n")
v3_by_censoring <- prediction_comparison_v3 %>%
  group_by(censored) %>%
  summarise(
    n_obs = n(),
    mae_bg = round(mean(bg_error), 4),
    mae_xgb_v3 = round(mean(xgb_error_v3), 4),
    improvement = round(mean(improvement_v3), 4),
    pct_better = round(mean(xgb_better) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(censoring_status = ifelse(censored == 1, "Censored", "Uncensored"))

print(v3_by_censoring)

#########################################
# PLOTS FOR V3 MODEL
#########################################

cat("\n=== Creating V3 Plots ===\n")

# Prepare data for plotting
combined_data_xgb_v3 <- bind_rows(
  data.frame(
    dailywage_final_orig = bg_train_v3,
    dailywage_final_pred = train_pred_lz_v3,
    lz_true = y_train_v3,
    censored = training_data_v3$bg_censored_flag[train_indices_v3]
  ),
  data.frame(
    dailywage_final_orig = prediction_comparison_v3$bg_observed,
    dailywage_final_pred = prediction_comparison_v3$xgb_predicted_v3,
    lz_true = prediction_comparison_v3$lz_true,
    censored = censored_v3
  )
) %>%
  mutate(was_imputed = censored == 1) %>%
  filter(is.finite(dailywage_final_orig), 
         is.finite(dailywage_final_pred),
         is.finite(lz_true))

# P1_xgb_v3: BG Original vs XGBoost V3 Predicted
wage_comparison_xgb_v3 <- combined_data_xgb_v3 %>%
  pivot_longer(cols = c(dailywage_final_orig, dailywage_final_pred),
               names_to = "wage_type", 
               values_to = "log_wage") %>%
  mutate(wage_type = ifelse(wage_type == "dailywage_final_orig", 
                            "BG Original", "XGBoost V3 Predicted"))

p1_xgb_v3 <- ggplot(wage_comparison_xgb_v3, aes(x = log_wage, fill = wage_type)) +
  geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
  scale_fill_manual(values = c("BG Original" = "blue", "XGBoost V3 Predicted" = "red")) +
  labs(title = "XGBoost V3: Distribution Comparison (BG Original vs XGBoost V3)",
       subtitle = "Model with temporal and geographic features",
       x = "ln Daily Wage (2000 Euros)",
       y = "Frequency",
       fill = "Wage Source") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(0, 7)

print(p1_xgb_v3)

# P2_xgb_v3: By censoring status
p2_xgb_v3 <- ggplot(wage_comparison_xgb_v3, aes(x = log_wage, fill = wage_type)) +
  geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
  scale_fill_manual(values = c("BG Original" = "blue", "XGBoost V3 Predicted" = "red")) +
  facet_wrap(~ifelse(was_imputed, "Censored Observations", "Uncensored Observations"), 
             scales = "free_y") +
  labs(title = "XGBoost V3: Wage Distribution by Censoring Status",
       x = "ln Daily Wage (2000 Euros)",
       y = "Frequency",
       fill = "Wage Source") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(0, 7)

print(p2_xgb_v3)

# P11_xgb_v3: Three-way comparison
combined_wage_xgb_v3 <- combined_data_xgb_v3 %>%
  pivot_longer(cols = c(dailywage_final_orig, dailywage_final_pred, lz_true),
               names_to = "wage_source", 
               values_to = "daily_wage") %>%
  mutate(wage_source = case_when(
    wage_source == "dailywage_final_orig" ~ "BG Original",
    wage_source == "dailywage_final_pred" ~ "XGBoost V3 Predicted", 
    wage_source == "lz_true" ~ "LZ Uncensored"
  ))

p11_xgb_v3 <- ggplot(combined_wage_xgb_v3, aes(x = daily_wage, fill = wage_source)) +
  geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
  scale_fill_manual(values = c("BG Original" = "blue", 
                               "XGBoost V3 Predicted" = "red", 
                               "LZ Uncensored" = "darkgreen")) +
  labs(title = "XGBoost V3: BG Original vs XGBoost V3 vs LZ Uncensored",
       subtitle = "Model with temporal and geographic features",
       x = "ln Daily Wage (2000 Euros)",
       y = "Frequency",
       fill = "Wage Source") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(0, 7)

print(p11_xgb_v3)

# P12_xgb_v3: Three-way by censoring status
p12_xgb_v3 <- ggplot(combined_wage_xgb_v3, aes(x = daily_wage, fill = wage_source)) +
  geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
  scale_fill_manual(values = c("BG Original" = "blue", 
                               "XGBoost V3 Predicted" = "red", 
                               "LZ Uncensored" = "darkgreen")) +
  facet_wrap(~ifelse(was_imputed, "Censored Observations", "Uncensored Observations"), 
             scales = "free_y") +
  labs(title = "XGBoost V3: Daily Wage Distribution by Censoring Status",
       x = "ln Daily Wage (2000 Euros)",
       y = "Frequency",
       fill = "Wage Source") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(0, 7)

print(p12_xgb_v3)

# P13_xgb_v3: Density overlay
p13_xgb_v3 <- ggplot(combined_wage_xgb_v3, aes(x = daily_wage, color = wage_source)) +
  geom_density(size = 1.2, alpha = 0.7) +
  scale_color_manual(values = c("BG Original" = "blue", 
                                "XGBoost V3 Predicted" = "red", 
                                "LZ Uncensored" = "darkgreen")) +
  labs(title = "XGBoost V3: Density Overlay (BG vs XGBoost V3 vs LZ)",
       subtitle = "Model with temporal and geographic features",
       x = "ln Daily Wage (2000 Euros)",
       y = "Density",
       color = "Wage Source") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(0, 7)

print(p13_xgb_v3)

#########################################
# V3 ERROR DISTRIBUTION AND COMPARISON
#########################################

cat("\n=== Creating V3 Error Comparison ===\n")

# V3 errors (all observations)
xgb_v3_errors_all <- prediction_comparison_v3 %>%
  mutate(
    error = xgb_predicted_v3 - lz_true,
    method = "XGBoost V3"
  ) %>%
  select(error, method, censored) %>%
  mutate(was_imputed = (censored == 1))

# V3 errors (censored only)
xgb_v3_errors_censored <- xgb_v3_errors_all %>%
  filter(was_imputed)

# Add V3 to comprehensive comparison (censored only)
censored_errors_with_v3 <- bind_rows(
  censored_errors_corrected,
  xgb_v3_errors_censored %>% select(error, method, was_imputed)
) %>%
  filter(is.finite(error))

# Plot: Censored observations with V3
p14_with_v3_censored <- ggplot(censored_errors_with_v3, aes(x = error, color = method, fill = method)) +
  geom_density(alpha = 0.3, size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
  scale_color_manual(values = c("Tobit" = "purple", 
                                "XGBoost V1" = "red", 
                                "XGBoost V2" = "orange",
                                "XGBoost V3" = "darkblue")) +
  scale_fill_manual(values = c("Tobit" = "purple", 
                               "XGBoost V1" = "red", 
                               "XGBoost V2" = "orange",
                               "XGBoost V3" = "darkblue")) +
  labs(title = "Error Distribution: All Methods Including V3",
       subtitle = "CENSORED observations only - Error = Predicted - LZ True Wage",
       x = "Prediction Error (ln wage units)",
       y = "Density",
       color = "Method",
       fill = "Method") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold")) +
  xlim(-2, 2)

print(p14_with_v3_censored)

# Add V3 to comprehensive comparison (all observations)
all_errors_with_v3 <- bind_rows(
  all_errors_corrected,
  xgb_v3_errors_all %>% select(error, method)
) %>%
  filter(is.finite(error))

# Plot: All observations with V3
p14_with_v3_all <- ggplot(all_errors_with_v3, aes(x = error, color = method, fill = method)) +
  geom_density(alpha = 0.3, size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
  scale_color_manual(values = c("Tobit" = "purple", 
                                "XGBoost V1" = "red", 
                                "XGBoost V2" = "orange",
                                "XGBoost V3" = "darkblue")) +
  scale_fill_manual(values = c("Tobit" = "purple", 
                               "XGBoost V1" = "red", 
                               "XGBoost V2" = "orange",
                               "XGBoost V3" = "darkblue")) +
  labs(title = "Error Distribution: All Methods Including V3",
       subtitle = "ALL observations - Error = Predicted - LZ True Wage",
       x = "Prediction Error (ln wage units)",
       y = "Density",
       color = "Method",
       fill = "Method") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold")) +
  xlim(-2, 2)

print(p14_with_v3_all)

# Summary statistics with V3 (censored only)
error_summary_with_v3_censored <- censored_errors_with_v3 %>%
  group_by(method) %>%
  summarise(
    n_obs = n(),
    mean_error = round(mean(error), 4),
    median_error = round(median(error), 4),
    sd_error = round(sd(error), 4),
    mae = round(mean(abs(error)), 4),
    rmse = round(sqrt(mean(error^2)), 4),
    .groups = "drop"
  )

cat("\n=== Error Summary WITH V3 (CENSORED ONLY) ===\n")
print(error_summary_with_v3_censored)

# Summary statistics with V3 (all observations)
error_summary_with_v3_all <- all_errors_with_v3 %>%
  group_by(method) %>%
  summarise(
    n_obs = n(),
    mean_error = round(mean(error), 4),
    median_error = round(median(error), 4),
    sd_error = round(sd(error), 4),
    mae = round(mean(abs(error)), 4),
    rmse = round(sqrt(mean(error^2)), 4),
    .groups = "drop"
  )

cat("\n=== Error Summary WITH V3 (ALL OBSERVATIONS) ===\n")
print(error_summary_with_v3_all)

# Performance ranking
cat("\n=== FINAL PERFORMANCE RANKING (CENSORED OBSERVATIONS) ===\n")
ranking_censored <- error_summary_with_v3_censored %>%
  arrange(rmse) %>%
  mutate(
    rank = row_number(),
    improvement_vs_worst = round((max(rmse) - rmse) / max(rmse) * 100, 1)
  )
print(ranking_censored)

cat("\n=== FINAL PERFORMANCE RANKING (ALL OBSERVATIONS) ===\n")
ranking_all <- error_summary_with_v3_all %>%
  arrange(rmse) %>%
  mutate(
    rank = row_number(),
    improvement_vs_worst = round((max(rmse) - rmse) / max(rmse) * 100, 1)
  )
print(ranking_all)

cat("\n=== XGBoost V3 Model with Temporal and Geographic Features Complete ===\n")
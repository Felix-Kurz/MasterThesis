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
       x = "Daily Wage (2000 Euros)",
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
       x = "Daily Wage (2000 Euros)",
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
       x = "Daily Wage (2000 Euros)",
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

# Step 2: Features for training (things we'll have in future from BG data)
# These should be features derivable from BG data alone
feature_cols <- c(
  # Individual characteristics (available from admin data)
  "age", "birthyear", "frau", 
  
  # Firm characteristics (available from BG data)
  "firm_size", "firm_size_square", "oneperson_dummy", "smallfirm_dummy",
  
  # Person history (available from BG data)
  "num_years", "oneyear_dummy", "mean_censored",
  
  # BG-derived wage information (censored but available)
  "interval_lower", "interval_upper", "bg_censored_flag",
  
  # Coworker characteristics (from BG data)
  "mean_censored_coworkers", "firm_size"
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
# SEGMENTED XGBOOST MODELS
# Approach: Train separate models for censored vs uncensored observations
#########################################

# Use the same training data from previous code
# training_data should already exist with both LZ and BG

cat("\n=== SEGMENTED MODELING APPROACH ===\n")
cat("Training separate models for censored and uncensored observations\n\n")

# Split training data by censoring status
training_censored <- training_data %>%
  filter(bg_censored_flag == 1)

training_uncensored <- training_data %>%
  filter(bg_censored_flag == 0)

cat("Censored observations:", nrow(training_censored), "\n")
cat("Uncensored observations:", nrow(training_uncensored), "\n")

# Feature columns (same as before)
feature_cols <- c(
  "age", "birthyear", "frau", 
  "firm_size", "firm_size_square", "oneperson_dummy", "smallfirm_dummy",
  "num_years", "oneyear_dummy", "mean_censored",
  "interval_lower", "interval_upper",
  "mean_censored_coworkers", "firm_size"
)

#########################################
# MODEL 1: For CENSORED observations
#########################################

cat("\n--- Training Model 1: CENSORED observations ---\n")

X_censored <- training_censored %>%
  select(all_of(feature_cols)) %>%
  mutate(across(everything(), ~ifelse(is.na(.) | is.infinite(.), 0, .))) %>%
  as.matrix()

y_censored <- training_censored$lz_dwage_log

# Train/test split for censored
set.seed(123)
n_censored <- length(y_censored)
train_idx_cens <- sample(seq_len(n_censored), size = floor(0.8 * n_censored))

X_train_cens <- X_censored[train_idx_cens, ]
X_test_cens <- X_censored[-train_idx_cens, ]
y_train_cens <- y_censored[train_idx_cens]
y_test_cens <- y_censored[-train_idx_cens]

dtrain_cens <- xgb.DMatrix(data = X_train_cens, label = y_train_cens)
dtest_cens <- xgb.DMatrix(data = X_test_cens, label = y_test_cens)

# Train censored model
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

xgb_cv_cens <- xgb.cv(
  params = xgb_params,
  data = dtrain_cens,
  nrounds = 1000,
  nfold = 5,
  early_stopping_rounds = 50,
  verbose = FALSE
)

best_rounds_cens <- xgb_cv_cens$best_iteration
cat("Optimal rounds (censored):", best_rounds_cens, "\n")

model_censored <- xgb.train(
  params = xgb_params,
  data = dtrain_cens,
  nrounds = best_rounds_cens,
  watchlist = list(train = dtrain_cens, test = dtest_cens),
  verbose = FALSE
)

#########################################
# MODEL 2: For UNCENSORED observations
#########################################

cat("\n--- Training Model 2: UNCENSORED observations ---\n")

X_uncensored <- training_uncensored %>%
  select(all_of(feature_cols)) %>%
  mutate(across(everything(), ~ifelse(is.na(.) | is.infinite(.), 0, .))) %>%
  as.matrix()

y_uncensored <- training_uncensored$lz_dwage_log

# Train/test split for uncensored
set.seed(124)  # Different seed for independence
n_uncensored <- length(y_uncensored)
train_idx_uncens <- sample(seq_len(n_uncensored), size = floor(0.8 * n_uncensored))

X_train_uncens <- X_uncensored[train_idx_uncens, ]
X_test_uncens <- X_uncensored[-train_idx_uncens, ]
y_train_uncens <- y_uncensored[train_idx_uncens]
y_test_uncens <- y_uncensored[-train_idx_uncens]

dtrain_uncens <- xgb.DMatrix(data = X_train_uncens, label = y_train_uncens)
dtest_uncens <- xgb.DMatrix(data = X_test_uncens, label = y_test_uncens)

# Train uncensored model
xgb_cv_uncens <- xgb.cv(
  params = xgb_params,
  data = dtrain_uncens,
  nrounds = 1000,
  nfold = 5,
  early_stopping_rounds = 50,
  verbose = FALSE
)

best_rounds_uncens <- xgb_cv_uncens$best_iteration
cat("Optimal rounds (uncensored):", best_rounds_uncens, "\n")

model_uncensored <- xgb.train(
  params = xgb_params,
  data = dtrain_uncens,
  nrounds = best_rounds_uncens,
  watchlist = list(train = dtrain_uncens, test = dtest_uncens),
  verbose = FALSE
)

#########################################
# EVALUATE BOTH APPROACHES
#########################################

cat("\n=== PERFORMANCE COMPARISON ===\n")

# Single model performance (from previous code - should already be computed)
# Assuming xgb_model exists from previous code

# Segmented models performance
pred_train_cens <- predict(model_censored, dtrain_cens)
pred_test_cens <- predict(model_censored, dtest_cens)
pred_train_uncens <- predict(model_uncensored, dtrain_uncens)
pred_test_uncens <- predict(model_uncensored, dtest_uncens)

# Metrics for censored observations
rmse_test_cens <- sqrt(mean((pred_test_cens - y_test_cens)^2))
r2_test_cens <- cor(pred_test_cens, y_test_cens)^2
mae_test_cens <- mean(abs(pred_test_cens - y_test_cens))

# Metrics for uncensored observations
rmse_test_uncens <- sqrt(mean((pred_test_uncens - y_test_uncens)^2))
r2_test_uncens <- cor(pred_test_uncens, y_test_uncens)^2
mae_test_uncens <- mean(abs(pred_test_uncens - y_test_uncens))

cat("\n--- Segmented Models Performance ---\n")
cat("CENSORED model:\n")
cat("  Test RMSE:", round(rmse_test_cens, 4), "R²:", round(r2_test_cens, 4), 
    "MAE:", round(mae_test_cens, 4), "\n")

cat("UNCENSORED model:\n")
cat("  Test RMSE:", round(rmse_test_uncens, 4), "R²:", round(r2_test_uncens, 4), 
    "MAE:", round(mae_test_uncens, 4), "\n")

# Combined weighted performance
n_test_total <- length(y_test_cens) + length(y_test_uncens)
weight_cens <- length(y_test_cens) / n_test_total
weight_uncens <- length(y_test_uncens) / n_test_total

weighted_rmse <- weight_cens * rmse_test_cens + weight_uncens * rmse_test_uncens
weighted_mae <- weight_cens * mae_test_cens + weight_uncens * mae_test_uncens

cat("\nWeighted average performance:\n")
cat("  RMSE:", round(weighted_rmse, 4), "\n")
cat("  MAE:", round(weighted_mae, 4), "\n")

# If single model exists, compare
if(exists("test_rmse_lz") && exists("test_pred_lz")) {
  cat("\n--- Comparison with Single Model ---\n")
  cat("Single model test RMSE:", round(test_rmse_lz, 4), "\n")
  cat("Segmented models weighted RMSE:", round(weighted_rmse, 4), "\n")
  cat("Improvement:", round(test_rmse_lz - weighted_rmse, 4), "\n")
  
  if(weighted_rmse < test_rmse_lz) {
    cat("✓ Segmented approach is BETTER\n")
  } else {
    cat("✗ Single model performs BETTER\n")
  }
}

#########################################
# FEATURE IMPORTANCE COMPARISON
#########################################

cat("\n=== FEATURE IMPORTANCE COMPARISON ===\n")

importance_censored <- xgb.importance(feature_names = colnames(X_censored), 
                                      model = model_censored)
importance_uncensored <- xgb.importance(feature_names = colnames(X_uncensored), 
                                        model = model_uncensored)

cat("\nTop 5 features for CENSORED observations:\n")
print(head(importance_censored, 5))

cat("\nTop 5 features for UNCENSORED observations:\n")
print(head(importance_uncensored, 5))

# Compare feature rankings
feature_comparison <- importance_censored %>%
  select(Feature, Gain_censored = Gain) %>%
  full_join(
    importance_uncensored %>% select(Feature, Gain_uncensored = Gain),
    by = "Feature"
  ) %>%
  mutate(
    Gain_censored = ifelse(is.na(Gain_censored), 0, Gain_censored),
    Gain_uncensored = ifelse(is.na(Gain_uncensored), 0, Gain_uncensored),
    Gain_difference = Gain_censored - Gain_uncensored
  ) %>%
  arrange(desc(abs(Gain_difference)))

cat("\nFeatures with largest importance differences:\n")
print(head(feature_comparison, 5))

#########################################
# PREDICTION FUNCTION FOR SEGMENTED APPROACH
#########################################

predict_true_wage_segmented <- function(bg_data, 
                                        model_cens = model_censored,
                                        model_uncens = model_uncensored,
                                        feature_columns = feature_cols) {
  # Prepare features
  prediction_features <- bg_data %>%
    select(all_of(feature_columns)) %>%
    mutate(across(everything(), ~ifelse(is.na(.) | is.infinite(.), 0, .))) %>%
    as.matrix()
  
  # Identify which observations are censored
  censored_flag <- bg_data$bg_censored_flag
  
  # Predict using appropriate model
  predicted_wages <- numeric(nrow(bg_data))
  
  if(sum(censored_flag == 1) > 0) {
    cens_idx <- which(censored_flag == 1)
    predicted_wages[cens_idx] <- predict(
      model_cens, 
      xgb.DMatrix(data = prediction_features[cens_idx, , drop = FALSE])
    )
  }
  
  if(sum(censored_flag == 0) > 0) {
    uncens_idx <- which(censored_flag == 0)
    predicted_wages[uncens_idx] <- predict(
      model_uncens,
      xgb.DMatrix(data = prediction_features[uncens_idx, , drop = FALSE])
    )
  }
  
  return(predicted_wages)
}

cat("\n=== Segmented models ready for predictions ===\n")
cat("Use predict_true_wage_segmented(bg_data) for new predictions\n")
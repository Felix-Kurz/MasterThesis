setwd("~/EBA/Forschungsprojekt")
data_250k <- read.csv("model_output_v5_temporal_plus_wage_lvl_250k/corrected_error_summary_with_bg_baseline_v5.csv")
data_250k_seed123_wo <- read.csv("model_output_v5_temporal_250k_data_seed123_wo_tobit_interval/corrected_error_summary_with_bg_baseline_v5.csv")
data_250k_seed123 <- read.csv("model_output_v5_temporal_250k_data_seed123/corrected_error_summary_with_bg_baseline_v5.csv")
data_250k

data_250k_seed123

data_250k_seed123_wo

wage_data <- read.csv("model_output_v5_250k/lz_wage_diagnostics/lz_wage_summary_overall.csv")
wage_data

wage_data_by_censoring_status <- read.csv("model_output_v5_250k/lz_wage_diagnostics/lz_wage_summary_by_censor.csv")
wage_data_by_censoring_status

data_temp_slit_seed123 <- read.csv("model_output_v5_temporal_250k_data_seed123/temporal_split_rmse_v5.csv")
data_temp_slit_seed123

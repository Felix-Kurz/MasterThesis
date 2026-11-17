e_default <- new.env()
e_200k <- new.env()
e_250k <- new.env()

# Load pre_model_full_history objects
load("pre_model_output_test/pre_model_full_history.RData", envir = e_default)
load("pre_model_output_test/pre_model_full_history_200k.RData", envir = e_200k)
load("pre_model_output_test/pre_model_full_history_250k.RData", envir = e_250k)

# Load qual_data objects
load("pre_model_output_test/qual_data_full_history.RData", envir = e_default)
load("pre_model_output_test/qual_data_full_history_200k.RData", envir = e_200k)
load("pre_model_output_test/qual_data_full_history_250k.RData", envir = e_250k)

# Helper to safely get dimensions
get_dims <- function(x) {
  d <- dim(x)
  if (is.null(d)) return(c(NA_integer_, NA_integer_))
  d
}

# Helper to safely get object size
get_size <- function(x) {
  as.numeric(object.size(x))
}

# Build comparison table for pre_model_full_history
pre_model_comparison <- data.frame(
  dataset = c("default", "200k", "250k"),
  object = "pre_model_full_history",
  rows = c(
    get_dims(e_default$pre_model_full_history)[1],
    get_dims(e_200k$pre_model_full_history)[1],
    get_dims(e_250k$pre_model_full_history)[1]
  ),
  cols = c(
    get_dims(e_default$pre_model_full_history)[2],
    get_dims(e_200k$pre_model_full_history)[2],
    get_dims(e_250k$pre_model_full_history)[2]
  ),
  object_size_bytes = c(
    get_size(e_default$pre_model_full_history),
    get_size(e_200k$pre_model_full_history),
    get_size(e_250k$pre_model_full_history)
  )
)

# Build comparison table for qual_data
qual_data_comparison <- data.frame(
  dataset = c("default", "200k", "250k"),
  object = "qual_data",
  rows = c(
    get_dims(e_default$qual_data)[1],
    get_dims(e_200k$qual_data)[1],
    get_dims(e_250k$qual_data)[1]
  ),
  cols = c(
    get_dims(e_default$qual_data)[2],
    get_dims(e_200k$qual_data)[2],
    get_dims(e_250k$qual_data)[2]
  ),
  object_size_bytes = c(
    get_size(e_default$qual_data),
    get_size(e_200k$qual_data),
    get_size(e_250k$qual_data)
  )
)

# Combine and add MB columns
comparison_table <- rbind(pre_model_comparison, qual_data_comparison)
comparison_table$object_size_mb <- round(comparison_table$object_size_bytes / 1024^2, 2)

print(comparison_table)

# Optional: print summaries separately
cat("\n=== pre_model_full_history comparison ===\n")
print(pre_model_comparison)

cat("\n=== qual_data comparison ===\n")
print(qual_data_comparison)

# > print(comparison_table)
# dataset                 object    rows cols object_size_bytes object_size_mb
# 1 default pre_model_full_history 3593709   33         776250096         740.29
# 2    200k pre_model_full_history 3645204   33         787372968         750.90
# 3    250k pre_model_full_history  180066   33          38903160          37.10
# 4 default              qual_data 1933501   11         162420104         154.90
# 5    200k              qual_data 3887642   11         326567944         311.44
# 6    250k              qual_data 4852896   11         407649280         388.76
# > 
#   > # Optional: print summaries separately
#   > cat("\n=== pre_model_full_history comparison ===\n")
# 
# === pre_model_full_history comparison ===
#   > print(pre_model_comparison)
# dataset                 object    rows cols object_size_bytes
# 1 default pre_model_full_history 3593709   33         776250096
# 2    200k pre_model_full_history 3645204   33         787372968
# 3    250k pre_model_full_history  180066   33          38903160
# > 
#   > cat("\n=== qual_data comparison ===\n")
# 
# === qual_data comparison ===
#   > print(qual_data_comparison)
# dataset    object    rows cols object_size_bytes
# 1 default qual_data 1933501   11         162420104
# 2    200k qual_data 3887642   11         326567944
# 3    250k qual_data 4852896   11         407649280
# > 
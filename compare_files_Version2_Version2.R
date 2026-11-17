e_default <- new.env()
e_10k <- new.env()
e_200k <- new.env()
e_250k <- new.env()

# Load pre_model_full_history objects
load("pre_model_output_test/pre_model_full_history.RData", envir = e_default)
load("pre_model_output_test/pre_model_full_history_10k.RData", envir = e_10k)
load("pre_model_output_test/pre_model_full_history_200k.RData", envir = e_200k)
load("pre_model_output_test/pre_model_full_history_250k.RData", envir = e_250k)

# Load qual_data objects
load("pre_model_output_test/qual_data_full_history.RData", envir = e_default)
load("pre_model_output_test/qual_data_full_history_10k.RData", envir = e_10k)
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

build_row <- function(dataset_label, obj_name, obj) {
  d <- get_dims(obj)
  data.frame(
    dataset = dataset_label,
    object = obj_name,
    rows = d[1],
    cols = d[2],
    object_size_bytes = get_size(obj),
    stringsAsFactors = FALSE
  )
}

# Build comparison tables
pre_model_comparison <- rbind(
  build_row("default", "pre_model_full_history", e_default$pre_model_full_history),
  build_row("10k",     "pre_model_full_history", e_10k$pre_model_full_history),
  build_row("200k",    "pre_model_full_history", e_200k$pre_model_full_history),
  build_row("250k",    "pre_model_full_history", e_250k$pre_model_full_history)
)

qual_data_comparison <- rbind(
  build_row("default", "qual_data", e_default$qual_data),
  build_row("10k",     "qual_data", e_10k$qual_data),
  build_row("200k",    "qual_data", e_200k$qual_data),
  build_row("250k",    "qual_data", e_250k$qual_data)
)

# Combine and add MB columns
comparison_table <- rbind(pre_model_comparison, qual_data_comparison)
comparison_table$object_size_mb <- round(comparison_table$object_size_bytes / 1024^2, 2)

print(comparison_table)

cat("\n=== pre_model_full_history comparison ===\n")
print(pre_model_comparison)

cat("\n=== qual_data comparison ===\n")
print(qual_data_comparison)

# > print(comparison_table)
# dataset                 object    rows cols object_size_bytes object_size_mb
# 1 default pre_model_full_history 3593709   33         776250096         740.29
# 2     10k pre_model_full_history  678984   33         146669448         139.87
# 3    200k pre_model_full_history 3645204   33         787372968         750.90
# 4    250k pre_model_full_history  180066   33          38903160          37.10
# 5 default              qual_data 1933501   11         162420104         154.90
# 6     10k              qual_data  191033   11          16052792          15.31
# 7    200k              qual_data 3887642   11         326567944         311.44
# 8    250k              qual_data 4852896   11         407649280         388.76
# > 
#   > cat("\n=== pre_model_full_history comparison ===\n")
# 
# === pre_model_full_history comparison ===
#   > print(pre_model_comparison)
# dataset                 object    rows cols object_size_bytes
# 1 default pre_model_full_history 3593709   33         776250096
# 2     10k pre_model_full_history  678984   33         146669448
# 3    200k pre_model_full_history 3645204   33         787372968
# 4    250k pre_model_full_history  180066   33          38903160
# > 
#   > cat("\n=== qual_data comparison ===\n")
# 
# === qual_data comparison ===
#   > print(qual_data_comparison)
# dataset    object    rows cols object_size_bytes
# 1 default qual_data 1933501   11         162420104
# 2     10k qual_data  191033   11          16052792
# 3    200k qual_data 3887642   11         326567944
# 4    250k qual_data 4852896   11         407649280
# > 
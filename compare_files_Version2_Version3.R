e_default <- new.env()
e_10k <- new.env()
e_200k <- new.env()
e_250k <- new.env()

# Load pre_model_full_history objects
load("pre_model_output_test/pre_model_full_history_100k.RData", envir = e_default)
load("pre_model_output_test/pre_model_full_history_10k.RData", envir = e_10k)
load("pre_model_output_test/pre_model_full_history_200k.RData", envir = e_200k)
load("pre_model_output_test/pre_model_full_history_250k.RData", envir = e_250k)

# Load qual_data objects
load("pre_model_output_test/qual_data_full_history_100k.RData", envir = e_default)
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
  build_row("100k", "pre_model_full_history", e_default$pre_model_full_history),
  build_row("10k",     "pre_model_full_history", e_10k$pre_model_full_history),
  build_row("200k",    "pre_model_full_history", e_200k$pre_model_full_history),
  build_row("250k",    "pre_model_full_history", e_250k$pre_model_full_history)
)

qual_data_comparison <- rbind(
  build_row("100k", "qual_data", e_default$qual_data),
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
# 1    100k pre_model_full_history  931687   33         201253344         191.93
# 2     10k pre_model_full_history   94409   33          20401296          19.46
# 3    200k pre_model_full_history 1854076   33         400489320         381.94
# 4    250k pre_model_full_history 2317104   33         500503368         477.32
# 5    100k              qual_data 1946430   11         163506136         155.93
# 6     10k              qual_data  196155   11          16483040          15.72
# 7    200k              qual_data 3885233   11         326365592         311.25
# 8    250k              qual_data 4852896   11         407649280         388.76
# > 
#   > cat("\n=== pre_model_full_history comparison ===\n")
# 
# === pre_model_full_history comparison ===
#   > print(pre_model_comparison)
# dataset                 object    rows cols object_size_bytes
# 1    100k pre_model_full_history  931687   33         201253344
# 2     10k pre_model_full_history   94409   33          20401296
# 3    200k pre_model_full_history 1854076   33         400489320
# 4    250k pre_model_full_history 2317104   33         500503368
# > 
#   > cat("\n=== qual_data comparison ===\n")
# 
# === qual_data comparison ===
#   > print(qual_data_comparison)
# dataset    object    rows cols object_size_bytes
# 1    100k qual_data 1946430   11         163506136
# 2     10k qual_data  196155   11          16483040
# 3    200k qual_data 3885233   11         326365592
# 4    250k qual_data 4852896   11         407649280
# > 

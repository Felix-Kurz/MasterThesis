# Compare corrected Version 3 outputs with the seeded 250k Version 4-style output.
#
# The seeded dataset is created by:
#   01_build_pre_model_dataset_Version3_250k_seed123.R
#
# It writes:
#   pre_model_output_test/pre_model_full_history_250k_seed123.RData
#   pre_model_output_test/qual_data_full_history_250k_seed123.RData

datasets <- data.frame(
  dataset = c(
    "100k",
    "10k",
    "200k",
    "250k",
    "250k_seed123"
  ),
  pre_model_file = c(
    "pre_model_output_test/pre_model_full_history_100k.RData",
    "pre_model_output_test/pre_model_full_history_10k.RData",
    "pre_model_output_test/pre_model_full_history_200k.RData",
    "pre_model_output_test/pre_model_full_history_250k.RData",
    "pre_model_output_test/pre_model_full_history_250k_seed123.RData"
  ),
  qual_data_file = c(
    "pre_model_output_test/qual_data_full_history_100k.RData",
    "pre_model_output_test/qual_data_full_history_10k.RData",
    "pre_model_output_test/qual_data_full_history_200k.RData",
    "pre_model_output_test/qual_data_full_history_250k.RData",
    "pre_model_output_test/qual_data_full_history_250k_seed123.RData"
  ),
  stringsAsFactors = FALSE
)

get_dims <- function(x) {
  d <- dim(x)
  if (is.null(d)) {
    return(c(NA_integer_, NA_integer_))
  }
  d
}

get_size <- function(x) {
  as.numeric(object.size(x))
}

load_named_object <- function(file, object_name) {
  if (!file.exists(file)) {
    return(NULL)
  }

  env <- new.env(parent = emptyenv())
  load(file, envir = env)

  if (!exists(object_name, envir = env, inherits = FALSE)) {
    warning(sprintf("Object '%s' not found in %s", object_name, file))
    return(NULL)
  }

  get(object_name, envir = env, inherits = FALSE)
}

build_row <- function(dataset_label, obj_name, file) {
  obj <- load_named_object(file, obj_name)

  if (is.null(obj)) {
    return(data.frame(
      dataset = dataset_label,
      object = obj_name,
      file = file,
      file_exists = file.exists(file),
      rows = NA_integer_,
      cols = NA_integer_,
      object_size_bytes = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  d <- get_dims(obj)
  data.frame(
    dataset = dataset_label,
    object = obj_name,
    file = file,
    file_exists = TRUE,
    rows = d[1],
    cols = d[2],
    object_size_bytes = get_size(obj),
    stringsAsFactors = FALSE
  )
}

pre_model_comparison <- do.call(
  rbind,
  Map(
    build_row,
    datasets$dataset,
    "pre_model_full_history",
    datasets$pre_model_file
  )
)

qual_data_comparison <- do.call(
  rbind,
  Map(
    build_row,
    datasets$dataset,
    "qual_data",
    datasets$qual_data_file
  )
)

comparison_table <- rbind(pre_model_comparison, qual_data_comparison)
comparison_table$object_size_mb <- round(comparison_table$object_size_bytes / 1024^2, 2)

print(comparison_table)

cat("\n=== pre_model_full_history comparison ===\n")
print(pre_model_comparison)

cat("\n=== qual_data comparison ===\n")
print(qual_data_comparison)

cat("\n=== Missing files ===\n")
missing_files <- unique(comparison_table$file[!comparison_table$file_exists])
if (length(missing_files) == 0) {
  cat("None\n")
} else {
  print(missing_files)
}

if (use_debug_sample) {
  set.seed(sample_seed)
  
  n_available_penr <- hv_full %>%
    distinct(penr) %>%
    nrow()
  
  n_to_sample <- min(sample_n_penr, n_available_penr)
  
  debug_penr <- hv_full %>%
    distinct(penr) %>%
    slice_sample(n = n_to_sample) %>%
    pull(penr)
  
  hv_full <- hv_full %>% filter(penr %in% debug_penr)
  qual_full <- qual_full %>% filter(penr %in% debug_penr)
  quali_full <- quali_full %>% filter(penr %in% debug_penr)
  
  cat("\nDEBUG MODE ACTIVE:", length(debug_penr), "penr\n")
  cat("Sample suffix:", sample_suffix, "\n")
  cat("Distinct penr in hv_full after sampling:", dplyr::n_distinct(hv_full$penr), "\n\n")
}
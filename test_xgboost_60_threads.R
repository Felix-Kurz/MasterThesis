# Run in a separate R session inside the same capsule (OMP_NUM_THREADS=60).
# This briefly competes with the ongoing model run for CPU time.
# Compare training time only: identical data, seed, settings, and 200 rounds.
local({
  set.seed(123)
  cat("OMP_NUM_THREADS:", Sys.getenv("OMP_NUM_THREADS"), "\n")
  cat("Watch this test from a terminal: top -p", Sys.getpid(), "\n")

  x <- matrix(rnorm(200000L * 40L), ncol = 40L)
  y <- x[, 1] * x[, 2] + sin(x[, 3]) + rnorm(nrow(x), sd = 0.2)
  results <- data.frame(Threads = integer(), Seconds = numeric(),
                        Average_CPU_Cores = numeric())
  for (threads in c(4L, 60L)) {
    # Fresh DMatrix for each run avoids reusing the previous fit's data cache.
    # Data preparation and DMatrix creation are excluded from training time.
    d <- xgboost::xgb.DMatrix(x, label = y)
    cat("\n=== Training with", threads, "threads ===\n")
    set.seed(123)
    elapsed <- system.time({
      fit <- xgboost::xgb.train(
        params = list(objective = "reg:squarederror", eval_metric = "rmse",
                      tree_method = "hist", max_depth = 8L, nthread = threads),
        data = d, nrounds = 200L, watchlist = list(train = d),
        verbose = 1, print_every_n = 10L
      )
    })
    results <- rbind(results, data.frame(
      Threads = threads,
      Seconds = elapsed[["elapsed"]],
      Average_CPU_Cores = round(
        sum(elapsed[c("user.self", "sys.self")]) / elapsed[["elapsed"]], 1)
    ))
    print(elapsed)
    rm(fit, d)
    invisible(gc())
  }
  cat("\n=== 4 versus 60 threads: training-time comparison ===\n")
  print(results, row.names = FALSE)
  if (all(results$Seconds > 0)) {
    speedup <- results$Seconds[1] / results$Seconds[2]
    cat(sprintf("60-thread speedup over 4 threads: %.2fx (ideal linear scaling: 15x)\n",
                speedup))
  }
  cat("Single-run synthetic histogram benchmark; full-model scaling may differ.\n")
})

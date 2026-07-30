# Batch driver for the commuter membership study (Sec. 7.8).
#
# Route randomization dominates the runtime (a few hours on the full grid) and does not
# depend on the travel-time model, so the cohorts are cached in COHORT_RDS. Repricing
# after the first run -- a different strike, a different sigma_profile -- takes seconds.
# Pass --refresh to discard the cache and resample the routes.
#
#   cd simulations/commuter_membership && Rscript run_commuter_membership.R
#   Rscript -e 'rmarkdown::render("commuter_membership.Rmd")'

source("commuter_membership.R")

COHORT_RDS <- "commuter_cohorts.rds"
OUT_RDS <- "../plot/commuter_membership_results.rds"
OUT_DIR <- "results"
refresh <- "--refresh" %in% commandArgs(trailingOnly = TRUE)

settings <- COMMUTER_DEFAULTS
settings$quotes_per_history_trip <- 10L

started <- Sys.time()
results <- run_commuter_membership(
  "../data/trips.csv", settings,
  cohort_cache = COHORT_RDS, refresh_cohorts = refresh
)
cat("Elapsed:", format(Sys.time() - started), "\n")

dir.create(dirname(OUT_RDS), showWarnings = FALSE, recursive = TRUE)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
saveRDS(results, OUT_RDS)

fwrite(results$summary, file.path(OUT_DIR, "commuter_summary.csv"))
fwrite(results$usage, file.path(OUT_DIR, "commuter_usage.csv"))
fwrite(
  rbindlist(lapply(results$runs, function(r) {
    cbind(
      data.table(model = r$model, k_factor = r$k_factor, history_size = r$history_size),
      r$by_rider
    )
  })),
  file.path(OUT_DIR, "commuter_riders.csv")
)

print(results$summary)

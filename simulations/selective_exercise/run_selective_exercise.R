# Batch driver for the selective-exercise study (Sec. 7.9).
#
# Everything expensive was already paid for by the commuter study: this reads its two
# cached artefacts and re-sums payouts under a selection rule, so a full run is a
# matter of seconds. Run the commuter study first if either artefact is missing.
#
#   cd simulations/commuter_membership && Rscript run_commuter_membership.R
#   cd ../selective_exercise && Rscript run_selective_exercise.R
#   Rscript -e 'rmarkdown::render("selective_exercise.Rmd")'
#
# Options:
#   --boot=N       bootstrap replicates (default 1000)
#   --sweep-h      run every history size in the commuter cache, not just h = 8;
#                  see the OD-pair caveat in SELECTIVE_DEFAULTS before reading it

source("selective_exercise.R")

OUT_RDS <- "../plot/selective_exercise_results.rds"
OUT_DIR <- "results"
args <- commandArgs(trailingOnly = TRUE)

settings <- SELECTIVE_DEFAULTS
if (any(grepl("^--boot=", args))) {
  settings$n_boot <- as.integer(sub("^--boot=", "", grep("^--boot=", args, value = TRUE)[1L]))
}
if ("--sweep-h" %in% args) {
  settings$history_sizes <- c(2L, 4L, 6L, 8L)
}

started <- Sys.time()
results <- run_selective_exercise(settings = settings)
cat("Elapsed:", format(Sys.time() - started), "\n")

dir.create(dirname(OUT_RDS), showWarnings = FALSE, recursive = TRUE)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
saveRDS(results, OUT_RDS)

fwrite(results$raw, file.path(OUT_DIR, "selective_exercise_raw.csv"))
fwrite(results$curves, file.path(OUT_DIR, "selective_exercise_curves.csv"))
fwrite(results$break_even, file.path(OUT_DIR, "break_even_utilization.csv"))
fwrite(results$lambda_star, file.path(OUT_DIR, "foresight_threshold.csv"))
fwrite(results$tests, file.path(OUT_DIR, "acceptance_tests.csv"))

cat("\nBreak-even utilization u*(lambda), NA = never breaks even within the month.\n")
cat("Intervals condition on the resamples that do break even; p_none is the rest.\n")
print(results$break_even[, .(model, strike, h, lambda,
                             u_star = round(u_star, 3),
                             lo = round(u_star_lo, 3),
                             hi = round(u_star_hi, 3),
                             p_none = round(p_no_break_even, 2))])

cat("\nForesight needed before the product loses money at utilization u0\n")
print(results$lambda_star[, .(model, strike, h, u0,
                              lambda_star = round(lambda_star, 3),
                              lo = round(lambda_star_lo, 3),
                              hi = round(lambda_star_hi, 3),
                              p_none = round(p_never_loses, 2))])

# Multi-sampler and route-sampling fidelity CDFs (Fig. fidelity-traveltime).
#
# Left panel: legacy plot4_data.csv — realized vs non-dependent, 1st/2nd-order,
# population, and HMM (no full-order).
#
# Right panel: legacy figure4_data_right.csv — route-varied travel-time simulation
# under z-test (normal) at 0.05/0.50/0.95, t-test at 0.05, and F-test at 0.05
# (normal and t variants).
#
#   cd simulations && Rscript fidelity_traveltime_figure.R
#   cd simulations && Rscript fidelity_traveltime_figure.R --recompute-left --mc=100
#   cd simulations && Rscript fidelity_traveltime_figure.R --recompute-right

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(Matrix)
  library(mvtnorm)
  library(traveltimeCLT)
})

source("fidelity_common.R")

SEED <- 1234L
N_TEST <- 2000L
N_MC <- 1000L
RHO <- 0.31
LATEX_IMG <- "../latex/preprint/img"

LEFT_LEGACY_CSV <- "plot/plot_data/plot4_data.csv"
RIGHT_LEGACY_CSV <- "plot/plot_data/figure4_data_right.csv"
OUT_LEFT_PNG <- "plot/sampled travel times.png"
OUT_RIGHT_PNG <- "plot/route_sampling_fidelity.png"

PANEL_WIDTH_IN <- 6.5
PANEL_HEIGHT_IN <- 4.8

args <- commandArgs(trailingOnly = TRUE)
parse_int_arg <- function(prefix, default) {
  hit <- grep(paste0("^", prefix), args, value = TRUE)
  if (length(hit) == 0L) {
    return(default)
  }
  as.integer(sub(prefix, "", hit[1L]))
}
N_MC <- parse_int_arg("--mc=", N_MC)
RECOMPUTE_LEFT <- "--recompute-left" %in% args
RECOMPUTE_RIGHT <- "--recompute-right" %in% args
SKIP_HMM <- "--skip-hmm" %in% args

dir.create(dirname(OUT_LEFT_PNG), showWarnings = FALSE, recursive = TRUE)

left_series_levels <- c(
  "Realized",
  "Non-dependent",
  "1st-order",
  "2nd-order",
  "Population",
  "HMM"
)

left_series_style <- list(
  color = c(
    "Realized" = "black",
    "Non-dependent" = "grey25",
    "1st-order" = "grey40",
    "2nd-order" = "grey55",
    "Population" = "grey65",
    "HMM" = "grey50"
  ),
  linetype = c(
    "Realized" = "solid",
    "Non-dependent" = "dashed",
    "1st-order" = "dotdash",
    "2nd-order" = "longdash",
    "Population" = "dotted",
    "HMM" = "F2"
  )
)

right_series_levels <- c(
  "Realized",
  "Normal 0.05",
  "Normal 0.50",
  "Normal 0.95",
  "Normal F 0.05",
  "t 0.05",
  "t F 0.05"
)

right_series_style <- list(
  color = c(
    "Realized" = "black",
    "Normal 0.05" = "grey20",
    "Normal 0.50" = "grey35",
    "Normal 0.95" = "grey50",
    "Normal F 0.05" = "grey45",
    "t 0.05" = "grey65",
    "t F 0.05" = "grey80"
  ),
  linetype = c(
    "Realized" = "solid",
    "Normal 0.05" = "dashed",
    "Normal 0.50" = "dotdash",
    "Normal 0.95" = "longdash",
    "Normal F 0.05" = "twodash",
    "t 0.05" = "dotted",
    "t F 0.05" = "F2"
  )
)

build_left_cdf_plot <- function(left_long) {
  left_long[, series := factor(series, levels = left_series_levels)]
  ggplot(left_long, aes(x = travel_time, color = series, linetype = series)) +
    stat_ecdf(linewidth = 0.65) +
    scale_color_manual(values = left_series_style$color, name = NULL) +
    scale_linetype_manual(values = left_series_style$linetype, name = NULL) +
    coord_cartesian(xlim = c(0, 4000), ylim = c(0, 1)) +
    labs(x = "Travel time (s)", y = "Cumulative probability") +
    fidelity_panel_theme(
      PANEL_FONT_PT, AXIS_TITLE_PT, LEGEND_TEXT_PT, legend_inside = TRUE
    )
}

build_right_cdf_plot <- function(right_long) {
  right_long[, series := factor(series, levels = right_series_levels)]
  ggplot(right_long, aes(x = travel_time, color = series, linetype = series)) +
    stat_ecdf(linewidth = 0.65) +
    scale_color_manual(values = right_series_style$color, name = NULL) +
    scale_linetype_manual(values = right_series_style$linetype, name = NULL) +
    coord_cartesian(xlim = c(0, 4000), ylim = c(0, 1)) +
    labs(x = "Travel time (s)", y = "Cumulative probability") +
    fidelity_panel_theme(
      PANEL_FONT_PT, AXIS_TITLE_PT, LEGEND_TEXT_PT, legend_inside = TRUE
    )
}

load_legacy_left_long <- function(path = LEFT_LEGACY_CSV) {
  if (!file.exists(path)) {
    stop("Legacy left-panel CSV not found: ", path)
  }
  dt <- fread(path)
  rbindlist(list(
    dt[, .(travel_time = sampled_time, series = "Realized")],
    dt[, .(travel_time = non_dependent_time, series = "Non-dependent")],
    dt[, .(travel_time = first_order_time, series = "1st-order")],
    dt[, .(travel_time = second_order_time, series = "2nd-order")],
    dt[, .(travel_time = population_time, series = "Population")],
    dt[, .(travel_time = HMM, series = "HMM")]
  ))
}

load_legacy_right_long <- function(path = RIGHT_LEGACY_CSV) {
  if (!file.exists(path)) {
    stop("Legacy right-panel CSV not found: ", path)
  }
  dt <- fread(path)
  rbindlist(list(
    dt[, .(travel_time = real, series = "Realized")],
    dt[, .(travel_time = normal_5, series = "Normal 0.05")],
    dt[, .(travel_time = normal_50, series = "Normal 0.50")],
    dt[, .(travel_time = normal_95, series = "Normal 0.95")],
    dt[, .(travel_time = normal_F, series = "Normal F 0.05")],
    dt[, .(travel_time = t_5, series = "t 0.05")],
    dt[, .(travel_time = t_5F, series = "t F 0.05")]
  ))
}

fit_hmm_predictions <- function(test, test_ids) {
  if (!requireNamespace("traveltimeHMM", quietly = TRUE)) {
    warning("traveltimeHMM not installed; skipping HMM series.")
    return(NULL)
  }
  suppressPackageStartupMessages(library(traveltimeHMM))

  tripdata <- as.data.frame(test[, .(
    logspeed,
    tripID,
    timeBin,
    linkID,
    length = distance_meters,
    time = entry_time,
    traveltime = duration_secs
  )])
  tripdata <- tripdata[order(tripdata$tripID, tripdata$time), ]
  fit <- traveltimeHMM(
    data = tripdata,
    nQ = 2L,
    max.it = 20L,
    model = "HMM"
  )

  starttimes <- stats::aggregate(time ~ tripID, data = tripdata, FUN = min)
  preds <- numeric(length(test_ids))
  for (i in seq_along(test_ids)) {
    tid <- test_ids[i]
    single_trip <- subset(tripdata, tripID == tid)
    preds[i] <- predict(
      object = fit,
      tripdata = single_trip,
      starttime = starttimes$time[match(tid, starttimes$tripID)],
      n = 1L
    )
    if (i %% 200L == 0L || i == length(test_ids)) {
      cat("  HMM predict", i, "/", length(test_ids), "\n")
    }
  }
  data.table(tripID = test_ids, hmm_secs = preds)
}

compute_left_panel <- function(test_edges, test_ids, n_mc, rho) {
  trip_ids <- unique(test_edges$tripID)
  n_trips <- length(trip_ids)
  out <- data.table(
    tripID = trip_ids,
    sampled_time = NA_real_,
    non_dependent_time = NA_real_,
    first_order_time = NA_real_,
    second_order_time = NA_real_,
    population_time = NA_real_,
    HMM = NA_real_
  )

  cat(
    "Left panel: simulating", n_trips, "trips with", n_mc,
    "MC draws for CLT samplers ...\n"
  )
  started <- Sys.time()
  for (i in seq_len(n_trips)) {
    tid <- trip_ids[i]
    edges <- test_edges[tripID == tid]
    mu <- edges$mean
    sigma <- edges$sd
    durs <- edges$duration_secs
    sims <- simulate_clt_samplers_mc(mu, sigma, durs, n_mc, rho)
    out[tripID == tid, `:=`(
      sampled_time = sum(durs),
      non_dependent_time = sims["independent"],
      first_order_time = sims["first"],
      second_order_time = sims["second"],
      population_time = population_simulator(durs)
    )]
    if (i %% 100L == 0L || i == n_trips) {
      cat(
        "  ", i, "/", n_trips,
        " elapsed:", format(Sys.time() - started), "\n"
      )
    }
  }
  out
}

compute_route_durations <- function(route_dt) {
  if (nrow(route_dt) == 0L) {
    return(data.table(trip = integer(), duration = numeric()))
  }
  route_dt[, .(
    duration = sum(exp(mean + stats::qnorm(dependent_uniform(.N, RHO)) * sd))
  ), by = trip]
}

compute_right_panel <- function(trips_raw, test_ids) {
  realized <- trips_raw[trip %in% test_ids, .(duration = sum(tt)), by = trip]
  setnames(realized, c("trip", "duration"), c("trip", "real"))

  cat("Right panel: similar_route() for z-test matching ...\n")
  started <- Sys.time()
  normal_05 <- similar_route(test_ids, trips_raw, sigma_n = 0, significance = 0.05)
  cat("  normal 0.05 elapsed:", format(Sys.time() - started), "\n")
  normal_50 <- similar_route(test_ids, trips_raw, sigma_n = 0, significance = 0.50)
  normal_95 <- similar_route(test_ids, trips_raw, sigma_n = 0, significance = 0.95)
  normal_F <- similar_route(
    test_ids, trips_raw, sigma_n = 0, significance = 0.05, Ftest_sd = TRUE
  )

  cat("Right panel: similar_route() for t-test matching ...\n")
  t_05 <- similar_route(
    test_ids, trips_raw, sigma_n = 0, model = "t", significance = 0.05
  )
  t_05F <- similar_route(
    test_ids, trips_raw, sigma_n = 0, model = "t", significance = 0.05, Ftest_sd = TRUE
  )

  out <- copy(realized)
  out <- merge(
    out,
    compute_route_durations(normal_05)[, .(trip, normal_5 = duration)],
    by = "trip", all.x = TRUE
  )
  out <- merge(
    out,
    compute_route_durations(normal_50)[, .(trip, normal_50 = duration)],
    by = "trip", all.x = TRUE
  )
  out <- merge(
    out,
    compute_route_durations(normal_95)[, .(trip, normal_95 = duration)],
    by = "trip", all.x = TRUE
  )
  out <- merge(
    out,
    compute_route_durations(normal_F)[, .(trip, normal_F = duration)],
    by = "trip", all.x = TRUE
  )
  out <- merge(
    out,
    compute_route_durations(t_05)[, .(trip, t_5 = duration)],
    by = "trip", all.x = TRUE
  )
  out <- merge(
    out,
    compute_route_durations(t_05F)[, .(trip, t_5F = duration)],
    by = "trip", all.x = TRUE
  )
  out
}

plot_left_from_legacy <- function() {
  left_long <- load_legacy_left_long()
  left_plot <- build_left_cdf_plot(left_long)
  save_fidelity_png(left_plot, OUT_LEFT_PNG, PANEL_WIDTH_IN, PANEL_HEIGHT_IN, LATEX_IMG)
  cat("Replotted left panel from", LEFT_LEGACY_CSV, "\n")
}

plot_right_from_legacy <- function() {
  right_long <- load_legacy_right_long()
  right_plot <- build_right_cdf_plot(right_long)
  save_fidelity_png(right_plot, OUT_RIGHT_PNG, PANEL_WIDTH_IN, PANEL_HEIGHT_IN, LATEX_IMG)
  cat("Replotted right panel from", RIGHT_LEGACY_CSV, "\n")
}

if (!RECOMPUTE_LEFT && !RECOMPUTE_RIGHT) {
  plot_left_from_legacy()
  plot_right_from_legacy()
  cat("Copied PNGs to", LATEX_IMG, "\n")
  quit(save = "no", status = 0)
}

trips_raw <- fread("data/trips.csv")

if (RECOMPUTE_LEFT) {
  cat("Loading trips ...\n")
  split <- load_fidelity_trips(seed = SEED, n_test = N_TEST)
  train <- split$train
  test <- split$test
  test_ids <- split$test_ids
  cat(
    "Training on", uniqueN(train$tripID), "trips, testing on",
    uniqueN(test$tripID), "trips\n"
  )

  cat("Fitting edge x time-bin statistics on training data ...\n")
  test_edges <- prepare_test_edges(train, test)
  left_wide <- compute_left_panel(test_edges, test_ids, N_MC, RHO)

  cat("Fitting HMM on test cohort and predicting test trips ...\n")
  hmm_dt <- if (SKIP_HMM) NULL else fit_hmm_predictions(test, test_ids)
  if (!is.null(hmm_dt)) {
    left_wide <- merge(left_wide, hmm_dt[, .(tripID, HMM = hmm_secs)], by = "tripID", all.x = TRUE)
  }

  fwrite(left_wide, LEFT_LEGACY_CSV)
  left_long <- load_legacy_left_long(LEFT_LEGACY_CSV)
  left_plot <- build_left_cdf_plot(left_long)
  save_fidelity_png(left_plot, OUT_LEFT_PNG, PANEL_WIDTH_IN, PANEL_HEIGHT_IN, LATEX_IMG)
  cat("Wrote", LEFT_LEGACY_CSV, OUT_LEFT_PNG, "\n")
}

if (RECOMPUTE_RIGHT) {
  if (!exists("test_ids")) {
    set.seed(SEED)
    test_ids <- sample(unique(trips_raw$trip), N_TEST)
  }
  right_wide <- compute_right_panel(trips_raw, test_ids)
  fwrite(right_wide, RIGHT_LEGACY_CSV)
  right_long <- load_legacy_right_long(RIGHT_LEGACY_CSV)
  right_plot <- build_right_cdf_plot(right_long)
  save_fidelity_png(right_plot, OUT_RIGHT_PNG, PANEL_WIDTH_IN, PANEL_HEIGHT_IN, LATEX_IMG)
  cat("Wrote", RIGHT_LEGACY_CSV, OUT_RIGHT_PNG, "\n")
}

cat("Copied PNGs to", LATEX_IMG, "\n")

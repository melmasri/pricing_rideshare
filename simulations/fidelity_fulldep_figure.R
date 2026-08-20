# Full-order sampler fidelity CDF (Fig. fidelity-fulldep).
#
# Paper-faithful setup: 2000 held-out test trips (seed 1234), edge x time-bin stats
# fit on training trips only, Global-bin fallback, 1000 MC draws averaged per trip
# under the full-order dependent sampler (rho = 0.31).
#
#   cd simulations && Rscript fidelity_fulldep_figure.R

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(traveltimeCLT)
})

SEED <- 1234L
N_TEST <- 2000L
N_MC <- 1000L
RHO <- 0.31
OUT_CSV <- "plot/plot_data/fidelity_fulldep.csv"
OUT_PNG <- "plot/fidelity_fulldep.png"
LATEX_IMG <- "../latex/preprint/img"

PANEL_WIDTH_IN <- 6.5
PANEL_HEIGHT_IN <- 4.5
PANEL_FONT_PT <- 11
AXIS_TITLE_PT <- 12
AXIS_TEXT_PT <- 12

fill_timebin_edges <- function(edge_dt, timebin_x_edges) {
  dt <- copy(edge_dt)
  global_dt <- timebin_x_edges[timeBin == "Global"]
  na_idx <- which(is.na(dt$mean))
  cols <- c("timeBin", "mean", "sd", "frequency", "length", "ID")
  if (length(na_idx) > 0L) {
    repl <- global_dt[dt[na_idx, .(linkID)], on = "linkID"]
    dt[na_idx, (cols) := repl[, mget(cols)]]
  }
  na.omit(dt)
}

simulate_full_order <- function(mu, sigma, rho = RHO) {
  n <- length(mu)
  U <- if (n > 1L) dependent_uniform(n, rho) else runif(1)
  sum(exp(mu + sigma * qnorm(U)))
}

simulate_full_order_mc <- function(mu, sigma, n_mc = N_MC, rho = RHO) {
  if (n_mc <= 1L) {
    return(simulate_full_order(mu, sigma, rho))
  }
  total <- 0
  for (i in seq_len(n_mc)) {
    total <- total + simulate_full_order(mu, sigma, rho)
  }
  total / n_mc
}

cat("Loading trips ...\n")
trips <- fread("data/trips.csv")
setnames(
  trips,
  c("trip", "time", "tt", "length", "linkId"),
  c("tripID", "entry_time", "duration_secs", "distance_meters", "linkID")
)
trips[, entry_time := as.POSIXct(entry_time, tz = "UTC")]
trips[, timeBin := time_bins_readable(entry_time)]
setorder(trips, tripID, entry_time)

set.seed(SEED)
test_ids <- sample(unique(trips$tripID), N_TEST)
train <- trips[!tripID %in% test_ids]
test <- trips[tripID %in% test_ids]
cat(
  "Training on", uniqueN(train$tripID), "trips, testing on",
  uniqueN(test$tripID), "trips\n"
)

cat("Fitting edge x time-bin statistics on training data ...\n")
timebin_x_edges <- get_timeBin_x_edges(
  tripID = train$tripID,
  linkId = train$linkID,
  length = train$distance_meters,
  timeBin = train$timeBin,
  duration = train$duration_secs
)
setnames(timebin_x_edges, c("timeBin", "linkId"), c("timeBin", "linkID"))

test_edges <- merge(
  test[, .(tripID, linkID, timeBin, duration_secs)],
  timebin_x_edges,
  by = c("linkID", "timeBin"),
  all.x = TRUE
)
test_edges <- fill_timebin_edges(test_edges, timebin_x_edges)
if (any(is.na(test_edges$mean))) {
  stop("Some test edges still missing mean/sd after Global fallback.")
}
setorder(test_edges, tripID)

trip_ids <- unique(test_edges$tripID)
n_trips <- length(trip_ids)
results <- vector("list", n_trips)

cat(
  "Simulating", n_trips, "test trips with", N_MC,
  "full-order draws each ...\n"
)
started <- Sys.time()
for (i in seq_len(n_trips)) {
  tid <- trip_ids[i]
  edges <- test_edges[tripID == tid]
  results[[i]] <- data.table(
    tripID = tid,
    realized_secs = sum(edges$duration_secs),
    simulated_secs = simulate_full_order_mc(edges$mean, edges$sd)
  )
  if (i %% 100L == 0L || i == n_trips) {
    cat(
      "  ", i, "/", n_trips,
      " elapsed:", format(Sys.time() - started), "\n"
    )
  }
}
fidelity <- rbindlist(results)

dir.create(dirname(OUT_CSV), showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(OUT_PNG), showWarnings = FALSE, recursive = TRUE)
fwrite(fidelity, OUT_CSV)

cat(
  "Median realized (s):", round(median(fidelity$realized_secs), 1),
  "| median simulated (s):", round(median(fidelity$simulated_secs), 1),
  "| mean abs error (s):",
  round(mean(abs(fidelity$realized_secs - fidelity$simulated_secs)), 1),
  "\n"
)

cdf_long <- rbind(
  fidelity[, .(tripID, travel_time = realized_secs, series = "Realized")],
  fidelity[, .(tripID, travel_time = simulated_secs, series = "Full-order simulated")]
)
cdf_long[, series := factor(
  series,
  levels = c("Realized", "Full-order simulated")
)]

plot_fidelity <- ggplot(cdf_long, aes(x = travel_time, color = series, linetype = series)) +
  stat_ecdf(linewidth = 0.7) +
  scale_color_manual(
    values = c("Realized" = "black", "Full-order simulated" = "grey35"),
    name = NULL
  ) +
  scale_linetype_manual(
    values = c("Realized" = "solid", "Full-order simulated" = "dashed"),
    name = NULL
  ) +
  coord_cartesian(xlim = c(0, 4000), ylim = c(0, 1)) +
  labs(x = "Travel time (s)", y = "Cumulative probability") +
  theme_minimal(base_size = PANEL_FONT_PT, base_family = "sans") +
  theme(
    panel.grid.minor = element_blank(),
    text = element_text(family = "sans", size = PANEL_FONT_PT),
    axis.title = element_text(size = AXIS_TITLE_PT),
    axis.text = element_text(size = AXIS_TEXT_PT),
    legend.position = "bottom"
  )

ggsave(
  OUT_PNG, plot_fidelity,
  width = PANEL_WIDTH_IN, height = PANEL_HEIGHT_IN,
  dpi = 300, bg = "white"
)
file.copy(OUT_PNG, file.path(LATEX_IMG, basename(OUT_PNG)), overwrite = TRUE)

cat("Wrote", OUT_CSV, OUT_PNG, "\n")
cat("Copied PNG to", LATEX_IMG, "\n")

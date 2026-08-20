# On-demand guarantee premiums vs travel time and distance (Fig. route-specific-R).
#
# Same train/test split and pricing setup as on_demand_pnl.R: 2000 held-out QCD trips,
# strike K = P, zeta = 0, trip-specific and population travel-time models.
#
#   cd simulations && Rscript on_demand_premium_figure.R

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(traveltimeCLT)
})

source("figure_theme.R")

SEED <- 1234L
N_TEST <- 2000L
MODELS <- c("trip-specific", "population")
OUT_CSV <- "plot/plot_data/on_demand_premium_figure.csv"
OUT_TIME <- "plot/R-example.pdf"
OUT_DISTANCE <- "plot/R-example-distance.pdf"
OUT_COMPARE <- "plot/R_route_vs_pop.pdf"
LATEX_IMG <- "../latex/preprint/img"

PANEL_WIDTH_IN <- 3.2
PANEL_HEIGHT_IN <- 2.6
MODEL_COLORS <- c("Trip-specific" = "grey55", "Population" = "black")
MODEL_SHAPES <- c("Trip-specific" = 16, "Population" = 16)
POINT_SIZE_DOT <- 1.1
POINT_SIZE_POP <- 1.1
POINT_ALPHA <- 0.65
POINT_STROKE <- 0.6

trips <- fread("data/trips.csv")
setnames(
  trips,
  c("trip", "time", "tt", "length", "linkId"),
  c("tripID", "entry_time", "duration_secs", "distance_meters", "linkID")
)
trips[, entry_time := as.POSIXct(entry_time, tz = "UTC")]
trips[, speed := exp(logspeed)]
trips[, timeBin := time_bins_readable(entry_time)]
setorder(trips, tripID, entry_time)

set.seed(SEED)
test_ids <- sample(unique(trips$tripID), N_TEST)
train <- trips[!tripID %in% test_ids]
test <- trips[tripID %in% test_ids]
cat("Training on", uniqueN(train$tripID), "trips, testing on", uniqueN(test$tripID), "\n")

realized <- test[, .(
  start_time = entry_time[1],
  duration = sum(duration_secs),
  distance = sum(distance_meters)
), by = tripID]
realized[, real_price := price(duration, distance)[, 1]]

calibrate_population_sigma <- function(fit, data) {
  realized_dt <- data[, .(realized = sum(duration_secs), n_edges = .N), by = tripID]
  predicted <- as.data.table(predict(fit, data))
  d <- na.omit(merge(realized_dt, predicted[, .(tripID, ETA)], by = "tripID"))
  sd((d$realized - d$ETA) / sqrt(d$n_edges * (1 + 1 / fit$nsamples)))
}

price_guarantee <- function(model) {
  cat("Fitting", model, "...\n")
  fit <- suppressWarnings(traveltimeCLT(
    copy(train), model,
    nsamples = if (model == "population") uniqueN(train$tripID) else 500L
  ))
  if (model == "population") {
    cat("  sigma_profile fitted =", round(fit$sigma.prof, 2))
    fit$sigma.prof <- calibrate_population_sigma(fit, train)
    cat(", calibrated =", round(fit$sigma.prof, 2), "\n")
  }
  predicted <- as.data.table(predict(fit, test))

  dt <- merge(realized, predicted[, .(tripID, ETA, variance)], by = "tripID")
  dt[, strike := request_K(.SD, distance, discount_factor = 1)]
  dt[, premium := request_R(
    .SD, start_time, start_time, distance,
    K = 1, risk_free = 0, zeta = 0
  )]
  dt[, model := model]
  na.omit(dt[, .(
    tripID, ETA, duration, distance,
    real_price, strike, premium, model
  )])
}

pnl <- rbindlist(lapply(MODELS, price_guarantee))
pnl[, eta_min := ETA / 60]
pnl[, distance_km := distance / 1000]

trip_prem <- pnl[model == "trip-specific", .(tripID, premium_trip = premium, eta_min, distance_km, duration, distance, real_price, strike)]
pop_prem <- pnl[model == "population", .(tripID, premium_pop = premium)]
wide <- merge(trip_prem, pop_prem, by = "tripID")

dir.create(dirname(OUT_CSV), showWarnings = FALSE, recursive = TRUE)
fwrite(wide, OUT_CSV)

cat(
  "Mean premium ($): trip-specific =", round(mean(wide$premium_trip), 3),
  ", population =", round(mean(wide$premium_pop), 3),
  ", ratio =", round(mean(wide$premium_pop / wide$premium_trip), 2), "\n"
)

long <- copy(pnl)
long[, model := factor(
  ifelse(model == "trip-specific", "Trip-specific", "Population"),
  levels = c("Trip-specific", "Population")
)]
long[, marker_size := fifelse(model == "Trip-specific", POINT_SIZE_DOT, POINT_SIZE_POP)]

panel_theme_legend <- standard_panel_theme() +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.title = element_blank(),
    legend.direction = "vertical",
    legend.key.height = unit(1.1, "lines"),
    legend.margin = margin(0, 2, 0, 0)
  )

panel_theme_no_legend <- standard_panel_theme() +
  theme(legend.position = "none")

model_guides <- guides(
  shape = "none",
  size = "none",
  color = guide_legend(
    override.aes = list(
      shape = MODEL_SHAPES,
      size = c(2.0, 2.0),
      alpha = 1,
      stroke = 0.7,
      color = MODEL_COLORS
    )
  )
)

save_panel <- function(plot, path) {
  ggsave(
    path, plot,
    width = PANEL_WIDTH_IN, height = PANEL_HEIGHT_IN,
    device = cairo_pdf
  )
  file.copy(path, file.path(LATEX_IMG, basename(path)), overwrite = TRUE)
}

plot_time <- ggplot(long, aes(x = eta_min, y = premium, color = model, shape = model, size = marker_size)) +
  geom_point(alpha = POINT_ALPHA, stroke = POINT_STROKE) +
  scale_size_identity() +
  scale_color_manual(values = MODEL_COLORS) +
  scale_shape_manual(values = MODEL_SHAPES) +
  coord_cartesian(xlim = c(0, 80), ylim = c(0, 8)) +
  labs(
    x = "Estimated travel time (min)",
    y = "Premium ($)"
  ) +
  panel_theme_no_legend +
  guides(color = "none", shape = "none", size = "none")

plot_distance <- ggplot(long, aes(x = distance_km, y = premium, color = model, shape = model, size = marker_size)) +
  geom_point(alpha = POINT_ALPHA, stroke = POINT_STROKE) +
  scale_size_identity() +
  scale_color_manual(values = MODEL_COLORS) +
  scale_shape_manual(values = MODEL_SHAPES) +
  coord_cartesian(xlim = c(0, 30), ylim = c(0, 8)) +
  labs(
    x = "Distance (km)",
    y = "Premium ($)"
  ) +
  panel_theme_legend +
  model_guides

axis_max <- max(wide$premium_pop, na.rm = TRUE) * 1.05

plot_compare <- ggplot(wide, aes(x = premium_trip, y = premium_pop)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.5) +
  geom_point(size = POINT_SIZE_DOT, alpha = POINT_ALPHA, color = "grey55", shape = 16) +
  coord_equal(xlim = c(0, axis_max), ylim = c(0, axis_max)) +
  labs(
    x = "Trip-specific ($)",
    y = "Population ($)"
  ) +
  panel_theme_no_legend

save_panel(plot_time, OUT_TIME)
save_panel(plot_distance, OUT_DISTANCE)
save_panel(plot_compare, OUT_COMPARE)

cat("Wrote", OUT_CSV, OUT_TIME, OUT_DISTANCE, OUT_COMPARE, "\n")
cat("Copied PDFs to", LATEX_IMG, "\n")

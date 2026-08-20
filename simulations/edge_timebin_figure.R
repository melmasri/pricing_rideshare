# Edge x time-bin descriptive panels (Fig. edge-timebin-descriptive).
#
# Same data pipeline as general.Rmd: filter implausible speeds, build edge x time-bin
# states from the CSV timeBins column, and export six B&W density histograms in a
# 3 x 2 panel grid (also saved individually).
#
#   cd simulations && Rscript edge_timebin_figure.R

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
})

source("figure_theme.R")

OUT_DIR <- "plot"
LATEX_IMG <- "../latex/preprint/img"
OUT_VISIT <- file.path(OUT_DIR, "visit_likelihood.png")
OUT_EDGES <- file.path(OUT_DIR, "edges_per_ride.png")
OUT_MEAN <- file.path(OUT_DIR, "edge_mean_logdur.png")
OUT_SD <- file.path(OUT_DIR, "edge_sd_logdur.png")
OUT_ETA <- file.path(OUT_DIR, "trip_eta_hist.png")
OUT_DISTANCE <- file.path(OUT_DIR, "trip_distance_hist.png")
OUT_COMBINED <- file.path(OUT_DIR, "edge_timebin_descriptive.png")
OUT_SORTED_CSV <- "data/timebin_x_edge_sorted.csv"
OUT_EDGE_CSV <- "data/timebin_x_edge.csv"

PANEL_WIDTH_IN <- 3.4
PANEL_HEIGHT_IN <- 2.8
BAR_FILL <- "grey75"
BAR_COLOR <- "grey30"

sd_na_is_0 <- function(x) {
  if (length(x) >= 2L) sd(x) else 0
}

load_timebin_data <- function() {
  trips <- fread("data/trips.csv")
  trips[, time := as.POSIXct(time)]
  trips[, duration_secs := as.numeric(
    difftime(shift(time, type = "lead"), time, units = "secs")
  ), by = trip]
  trips[, speed := length / duration_secs]

  bad_trips <- trips[3.6 * exp(logspeed) > 150, unique(trip)]
  trips <- trips[!trip %in% bad_trips][order(trip, time)]
  trips <- trips[, .SD[-.N], by = trip][order(trip, time)]

  trips <- as.data.frame(trips)
  names(trips)[names(trips) == "linkId"] <- "linkID"

  timebin_x_edge <- trips %>%
    arrange(timeBins, linkID) %>%
    mutate(
      timebin_x_edge = (match(timeBins, unique(timeBins)) - 1L) * length(linkID) +
        match(linkID, linkID),
      timebin_x_edge_continuous = dense_rank(timebin_x_edge)
    )

  timebin_x_edge_sorted <- timebin_x_edge %>%
    count(timebin_x_edge_continuous) %>%
    mutate(density = n / sum(n)) %>%
    arrange(desc(density))
  timebin_x_edge_sorted$state_rank <- seq_len(nrow(timebin_x_edge_sorted))

  speed_statistic <- timebin_x_edge %>%
    group_by(timebin_x_edge_continuous) %>%
    mutate(log_duration = log(duration_secs)) %>%
    summarise(
      mean_log_duration = mean(log_duration),
      sd_log_duration = sd_na_is_0(log_duration),
      mean_duration = mean(duration_secs),
      sd_duration = sd_na_is_0(duration_secs),
      frequency = length(log_duration),
      ave_speed = mean(speed),
      sd_speed = sd_na_is_0(speed),
      .groups = "drop"
    )

  timebin_x_edge_sorted <- timebin_x_edge_sorted %>%
    left_join(speed_statistic, by = "timebin_x_edge_continuous")

  trip_summary <- as.data.table(trips)[, .(
    duration_secs = sum(duration_secs),
    distance_m = sum(length),
    n_edges = .N
  ), by = trip]
  trip_summary[, duration_min := duration_secs / 60]
  trip_summary[, distance_km := distance_m / 1000]

  list(
    sorted = timebin_x_edge_sorted,
    edge = as.data.table(timebin_x_edge),
    trip_summary = trip_summary
  )
}

panel_theme <- standard_panel_theme() +
  theme(
    panel.grid.major.x = element_blank()
  )

density_hist_layers <- function(binwidth = NULL, bins = NULL, x_scale = NULL) {
  layers <- list(
    geom_histogram(
      aes(y = after_stat(density)),
      fill = BAR_FILL,
      color = BAR_COLOR,
      linewidth = 0.25,
      boundary = 0,
      closed = "left",
      binwidth = binwidth,
      bins = bins
    ),
    labs(y = "Density"),
    panel_theme
  )
  if (!is.null(x_scale)) {
    layers <- c(list(x_scale), layers)
  }
  layers
}

log10_axis <- function(breaks, scientific = FALSE) {
  labels <- if (scientific) {
    scales::label_scientific(digits = 0)
  } else {
    scales::label_number(accuracy = 1)
  }
  scale_x_log10(breaks = breaks, labels = labels)
}

save_panel <- function(plot, path, latex_name) {
  ggsave(
    path, plot,
    width = PANEL_WIDTH_IN, height = PANEL_HEIGHT_IN,
    dpi = 300, bg = "white"
  )
  file.copy(path, file.path(LATEX_IMG, latex_name), overwrite = TRUE)
}

cat("Loading and aggregating edge x time-bin data ...\n")
data <- load_timebin_data()
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(OUT_SORTED_CSV), showWarnings = FALSE, recursive = TRUE)

fwrite(as.data.table(data$sorted), OUT_SORTED_CSV)
fwrite(data$edge, OUT_EDGE_CSV)
cat(
  "States:", nrow(data$sorted),
  "| trips:", uniqueN(data$trip_summary$trip),
  "| median duration (min):", round(median(data$trip_summary$duration_min), 1),
  "| median distance (km):", round(median(data$trip_summary$distance_km), 1),
  "\n"
)

state_x <- "State rank (log scale)"
rank_breaks <- c(1, 100, 10000)
duration_breaks <- c(1, 10, 100, 1000)
sd_breaks <- c(0.1, 1, 10, 100, 1000)

plot_visit <- Reduce(
  `+`,
  c(
    list(
      ggplot(data$sorted, aes(x = state_rank, weight = frequency)) +
        labs(x = state_x, y = "Density")
    ),
    density_hist_layers(bins = 40, x_scale = log10_axis(rank_breaks))
  )
)

plot_edges <- Reduce(
  `+`,
  c(
    list(
      ggplot(data$trip_summary, aes(x = n_edges)) +
        labs(x = "Number of edges per trip", y = "Density")
    ),
    density_hist_layers(
      binwidth = 5,
      x_scale = scale_x_continuous(breaks = c(0, 150, 300))
    )
  )
)

plot_mean <- Reduce(
  `+`,
  c(
    list(
      ggplot(data$sorted, aes(x = mean_duration, weight = frequency)) +
        labs(x = "Edge duration (sec, log scale)", y = "Density")
    ),
    density_hist_layers(bins = 40, x_scale = log10_axis(duration_breaks))
  )
)

data$sorted <- data$sorted %>%
  mutate(sd_duration_plot = pmax(sd_duration, 0.1))

plot_sd <- Reduce(
  `+`,
  c(
    list(
      ggplot(data$sorted, aes(x = sd_duration_plot, weight = frequency)) +
        labs(x = "Edge duration SD (sec, log scale)", y = "Density")
    ),
    density_hist_layers(bins = 40, x_scale = log10_axis(sd_breaks))
  )
)

plot_eta <- Reduce(
  `+`,
  c(
    list(
      ggplot(data$trip_summary, aes(x = duration_min)) +
        labs(x = "Trip duration (min)", y = "Density") +
        scale_x_continuous(breaks = seq(0, 250, 50))
    ),
    density_hist_layers(binwidth = 5)
  )
)

plot_distance <- Reduce(
  `+`,
  c(
    list(
      ggplot(data$trip_summary, aes(x = distance_km)) +
        labs(x = "Trip distance (km)", y = "Density") +
        scale_x_continuous(breaks = seq(0, 200, 50))
    ),
    density_hist_layers(binwidth = 2)
  )
)

save_panel(plot_visit, OUT_VISIT, "visit_likelihood.png")
save_panel(plot_edges, OUT_EDGES, "edges_per_ride.png")
save_panel(plot_mean, OUT_MEAN, "edge_mean_logdur.png")
save_panel(plot_sd, OUT_SD, "edge_sd_logdur.png")
save_panel(plot_eta, OUT_ETA, "trip_eta_hist.png")
save_panel(plot_distance, OUT_DISTANCE, "trip_distance_hist.png")

combined <- (plot_visit | plot_edges | plot_mean) / (plot_sd | plot_eta | plot_distance)
ggsave(
  OUT_COMBINED,
  combined,
  width = 3 * PANEL_WIDTH_IN,
  height = 2 * PANEL_HEIGHT_IN,
  dpi = 300,
  bg = "white"
)
file.copy(OUT_COMBINED, file.path(LATEX_IMG, basename(OUT_COMBINED)), overwrite = TRUE)

cat("Wrote", OUT_VISIT, OUT_EDGES, OUT_MEAN, OUT_SD, OUT_ETA, OUT_DISTANCE, OUT_COMBINED, "\n")
cat("Copied PNGs to", LATEX_IMG, "\n")

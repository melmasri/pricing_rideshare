# Shared helpers for sampler fidelity figures (Fig. fidelity-traveltime).

source("figure_theme.R")

fill_timebin_edges <- function(edge_dt, timebin_x_edges) {
  dt <- data.table::copy(edge_dt)
  global_dt <- timebin_x_edges[timeBin == "Global"]
  na_idx <- which(is.na(dt$mean))
  cols <- c("timeBin", "mean", "sd", "frequency", "length", "ID")
  if (length(na_idx) > 0L) {
    repl <- global_dt[dt[na_idx, .(linkID)], on = "linkID"]
    dt[na_idx, (cols) := repl[, mget(cols)]]
  }
  na.omit(dt)
}

sd_na_is_0 <- function(x) {
  if (length(x) >= 2L) stats::sd(x) else 0
}

first_order_uniform <- function(n, rho = 0.31) {
  if (n <= 1L) {
    return(runif(1))
  }
  S <- diag(n)
  for (i in seq_len(n)) {
    if (i - 1L > 0L) S[i, i - 1L] <- rho
    if (i + 1L <= n) S[i, i + 1L] <- rho
  }
  diag(S) <- 1
  if (!all(eigen(S, symmetric = TRUE)$values >= 0)) {
    S <- as.matrix(Matrix::nearPD(S, corr = TRUE)$mat)
  }
  c(stats::pnorm(mvtnorm::rmvnorm(1, sigma = S)))
}

second_order_uniform <- function(n, rho = 0.31) {
  if (n <= 2L) {
    return(runif(n))
  }
  S <- diag(n)
  for (i in seq_len(n)) {
    if (i - 2L > 0L) S[i, i - 2L] <- rho^2
    if (i + 2L <= n) S[i, i + 2L] <- rho^2
    if (i - 1L > 0L) S[i, i - 1L] <- rho
    if (i + 1L <= n) S[i, i + 1L] <- rho
  }
  S[1, 3] <- rho
  S[3, 1] <- rho
  diag(S) <- 1
  if (!all(eigen(S, symmetric = TRUE)$values >= 0)) {
    S <- as.matrix(Matrix::nearPD(S, corr = TRUE)$mat)
  }
  c(stats::pnorm(mvtnorm::rmvnorm(1, sigma = S)))
}

simulate_log_normal_sum <- function(mu, sigma, U) {
  sum(exp(mu + sigma * stats::qnorm(U)))
}

population_simulator <- function(duration_secs) {
  l <- length(duration_secs) + 1L
  Z <- stats::rnorm(l, 0, sqrt(1))
  mu <- mean(duration_secs)
  sigma <- sd_na_is_0(duration_secs)
  t <- numeric(l)
  for (i in 2:l) {
    t[i] <- t[i - 1L] + mu + sigma * Z[i]
  }
  t[l]
}

simulate_clt_samplers_mc <- function(mu, sigma, duration_secs, n_mc, rho = 0.31) {
  n <- length(mu)
  acc <- c(independent = 0, full = 0, first = 0, second = 0)
  for (k in seq_len(n_mc)) {
    acc["independent"] <- acc["independent"] +
      simulate_log_normal_sum(mu, sigma, runif(n))
    acc["full"] <- acc["full"] +
      simulate_log_normal_sum(mu, sigma, if (n > 1L) dependent_uniform(n, rho) else runif(1))
    acc["first"] <- acc["first"] +
      simulate_log_normal_sum(mu, sigma, first_order_uniform(n, rho))
    acc["second"] <- acc["second"] +
      simulate_log_normal_sum(mu, sigma, second_order_uniform(n, rho))
  }
  acc / n_mc
}

load_fidelity_trips <- function(seed = 1234L, n_test = 2000L) {
  trips <- data.table::fread("data/trips.csv")
  data.table::setnames(
    trips,
    c("trip", "time", "tt", "length", "linkId"),
    c("tripID", "entry_time", "duration_secs", "distance_meters", "linkID")
  )
  trips[, entry_time := as.POSIXct(entry_time, tz = "UTC")]
  trips[, timeBin := time_bins_readable(entry_time)]
  data.table::setorder(trips, tripID, entry_time)

  set.seed(seed)
  test_ids <- sample(unique(trips$tripID), n_test)
  list(
    train = trips[!tripID %in% test_ids],
    test = trips[tripID %in% test_ids],
    test_ids = test_ids
  )
}

prepare_test_edges <- function(train, test) {
  timebin_x_edges <- get_timeBin_x_edges(
    tripID = train$tripID,
    linkId = train$linkID,
    length = train$distance_meters,
    timeBin = train$timeBin,
    duration = train$duration_secs
  )
  data.table::setnames(timebin_x_edges, c("timeBin", "linkId"), c("timeBin", "linkID"))

  test_edges <- merge(
    test[, .(tripID, linkID, timeBin, duration_secs, distance_meters, entry_time)],
    timebin_x_edges,
    by = c("linkID", "timeBin"),
    all.x = TRUE
  )
  test_edges <- fill_timebin_edges(test_edges, timebin_x_edges)
  if (any(is.na(test_edges$mean))) {
    stop("Some test edges still missing mean/sd after Global fallback.")
  }
  data.table::setorder(test_edges, tripID, entry_time)
  test_edges
}

route_duration_from_similar <- function(route_dt) {
  route_dt[, .(duration = sum(exp(mean + stats::qnorm(dependent_uniform(.N)) * sd))), by = tripID]
}

fidelity_panel_theme <- function(base = PANEL_FONT_PT, axis = AXIS_TITLE_PT,
                               legend = LEGEND_TEXT_PT,
                               legend_inside = FALSE) {
  theme <- ggplot2::theme_minimal(base_size = base, base_family = "sans") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      text = ggplot2::element_text(family = "sans", size = base),
      axis.title = ggplot2::element_text(size = axis),
      axis.text = ggplot2::element_text(size = axis),
      legend.text = ggplot2::element_text(size = legend),
      legend.key = ggplot2::element_rect(fill = "white", colour = NA),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
  if (legend_inside) {
    theme + ggplot2::theme(
      legend.position = c(0.98, 0.05),
      legend.justification = c(1, 0),
      legend.box = "vertical"
    )
  } else {
    theme + ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical"
    )
  }
}

save_fidelity_png <- function(plot, path, width, height, latex_img) {
  ggplot2::ggsave(path, plot, width = width, height = height, dpi = 300, bg = "white")
  file.copy(path, file.path(latex_img, basename(path)), overwrite = TRUE)
}

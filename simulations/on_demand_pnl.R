# Realized profit and loss of the on-demand guarantee over the held-out trips (Sec. 7.5).
#
# Produces the empirical panel of the paper's profit-and-loss figure: for every test trip the
# provider collects the premium R and pays the overrun above the strike K, so the per-ride
# profit is R - (P - K)^+ at cutoff zeta = 0. Realized fares come from the observed edge
# durations, not from a sampler, so nothing here is simulated except the premium itself.
#
#   cd simulations && Rscript on_demand_pnl.R

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(traveltimeCLT)
})

SEED <- 1234L
N_TEST <- 2000L
MODELS <- c("trip-specific", "population")
# The panel in the paper pairs one empirical payoff against the theoretical one, so only the
# trip-specific series is drawn; both models are still summarised.
PLOT_MODELS <- "trip-specific"
OUT_PNG <- "plot/on_demand_pnl.png"
OUT_CSV <- "plot/plot_data/on_demand_pnl.csv"

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

# Realized fare uses the observed travel time, summed over the trip's edges.
realized <- test[, .(
  start_time = entry_time[1],
  duration = sum(duration_secs),
  distance = sum(distance_meters)
), by = tripID]
realized[, real_price := price(duration, distance)[, 1]]

#' Rescale sigma_profile so predicted and realized travel times have matching spread.
#'
#' The profile estimator charges all between-trip variation in mean edge duration to
#' within-trip edge noise and rescales by 1/E[1/N] ~ 49. With edge lengths spanning three
#' orders of magnitude that inflates it: the fitted value leaves the predicted spread about
#' 2.6 times too wide, and it swings between 48 and 375 across random 500-trip subsamples.
calibrate_population_sigma <- function(fit, data) {
  realized <- data[, .(realized = sum(duration_secs), n_edges = .N), by = tripID]
  predicted <- as.data.table(predict(fit, data))
  d <- na.omit(merge(realized, predicted[, .(tripID, ETA)], by = "tripID"))
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
  dt[, profit := premium - pmax(real_price - strike, 0)]
  dt[, model := model]
  na.omit(dt)
}

pnl <- rbindlist(lapply(MODELS, price_guarantee))

# The five summaries of Sec. 7.3.
summary <- pnl[, .(
  win_rate = mean(profit > 0),
  profit_factor = sum(profit[profit > 0]) / abs(sum(profit[profit < 0])),
  avg_profit = mean(profit),
  max_loss = min(profit),
  premium_over_price = 100 * mean(premium / real_price)
), by = model]
print(summary)

dir.create(dirname(OUT_CSV), showWarnings = FALSE, recursive = TRUE)
fwrite(pnl[, .(model, tripID, real_price, strike, premium, profit)], OUT_CSV)

# Axes are scaled by each trip's strike so that the kink of the payoff lands at 1 for every
# ride; what the panel then shows is where the realized fares fall along that payoff.
pnl[, price_ratio := real_price / strike]
pnl[, profit_ratio := 100 * profit / strike]
drawn <- pnl[model %in% PLOT_MODELS]

figure <- ggplot(drawn, aes(x = price_ratio, y = profit_ratio)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey40") +
  geom_vline(xintercept = 1, linewidth = 0.3, linetype = 2, color = "grey40") +
  geom_point(size = 0.45, alpha = 0.3, color = "#4575b4") +
  coord_cartesian(
    xlim = quantile(drawn$price_ratio, c(0.002, 0.998)),
    ylim = quantile(drawn$profit_ratio, c(0.002, 0.999))
  ) +
  labs(
    x = expression(paste("Realized fare relative to the guarantee, ", P / K)),
    y = expression(paste("Profit, % of ", K))
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(OUT_PNG, figure, width = 4.6, height = 3.6, dpi = 300)
cat("Wrote", OUT_PNG, "and", OUT_CSV, "\n")

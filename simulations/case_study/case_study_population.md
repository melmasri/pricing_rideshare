Return Analysis
================
Mingze Li
2015-04-16

``` r
library(traveltimeCLT)
library(data.table)
# setwd("D:/李明泽/留学/Statistics Master/travel pricing/R-simulation/")
```

``` r
trips <- fread("data/trips.csv")
trips$timeBin <- time_bins_readable(trips$time)
start_end <- trips[
  , .(
    start = linkId[order(time)][1],
    end = linkId[order(time)[.N]],
    start_time = time[order(time)][1],
    distance = sum(length)
  ), trip
]

# start_end[, sorted_pair := fcase(
#   start < end, paste(start, end, sep = "->"),
#   start > end, paste(end, start, sep = "->")
# )]

# pair_counts <- start_end[, .(count = .N), by = sorted_pair][order(-count)]
pair_counts <- start_end[, .(count = .N, start_time = max(start_time)), by = .(start, end)][order(-count)]
```

``` r
# hyperparameters of the simulation.
set.seed(1234)
# repeat time: how many times to repeat the simulation.
repeat_time <- 100
# X: how many trips to simulate for each real trip.
X <- 3
# significance level: how similar the new edges are to the old edges.
# a larger value means less similar, a smaller value means more similar.
significance_level <- 0.05
# sigma_n: the standard deviation of the noise added to the number of edges.
sigma_n <- 2
# trip_num: the number of trips to simulate.
trip_num <- 30
```

``` r
# Run the same case study for multiple group sizes in one report.
# group_size controls how many real trips are bundled per "counterfactual rider".

group_sizes <- c(2, 4, 7, 11)

# Keep raw `trips` for `similar_route()` (needs `trip` + `time`).
# Build a modeling copy for `traveltimeCLT()` / `predict()` (needs `tripID`, etc.).
trips_fit <- copy(trips)
names(trips_fit)[c(2, 3, 5, 7, 8)] <- c(
  "tripID", "entry_time", "duration_secs", "distance_meters", "linkID"
)
trips_fit$speed <- exp(trips_fit$logspeed)
fit <- traveltimeCLT(trips_fit, "population")

run_case <- function(group_size) {
  group_size <- as.integer(group_size)
  if (group_size <= 0) stop("group_size must be positive")

  simulated_data <- data.table(
    repeat_time = rep(1:repeat_time, each = group_size)
  )
  groups <- pair_counts[count >= group_size + 1, ]
  if (nrow(groups) == 0) stop("No (start,end) pairs with enough trips for this group_size")

  simulated_data <- simulated_data[,
    {
      group <- groups[sample.int(nrow(groups), 1)]
      trip_set <- start_end[(start == group$start & end == group$end)]
      id <- sample.int(nrow(trip_set), group_size)
      id2 <- which(!1:nrow(trip_set) %in% id)
      if (length(id2) == 1) {
        id2 <- rep(id2, group_size)
      } else {
        id2 <- sample(id2, group_size, replace = TRUE)
      }
      .(
        trip = trip_set$trip[id],
        start_time = trip_set$start_time[id],
        trip2 = trip_set$trip[id2],
        start_time2 = trip_set$start_time[id2]
      )
    },
    by = repeat_time
  ]

  simulated_test <- similar_route(
    simulated_data$trip, trips,
    r = X,
    sigma_n = sigma_n,
    significance = significance_level,
    Ftest_sd = FALSE
  )
  simulated_test <- merge(simulated_test, unique(simulated_data[, c("trip", "start_time")]), by = "trip")
  setnames(
    simulated_test,
    old = c("newtrip", "linkId", "length", "start_time"),
    new = c("tripID", "linkID", "distance_meters", "entry_time"),
    skip_absent = TRUE
  )

  pricing_routes <- trips_fit[
    data.table(trip = simulated_data$trip2)[, idx := .I],
    on = .(tripID = trip),
    nomatch = 0
  ]
  pricing_routes[, tripID := idx]

  fit_price <- predict(fit, pricing_routes)
  fit_test <- predict(fit, simulated_test)

  price_stat <- pricing_routes[, .(
    start_time = min(entry_time),
    distance = sum(distance_meters)
  ), by = tripID]
  price_stat$R <- request_R(fit_price, price_stat$start_time, price_stat$start_time, price_stat$distance, K = 0.9, risk_free = 0, zeta = 0)
  price_stat$K <- request_K(fit_price, price_stat$distance, discount_factor = 0.9)
  price_stat <- price_stat[rep(1:.N, each = X)]

  test_time <- simulated_test[, .(
    duration = sum(exp(mean + qnorm(dependent_uniform(.N, 0.31)) * sd)),
    distance = sum(distance_meters)
  ), tripID]
  test_time$price <- price(test_time$duration, test_time$distance)[, 1]

  profit <- price_stat$R - pmax(test_time$price - price_stat$K, 0)

  # profit stats under different realized-trip counts
  used_trip <- c(1, 0.9, 0.8, 0.7, 0.6)
  trips_per_batch <- group_size * X
  profit_real <- rbindlist(lapply(used_trip, function(used_trip) {
    keep_num <- floor(trips_per_batch * used_trip)
    dt <- data.table(
      used_trip = used_trip,
      index = seq_along(profit)
    )
    dt[, `:=`(
      batch = (index - 1) %/% trips_per_batch + 1,
      pos_in_batch = (index - 1) %% trips_per_batch + 1
    )]
    dt[, profit := fifelse(
      pos_in_batch <= keep_num,
      price_stat$R[index] - pmax(test_time$price[index] - price_stat$K[index], 0),
      price_stat$R[index]
    )]
    dt[, .(
      mean = mean(profit),
      var = var(profit),
      max = max(profit),
      min = min(profit),
      winrate = sum(profit > 0) / .N
    ), by = used_trip]
  }))

  list(
    group_size = group_size,
    profit = profit,
    profit_summary = data.table(
      mean = mean(profit),
      var = var(profit),
      max = max(profit),
      min = min(profit),
      winrate = sum(profit > 0) / length(profit)
    ),
    profit_real = profit_real
  )
}

results <- lapply(group_sizes, run_case)
names(results) <- paste0("g", group_sizes)

for (res in results) {
  cat("\n\n## Group size = ", res$group_size, "\n\n", sep = "")
  print(res$profit_summary)
  print(res$profit_real)
}
```

    ## 
    ## 
    ## ## Group size = 2
    ## 
    ##         mean      var      max       min   winrate
    ##        <num>    <num>    <num>     <num>     <num>
    ## 1: -1.243649 47.24107 4.868596 -39.48475 0.6316667
    ##    used_trip        mean      var      max       min   winrate
    ##        <num>       <num>    <num>    <num>     <num>     <num>
    ## 1:       1.0 -1.24364933 47.24107 4.868596 -39.48475 0.6316667
    ## 2:       0.9 -0.59163574 41.72582 4.868596 -39.48475 0.6950000
    ## 3:       0.8  0.07982623 34.24396 4.868596 -39.48475 0.7566667
    ## 4:       0.7  0.07982623 34.24396 4.868596 -39.48475 0.7566667
    ## 5:       0.6  0.74036184 27.38610 4.868596 -39.48475 0.8233333
    ## 
    ## 
    ## ## Group size = 4
    ## 
    ##         mean      var      max       min winrate
    ##        <num>    <num>    <num>     <num>   <num>
    ## 1: -1.099588 40.13983 5.219305 -28.56151  0.6425
    ##    used_trip        mean      var      max       min   winrate
    ##        <num>       <num>    <num>    <num>     <num>     <num>
    ## 1:       1.0 -1.09958827 40.13983 5.219305 -28.56151 0.6425000
    ## 2:       0.9 -0.52388434 36.23084 5.219305 -26.44385 0.7008333
    ## 3:       0.8 -0.25099114 34.28088 5.219305 -26.44385 0.7291667
    ## 4:       0.7  0.04210093 31.56271 5.219305 -24.41083 0.7608333
    ## 5:       0.6  0.32753162 28.99518 5.219305 -23.86385 0.7916667
    ## 
    ## 
    ## ## Group size = 7
    ## 
    ##          mean      var      max       min   winrate
    ##         <num>    <num>    <num>     <num>     <num>
    ## 1: -0.5112724 42.13603 5.780474 -55.32205 0.6828571
    ##    used_trip        mean      var      max       min   winrate
    ##        <num>       <num>    <num>    <num>     <num>     <num>
    ## 1:       1.0 -0.51127236 42.13603 5.780474 -55.32205 0.6828571
    ## 2:       0.9 -0.02679501 38.08059 5.780474 -55.32205 0.7328571
    ## 3:       0.8  0.37858227 33.01644 5.780474 -55.32205 0.7680952
    ## 4:       0.7  0.72553857 28.78933 5.780474 -55.32205 0.7990476
    ## 5:       0.6  1.02697757 25.73811 5.780474 -55.32205 0.8300000
    ## 
    ## 
    ## ## Group size = 11
    ## 
    ##         mean      var      max       min  winrate
    ##        <num>    <num>    <num>     <num>    <num>
    ## 1: 0.8678659 13.56614 2.994783 -22.27374 0.819697
    ##    used_trip      mean       var      max       min   winrate
    ##        <num>     <num>     <num>    <num>     <num>     <num>
    ## 1:       1.0 0.8678659 13.566141 2.994783 -22.27374 0.8196970
    ## 2:       0.9 1.0151709 12.655415 2.994783 -22.27374 0.8393939
    ## 3:       0.8 1.1391774 11.784924 2.994783 -22.27374 0.8548485
    ## 4:       0.7 1.2622801 10.829116 2.994783 -22.27374 0.8687879
    ## 5:       0.6 1.4530239  9.313038 2.994783 -22.27374 0.8903030

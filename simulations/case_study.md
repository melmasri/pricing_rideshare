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
fit <- traveltimeCLT(trips_fit, "trip-specific")
```

    ## Warning in traveltimeCLT(trips_fit, "trip-specific"): 4 trips have less than 1
    ## observation, and will not be used to estimate autocorrelations, or residual
    ## variance parameters

``` r
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
  profit_real <- rbindlist(lapply(used_trip, function(used_trip) {
    keep_num <- floor(trip_num * used_trip)
    dt <- data.table(
      used_trip = used_trip,
      index = 1:(trip_num * repeat_time)
    )
    dt[, `:=`(
      batch = (index - 1) %/% trip_num + 1,
      pos_in_batch = (index - 1) %% trip_num + 1
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
    ## 1: -1.721996 73.56888 14.35629 -34.97689 0.6466667
    ##    used_trip  mean   var   max   min winrate
    ##        <num> <num> <num> <num> <num>   <num>
    ## 1:       1.0    NA    NA    NA    NA      NA
    ## 2:       0.9    NA    NA    NA    NA      NA
    ## 3:       0.8    NA    NA    NA    NA      NA
    ## 4:       0.7    NA    NA    NA    NA      NA
    ## 5:       0.6    NA    NA    NA    NA      NA
    ## 
    ## 
    ## ## Group size = 4
    ## 
    ##         mean      var      max       min   winrate
    ##        <num>    <num>    <num>     <num>     <num>
    ## 1: -1.064359 44.32862 6.843418 -33.15848 0.6391667
    ##    used_trip  mean   var   max   min winrate
    ##        <num> <num> <num> <num> <num>   <num>
    ## 1:       1.0    NA    NA    NA    NA      NA
    ## 2:       0.9    NA    NA    NA    NA      NA
    ## 3:       0.8    NA    NA    NA    NA      NA
    ## 4:       0.7    NA    NA    NA    NA      NA
    ## 5:       0.6    NA    NA    NA    NA      NA
    ## 
    ## 
    ## ## Group size = 7
    ## 
    ##         mean      var      max       min   winrate
    ##        <num>    <num>    <num>     <num>     <num>
    ## 1: -1.754819 76.06654 14.35629 -48.01681 0.6109524
    ##    used_trip  mean   var   max   min winrate
    ##        <num> <num> <num> <num> <num>   <num>
    ## 1:       1.0    NA    NA    NA    NA      NA
    ## 2:       0.9    NA    NA    NA    NA      NA
    ## 3:       0.8    NA    NA    NA    NA      NA
    ## 4:       0.7    NA    NA    NA    NA      NA
    ## 5:       0.6    NA    NA    NA    NA      NA
    ## 
    ## 
    ## ## Group size = 11
    ## 
    ##          mean      var      max       min   winrate
    ##         <num>    <num>    <num>     <num>     <num>
    ## 1: -0.1897144 19.21852 2.983138 -19.50903 0.7109091
    ##    used_trip        mean      var      max       min   winrate
    ##        <num>       <num>    <num>    <num>     <num>     <num>
    ## 1:       1.0 -0.28601438 20.00299 2.983138 -19.50903 0.7056667
    ## 2:       0.9 -0.02760198 18.25524 2.983138 -19.50903 0.7356667
    ## 3:       0.8  0.25462840 16.21548 2.983138 -17.56303 0.7663333
    ## 4:       0.7  0.50619906 14.42990 2.983138 -17.56303 0.7960000
    ## 5:       0.6  0.74147463 12.76246 2.983138 -17.56303 0.8253333

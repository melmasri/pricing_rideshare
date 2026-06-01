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
group_size <- ceiling(trip_num / X)
simulated_data <- data.table(
  repeat_time = rep(1:repeat_time, each = group_size)
)
groups <- pair_counts[count >= group_size + 1, ]
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
simulated_test <- similar_route(simulated_data$trip, trips, r = X, sigma_n = sigma_n, significance = significance_level, Ftest_sd = F)
simulated_test <- merge(simulated_test, unique(simulated_data[, c("trip", "start_time")]), by = "trip")
setnames(simulated_test, old = c("newtrip", "linkId", "length", "start_time"), new = c("tripID", "linkID", "distance_meters", "entry_time"), skip_absent = TRUE)
pricing_routes <- trips[data.table(trip = simulated_data$trip2)[, idx := .I], on = .(trip), nomatch = 0]
setnames(pricing_routes, old = c("tripID", "linkId", "length", "time", "idx"), new = c("trip", "linkID", "distance_meters", "entry_time", "tripID"), skip_absent = TRUE)
```

``` r
# find all R and K in the trips set
names(trips)[c(2, 3, 5, 7, 8)] <- c("tripID", "entry_time", "duration_secs", "distance_meters", "linkID")
trips_stat <- trips[, .(
  real_price = price(max(entry_time) - min(entry_time), sum(distance_meters))[, 1]
), tripID]
trips$speed <- exp(trips$logspeed)
fit <- traveltimeCLT(trips, "trip-specific")
```

    ## Warning in traveltimeCLT(trips, "trip-specific"): 4 trips have less than 1
    ## observation, and will not be used to estimate autocorrelations, or residual
    ## variance parameters

``` r
fit_price <- predict(fit, pricing_routes)
fit_test <- predict(fit, simulated_test)
price_stat <- pricing_routes[, .(
  start_time = min(entry_time),
  distance = sum(distance_meters)
), by = tripID]
price_stat$R <- request_R(fit_price, price_stat$start_time, price_stat$start_time, price_stat$distance, K = 0.9, risk_free = 0, zeta = 0)
price_stat$K <- request_K(fit_price, price_stat$distance, discount_factor = 0.9)
# repeat each row X times
price_stat <- price_stat[
  rep(1:.N, each = X)
]


test_stat <- simulated_test[, .(
  start_time = min(entry_time),
  distance = sum(distance_meters)
), tripID]
test_stat$R <- request_R(fit_test, test_stat$start_time, test_stat$start_time, test_stat$distance, K = 0.9, risk_free = 0, zeta = 0)
test_stat$K <- request_K(fit_test, test_stat$distance, discount_factor = 0.9)
```

``` r
test_time <- simulated_test[, .(
  duration = sum(exp(mean + qnorm(dependent_uniform(.N, 0.31)) * sd)),
  distance = sum(distance_meters)
), tripID]
test_time$price <- price(test_time$duration, test_time$distance)[, 1]
revenue <- sum(price_stat$R) / repeat_time
revenue
```

    ## [1] 103.0014

``` r
profit <- price_stat$R - pmax(test_time$price - price_stat$K, 0)
```

``` r
mean(profit)
```

    ## [1] -0.3407812

``` r
var(profit)
```

    ## [1] 81.89412

``` r
max(profit)
```

    ## [1] 14.34487

``` r
min(profit)
```

    ## [1] -39.59296

``` r
length(profit[profit > 0]) / length(profit) # win rate
```

    ## [1] 0.7423333

``` r
# calculate the profit stats under different real travel number
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
group_size <- group_size + 1
group_size
```

    ## [1] 11

``` r
repeat_time
```

    ## [1] 100

``` r
profit_real
```

    ##    used_trip        mean      var      max       min   winrate
    ##        <num>       <num>    <num>    <num>     <num>     <num>
    ## 1:       1.0 -0.34078125 81.89412 14.34487 -39.59296 0.7423333
    ## 2:       0.9  0.01313691 76.02364 14.34487 -39.59296 0.7686667
    ## 3:       0.8  0.41056908 69.01463 14.34487 -39.59296 0.7950000
    ## 4:       0.7  0.75311683 63.03109 14.34487 -39.59296 0.8186667
    ## 5:       0.6  1.07961718 57.46031 14.34487 -39.59296 0.8420000

``` r
setnames(trips, old = c("tripID", "linkID", "distance_meters", "entry_time"), new = c("trip", "linkId", "length", "time"), skip_absent = TRUE)
timeBin_x_edge <- get_timeBin_x_edges(trips)
i <- which(profit == min(profit)) - 0
profit[i]
```

    ## [1] -39.59296

``` r
price_stat[i, ]
```

    ##    tripID          start_time distance        R        K
    ##     <int>              <POSc>    <num>    <num>    <num>
    ## 1:    541 2014-05-01 06:40:09 9402.861 1.579528 14.21565

``` r
test_time[i, "duration"]
```

    ##    duration
    ##       <num>
    ## 1: 4051.071

``` r
edge <- simulated_test[simulated_test$tripID == test_stat[i, tripID], ]
template <- na.omit(trips[trip == edge$trip[1], ])
setnames(template, old = c("linkID"), new = c("linkId"), skip_absent = TRUE)
template <- merge(template[, c("linkId", "timeBin")], timeBin_x_edge, by = c("linkId", "timeBin"))
edge
```

    ## Key: <trip>
    ##       trip tripID linkID     timeBin frequency     mean        sd
    ##      <int>  <num>  <int>      <char>     <int>    <num>     <num>
    ##   1: 10330   1878  10277 EveningRush         5 1.872852 0.1813295
    ##   2: 10330   1878    995 MorningRush        25 1.250507 0.3013743
    ##   3: 10330   1878   8666 MorningRush         3 1.574945 0.1467966
    ##   4: 10330   1878  17036 MorningRush        11 2.303579 0.1161205
    ##   5: 10330   1878  39419     Weekday         2 1.480700 0.1820632
    ##  ---                                                             
    ## 266: 10330   1878  41219 EveningRush         4 2.696539 0.1745173
    ## 267: 10330   1878  42858     Weekday         4 2.232508 0.1612023
    ## 268: 10330   1878  32793 EveningRush         3 3.664017 0.2191419
    ## 269: 10330   1878   2461 EveningRush         3 3.067973 0.1552725
    ## 270: 10330   1878  30482 EveningRush        11 1.375329 0.2833914
    ##      distance_meters          entry_time
    ##                <num>              <POSc>
    ##   1:          77.222 2014-05-06 07:10:18
    ##   2:          42.123 2014-05-06 07:10:18
    ##   3:          78.482 2014-05-06 07:10:18
    ##   4:         141.740 2014-05-06 07:10:18
    ##   5:          51.335 2014-05-06 07:10:18
    ##  ---                                    
    ## 266:         147.255 2014-05-06 07:10:18
    ## 267:         181.541 2014-05-06 07:10:18
    ## 268:         147.055 2014-05-06 07:10:18
    ## 269:         274.900 2014-05-06 07:10:18
    ## 270:          56.450 2014-05-06 07:10:18

``` r
edge[, .(
  duration = sum(exp(mean + qnorm(dependent_uniform(.N, 0.31)) * sd)),
  expection = sum(exp(mean)),
  noise = sum(sd)
), tripID]
```

    ##    tripID duration expection    noise
    ##     <num>    <num>     <num>    <num>
    ## 1:   1878 3643.283  3251.542 97.55064

``` r
template[, .(
  duration = sum(exp(mean + qnorm(dependent_uniform(.N, 0.31)) * sd)),
  expection = sum(exp(mean)),
  noise = sum(sd)
)]
```

    ##    duration expection    noise
    ##       <num>     <num>    <num>
    ## 1: 3323.095  3205.467 97.45884

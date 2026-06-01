Return Analysis
================
Mingze Li
2015-04-16

``` r
# Load required libraries
library(traveltimeCLT)
library(data.table)
```

``` r
set.seed(12345)
trips <- fread("data/trips.csv")
id <- sample(unique(trips$trip), 1000)
train <- trips[!trips$trip %in% id, ]
test <- trips[trips$trip %in% id, ]
timeBin_x_edge <- get_timeBin_x_edges(train)
```

``` r
sample <- pressure_test1(id, trips, r = 1000, timeBin_x_edges = timeBin_x_edge, lambda = 0, severity = 0)
pressure_test <- pressure_test1(id, trips, r = 1000, timeBin_x_edges = timeBin_x_edge, lambda = 0.1, severity = 0)
```

``` r
plot_CDF_compare(sample[[2]]$real_time, sample[[1]]$dependent_time, "frequency simulation")
```

![](return_analysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
plot_CDF_compare(pressure_test[[2]]$real_time, pressure_test[[1]]$dependent_time, "frequency simulation")
```

![](return_analysis_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
real_price <- price(sample[[2]]$real_time, sample[[2]]$real_length)[, 1]
simulated_price <- data.table(
  simulated_price = price(sample[[1]]$dependent_time, sample[[1]]$simulated_length)[1:1000, 1],
  pressured_price = price(pressure_test[[1]]$dependent_time, pressure_test[[1]]$simulated_length)[, 1]
)
```

``` r
names(train)[c(2, 3, 5, 7, 8)] <- c("tripID", "entry_time", "duration_secs", "distance_meters", "linkID")
train$speed <- exp(train$logspeed)
train$timeBin <- time_bins_readable(train$entry_time)
fit <- traveltimeCLT(train)
```

    ## Warning in traveltimeCLT(train): 4 trips have less than 1 observation, and will
    ## not be used to estimate autocorrelations, or residual variance parameters

``` r
test <- trips[trips$trip %in% id, ]
names(test)[c(2, 3, 5, 7, 8)] <- c("tripID", "entry_time", "1", "distance_meters", "linkID")
prep_sim_data <- function(sim) {
  d <- copy(sim[[3]])
  setnames(d, c("trip", "linkId", "start_time", "length"), c("tripID", "linkID", "entry_time", "distance_meters"))
  d
}
sample_routes <- prep_sim_data(sample)
pressure_routes <- prep_sim_data(pressure_test)
p <- predict(fit, test)
simulated_p <- predict(fit, sample_routes)
pressure_p <- predict(fit, pressure_routes)
```

``` r
start_times <- test[, .(start_time = entry_time[1]), by = tripID]
simulated_start_time <- sample_routes[, .(start_time = entry_time[1]), by = tripID][["start_time"]]
pressure_start_time <- pressure_routes[, .(start_time = entry_time[1]), by = tripID][["start_time"]]
R <- request_R(p, start_times$start_time, start_times$start_time, sample[[2]]$real_length, 1, risk_free = 0)
simulated_R <- request_R(simulated_p, simulated_start_time, simulated_start_time, sample[[1]]$simulated_length, 1, risk_free = 0)
pressure_R <- request_R(pressure_p, pressure_start_time, pressure_start_time, pressure_test[[1]]$simulated_length, 1, risk_free = 0)
```

``` r
K <- request_K(p, sample[[2]]$real_length)
simulated_K <- request_K(simulated_p, sample[[1]]$simulated_length)
pressure_K <- request_K(pressure_p, pressure_test[[1]]$simulated_length)
income <- mean(simulated_R + simulated_K, na.rm = TRUE)
normal_expand <- real_price
pressure_expand <- simulated_price$pressured_price
mean(real_price)
```

    ## [1] 24.14148

``` r
income
```

    ## [1] 25.80855

``` r
mean(income - normal_expand) / income * 100
```

    ## [1] 6.459354

``` r
mean(income - pressure_expand) / income * 100
```

    ## [1] 5.9471

``` r
sd((income - pressure_expand) / income * 100)
```

    ## [1] 43.00889

``` r
var((income - pressure_expand) / income * 100)
```

    ## [1] 1849.765

``` r
par(mfrow = c(1, 2))
plot(density(na.omit(income - normal_expand)), xlab = "income", main = "normal state")
abline(v = 0, col = "red", lty = 2)
plot(density(na.omit(income - pressure_expand)), xlab = "income", main = "lambda = 0.6, alpha = 0.6")
abline(v = 0, col = "red", lty = 2)
```

![](return_analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

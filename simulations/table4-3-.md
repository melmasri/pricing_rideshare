Table 4
================
Mingze Li
2015-08-10

``` r
library(traveltimeCLT)
library(data.table)
```

``` r
trips <- fread("data/trips.csv")
trips2 <- fread("data/trips.csv")
names(trips)[c(2, 3, 5, 7, 8)] <- c("tripID", "entry_time", "duration_secs", "distance_meters", "linkID")
trips$speed <- exp(trips$logspeed)
trips$timeBin <- time_bins_readable(trips$entry_time)

trips_record <- trips[, .(
    entry_time = rep(entry_time[1], .N),
    timeBin,
    logspeed,
    tripID,
    linkID,
    distance_meters
), tripID]
trips_stat <- trips[, .(
    start_time = entry_time[1],
    distance = sum(distance_meters),
    duration_secs = max(entry_time) - min(entry_time),
    real_price = price(max(entry_time) - min(entry_time), sum(distance_meters))[, 1],
    timeBin = timeBin[1]
), tripID]
```

``` r
# find R and K for all trips in trips.csv.
fit <- traveltimeCLT(trips, "trip-specific")
```

    ## Warning in traveltimeCLT(trips, "trip-specific"): 4 trips have less than 1
    ## observation, and will not be used to estimate autocorrelations, or residual
    ## variance parameters

``` r
pt <- predict(fit, trips_record)
```

    ## Warning in aux[(nlink - length(a) + 1):nlink] <- a: number of items to replace
    ## is not a multiple of replacement length

    ## Warning in aux[(nlink - length(a) + 1):nlink] <- a: number of items to replace
    ## is not a multiple of replacement length
    ## Warning in aux[(nlink - length(a) + 1):nlink] <- a: number of items to replace
    ## is not a multiple of replacement length
    ## Warning in aux[(nlink - length(a) + 1):nlink] <- a: number of items to replace
    ## is not a multiple of replacement length

``` r
Rt <- request_R(pt, trips_stat$start_time, trips_stat$start_time, trips_stat$distance, K = 0.9, risk_free = 0, zeta = 0)
Kt <- request_K(pt, trips_stat$distance, discount_factor = 0.9)
trips_stat$Rt <- Rt
trips_stat$Kt <- Kt
# trips_stat$Rt <- -pmin(mean(Kt) - trips_stat$real_price, 0)
trips_stat <- na.omit(trips_stat) # remove trips with NA values in Rt and Kt.
```

``` r
# hyperparameters of the simulation.
# repeat time: how many times to repeat the simulation.
repeat_time <- 10
# number of riders: how many riders to simulate.
rider <- 10
# test size: how many trips to simulate for each rider.
test_size <- 40
quantile <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
# quantile <- c(0)
M <- seq(0, floor(test_size * 0.5), floor(test_size * 0.1))
# M <- c(0)
set.seed(1234)

pressure_data <- data.table(
    repeat_time = rep(1:repeat_time, each = length(quantile) * max(rider) * length(M)),
    quantile = rep(rep(quantile, each = max(rider)), repeat_time * length(M)),
    rider = rep(1:rider, length(M) * length(quantile) * repeat_time),
    M = rep(rep(M, each = length(quantile) * max(rider)), repeat_time)
)

pressure_data <- pressure_data[, .(
    i = seq(1, test_size, 1)
), by = .(repeat_time, quantile, rider, M)]

# sample from trips_stat with replacement, the distance limited from quantile to 1.
sampled_stat <- pressure_data[,
    {
        id <- sample.int(nrow(trips_stat), test_size, replace = FALSE)
        test <- trips_stat[id]
        train <- trips_stat[-id]
        samples <- .SD[,
            {
                q_value <- quantile(test$distance, probs = .BY$quantile)
                abuse_trips <- trips_stat[distance >= q_value]
                abuse_sample <- abuse_trips[sample.int(nrow(abuse_trips), size = .BY$M, replace = TRUE)]
                normal_sample <- test[sample.int(test_size, size = test_size - .BY$M, replace = TRUE)]

                # combine the abuse sample and normal sample, then add some columns.
                combined <- rbindlist(list(
                    abuse_sample[, .(tripID, Kt, timeBin)],
                    normal_sample[, .(tripID, Kt, timeBin)]
                ))[, `:=`(
                    # Rt = rep(mean(-pmin(mean(train$Kt) - trips_stat$real_price, 0)), test_size),
                    Rt = train[sample.int(nrow(train), size = test_size, replace = TRUE)]$Rt
                )]

                cbind(.SD, combined)
            },
            by = .(quantile, rider, M)
        ]

        samples
    },
    by = .(repeat_time)
]
pressure_data <- merge(pressure_data, sampled_stat, by = c("repeat_time", "quantile", "rider", "M", "i"), all.x = TRUE)
pressure_data$timeBin <- NULL
```

``` r
# simulate the travel time for each trip
# obtain the frequence of trips and timebin group
unique_trips <- sampled_stat[
    ,
    .(repeat_time = .N),
    .(tripID, timeBin)
]

# get the linkID in each trip
unique_trips <- merge(unique_trips, trips[, .(tripID, linkID)], by = "tripID", allow.cartesian = TRUE)
# use timebin x edges simulation to obtain the predicted travel time
timebin_x_edges <- get_timeBin_x_edges(trips2)
setnames(timebin_x_edges, old = c("timeBin", "linkId"), new = c("timeBin", "linkID"))
unique_trips <- merge(unique_trips, timebin_x_edges, by = c("timeBin", "linkID"), all.x = TRUE)
unique_trips$old_timeBin <- unique_trips$timeBin
global_dt <- timebin_x_edges[timeBin == "Global", ]
na_index <- which(is.na(unique_trips$mean))
cols <- c("timeBin", "mean", "sd", "frequency", "length", "ID")
if (length(na_index) > 0) {
    # 创建临时数据表包含需要替换的linkID
    temp_dt <- unique_trips[na_index, .(linkID)]

    # 执行连接操作（指定on参数）
    replacement <- global_dt[temp_dt, on = .(linkID), ..cols]

    # 执行替换
    unique_trips[na_index, (cols) := replacement]
}
unique_trips <- na.omit(unique_trips)
# simulate the travel time for each trip
sampled_travel_time <- unique_trips[,
    {
        time <- numeric(.SD$repeat_time[1])
        for (i in 1:.SD$repeat_time[1]) {
            time[i] <- sum(exp(mean + qnorm(dependent_uniform(nrow(.SD))) * sd))
        }
        distance <- sum(length)
        .(time = time, distance = rep(distance, .SD$repeat_time[1]), index = seq(1, .SD$repeat_time[1]))
    },
    by = .(old_timeBin, tripID)
]
setnames(sampled_travel_time, old = c("old_timeBin"), new = c("timeBin"))
sampled_stat <- sampled_stat[,
    index := 1:.N,
    by = .(tripID, timeBin)
]
sampled_stat <- merge(sampled_stat, sampled_travel_time, by = c("tripID", "timeBin", "index"), no.dup = FALSE)
sampled_stat$real_price <- price(sampled_stat$time, sampled_stat$distance)[, 1]
pressure_data <- merge(pressure_data, sampled_stat[, c("repeat_time", "quantile", "rider", "M", "i", "real_price")], by = c("repeat_time", "quantile", "M", "rider", "i"), all.x = TRUE)
```

``` r
# calculate the profit
profit <- pressure_data[, .(
    revenue = sum(Rt),
    cost = sum(-pmin(Kt - real_price, 0)),
    profit = sum(Rt) - sum(-pmin(Kt - real_price, 0))
), by = .(repeat_time, M, quantile, rider)]
cost_stats_table <- profit[, .(
    profit_over_revenue = mean(profit / cost),
    sd = sqrt(var(profit / cost, na.rm = TRUE) / sqrt(test_size)),
    dollar_mean = mean(profit, na.rm = TRUE)
), by = .(M, quantile)]
# table(sampled_data$real_price)
```

``` r
# reshape the data to draw the table
table4 <- dcast(cost_stats_table, M ~ quantile, value.var = "profit_over_revenue")
table5 <- dcast(cost_stats_table, M ~ quantile, value.var = "sd")
table6 <- dcast(cost_stats_table, M ~ quantile, value.var = "dollar_mean")
table4
```

    ## Key: <M>
    ##        M           0          0.1           0.2          0.3          0.4
    ##    <num>       <num>        <num>         <num>        <num>        <num>
    ## 1:     0  0.02473150 -0.013325544  0.0104324706  0.001506374  0.011163751
    ## 2:     4 -0.01413810 -0.012533071 -0.0008463387 -0.018754847 -0.007615085
    ## 3:     8 -0.02481657 -0.008949884 -0.0430692255 -0.038021231 -0.061798595
    ## 4:    12  0.00961885 -0.015901941 -0.0253572582 -0.043227249 -0.072145784
    ## 5:    16 -0.02781936 -0.027450025 -0.0413757317 -0.069931299 -0.081439843
    ## 6:    20  0.01709027 -0.065435187 -0.0526510526 -0.047912983 -0.108427153
    ##            0.5          0.6         0.7         0.8         0.9
    ##          <num>        <num>       <num>       <num>       <num>
    ## 1: -0.01856682  0.001057603  0.00711543  0.02191876 -0.01322781
    ## 2: -0.02975002 -0.020354627 -0.04057959 -0.07042229 -0.05859063
    ## 3: -0.08128001 -0.038428337 -0.08335399 -0.08606749 -0.13624624
    ## 4: -0.12919291 -0.110737345 -0.13854772 -0.13884464 -0.19980296
    ## 5: -0.11293802 -0.162297430 -0.20813657 -0.21671192 -0.26399433
    ## 6: -0.13244485 -0.176888437 -0.17272220 -0.23467599 -0.27214387

``` r
table5
```

    ## Key: <M>
    ##        M          0        0.1        0.2        0.3        0.4        0.5
    ##    <num>      <num>      <num>      <num>      <num>      <num>      <num>
    ## 1:     0 0.08577481 0.08879070 0.09093300 0.10509986 0.07942140 0.08455376
    ## 2:     4 0.08605922 0.08795636 0.06230677 0.08761973 0.07775329 0.08701536
    ## 3:     8 0.09749037 0.08643971 0.09689368 0.07379914 0.08398749 0.06907602
    ## 4:    12 0.07556302 0.07689052 0.07617334 0.06543877 0.06628983 0.08546911
    ## 5:    16 0.07009748 0.06326850 0.07241897 0.07218033 0.05674925 0.06164084
    ## 6:    20 0.09128670 0.08071390 0.07482047 0.07063003 0.06055785 0.05457888
    ##           0.6        0.7        0.8        0.9
    ##         <num>      <num>      <num>      <num>
    ## 1: 0.07330410 0.11272080 0.10252695 0.09060382
    ## 2: 0.07024870 0.08280246 0.08229929 0.07822797
    ## 3: 0.06699062 0.07602095 0.07840980 0.07139922
    ## 4: 0.07923848 0.06128053 0.06594091 0.06379282
    ## 5: 0.06121203 0.08162883 0.05894692 0.05843753
    ## 6: 0.07031759 0.06378396 0.06394317 0.04769099

``` r
table6
```

    ## Key: <M>
    ##        M          0       0.1        0.2         0.3        0.4        0.5
    ##    <num>      <num>     <num>      <num>       <num>      <num>      <num>
    ## 1:     0 -11.609728 -11.25325  -6.597925 -334.648772  -4.027515  -13.89953
    ## 2:     4  -8.673456 -91.16015  -2.220561  -19.623151  -5.953978 -422.34664
    ## 3:     8 -62.633554 -10.24307 -19.335912  -10.381475 -20.014873  -12.37648
    ## 4:    12  -2.439616 -72.64373  -6.913419   -8.173224 -10.983541  -35.42067
    ## 5:    16  -5.115875  -4.67243 -14.696243  -12.573609 -10.870190  -16.46359
    ## 6:    20 -60.214338 -17.11458 -17.128334   -8.518890 -15.101300  -17.32899
    ##             0.6        0.7        0.8        0.9
    ##           <num>      <num>      <num>      <num>
    ## 1:    -2.523072  -54.44545  -47.36123  -23.11488
    ## 2:    -4.759880  -34.68904  -43.37287  -21.50322
    ## 3:    -5.930181  -65.76471 -133.79217  -32.50849
    ## 4:   -28.231159  -25.84628  -20.59762 -259.26711
    ## 5:   -27.577138  -69.97998  -31.58314  -47.06194
    ## 6: -1183.631007 -326.33086  -37.34410  -40.99168

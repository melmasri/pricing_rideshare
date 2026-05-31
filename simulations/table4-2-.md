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
    real_price = price(max(entry_time) - min(entry_time), sum(distance_meters))[, 1]
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
# trips_stat$Rt <- Rt
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
                abuse_trips <- test[distance >= q_value]
                abuse_sample <- abuse_trips[sample.int(nrow(abuse_trips), size = .BY$M, replace = TRUE)]
                normal_sample <- test[sample.int(test_size, size = test_size - .BY$M, replace = TRUE)]

                # 合并抽样数据并添加列
                combined <- rbindlist(list(
                    abuse_sample[, .(tripID)],
                    normal_sample[, .(tripID)]
                ))[, `:=`(
                    Rt = rep(mean(-pmin(mean(train$Kt) - trips_stat$real_price, 0)), test_size),
                    Kt = rep(mean(train$Kt), test_size),
                    timeBin = sample(c("EveningNight", "EveningRush", "Weekday", "MorningRush", "Weekendday"),
                        .N,
                        replace = TRUE
                    )
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
    profit = sum(Rt) + sum(pmin(Kt - real_price, 0)),
    expand = sum((real_price))
), by = .(repeat_time, M, quantile, rider)]
cost_stats_table <- profit[, .(
    pct_mean = mean((profit) / expand * test_size, na.rm = TRUE),
    sd = sqrt(var(profit, na.rm = TRUE) / sqrt(test_size)),
    dollar_mean = mean(profit, na.rm = TRUE)
), by = .(M, quantile)]
# table(sampled_data$real_price)
```

``` r
# reshape the data to draw the table
table4 <- dcast(cost_stats_table, M ~ quantile, value.var = "pct_mean")
table5 <- dcast(cost_stats_table, M ~ quantile, value.var = "sd")
table6 <- dcast(cost_stats_table, M ~ quantile, value.var = "dollar_mean")
table4
```

    ## Key: <M>
    ##        M            0        0.1        0.2        0.3        0.4        0.5
    ##    <num>        <num>      <num>      <num>      <num>      <num>      <num>
    ## 1:     0 -0.016628351  0.5041737  1.3224239  0.3086932  0.6820358  0.6395594
    ## 2:     4  0.639529060  0.6487509  0.6424944 -0.6517476  0.1126162  0.3472028
    ## 3:     8  0.438389707  1.2285398  0.4378355  0.1536962 -0.3089637 -0.5973717
    ## 4:    12  0.709628680  0.3938637 -0.8126899 -1.0820002 -1.1450394 -1.8316902
    ## 5:    16  0.579578258 -0.4762127 -0.3812794 -0.4264720 -2.2540956 -2.7006011
    ## 6:    20 -0.009651721 -0.3104756 -0.8277554 -1.0306219 -2.2622320 -3.4678107
    ##           0.6       0.7        0.8         0.9
    ##         <num>     <num>      <num>       <num>
    ## 1:  0.3791580  0.566643  0.4625045   0.8080865
    ## 2:  0.1237448 -1.503970 -1.8401113  -2.8948500
    ## 3: -1.4414936 -2.842030 -4.3619997  -5.9603213
    ## 4: -2.7415782 -4.703096 -5.3387434  -8.3770778
    ## 5: -4.0189171 -5.447157 -7.3330741 -10.5132544
    ## 6: -4.6894584 -6.644424 -8.6224060 -12.1036376

``` r
table5
```

    ## Key: <M>
    ##        M        0       0.1      0.2      0.3      0.4      0.5      0.6
    ##    <num>    <num>     <num>    <num>    <num>    <num>    <num>    <num>
    ## 1:     0 43.50597  43.92618 38.79329 43.12601 37.59950 40.40725 42.06423
    ## 2:     4 43.64849  39.95338 41.21954 44.94043 52.71305 45.89935 45.89592
    ## 3:     8 42.52709  38.40525 40.21682 45.61521 42.75296 47.40070 52.71830
    ## 4:    12 42.70519  41.93812 48.76722 46.37810 42.60885 52.04419 47.75789
    ## 5:    16 44.96777 631.75241 41.11982 47.08102 48.22036 58.23461 50.42216
    ## 6:    20 42.62094  43.45754 42.32931 44.15903 50.29024 55.36654 55.27859
    ##         0.7      0.8       0.9
    ##       <num>    <num>     <num>
    ## 1: 43.82374 38.57441  43.71775
    ## 2: 49.18044 43.13959  63.85540
    ## 3: 52.37576 54.03529  76.17104
    ## 4: 62.39245 59.72402  81.54956
    ## 5: 58.21422 65.08503 100.56275
    ## 6: 64.66007 75.77053 112.24827

``` r
table6
```

    ## Key: <M>
    ##        M           0         0.1         0.2        0.3       0.4         0.5
    ##    <num>       <num>       <num>       <num>      <num>     <num>       <num>
    ## 1:     0 -13.0022883   -1.878260  20.5841742  -5.526597   6.26908    3.874340
    ## 2:     4   1.4452344    3.951413   3.5770197 -29.569032 -14.74703   -5.713461
    ## 3:     8  -2.5256356   18.137876  -0.7394619  -9.337220 -19.35971  -29.868762
    ## 4:    12   4.6888107   -2.583156 -36.5290685 -41.877262 -41.54295  -65.154928
    ## 5:    16   0.1970071 -176.437476 -21.4950104 -25.316588 -74.81589  -92.162795
    ## 6:    20 -12.1533330  -21.432826 -33.2654184 -38.594544 -76.60236 -116.204050
    ##           0.6          0.7           0.8         0.9
    ##         <num>        <num>         <num>       <num>
    ## 1:   -3.53798    0.5927575    0.07479719    5.850382
    ## 2:  -11.29899  -53.6401352  -59.17715795  -98.558510
    ## 3:  -53.81344  -93.4132298 -138.99629191 -206.436154
    ## 4:  -87.90210 -156.7705900 -175.54312414 -302.418917
    ## 5: -128.92296 -180.1893339 -252.32339806 -412.341445
    ## 6: -155.50802 -228.4657692 -313.16543225 -505.184272

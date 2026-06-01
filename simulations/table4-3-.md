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
    ##        M            0          0.1           0.2          0.3          0.4
    ##    <num>        <num>        <num>         <num>        <num>        <num>
    ## 1:     0  0.024667974 -0.013388911  0.0103638162  0.001441849  0.011103320
    ## 2:     4 -0.014204549 -0.012601660 -0.0009187318 -0.018812086 -0.007682874
    ## 3:     8 -0.024883674 -0.009010962 -0.0431300563 -0.038078221 -0.061858451
    ## 4:    12  0.009552651 -0.015972098 -0.0254218453 -0.043283965 -0.072206856
    ## 5:    16 -0.027891712 -0.027512252 -0.0414382696 -0.069991095 -0.081502105
    ## 6:    20  0.017023831 -0.065506697 -0.0527141265 -0.047979200 -0.108486574
    ##            0.5           0.6          0.7         0.8         0.9
    ##          <num>         <num>        <num>       <num>       <num>
    ## 1: -0.01862696  0.0009929597  0.007047628  0.02185153 -0.01329717
    ## 2: -0.02981556 -0.0204216493 -0.040645504 -0.07048072 -0.05864855
    ## 3: -0.08134668 -0.0384993099 -0.083416467 -0.08612646 -0.13630736
    ## 4: -0.12925024 -0.1107909440 -0.138606523 -0.13890205 -0.19985722
    ## 5: -0.11299800 -0.1623498258 -0.208185448 -0.21676145 -0.26404319
    ## 6: -0.13250384 -0.1769522408 -0.172782250 -0.23472681 -0.27219618

``` r
table5
```

    ## Key: <M>
    ##        M          0        0.1        0.2        0.3        0.4        0.5
    ##    <num>      <num>      <num>      <num>      <num>      <num>      <num>
    ## 1:     0 0.08576907 0.08878578 0.09092548 0.10509117 0.07941865 0.08454953
    ## 2:     4 0.08605332 0.08794876 0.06230122 0.08761351 0.07774740 0.08700703
    ## 3:     8 0.09748214 0.08643532 0.09688656 0.07379428 0.08398212 0.06906758
    ## 4:    12 0.07555498 0.07688597 0.07616778 0.06543584 0.06628615 0.08546280
    ## 5:    16 0.07008939 0.06326382 0.07241397 0.07217501 0.05674642 0.06163387
    ## 6:    20 0.09128186 0.08070265 0.07481503 0.07062583 0.06055111 0.05457484
    ##           0.6        0.7        0.8        0.9
    ##         <num>      <num>      <num>      <num>
    ## 1: 0.07329890 0.11271347 0.10252031 0.09059744
    ## 2: 0.07024242 0.08279557 0.08229492 0.07822242
    ## 3: 0.06698276 0.07601215 0.07840411 0.07139060
    ## 4: 0.07923413 0.06127572 0.06593740 0.06378876
    ## 5: 0.06120857 0.08162305 0.05894385 0.05843291
    ## 6: 0.07030973 0.06377649 0.06393626 0.04768480

``` r
table6
```

    ## Key: <M>
    ##        M          0        0.1        0.2         0.3        0.4        0.5
    ##    <num>      <num>      <num>      <num>       <num>      <num>      <num>
    ## 1:     0 -11.616132 -11.259864  -6.604949 -334.655179  -4.033741  -13.90591
    ## 2:     4  -8.680268 -91.167254  -2.227933  -19.629131  -5.960987 -422.35365
    ## 3:     8 -62.640663 -10.249412 -19.342345  -10.387612 -20.021458  -12.38380
    ## 4:    12  -2.446321 -72.651062  -6.920263   -8.179392 -10.990411  -35.42732
    ## 5:    16  -5.123368  -4.678894 -14.702957  -12.580249 -10.877085  -16.47039
    ## 6:    20 -60.221266 -17.122359 -17.135071   -8.526230 -15.108095  -17.33600
    ##             0.6        0.7        0.8        0.9
    ##           <num>      <num>      <num>      <num>
    ## 1:    -2.529589  -54.45240  -47.36784  -23.12208
    ## 2:    -4.766899  -34.69636  -43.37958  -21.50955
    ## 3:    -5.937747  -65.77160 -133.79873  -32.51558
    ## 4:   -28.237438  -25.85341  -20.60454 -259.27405
    ## 5:   -27.583511  -69.98614  -31.58951  -47.06867
    ## 6: -1183.638952 -326.33836  -37.35087  -40.99915

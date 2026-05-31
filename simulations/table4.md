Table 4
================
Mingze Li
2015-07-02

``` r
library(traveltimeCLT)
library(data.table)
```

``` r
test_size <- 40 # number of trips for test set
trips <- fread("data/trips.csv")
trips2 <- fread("data/trips.csv")
names(trips)[c(2, 3, 5, 7, 8)] <- c("tripID", "entry_time", "duration_secs", "distance_meters", "linkID")
trips$speed <- exp(trips$logspeed)
trips$timeBin <- time_bins_readable(trips$entry_time)
set.seed(1234)
id <- sample(unique(trips$trip), test_size)
user_records <- trips[trips$tripID %in% id, ]
unique_trips = unique(trips$tripID)
```

``` r
user_records[, time_category := ifelse(timeBin %in% c("MorningRush", "EveningRush"), timeBin, "Otherwise")]
simulation_record <- user_records[, .(
    distance_meters,
    logspeed,
    linkID,
    entry_time = as.POSIXct(rep(runif(1, min = min(entry_time), max = max(entry_time)), .N))
), by = tripID]
user_data <- simulation_record[, .(
    distance = sum(distance_meters),
    start_time = min(entry_time)
), by = tripID]
```

``` r
rider <- 1:100
M <- seq(0, floor(test_size * 0.5), floor(test_size * 0.1))
quantile <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
# produce data table with M and quantile as groups, and every group contains all riders
pressure_data <- data.table(
    M = rep(M, each = length(quantile) * max(rider)),
    quantile = rep(quantile, each = max(rider)),
    rider = rep(rider, length(M) * length(quantile))
)
# for each row, repeat it for test_size times
pressure_data <- pressure_data[, .(
    i = seq(1, test_size, 1)
), by = .(M, quantile, rider)]
# sample from user_data with replacement, the distance limited from quantile to 1.
sampled_data <- pressure_data[,
    {
        q_value <- quantile(user_data$distance, probs = quantile)
        abuse_trips <- user_data[distance >= q_value]
        abuse_sample <- abuse_trips[sample(1:nrow(abuse_trips), size = M, replace = TRUE)]
        normal_sample <- user_data[sample(1:nrow(user_data), size = test_size - M, replace = TRUE)]
        sampled <- rbind(abuse_sample, normal_sample)
        cbind(.SD, sampled)
    },
    by = .(quantile, rider, M)
]
sampled_data$timeBin <- sample(c("EveningNight", "EveningRush", "Weekday", "MorningRush", "Weekendday"),
    nrow(sampled_data),
    replace = TRUE, prob = c(1, 1, 1, 1, 1)
)
```

``` r
# obtain the edges in each trip
sampled_trips <- merge(sampled_data, trips[, .(tripID, linkID)], by = c("tripID"), allow.cartesian = TRUE)
# use timebin x edges simulation to obtain the predicted travel time
timebin_x_edges <- get_timeBin_x_edges(trips2)
setnames(timebin_x_edges, old = c("timeBin", "linkId"), new = c("timeBin", "linkID"))
sampled_trips <- merge(sampled_trips, timebin_x_edges, by = c("timeBin", "linkID"), all.x = TRUE)
sampled_trips$old_timeBin <- sampled_trips$timeBin
# use the Global timebin in timebin x edges to replace the NA in unique_trips
names(unique_trips)
```

    ## NULL

``` r
names(timebin_x_edges)
```

    ## [1] "linkID"    "timeBin"   "mean"      "sd"        "frequency" "length"   
    ## [7] "ID"

``` r
global_dt <- timebin_x_edges[timeBin == "Global", ]
na_index <- which(is.na(sampled_trips$mean))
cols <- c("timeBin", "mean", "sd", "frequency", "length", "ID")
if (length(na_index) > 0) {
    # 创建临时数据表包含需要替换的linkID
    temp_dt <- sampled_trips[na_index, .(linkID)]

    # 执行连接操作（指定on参数）
    replacement <- global_dt[temp_dt, on = .(linkID), ..cols]

    # 执行替换
    sampled_trips[na_index, (cols) := replacement]
}
sampled_trips <- na.omit(sampled_trips)
# simulate the travel time for each trip
sampled_travel_time <- sampled_trips[, .(
    time = sum(exp(mean + qnorm(dependent_uniform(.N)) * sd))
),
by = .(quantile, rider, M, i)
]
# merge the simulated travel time with unique_trips
sampled_data <- merge(sampled_data, sampled_travel_time, by = c("quantile", "rider", "M", "i"), all.x = TRUE)
sampled_data$real_price <- price(sampled_data$time, sampled_data$distance)[, 1]
```

    multipler <- 1 # indicate how many times the simulation should be repeated
    My <- test_size * multipler
    bk <- sample(c("MorningRush", "EveningRush", "Otherwise"), sum(My) * 1, replace = TRUE)
    rho_k <- user_records[.(bk = bk),
        on = .(time_category = bk),
        .(rep(sample(unique(tripID), 1, replace = TRUE), 1)),
        by = .EACHI
    ]$V1
    temp_dt <- data.table(tripID = rho_k, i = seq_along(rho_k))
    simulated_record <- user_records[temp_dt,
        on = .(tripID),
        nomatch = 0,
        allow.cartesian = TRUE
    ]
    rm(temp_dt)
    names(simulated_record)[c(2, ncol(simulated_record))] <- c("trip", "tripID")
    start_time <- runif(max(simulated_record$tripID), min = min(trips$entry_time), max = max(trips$entry_time))
    start_time <- as.POSIXct(start_time)
    simulated_record$entry_time <- start_time[simulated_record$tripID]
    simulated_data <- simulated_record[, .(
        trip = unique(trip),
        start_time = min(entry_time),
        distance = sum(distance_meters),
        duration = sum(duration_secs)
    ), by = tripID]

``` r
fit1 <- traveltimeCLT(trips, "trip-specific")
```

    ## Warning in traveltimeCLT(trips, "trip-specific"): 4 trips have less than 1
    ## observation, and will not be used to estimate autocorrelations, or residual
    ## variance parameters

``` r
fit2 <- traveltimeCLT(trips, "population")
p1 <- predict(fit1, simulation_record)
p2 <- predict(fit2, simulation_record)
p1 <- data.table(p1)
p2 <- data.table(p2)
p1 <- merge(p1, user_data, by = c("tripID"), all.y = TRUE)
p2 <- merge(p2, user_data, by = c("tripID"), all.y = TRUE)
```

``` r
Rt_0 <- request_R(p1, p1$start_time, p1$start_time, p1$distance, K = 0.9, risk_free = 0, zeta = 0)
Kt <- request_K(p1, p1$distance, discount_factor = 0.9)
charge <- data.table(Rt_0 = Rt_0, Kt = Kt, tripID = p1$trip)
sampled_data <- merge(sampled_data, unique(charge, by = "tripID"), by = "tripID", all.x = TRUE, )
profit <- sampled_data[, .(
    profit = sum(Rt_0) + sum(pmin(Kt - real_price, 0)),
    expand = sum((real_price))
), by = .(M, quantile, rider)]
cost_stats_table <- profit[, .(
    pct_mean = mean((profit) / expand * test_size, na.rm = TRUE),
    variance = var(profit, na.rm = TRUE) / sqrt(test_size),
    dollar_mean = mean(profit, na.rm = TRUE)
), by = .(M, quantile)]
# table(sampled_data$real_price)
```

``` r
# reshape the data to draw the table
table4 <- dcast(cost_stats_table, M ~ quantile, value.var = "pct_mean")
table5 <- dcast(cost_stats_table, M ~ quantile, value.var = "variance")
table6 <- dcast(cost_stats_table, M ~ quantile, value.var = "dollar_mean")
table4
```

    ## Key: <M>
    ##        M         0       0.1        0.2        0.3        0.4        0.5
    ##    <num>     <num>     <num>      <num>      <num>      <num>      <num>
    ## 1:     0 -1.199039 -1.074728 -1.2050112 -1.1101525 -1.2061847 -1.1362935
    ## 2:     4 -1.210384 -1.076430 -1.0537776 -1.1788483 -0.9515781 -1.0561444
    ## 3:     8 -1.196063 -1.072149 -1.0976445 -1.0038081 -1.0709652 -1.0291945
    ## 4:    12 -1.111783 -1.258600 -1.0359544 -1.1716007 -1.0671190 -0.9607555
    ## 5:    16 -1.279328 -1.137899 -0.9932501 -0.9984618 -1.0334981 -0.9829078
    ## 6:    20 -1.047946 -1.112941 -0.9939745 -1.0298220 -0.9668847 -0.9990230
    ##           0.6        0.7        0.8         0.9
    ##         <num>      <num>      <num>       <num>
    ## 1: -1.1387432 -1.0770903 -1.3516683 -1.25102104
    ## 2: -1.0661820 -1.0697525 -0.8890084 -0.69307433
    ## 3: -0.8762767 -0.8453955 -0.6302862 -0.41912504
    ## 4: -0.7961772 -0.7159202 -0.4536131 -0.20822210
    ## 5: -0.6958876 -0.6065869 -0.3058363  0.02289974
    ## 6: -0.8493535 -0.4763694 -0.1768105  0.24402677

``` r
table5
```

    ## Key: <M>
    ##        M         0       0.1      0.2       0.3       0.4       0.5        0.6
    ##    <num>     <num>     <num>    <num>     <num>     <num>     <num>      <num>
    ## 1:     0  29.50762  25.35259 85.59953  19.22250 145.74293  22.56202   46.71594
    ## 2:     4  52.29365  27.84107 26.31115  45.51785  20.36707  24.48704  151.08561
    ## 3:     8 181.39031  44.20195 36.04996  23.54363  50.78523  34.89449   71.26012
    ## 4:    12  27.68225 240.93274 28.54989 195.45013  36.53644  60.67080   33.94416
    ## 5:    16 210.22934  25.93849 28.02636  38.86920 131.81802  86.17152  163.12612
    ## 6:    20  29.91868  45.48348 21.36467  48.91417  31.54967 126.06854 4876.77574
    ##          0.7        0.8       0.9
    ##        <num>      <num>     <num>
    ## 1:  20.32975 2250.43653  75.95573
    ## 2: 529.52959   34.18054  34.38994
    ## 3:  25.20672   31.96937  55.28885
    ## 4:  36.52360   67.17055  27.14316
    ## 5:  31.59347   21.77621 177.54370
    ## 6:  24.42488   24.25010  57.26687

``` r
table6
```

    ## Key: <M>
    ##        M         0       0.1       0.2       0.3       0.4       0.5       0.6
    ##    <num>     <num>     <num>     <num>     <num>     <num>     <num>     <num>
    ## 1:     0 -29.67235 -26.74013 -30.49440 -26.95724 -30.66448 -28.01488 -28.25258
    ## 2:     4 -29.81310 -27.66060 -27.08536 -30.35722 -24.80688 -26.90712 -28.65054
    ## 3:     8 -30.50836 -26.98872 -28.27846 -26.32523 -28.23186 -27.74954 -24.75244
    ## 4:    12 -27.81836 -32.76590 -27.33316 -32.57972 -29.67532 -26.55724 -23.47320
    ## 5:    16 -32.36883 -29.77002 -26.68447 -28.06822 -29.84293 -29.52927 -21.88102
    ## 6:    20 -25.82141 -29.27316 -27.08410 -29.72287 -28.57095 -30.96114 -37.59834
    ##          0.7        0.8        0.9
    ##        <num>      <num>      <num>
    ## 1: -26.43823 -40.240500 -30.897032
    ## 2: -30.53199 -24.513385 -20.124494
    ## 3: -23.88862 -18.577716 -13.789094
    ## 4: -21.78895 -14.767514  -7.452973
    ## 5: -19.93636 -10.765359   1.010044
    ## 6: -16.51800  -6.444328  11.163053

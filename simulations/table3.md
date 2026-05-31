Table 3
================
Mingze Li
2015-06-18

``` r
library(traveltimeCLT)
library(data.table)
```

``` r
trips <- fread("data/trips.csv")
names(trips)[c(2, 3, 5, 7, 8)] <- c("tripID", "entry_time", "duration_secs", "distance_meters", "linkID")
trips$speed <- exp(trips$logspeed)
trips$timeBin <- time_bins_readable(trips$entry_time)
set.seed(1234)
id <- sample(unique(trips$trip), 100)
user_records <- trips[trips$trip %in% id, ]
```

``` r
user_data <- user_records[, .(
    start_time = min(entry_time),
    end_time = max(entry_time),
    duration = sum(duration_secs),
    distance = sum(distance_meters)
), by = tripID]
user_data$real_price <- price(user_data$duration, user_data$distance)[, 1]
```

``` r
fit_t <- traveltimeCLT(trips, "trip-specific")
```

    ## Warning in traveltimeCLT(trips, "trip-specific"): 4 trips have less than 1
    ## observation, and will not be used to estimate autocorrelations, or residual
    ## variance parameters

``` r
fit_p <- traveltimeCLT(trips, "population")
pt <- predict(fit_t, user_records)
pp <- predict(fit_p, user_records)
```

``` r
zeta <- 0.1
Rt0 <- request_R(pt, user_data$start_time, user_data$start_time, user_data$distance, K = 0.9, zeta = 0, risk_free = 0)
Rt1 <- request_R(pt, user_data$start_time, user_data$start_time, user_data$distance, K = 0.9, zeta = zeta, risk_free = 0)
Rp0 <- request_R(pp, user_data$start_time, user_data$start_time, user_data$distance, K = 0.9, zeta = 0, risk_free = 0)
Rp1 <- request_R(pp, user_data$start_time, user_data$start_time, user_data$distance, K = 0.9, zeta = zeta, risk_free = 0)
Kt <- request_K(pt, user_data$distance, discount_factor = 0.9)
Kp <- request_K(pp, user_data$distance, discount_factor = 0.9)
P <- user_data$real_price
Pt <- P
if (any(is.na(Rt0))) {
    na_index <- which(is.na(Rt0))
    Rt0 <- Rt0[-na_index]
    Rt1 <- Rt1[-na_index]
    Kt <- Kt[-na_index]
    Pt <- Pt[-na_index]
}
```

``` r
Model <- c(rep("Trip-Specific", 2), rep("Population", 2))
Cutoff <- rep(c(0, zeta), 2)
Percentage_profit <- c(
    mean((Rt0 - pmax(Pt - Kt, 0)) / Pt * 100),
    mean((Rt1 - (Pt >= Kt * (zeta + 1)) * pmax(Pt - Kt, 0)) / Pt * 100),
    mean((Rp0 - pmax(P - Kp, 0)) / P * 100),
    mean((Rp1 - (P >= Kp * (zeta + 1)) * pmax(P - Kp, 0)) / P * 100)
)
Average_profit <- c(
    mean((Rt0 - pmax(Pt - Kt, 0))),
    mean((Rt1 - (Pt >= Kt * (zeta + 1)) * pmax(Pt - Kt, 0))),
    mean((Rp0 - pmax(P - Kp, 0))),
    mean((Rp1 - (P >= Kp * (zeta + 1)) * pmax(P - Kp, 0)))
)
Maximum_Loss <- c(
    min((Rt0 - pmax(Pt - Kt, 0))),
    min((Rt1 - (Pt >= Kt * (zeta + 1)) * pmax(Pt - Kt, 0))),
    min((Rp0 - pmax(P - Kp, 0))),
    min((Rp1 - (P >= Kp * (zeta + 1)) * pmax(P - Kp, 0)))
)
Average_premium <- c(
    mean(Rt0),
    mean(Rt1),
    mean(Rp0),
    mean(Rp1)
)
Average_pct_premium_to_P <- c(
    mean(Rt0 / Pt) * 100,
    mean(Rt1 / Pt) * 100,
    mean(Rp0 / P) * 100,
    mean(Rp1 / P) * 100
)
Total_trips <- c(length(Rt0), length(Rt1), length(Rp0), length(Rp1))
```

``` r
table3 <- data.table(
    Model = Model,
    Cutoff = Cutoff,
    Percentage_Profit = round(Percentage_profit, 5),
    Average_Profit = round(Average_profit, 5),
    Maximum_Loss = round(Maximum_Loss, 5),
    Average_premium = Average_premium,
    Average_pct_premium_to_P = Average_pct_premium_to_P,
    Total_trips = Total_trips
)
table3
```

    ##            Model Cutoff Percentage_Profit Average_Profit Maximum_Loss
    ##           <char>  <num>             <num>          <num>        <num>
    ## 1: Trip-Specific    0.0          -0.44898       -0.22875     -6.90065
    ## 2: Trip-Specific    0.1          -0.53332       -0.35910     -7.99374
    ## 3:    Population    0.0           2.86038        0.08508     -8.41027
    ## 4:    Population    0.1           2.70674       -0.03453     -9.28368
    ##    Average_premium Average_pct_premium_to_P Total_trips
    ##              <num>                    <num>       <int>
    ## 1:        2.487087                10.490765         100
    ## 2:        1.813777                 7.895669         100
    ## 3:        2.621399                11.420297         100
    ## 4:        2.154960                 9.728550         100

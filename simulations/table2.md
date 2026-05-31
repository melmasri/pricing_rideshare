Table 2
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
set.seed(1)
id <- sample(unique(trips$trip), 10000)
user_records <- trips[trips$trip %in% id, ]
user_records
```

    ##              V1 tripID          entry_time timeBins duration_secs logspeed
    ##           <int>  <int>              <POSc>   <char>         <num>    <num>
    ##      1:      95      3 2014-05-07 06:24:56    Other          5.50    2.531
    ##      2:      96      3 2014-05-07 06:25:02    Other          6.63    2.435
    ##      3:      97      3 2014-05-07 06:25:08    Other         12.85    1.822
    ##      4:      98      3 2014-05-07 06:25:21    Other         10.56    2.451
    ##      5:      99      3 2014-05-07 06:25:31    Other          9.69    1.684
    ##     ---                                                                   
    ## 739734: 1719315  23054 2014-05-01 17:06:08       ER         11.67    2.881
    ## 739735: 1719316  23054 2014-05-01 17:06:20       ER          1.71    2.919
    ## 739736: 1719317  23054 2014-05-01 17:06:22       ER         15.07    2.807
    ## 739737: 1719318  23054 2014-05-01 17:06:36       ER         28.40    2.273
    ## 739738: 1719319  23054 2014-05-01 17:07:04       ER         35.94    1.513
    ##         distance_meters linkID        src       osm        dst     speed
    ##                   <num>  <int>      <i64>     <int>      <i64>     <num>
    ##      1:          69.095  13667  303684358  27650210  303684376 12.566066
    ##      2:          75.694  13668  303684376  27650210  303684356 11.415819
    ##      3:          79.450  13663  303684356  27650210  303684357  6.184215
    ##      4:         122.432  13632  303684357  27650186  303684092 11.599941
    ##      5:          52.214  13627  303684092  27650186  303684090  5.387061
    ##     ---                                                                 
    ## 739734:         208.189  33783  281651284 164496700  289276240 17.832096
    ## 739735:          31.670  33954  289276241 167489638 1790858048 18.522755
    ## 739736:         249.508  33956 1790858048 167489638 1789369576 16.560163
    ## 739737:         275.716  33959 1789369564 167489639  289276241  9.708483
    ## 739738:         163.180   7902  289276241  26404035  286892128  4.540331
    ##             timeBin
    ##              <char>
    ##      1:     Weekday
    ##      2:     Weekday
    ##      3:     Weekday
    ##      4:     Weekday
    ##      5:     Weekday
    ##     ---            
    ## 739734: EveningRush
    ## 739735: EveningRush
    ## 739736: EveningRush
    ## 739737: EveningRush
    ## 739738: EveningRush

``` r
user_data <- user_records[, .(
    start_time = min(entry_time),
    end_time = max(entry_time),
    duration = sum(duration_secs),
    distance = sum(distance_meters)
), by = tripID]
user_data$real_price <- price(user_data$duration, user_data$distance)[, 1]
user_data
```

    ##        tripID          start_time            end_time duration  distance
    ##         <int>              <POSc>              <POSc>    <num>     <num>
    ##     1:      3 2014-05-07 06:24:56 2014-05-07 06:43:48  1136.58 18400.768
    ##     2:      4 2014-05-05 16:42:14 2014-05-05 17:11:17  1759.76 13208.625
    ##     3:      7 2014-05-08 05:03:23 2014-05-08 05:21:57  1155.94 26647.648
    ##     4:      8 2014-05-08 05:39:08 2014-05-08 05:49:46   654.05 11081.928
    ##     5:     14 2014-05-07 07:33:53 2014-05-07 07:54:42  1258.64 14551.819
    ##    ---                                                                  
    ##  9996:  23049 2014-05-01 17:20:55 2014-05-01 17:39:51  1148.30 20018.497
    ##  9997:  23050 2014-05-01 17:36:58 2014-05-01 17:41:36   297.18  2669.792
    ##  9998:  23051 2014-05-01 17:27:00 2014-05-01 17:41:26   889.58  4847.966
    ##  9999:  23053 2014-05-01 17:35:12 2014-05-01 17:43:26   501.69  6463.162
    ## 10000:  23054 2014-05-01 16:56:44 2014-05-01 17:07:04   748.40  6737.592
    ##        real_price
    ##             <num>
    ##     1:  25.603021
    ##     2:  24.149856
    ##     3:  33.125240
    ##     4:  16.522994
    ##     5:  22.769610
    ##    ---           
    ##  9996:  27.119531
    ##  9997:   7.108243
    ##  9998:  12.129333
    ##  9999:  11.578911
    ## 10000:  13.100566

``` r
fit_t <- traveltimeCLT(trips, "trip-specific")
```

    ## Warning in traveltimeCLT(trips, "trip-specific"): 4 trips have less than 1
    ## observation, and will not be used to estimate autocorrelations, or residual
    ## variance parameters

``` r
fit_p <- traveltimeCLT(trips, "population")
pt <- predict(fit_t, user_records)
```

    ## Warning in aux[(nlink - length(a) + 1):nlink] <- a: number of items to replace
    ## is not a multiple of replacement length

    ## Warning in aux[(nlink - length(a) + 1):nlink] <- a: number of items to replace
    ## is not a multiple of replacement length

``` r
pp <- predict(fit_p, user_records)
pt <- data.table(pt)
pp <- data.table(pp)
pt <- pt[, coefD := ETA / sqrt(variance)]
pp <- pp[, coefD := ETA / sqrt(variance)]
# if coefD > 30 increase the variance by 4
# pt[coefD > 30, variance := variance * 4]
# pp[coefD > 30, variance := variance * 4]
```

``` r
Rt1 <- request_R(pt, user_data$start_time, user_data$start_time, user_data$distance, K = 1, risk_free = 0)
Rt2 <- request_R(pt, user_data$start_time, user_data$start_time, user_data$distance, K = exp(0.05), risk_free = 0)
Rt3 <- request_R(pt, user_data$start_time, user_data$start_time, user_data$distance, K = exp(0.1), risk_free = 0)
Rp1 <- request_R(pp, user_data$start_time, user_data$start_time, user_data$distance, K = 1, risk_free = 0)
Rp2 <- request_R(pp, user_data$start_time, user_data$start_time, user_data$distance, K = exp(0.05), risk_free = 0)
Rp3 <- request_R(pp, user_data$start_time, user_data$start_time, user_data$distance, K = exp(0.1), risk_free = 0)
# Rt01 <- request_R(pt, user_data$start_time, user_data$start_time, user_data$distance, K = 0.01, risk_free = 0)
# Rp01 <- request_R(pp, user_data$start_time, user_data$start_time, user_data$distance, K = 0.01, risk_free = 0)
Kt1 <- request_K(pt, user_data$distance, discount_factor = 1)
Kt2 <- request_K(pt, user_data$distance, discount_factor = exp(0.05))
Kt3 <- request_K(pt, user_data$distance, discount_factor = exp(0.1))
Kp1 <- request_K(pp, user_data$distance, discount_factor = 1)
Kp2 <- request_K(pp, user_data$distance, discount_factor = exp(0.05))
Kp3 <- request_K(pp, user_data$distance, discount_factor = exp(0.1))
# Kt01 <- request_K(pt, user_data$distance, discount_factor = 0.01)
# Kp01 <- request_K(pp, user_data$distance, discount_factor = 0.01)
P <- user_data$real_price
Pt <- P

if (any(is.na(Rt1))) {
    # find the NA values
    na_index <- which(is.na(Rt1))
    # drop the index from Rt1, Rt2, Rt3, Rp1, Rp2, Rp3, Kt1, Kt2, Kt3, Kp1, Kp2, Kp3, P
    Rt1 <- Rt1[-na_index]
    Rt2 <- Rt2[-na_index]
    Rt3 <- Rt3[-na_index]
    Kt1 <- Kt1[-na_index]
    Kt2 <- Kt2[-na_index]
    Kt3 <- Kt3[-na_index]
    # Kt01 <- Kt01[-na_index]
    # Kp01 <- Kp01[-na_index]
    Pt <- P[-na_index]
}
```

``` r
print(min(Rt1))
```

    ## [1] 0.0226876

``` r
print(min(Rt2))
```

    ## [1] -1.468355e-18

``` r
print(min(Rt3))
```

    ## [1] -3.392613e-16

``` r
print(min(Rp1))
```

    ## [1] 0.1092341

``` r
print(min(Rp2))
```

    ## [1] 2.209322e-07

``` r
print(min(Rp3))
```

    ## [1] 1.269224e-20

``` r
Model <- c(rep("Trip-Specific", 3), rep("Population", 3))
Strike <- rep(c(1, exp(0.05), exp(0.1)), 2)
Percentage_profit <- c(
    mean((Rt1 - pmax(Pt - Kt1, 0)) / Pt),
    mean((Rt2 - pmax(Pt - Kt2, 0)) / Pt),
    mean((Rt3 - pmax(Pt - Kt3, 0)) / Pt),
    #    mean((Rt01 - pmax(P - Kt01, 0)) / P) * 100,
    mean((Rp1 - pmax(P - Kp1, 0)) / P),
    mean((Rp2 - pmax(P - Kp2, 0)) / P),
    mean((Rp3 - pmax(P - Kp3, 0)) / P)
    #    mean((Rp01 - pmax(P - Kp01, 0)) / P) * 100
)
pct_capture_rate <- c(
    mean((Rt1 - pmax(Pt - Kt1, 0)) / Rt1),
    mean((Rt2 - pmax(Pt - Kt2, 0)) / Rt2),
    mean((Rt3 - pmax(Pt - Kt3, 0)) / Rt3),
    mean((Rp1 - pmax(P - Kp1, 0)) / Rp1),
    mean((Rp2 - pmax(P - Kp2, 0)) / Rp2),
    mean((Rp3 - pmax(P - Kp3, 0)) / Rp3)
)
win_rate <- c(
    mean(Rt1 - pmax(Pt - Kt1, 0) > 0),
    mean(Rt2 - pmax(Pt - Kt2, 0) > 0),
    mean(Rt3 - pmax(Pt - Kt3, 0) > 0),
    mean(Rp1 - pmax(P - Kp1, 0) > 0),
    mean(Rp2 - pmax(P - Kp2, 0) > 0),
    mean(Rp3 - pmax(P - Kp3, 0) > 0)
)
profit_factor <- c( # sum of profit divided by sum of losses when positive divided by sum of losses when negative
    sum(ifelse(Rt1 - pmax(Pt - Kt1, 0) > 0, Rt1 - pmax(Pt - Kt1, 0), 0)) / sum(ifelse(Rt1 - pmax(Pt - Kt1, 0) < 0, Rt1 - pmax(Pt - Kt1, 0), 0)),
    sum(ifelse(Rt2 - pmax(Pt - Kt2, 0) > 0, Rt2 - pmax(Pt - Kt2, 0), 0)) / sum(ifelse(Rt2 - pmax(Pt - Kt2, 0) < 0, Rt2 - pmax(Pt - Kt2, 0), 0)),
    sum(ifelse(Rt3 - pmax(Pt - Kt3, 0) > 0, Rt3 - pmax(Pt - Kt3, 0), 0)) / sum(ifelse(Rt3 - pmax(Pt - Kt3, 0) < 0, Rt3 - pmax(Pt - Kt3, 0), 0)),
    sum(ifelse(Rp1 - pmax(P - Kp1, 0) > 0, Rp1 - pmax(P - Kp1, 0), 0)) / sum(ifelse(Rp1 - pmax(P - Kp1, 0) < 0, Rp1 - pmax(P - Kp1, 0), 0)),
    sum(ifelse(Rp2 - pmax(P - Kp2, 0) > 0, Rp2 - pmax(P - Kp2, 0), 0)) / sum(ifelse(Rp2 - pmax(P - Kp2, 0) < 0, Rp2 - pmax(P - Kp2, 0), 0)),
    sum(ifelse(Rp3 - pmax(P - Kp3, 0) > 0, Rp3 - pmax(P - Kp3, 0), 0)) / sum(ifelse(Rp3 - pmax(P - Kp3, 0) < 0, Rp3 - pmax(P - Kp3, 0), 0))
)

Average_profit <- c(
    mean(Rt1 - pmax(Pt - Kt1, 0)),
    mean(Rt2 - pmax(Pt - Kt2, 0)),
    mean(Rt3 - pmax(Pt - Kt3, 0)),
    #    mean(Rt01 - pmax(P - Kt01, 0)),
    mean(Rp1 - pmax(P - Kp1, 0)),
    mean(Rp2 - pmax(P - Kp2, 0)),
    mean(Rp3 - pmax(P - Kp3, 0))
    #    mean(Rp01 - pmax(P - Kp01, 0))
)
Maximum_Loss <- c(
    min(Rt1 - pmax(Pt - Kt1, 0)),
    min(Rt2 - pmax(Pt - Kt2, 0)),
    min(Rt3 - pmax(Pt - Kt3, 0)),
    #    min(Rt01 - pmax(P - Kt01, 0)),
    min(Rp1 - pmax(P - Kp1, 0)),
    min(Rp2 - pmax(P - Kp2, 0)),
    min(Rp3 - pmax(P - Kp3, 0))
    #    min(Rp01 - pmax(P - Kp01, 0))
)
Average_premium <- c(
    mean(Rt1),
    mean(Rt2),
    mean(Rt3),
    #    mean(Rt01),
    mean(Rp1),
    mean(Rp2),
    mean(Rp3)
    #    mean(Rp01)
)
Average_pct_premium_to_P <- c(
    mean(Rt1 / Pt) * 100,
    mean(Rt2 / Pt) * 100,
    mean(Rt3 / Pt) * 100,
    #    mean(Rt01 / P) * 100,
    mean(Rp1 / P) * 100,
    mean(Rp2 / P) * 100,
    mean(Rp3 / P) * 100
    #    mean(Rp01 / P) * 100
)
Total_trips <- c(
    length(Rt1),
    length(Rt2),
    length(Rt3),
    #    length(Rt01),
    length(Rp1),
    length(Rp2),
    length(Rp3)
    #    length(Rp01)
)
```

``` r
table2 <- data.table(
    Model = Model,
    Strike = round(Strike, 3),
    Win_Rate = round(win_rate, 2),
    Profit_Factor = round(abs(profit_factor), 2),
    Capture_Rate = round(pct_capture_rate, 2),
    Percentage_Profit = round(Percentage_profit, 2),
    Average_Profit = round(Average_profit, 2),
    Maximum_Loss = round(Maximum_Loss, 2),
    Average_Premium = round(Average_premium, 2),
    Average_PCT_Premium_to_P = round(Average_pct_premium_to_P, 2),
    Total_Trips = Total_trips
)
table2
```

    ##            Model Strike Win_Rate Profit_Factor  Capture_Rate Percentage_Profit
    ##           <char>  <num>    <num>         <num>         <num>             <num>
    ## 1: Trip-Specific  1.000     0.67          1.12 -2.900000e-01              0.00
    ## 2: Trip-Specific  1.051     0.87          1.23 -1.634200e+02              0.00
    ## 3: Trip-Specific  1.105     0.95          1.99           NaN              0.00
    ## 4:    Population  1.000     0.75          1.39  1.200000e-01              0.02
    ## 5:    Population  1.051     0.84          1.41 -5.870540e+03              0.01
    ## 6:    Population  1.105     0.92          1.36 -4.932599e+16              0.01
    ##    Average_Profit Maximum_Loss Average_Premium Average_PCT_Premium_to_P
    ##             <num>        <num>           <num>                    <num>
    ## 1:           0.04       -21.85            0.50                     2.15
    ## 2:           0.03       -20.73            0.17                     0.73
    ## 3:           0.05       -19.36            0.10                     0.36
    ## 4:           0.18       -28.79            0.91                     4.43
    ## 5:           0.11       -27.58            0.43                     2.35
    ## 6:           0.04       -25.88            0.18                     1.11
    ##    Total_Trips
    ##          <int>
    ## 1:        9998
    ## 2:        9998
    ## 3:        9998
    ## 4:       10000
    ## 5:       10000
    ## 6:       10000

``` r
# latex table
library(knitr)
kable(table2, caption = "Table 2: Results of the simulation", format = "latex")
```

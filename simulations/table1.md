Table 1
================
Mingze Li
2015-06-11

``` r
library(traveltimeCLT)
library(data.table)
```

``` r
trips <- fread("data/trips.csv")
trips$timeBin <- time_bins_readable(trips$time)
timebin <- c("Global", unique(trips$timeBin))
```

``` r
trips[, duration := as.numeric(difftime(shift(time, type = "lead"), time, units = "secs")), by = trip]
trips[, log_duration := log(duration)]

set.seed(1234)
id1 <- sample(unique(trips$trip), 1000)
id2 <- sample(unique(trips$trip), 4000)
trips1 <- trips[trips$trip %in% id1, ]
trips2 <- trips[trips$trip %in% id2, ]

trips1[, time_group := fcase(
    timeBin %in% c("MorningRush", "EveningRush"), "rush",
    timeBin %in% c("EveningNight", "Weekday", "Weekendday"), "norush",
    default = "Global"
)]


stats <- trips1[, .(
    mean = mean(duration, na.rm = TRUE),
    sd = sd_one_input_is_0(duration),
    frequency = .N
), by = .(time_group)]

stats <- rbind(stats, list(
    time_group = "Global",
    mean = mean(trips1$duration, na.rm = TRUE),
    sd = sd_one_input_is_0(trips1$duration),
    frequency = nrow(trips1)
))
stats
```

    ##    time_group     mean       sd frequency
    ##        <char>    <num>    <num>     <int>
    ## 1:     norush 13.70724 18.44166     18410
    ## 2:       rush 17.34234 26.91234     58071
    ## 3:     Global 16.46806 25.18497     76481

``` r
names(trips2)[c(2, 3, 5, 7, 8)] <- c("tripID", "entry_time", "duration_secs", "distance_meters", "linkID")
trips2$speed <- exp(trips2$logspeed)
links_stats <- link_mean_variance(trips2)
global_correlation <- residual_autocorrelation(trips2, links_stats)[2, 2]
rush_correlation <- residual_autocorrelation(trips2[timeBin %in% c("MorningRush", "EveningRush")], links_stats)[2, 2]
norush_correlation <- residual_autocorrelation(trips2[timeBin %in% c("EveningNight", "Weekday", "Weekendday")], links_stats)[2, 2]
correlation <- c(norush_correlation, rush_correlation, global_correlation)

correlation_dt <- data.table(
    time_group = c("norush", "rush", "Global"),
    correlation = correlation
)

stats <- merge(stats, correlation_dt, by = "time_group", all = TRUE)
```

``` r
global_residual <- residual_variance(trips2, links_stats)
rush_residual <- residual_variance(trips2[timeBin %in% c("MorningRush", "EveningRush")], links_stats)
norush_residual <- residual_variance(trips2[timeBin %in% c("EveningNight", "Weekday", "Weekendday")], links_stats)
residual <- c(norush_residual, rush_residual, global_residual)
residual_dt <- data.table(
    time_group = c("norush", "rush", "Global"),
    residual = residual
)
stats <- merge(stats, residual_dt, by = "time_group", all = TRUE)
stats
```

    ## Key: <time_group>
    ##    time_group     mean       sd frequency correlation residual
    ##        <char>    <num>    <num>     <int>      <list>    <num>
    ## 1:     Global 16.46806 25.18497     76481   0.2973943 1.743823
    ## 2:     norush 13.70724 18.44166     18410   0.2861722 1.150558
    ## 3:       rush 17.34234 26.91234     58071    0.309974 2.037341

``` r
trips2[, time_group := fcase(
    timeBin %in% c("MorningRush", "EveningRush"), "rush",
    timeBin %in% c("EveningNight", "Weekday", "Weekendday"), "norush",
    default = "Global"
)]

trips2_freq <- trips2[, .(frequency2 = .N), by = .(time_group)]
trips2_freq <- rbind(trips2_freq, list(
    time_group = "Global",
    frequency2 = nrow(trips2)
))
trips2_freq
```

    ##    time_group frequency2
    ##        <char>      <int>
    ## 1:     norush      69285
    ## 2:       rush     231386
    ## 3:     Global     300671

``` r
# stats <- merge(stats, trips2_freq, by = "time_group", all = TRUE)

# stats
```

``` r
library(knitr)
kable(stats, caption = "Table 1: Statistics of trips")
```

| time_group |     mean |       sd | frequency | correlation | residual |
|:-----------|---------:|---------:|----------:|:------------|---------:|
| Global     | 16.46806 | 25.18497 |     76481 | 0.2973943   | 1.743823 |
| norush     | 13.70724 | 18.44166 |     18410 | 0.2861722   | 1.150558 |
| rush       | 17.34234 | 26.91234 |     58071 | 0.309974    | 2.037341 |

Table 1: Statistics of trips

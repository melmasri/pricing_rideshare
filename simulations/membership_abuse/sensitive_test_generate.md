Membership abuse stress test — 100 riders × 40 trips
================
Mingze Li
2026-06-09

Stress-tests **discount membership** (10% cap, `K = 0.9P`) by simulating
**100 counterfactual riders** with **40 trips** each. A fraction
**M₁/40** of trips are drawn from the upper-**q** quantile of the
trip-length distribution on a per-rider test sample; **q** controls
severity, **M₁/40** controls frequency. When **M₁ = 0**, no trip is
drawn from the tail and **q** is irrelevant—the top row is nine
independent Monte-Carlo replicates of the no-abuse baseline.

Based on `table4(2).rmd` / paper Sec. Membership Abuse
(`tab:abuse-return`, `tab:abuse-sd`). Runs the same grid under
**trip-specific** and **population** travel-time models
(`traveltimeCLT`).

``` r
library(traveltimeCLT)
library(data.table)

SEED <- 1235L
N_RIDERS <- 100L          # 100 counterfactual riders (10 repeats × 10 riders)
N_REPEATS <- 10L
RIDERS_PER_REPEAT <- 10L
TRIPS_PER_RIDER <- 40L    # M_1 <= 40 abusive trips per rider
Q_LEVELS <- seq(0.1, 0.9, by = 0.1)   # severity quantile (columns in paper tables)
M_VALUES <- seq(0, TRIPS_PER_RIDER * 0.9, by = TRIPS_PER_RIDER * 0.1)  # M_1/40 = 0, 0.1, …, 0.9
stopifnot(N_RIDERS == N_REPEATS * RIDERS_PER_REPEAT)
stopifnot(all(M_VALUES == floor(M_VALUES)))
```

``` r
fill_timebin_edges <- function(unique_trips, timebin_x_edges) {
  dt <- copy(unique_trips)
  global_dt <- timebin_x_edges[timeBin == "Global"]
  na_idx <- which(is.na(dt$mean))
  cols <- c("timeBin", "mean", "sd", "frequency", "length", "ID")
  if (length(na_idx) > 0) {
    repl <- global_dt[dt[na_idx, .(linkID)], on = "linkID"]
    dt[na_idx, (cols) := repl[, mget(cols)]]
  }
  na.omit(dt)
}

simulate_trip_prices <- function(sampled, trips, timebin_x_edges) {
  unique_trips <- sampled[, .(n_rep = .N), .(tripID, timeBin)]
  unique_trips <- merge(unique_trips, trips[, .(tripID, linkID)], by = "tripID", allow.cartesian = TRUE)
  unique_trips <- merge(unique_trips, timebin_x_edges, by = c("timeBin", "linkID"), all.x = TRUE)
  unique_trips <- fill_timebin_edges(unique_trips, timebin_x_edges)

  travel <- unique_trips[, {
    reps <- n_rep[1]
    times <- numeric(reps)
    dist <- sum(length)
    for (i in seq_len(reps)) {
      times[i] <- sum(exp(mean + stats::qnorm(dependent_uniform(.N)) * sd))
    }
    .(time = times, distance = rep(dist, reps), index = seq_len(reps))
  }, by = .(timeBin, tripID)]

  out <- copy(sampled)
  out[, index := seq_len(.N), by = .(tripID, timeBin)]
  out <- merge(out, travel, by = c("tripID", "timeBin", "index"), all.x = TRUE)
  out[, real_price := price(time, distance)[, 1]]
  out
}

run_membership_abuse_grid <- function(
  trips_stat,
  trips,
  timebin_x_edges,
  M_values = M_VALUES,
  q_levels = Q_LEVELS,
  n_repeats = N_REPEATS,
  riders_per_repeat = RIDERS_PER_REPEAT,
  trips_per_rider = TRIPS_PER_RIDER
) {
  pressure_data <- CJ(
    repeat_time = seq_len(n_repeats),
    q = q_levels,
    rider = seq_len(riders_per_repeat),
    M = M_values
  )
  pressure_data <- pressure_data[, .(trip_idx = seq_len(trips_per_rider)), by = .(repeat_time, q, rider, M)]

  cat(
    "Sampling", n_repeats, "repeats ×", riders_per_repeat, "riders ×",
    length(M_values), "abuse levels ×", length(q_levels), "q levels …\n"
  )

  sampled <- pressure_data[, {
    id <- sample.int(nrow(trips_stat), trips_per_rider, replace = FALSE)
    test <- trips_stat[id]
    train <- trips_stat[-id]
    assignments <- .SD[, {
      q_cut <- as.numeric(stats::quantile(test$distance, probs = q, names = FALSE))
      abuse_pool <- test[distance >= q_cut]
      if (M > 0L && nrow(abuse_pool) == 0L) {
        stop("Empty abuse pool for q = ", q, "; increase test sample or lower q.")
      }
      abuse_ids <- if (M > 0L) {
        abuse_pool$tripID[sample.int(nrow(abuse_pool), M, replace = TRUE)]
      } else {
        integer(0)
      }
      normal_ids <- test$tripID[sample.int(trips_per_rider, trips_per_rider - M, replace = TRUE)]
      tripID <- c(abuse_ids, normal_ids)
      Rt <- rep(mean(-pmin(mean(train$Kt) - trips_stat$real_price, 0)), trips_per_rider)
      Kt <- rep(mean(train$Kt), trips_per_rider)
      timeBin <- sample(
        c("EveningNight", "EveningRush", "Weekday", "MorningRush", "Weekendday"),
        trips_per_rider,
        replace = TRUE
      )
      .(trip_idx = seq_len(trips_per_rider), tripID, Rt, Kt, timeBin)
    }, by = .(q, rider, M)]
    assignments
  }, by = .(repeat_time)]

  cat("Simulating travel times …\n")
  priced <- simulate_trip_prices(sampled, trips[, .(tripID, linkID)], timebin_x_edges)

  cat("Aggregating rider-level returns …\n")
  rider_returns <- priced[, {
    profit <- sum(Rt) + sum(pmin(Kt - real_price, 0))
    expand <- sum(real_price)
    pct_return <- profit / expand * trips_per_rider
    .(pct_return = pct_return, profit = profit, expand = expand)
  }, by = .(M, q, repeat_time, rider)]

  rider_returns[, abuse_rate := M / trips_per_rider]
  rider_returns[, .(
    mean_return = mean(pct_return, na.rm = TRUE),
    sd_return_se = stats::sd(pct_return, na.rm = TRUE) / sqrt(.N)
  ), by = .(abuse_rate, q)]
}

build_trips_stat <- function(trips, trips_record, model = c("trip-specific", "population")) {
  model <- match.arg(model)
  fit <- traveltimeCLT(trips, model)
  pt <- predict(fit, trips_record)
  stat <- trips[, .(
    start_time = entry_time[1],
    distance = sum(distance_meters),
    duration_secs = max(entry_time) - min(entry_time),
    real_price = price(max(entry_time) - min(entry_time), sum(distance_meters))[, 1]
  ), tripID]
  stat[, Kt := request_K(pt, distance, discount_factor = 0.9)]
  na.omit(stat)
}

to_wide_tables <- function(results_long) {
  return_dt <- dcast(results_long, abuse_rate ~ q, value.var = "mean_return", fun.aggregate = identity)
  setnames(return_dt, as.character(Q_LEVELS), paste0("q_", Q_LEVELS))
  sd_dt <- dcast(results_long, abuse_rate ~ q, value.var = "sd_return_se", fun.aggregate = identity)
  setnames(sd_dt, as.character(Q_LEVELS), paste0("q_", Q_LEVELS))
  list(return_dt = return_dt, sd_dt = sd_dt)
}

run_membership_abuse_scenario <- function(
  trips,
  trips_record,
  timebin_x_edges,
  model = c("trip-specific", "population")
) {
  model <- match.arg(model)
  cat("\n=== ", model, " model ===\n", sep = "")
  set.seed(SEED)
  trips_stat <- build_trips_stat(trips, trips_record, model)
  cat("Trips in pool:", nrow(trips_stat), "\n")
  results_long <- run_membership_abuse_grid(
    trips_stat = trips_stat,
    trips = trips,
    timebin_x_edges = timebin_x_edges
  )
  wide <- to_wide_tables(results_long)
  c(
    list(model = model, trips_stat = trips_stat, results_long = results_long),
    wide
  )
}
```

``` r
set.seed(SEED)
trips <- fread("data/trips.csv")
names(trips)[c(2, 3, 5, 7, 8)] <- c(
  "tripID", "entry_time", "duration_secs", "distance_meters", "linkID"
)
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

timebin_x_edges <- get_timeBin_x_edges(
  tripID = trips$tripID,
  linkId = trips$linkID,
  length = trips$distance_meters,
  timeBin = trips$timeBin,
  duration = trips$duration_secs
)
setnames(timebin_x_edges, c("timeBin", "linkId"), c("timeBin", "linkID"))

cat("Loaded", uniqueN(trips$tripID), "trips for membership-abuse grid\n")
```

    ## Loaded 23054 trips for membership-abuse grid

``` r
trip_specific <- run_membership_abuse_scenario(
  trips = trips,
  trips_record = trips_record,
  timebin_x_edges = timebin_x_edges,
  model = "trip-specific"
)
```

    ## 
    ## === trip-specific model ===

    ## Trips in pool: 23054 
    ## Sampling 10 repeats × 10 riders × 10 abuse levels × 9 q levels …
    ## Simulating travel times …
    ## Aggregating rider-level returns …

``` r
trip_specific$results_long
```

    ##     abuse_rate     q   mean_return sd_return_se
    ##          <num> <num>         <num>        <num>
    ##  1:        0.3   0.1   1.642270627    0.3057347
    ##  2:        0.4   0.1   1.038651990    0.4424149
    ##  3:        0.5   0.1   0.558224648    0.3386312
    ##  4:        0.0   0.1   1.790298510    0.4218917
    ##  5:        0.6   0.1   0.935479748    0.3653429
    ##  6:        0.8   0.1   0.277269971    0.4490596
    ##  7:        0.9   0.1   0.743954440    0.2972175
    ##  8:        0.1   0.1   1.609928220    0.3163621
    ##  9:        0.2   0.1   1.656425999    0.2665111
    ## 10:        0.7   0.1   0.788123217    0.3303416
    ## 11:        0.4   0.2   0.406178234    0.3058656
    ## 12:        0.5   0.2   0.555325699    0.3077221
    ## 13:        0.8   0.2  -0.614715138    0.3870076
    ## 14:        0.3   0.2   1.184192247    0.3485495
    ## 15:        0.6   0.2  -0.534658529    0.3518995
    ## 16:        0.7   0.2  -0.314024325    0.3314525
    ## 17:        0.1   0.2   1.217550871    0.3495252
    ## 18:        0.9   0.2  -0.220214563    0.3023062
    ## 19:        0.0   0.2   1.443073704    0.3533455
    ## 20:        0.2   0.2   0.531773393    0.3094405
    ## 21:        0.0   0.3   1.414715995    0.3540388
    ## 22:        0.1   0.3   0.321045328    0.3458466
    ## 23:        0.5   0.3  -0.530456663    0.3604101
    ## 24:        0.9   0.3  -1.361407152    0.3295146
    ## 25:        0.4   0.3  -0.156916935    0.2921067
    ## 26:        0.7   0.3  -1.061538684    0.3238808
    ## 27:        0.8   0.3  -1.397203824    0.3005911
    ## 28:        0.2   0.3   0.571619326    0.3321294
    ## 29:        0.6   0.3  -0.875412029    0.3364853
    ## 30:        0.2   0.4   0.242541238    0.3254714
    ## 31:        0.8   0.4  -2.682221768    0.3348531
    ## 32:        0.4   0.4  -1.149350215    0.2982435
    ## 33:        0.7   0.4  -2.321302626    0.3114922
    ## 34:        0.1   0.4   0.904452343    0.3539024
    ## 35:        0.3   0.4  -0.188198850    0.2993755
    ## 36:        0.6   0.4  -1.646493822    0.3235863
    ## 37:        0.9   0.4  -3.024646477    0.2970395
    ## 38:        0.5   0.4  -1.079949346    0.2896911
    ## 39:        0.0   0.4   1.452471413    0.3149186
    ## 40:        0.0   0.5   1.721197564    0.4366589
    ## 41:        0.9   0.5  -4.960543805    0.3520958
    ## 42:        0.4   0.5  -1.507796152    0.2899861
    ## 43:        0.6   0.5  -3.071435795    0.2952774
    ## 44:        0.1   0.5   0.930887046    0.3363129
    ## 45:        0.2   0.5  -0.582950231    0.2896250
    ## 46:        0.7   0.5  -3.496083646    0.3020603
    ## 47:        0.8   0.5  -4.358351918    0.3593734
    ## 48:        0.1   0.6  -0.294242781    0.4193681
    ## 49:        0.4   0.6  -3.383399589    0.3365766
    ## 50:        0.7   0.6  -5.828834390    0.3090280
    ## 51:        0.0   0.6   1.696136902    0.3637136
    ## 52:        0.6   0.6  -4.909722335    0.4004056
    ## 53:        0.3   0.6  -1.952380648    0.3185012
    ## 54:        0.8   0.6  -6.071182217    0.2655225
    ## 55:        0.5   0.6  -4.137157359    0.2932220
    ## 56:        0.9   0.6  -6.784725919    0.2898418
    ## 57:        0.2   0.6  -0.685886641    0.3233690
    ## 58:        0.6   0.7  -6.672204631    0.2840901
    ## 59:        0.9   0.7  -9.285413561    0.2706274
    ## 60:        0.5   0.7  -6.073254431    0.3661622
    ## 61:        0.7   0.7  -7.905265687    0.3042686
    ## 62:        0.8   0.7  -8.644652107    0.2697861
    ## 63:        0.3   0.7  -4.191118370    0.4723794
    ## 64:        0.4   0.7  -4.408880232    0.3074211
    ## 65:        0.1   0.7  -0.092929773    0.3659176
    ## 66:        0.2   0.7  -2.207873062    0.3817408
    ## 67:        0.0   0.7   1.964997975    0.4052151
    ## 68:        0.2   0.8  -3.490319397    0.4222304
    ## 69:        0.4   0.8  -6.640553047    0.3502613
    ## 70:        0.5   0.8  -8.356594544    0.3674033
    ## 71:        0.7   0.8 -10.003574040    0.3272855
    ## 72:        0.8   0.8 -11.582632572    0.3722619
    ## 73:        0.9   0.8 -12.216174232    0.3032647
    ## 74:        0.6   0.8  -9.279619511    0.3222383
    ## 75:        0.1   0.8  -1.079501874    0.4485329
    ## 76:        0.3   0.8  -4.565656481    0.2941980
    ## 77:        0.7   0.9 -13.833363977    0.3651627
    ## 78:        0.0   0.9   1.925519444    0.2993692
    ## 79:        0.3   0.9  -7.344962031    0.3542388
    ## 80:        0.5   0.9 -10.956753489    0.3449771
    ## 81:        0.1   0.9  -2.080790295    0.3337426
    ## 82:        0.2   0.9  -5.039804264    0.3493038
    ## 83:        0.4   0.9  -9.181419912    0.3207740
    ## 84:        0.6   0.9 -12.560593098    0.3659260
    ## 85:        0.3   0.3  -0.002813792    0.4675367
    ## 86:        0.5   0.5  -2.405369503    0.2944146
    ## 87:        0.3   0.5  -0.580370840    0.2790758
    ## 88:        0.0   0.8   1.666181637    0.3189682
    ## 89:        0.8   0.9 -14.883163391    0.3547437
    ## 90:        0.9   0.9 -16.222641424    0.4076702
    ##     abuse_rate     q   mean_return sd_return_se
    ##          <num> <num>         <num>        <num>

``` r
return_dt <- trip_specific$return_dt
sd_dt <- trip_specific$sd_dt
return_dt
```

    ## Key: <abuse_rate>
    ##     abuse_rate     q_0.1      q_0.2        q_0.3      q_0.4      q_0.5
    ##          <num>     <num>      <num>        <num>      <num>      <num>
    ##  1:        0.0 1.7902985  1.4430737  1.414715995  1.4524714  1.7211976
    ##  2:        0.1 1.6099282  1.2175509  0.321045328  0.9044523  0.9308870
    ##  3:        0.2 1.6564260  0.5317734  0.571619326  0.2425412 -0.5829502
    ##  4:        0.3 1.6422706  1.1841922 -0.002813792 -0.1881989 -0.5803708
    ##  5:        0.4 1.0386520  0.4061782 -0.156916935 -1.1493502 -1.5077962
    ##  6:        0.5 0.5582246  0.5553257 -0.530456663 -1.0799493 -2.4053695
    ##  7:        0.6 0.9354797 -0.5346585 -0.875412029 -1.6464938 -3.0714358
    ##  8:        0.7 0.7881232 -0.3140243 -1.061538684 -2.3213026 -3.4960836
    ##  9:        0.8 0.2772700 -0.6147151 -1.397203824 -2.6822218 -4.3583519
    ## 10:        0.9 0.7439544 -0.2202146 -1.361407152 -3.0246465 -4.9605438
    ##          q_0.6       q_0.7      q_0.8      q_0.9
    ##          <num>       <num>      <num>      <num>
    ##  1:  1.6961369  1.96499798   1.666182   1.925519
    ##  2: -0.2942428 -0.09292977  -1.079502  -2.080790
    ##  3: -0.6858866 -2.20787306  -3.490319  -5.039804
    ##  4: -1.9523806 -4.19111837  -4.565656  -7.344962
    ##  5: -3.3833996 -4.40888023  -6.640553  -9.181420
    ##  6: -4.1371574 -6.07325443  -8.356595 -10.956753
    ##  7: -4.9097223 -6.67220463  -9.279620 -12.560593
    ##  8: -5.8288344 -7.90526569 -10.003574 -13.833364
    ##  9: -6.0711822 -8.64465211 -11.582633 -14.883163
    ## 10: -6.7847259 -9.28541356 -12.216174 -16.222641

``` r
sd_dt
```

    ## Key: <abuse_rate>
    ##     abuse_rate     q_0.1     q_0.2     q_0.3     q_0.4     q_0.5     q_0.6
    ##          <num>     <num>     <num>     <num>     <num>     <num>     <num>
    ##  1:        0.0 0.4218917 0.3533455 0.3540388 0.3149186 0.4366589 0.3637136
    ##  2:        0.1 0.3163621 0.3495252 0.3458466 0.3539024 0.3363129 0.4193681
    ##  3:        0.2 0.2665111 0.3094405 0.3321294 0.3254714 0.2896250 0.3233690
    ##  4:        0.3 0.3057347 0.3485495 0.4675367 0.2993755 0.2790758 0.3185012
    ##  5:        0.4 0.4424149 0.3058656 0.2921067 0.2982435 0.2899861 0.3365766
    ##  6:        0.5 0.3386312 0.3077221 0.3604101 0.2896911 0.2944146 0.2932220
    ##  7:        0.6 0.3653429 0.3518995 0.3364853 0.3235863 0.2952774 0.4004056
    ##  8:        0.7 0.3303416 0.3314525 0.3238808 0.3114922 0.3020603 0.3090280
    ##  9:        0.8 0.4490596 0.3870076 0.3005911 0.3348531 0.3593734 0.2655225
    ## 10:        0.9 0.2972175 0.3023062 0.3295146 0.2970395 0.3520958 0.2898418
    ##         q_0.7     q_0.8     q_0.9
    ##         <num>     <num>     <num>
    ##  1: 0.4052151 0.3189682 0.2993692
    ##  2: 0.3659176 0.4485329 0.3337426
    ##  3: 0.3817408 0.4222304 0.3493038
    ##  4: 0.4723794 0.2941980 0.3542388
    ##  5: 0.3074211 0.3502613 0.3207740
    ##  6: 0.3661622 0.3674033 0.3449771
    ##  7: 0.2840901 0.3222383 0.3659260
    ##  8: 0.3042686 0.3272855 0.3651627
    ##  9: 0.2697861 0.3722619 0.3547437
    ## 10: 0.2706274 0.3032647 0.4076702

``` r
population <- run_membership_abuse_scenario(
  trips = trips,
  trips_record = trips_record,
  timebin_x_edges = timebin_x_edges,
  model = "population"
)
```

    ## 
    ## === population model ===
    ## Trips in pool: 23054 
    ## Sampling 10 repeats × 10 riders × 10 abuse levels × 9 q levels …
    ## Simulating travel times …
    ## Aggregating rider-level returns …

``` r
population$results_long
```

    ##     abuse_rate     q mean_return sd_return_se
    ##          <num> <num>       <num>        <num>
    ##  1:        0.0   0.1   1.8826834    0.3125242
    ##  2:        0.1   0.1   1.3384680    0.4603688
    ##  3:        0.3   0.1   1.5709274    0.3130571
    ##  4:        0.6   0.1   1.3703210    0.3026270
    ##  5:        0.7   0.1   0.9914921    0.2904441
    ##  6:        0.8   0.1   0.4663228    0.2887489
    ##  7:        0.9   0.1   0.6014767    0.2834374
    ##  8:        0.2   0.1   1.5372270    0.3062765
    ##  9:        0.5   0.1   0.6518516    0.4265856
    ## 10:        0.4   0.2   0.8786760    0.2889084
    ## 11:        0.1   0.2   1.4412638    0.3049772
    ## 12:        0.2   0.2   0.9611935    0.2852457
    ## 13:        0.6   0.2   0.2503560    0.2651808
    ## 14:        0.9   0.2  -0.1036652    0.3085109
    ## 15:        0.3   0.2   1.3891334    0.2770349
    ## 16:        0.5   0.2   0.9578542    0.3068053
    ## 17:        0.7   0.2  -0.1052005    0.3615430
    ## 18:        0.0   0.2   1.7254771    0.3502614
    ## 19:        0.8   0.2  -0.2591299    0.3204636
    ## 20:        0.3   0.3   0.9946103    0.3081554
    ## 21:        0.4   0.3  -0.1557307    0.3139416
    ## 22:        0.6   0.3  -0.2653590    0.2897675
    ## 23:        0.1   0.3   0.8280167    0.3672253
    ## 24:        0.5   0.3  -0.3832949    0.3267877
    ## 25:        0.0   0.3   2.2659152    0.3077780
    ## 26:        0.7   0.3  -0.9812224    0.3864888
    ## 27:        0.8   0.3  -0.8138860    0.2717095
    ## 28:        0.2   0.3   0.6819938    0.2690175
    ## 29:        0.9   0.3  -1.3538632    0.3758081
    ## 30:        0.5   0.4  -0.7752443    0.2615736
    ## 31:        0.8   0.4  -2.1746242    0.2681451
    ## 32:        0.2   0.4   0.5742270    0.2692577
    ## 33:        0.6   0.4  -1.5798035    0.2863901
    ## 34:        0.9   0.4  -2.4712737    0.2636932
    ## 35:        0.4   0.4  -1.0270069    0.2694608
    ## 36:        0.1   0.4   0.8908588    0.4398151
    ## 37:        0.7   0.4  -1.6991212    0.2570870
    ## 38:        0.3   0.4  -0.4530453    0.3353822
    ## 39:        0.0   0.4   1.9847963    0.3278861
    ## 40:        0.1   0.5   1.1511663    0.2650798
    ## 41:        0.3   0.5  -0.7665800    0.3056391
    ## 42:        0.2   0.5  -0.4643730    0.2805081
    ## 43:        0.0   0.5   1.7413058    0.4155280
    ## 44:        0.5   0.5  -1.9256912    0.2615312
    ## 45:        0.1   0.6   0.0078501    0.2956255
    ## 46:        0.6   0.6  -4.1117187    0.2654191
    ## 47:        0.3   0.6  -1.9062337    0.2947763
    ## 48:        0.0   0.6   1.6592846    0.3145970
    ## 49:        0.2   0.6  -0.6490363    0.2731835
    ## 50:        0.3   0.7  -3.4854169    0.2686158
    ## 51:        0.2   0.7  -1.0889130    0.2724718
    ## 52:        0.4   0.7  -4.4499099    0.3505115
    ## 53:        0.1   0.7   0.3079969    0.3021825
    ## 54:        0.0   0.7   2.2522969    0.2738843
    ## 55:        0.8   0.7  -8.0260091    0.2112156
    ## 56:        0.1   0.8  -0.5013697    0.3070311
    ## 57:        0.3   0.8  -4.1614286    0.2459954
    ## 58:        0.0   0.8   1.7549642    0.2565116
    ## 59:        0.2   0.8  -2.6250751    0.2454655
    ## 60:        0.6   0.8  -8.6389340    0.2432974
    ## 61:        0.4   0.8  -5.9750451    0.3254319
    ## 62:        0.0   0.9   1.2881191    0.5255494
    ## 63:        0.2   0.9  -4.6062949    0.3034378
    ## 64:        0.1   0.9  -1.6296507    0.3144216
    ## 65:        0.3   0.9  -6.6768816    0.3130555
    ## 66:        0.7   0.9 -12.9032401    0.2976828
    ## 67:        0.5   0.9  -9.9664419    0.2769221
    ## 68:        0.4   0.1   1.2791946    0.2592965
    ## 69:        0.6   0.5  -2.8353288    0.2656698
    ## 70:        0.4   0.5  -1.0591529    0.2564725
    ## 71:        0.4   0.6  -2.8597627    0.3638168
    ## 72:        0.7   0.6  -5.0916288    0.2707198
    ## 73:        0.9   0.6  -6.6771749    0.2372705
    ## 74:        0.5   0.6  -3.2628615    0.2329896
    ## 75:        0.5   0.7  -5.1723874    0.2742976
    ## 76:        0.6   0.7  -6.4339987    0.2803253
    ## 77:        0.8   0.8 -10.7419176    0.2644656
    ## 78:        0.7   0.8  -9.3941256    0.2638209
    ## 79:        0.6   0.9 -11.5713302    0.2949262
    ## 80:        0.4   0.9  -8.1186884    0.2934770
    ## 81:        0.7   0.5  -3.6406077    0.3046373
    ## 82:        0.8   0.6  -6.0172927    0.3676051
    ## 83:        0.7   0.7  -7.4941534    0.2998941
    ## 84:        0.8   0.5  -3.8216859    0.2538526
    ## 85:        0.5   0.8  -7.5859100    0.3413216
    ## 86:        0.8   0.9 -13.9009818    0.3096060
    ## 87:        0.9   0.5  -4.3849565    0.2619049
    ## 88:        0.9   0.7  -8.8027165    0.2322244
    ## 89:        0.9   0.8 -11.4588206    0.2519599
    ## 90:        0.9   0.9 -15.0228401    0.2984553
    ##     abuse_rate     q mean_return sd_return_se
    ##          <num> <num>       <num>        <num>

``` r
return_dt_pop <- population$return_dt
sd_dt_pop <- population$sd_dt
return_dt_pop
```

    ## Key: <abuse_rate>
    ##     abuse_rate     q_0.1      q_0.2      q_0.3      q_0.4     q_0.5      q_0.6
    ##          <num>     <num>      <num>      <num>      <num>     <num>      <num>
    ##  1:        0.0 1.8826834  1.7254771  2.2659152  1.9847963  1.741306  1.6592846
    ##  2:        0.1 1.3384680  1.4412638  0.8280167  0.8908588  1.151166  0.0078501
    ##  3:        0.2 1.5372270  0.9611935  0.6819938  0.5742270 -0.464373 -0.6490363
    ##  4:        0.3 1.5709274  1.3891334  0.9946103 -0.4530453 -0.766580 -1.9062337
    ##  5:        0.4 1.2791946  0.8786760 -0.1557307 -1.0270069 -1.059153 -2.8597627
    ##  6:        0.5 0.6518516  0.9578542 -0.3832949 -0.7752443 -1.925691 -3.2628615
    ##  7:        0.6 1.3703210  0.2503560 -0.2653590 -1.5798035 -2.835329 -4.1117187
    ##  8:        0.7 0.9914921 -0.1052005 -0.9812224 -1.6991212 -3.640608 -5.0916288
    ##  9:        0.8 0.4663228 -0.2591299 -0.8138860 -2.1746242 -3.821686 -6.0172927
    ## 10:        0.9 0.6014767 -0.1036652 -1.3538632 -2.4712737 -4.384957 -6.6771749
    ##          q_0.7       q_0.8      q_0.9
    ##          <num>       <num>      <num>
    ##  1:  2.2522969   1.7549642   1.288119
    ##  2:  0.3079969  -0.5013697  -1.629651
    ##  3: -1.0889130  -2.6250751  -4.606295
    ##  4: -3.4854169  -4.1614286  -6.676882
    ##  5: -4.4499099  -5.9750451  -8.118688
    ##  6: -5.1723874  -7.5859100  -9.966442
    ##  7: -6.4339987  -8.6389340 -11.571330
    ##  8: -7.4941534  -9.3941256 -12.903240
    ##  9: -8.0260091 -10.7419176 -13.900982
    ## 10: -8.8027165 -11.4588206 -15.022840

``` r
sd_dt_pop
```

    ## Key: <abuse_rate>
    ##     abuse_rate     q_0.1     q_0.2     q_0.3     q_0.4     q_0.5     q_0.6
    ##          <num>     <num>     <num>     <num>     <num>     <num>     <num>
    ##  1:        0.0 0.3125242 0.3502614 0.3077780 0.3278861 0.4155280 0.3145970
    ##  2:        0.1 0.4603688 0.3049772 0.3672253 0.4398151 0.2650798 0.2956255
    ##  3:        0.2 0.3062765 0.2852457 0.2690175 0.2692577 0.2805081 0.2731835
    ##  4:        0.3 0.3130571 0.2770349 0.3081554 0.3353822 0.3056391 0.2947763
    ##  5:        0.4 0.2592965 0.2889084 0.3139416 0.2694608 0.2564725 0.3638168
    ##  6:        0.5 0.4265856 0.3068053 0.3267877 0.2615736 0.2615312 0.2329896
    ##  7:        0.6 0.3026270 0.2651808 0.2897675 0.2863901 0.2656698 0.2654191
    ##  8:        0.7 0.2904441 0.3615430 0.3864888 0.2570870 0.3046373 0.2707198
    ##  9:        0.8 0.2887489 0.3204636 0.2717095 0.2681451 0.2538526 0.3676051
    ## 10:        0.9 0.2834374 0.3085109 0.3758081 0.2636932 0.2619049 0.2372705
    ##         q_0.7     q_0.8     q_0.9
    ##         <num>     <num>     <num>
    ##  1: 0.2738843 0.2565116 0.5255494
    ##  2: 0.3021825 0.3070311 0.3144216
    ##  3: 0.2724718 0.2454655 0.3034378
    ##  4: 0.2686158 0.2459954 0.3130555
    ##  5: 0.3505115 0.3254319 0.2934770
    ##  6: 0.2742976 0.3413216 0.2769221
    ##  7: 0.2803253 0.2432974 0.2949262
    ##  8: 0.2998941 0.2638209 0.2976828
    ##  9: 0.2112156 0.2644656 0.3096060
    ## 10: 0.2322244 0.2519599 0.2984553

``` r
# When M_1/40 = 0, q should not matter (independent MC replicates of the same baseline).
for (scenario in list(trip_specific, population)) {
  baseline <- scenario$results_long[abuse_rate == 0, .(mean_return, sd_return_se, q)]
  cat("\n", scenario$model, " — no-abuse row: mean return range =",
      round(range(baseline$mean_return), 3),
      ", SE range =",
      round(range(baseline$sd_return_se), 3), "\n")
  print(baseline)
}
```

    ## 
    ##  trip-specific  — no-abuse row: mean return range = 1.415 1.965 , SE range = 0.299 0.437 
    ##    mean_return sd_return_se     q
    ##          <num>        <num> <num>
    ## 1:    1.790299    0.4218917   0.1
    ## 2:    1.443074    0.3533455   0.2
    ## 3:    1.414716    0.3540388   0.3
    ## 4:    1.452471    0.3149186   0.4
    ## 5:    1.721198    0.4366589   0.5
    ## 6:    1.696137    0.3637136   0.6
    ## 7:    1.964998    0.4052151   0.7
    ## 8:    1.925519    0.2993692   0.9
    ## 9:    1.666182    0.3189682   0.8
    ## 
    ##  population  — no-abuse row: mean return range = 1.288 2.266 , SE range = 0.257 0.526 
    ##    mean_return sd_return_se     q
    ##          <num>        <num> <num>
    ## 1:    1.882683    0.3125242   0.1
    ## 2:    1.725477    0.3502614   0.2
    ## 3:    2.265915    0.3077780   0.3
    ## 4:    1.984796    0.3278861   0.4
    ## 5:    1.741306    0.4155280   0.5
    ## 6:    1.659285    0.3145970   0.6
    ## 7:    2.252297    0.2738843   0.7
    ## 8:    1.754964    0.2565116   0.8
    ## 9:    1.288119    0.5255494   0.9

``` r
dir.create("plot_abuse", showWarnings = FALSE, recursive = TRUE)
fwrite(trip_specific$return_dt, "plot_abuse/return_dt_trip_specific.csv")
fwrite(trip_specific$sd_dt, "plot_abuse/sd_dt_trip_specific.csv")
fwrite(population$return_dt, "plot_abuse/return_dt_population.csv")
fwrite(population$sd_dt, "plot_abuse/sd_dt_population.csv")
saveRDS(
  list(
    trip_specific = trip_specific[c("return_dt", "sd_dt", "results_long")],
    population = population[c("return_dt", "sd_dt", "results_long")]
  ),
  "plot/sensitive_test_results.rds"
)
```

``` r
library(ggplot2)
source("sensitive_test_heatmap.R")

ts_plots <- plot_scenario_heatmaps(trip_specific, "Trip-specific model")
ts_plots$return
```

![](sensitive_test_generate_files/figure-gfm/heatmaps-1.png)<!-- -->

``` r
ts_plots$sd
```

![](sensitive_test_generate_files/figure-gfm/sd-heatmap-1.png)<!-- -->

``` r
ts_plots$combined
```

![](sensitive_test_generate_files/figure-gfm/combined-heatmap-1.png)<!-- -->

``` r
ts_plots$facet
```

![](sensitive_test_generate_files/figure-gfm/facet-heatmap-1.png)<!-- -->

``` r
pop_plots <- plot_scenario_heatmaps(population, "Population model")
pop_plots$return
```

![](sensitive_test_generate_files/figure-gfm/population-heatmaps-1.png)<!-- -->

``` r
pop_plots$sd
```

![](sensitive_test_generate_files/figure-gfm/population-sd-heatmap-1.png)<!-- -->

``` r
pop_plots$combined
```

![](sensitive_test_generate_files/figure-gfm/population-combined-heatmap-1.png)<!-- -->

``` r
pop_plots$facet
```

![](sensitive_test_generate_files/figure-gfm/population-facet-heatmap-1.png)<!-- -->

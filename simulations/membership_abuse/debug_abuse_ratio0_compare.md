Debug: No-Abuse Comparison (1 rider x 10 repeats)
================
Mingze Li
2026-06-10

Focused debug for `abuse_ratio = 0` only. This runs exactly `1` rider
and `10` repeats, and compares **trip-specific** vs **population**
models using matched repeat/test-trip draws so differences are easier to
attribute.

``` r
library(traveltimeCLT)
library(data.table)
library(ggplot2)

SEED <- 1234L
N_REPEATS <- 1L
RIDERS_PER_REPEAT <- 1L
TRIPS_PER_RIDER <- 100L
Q_REF <- 0.5

TIMEBINS <- c("EveningNight", "EveningRush", "Weekday", "MorningRush", "Weekendday")

stopifnot(RIDERS_PER_REPEAT == 1L)
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
  stat[, Kt := request_K(pt, distance, discount_factor = 1)]
  stat[, Rt := request_R(pt, start_time, start_time, distance, K = 1, risk_free = 0, zeta = 0)]
  stat[, ETA := pt$ETA]
  list(stat = na.omit(stat), fit = fit)
}

make_repeat_design <- function(common_trip_ids,
                               n_repeats = N_REPEATS,
                               trips_per_rider = TRIPS_PER_RIDER,
                               q_ref = Q_REF) {
  set.seed(SEED)
  rbindlist(lapply(seq_len(n_repeats), function(rep_i) {
    test_ids <- sample(common_trip_ids, trips_per_rider, replace = FALSE)
    sampled_ids <- sample(test_ids, trips_per_rider, replace = TRUE)
    data.table(
      repeat_time = rep_i,
      rider = 1L,
      q = q_ref,
      M = 0L,
      trip_idx = seq_len(trips_per_rider),
      test_ids = list(test_ids),
      tripID = sampled_ids,
      timeBin = sample(TIMEBINS, trips_per_rider, replace = TRUE)
    )
  }))
}

attach_contract_terms <- function(design_dt, trips_stat) {
  dt <- copy(design_dt)
  # Compute contract terms from the model-specific trip stats but using the same repeat test sets.
  terms <- dt[, {
    ids <- unique(unlist(test_ids))
    train <- trips_stat[!tripID %in% ids]
    Kt_mean <- mean(train$Kt)
    Rt_const <- mean(train$Rt)
    .(Kt = Kt_mean, Rt = Rt_const)
  }, by = .(repeat_time)]
  merge(dt[, !"test_ids"], terms, by = "repeat_time", all.x = TRUE)
}

summarize_repeat_metrics <- function(priced, trips_per_rider = TRIPS_PER_RIDER) {
  priced[, shortfall := as.integer(real_price > Kt)]
  priced[, .(
    n_trips = .N,
    total_profit = sum(Rt) + sum(pmin(Kt - real_price, 0)),
    total_expand = sum(real_price),
    pct_return = (sum(Rt) + sum(pmin(Kt - real_price, 0))) / sum(real_price) * trips_per_rider,
    total_Rt = sum(Rt),
    total_Kt = sum(Kt),
    mean_real_price = mean(real_price),
    sd_real_price = stats::sd(real_price),
    shortfall_rate = mean(shortfall)
  ), by = .(repeat_time, rider)]
}

summarize_model <- function(repeat_metrics, model_name) {
  repeat_metrics[, .(
    model = model_name,
    repeats = .N,
    mean_pct_return = mean(pct_return),
    sd_pct_return = stats::sd(pct_return),
    se_pct_return = stats::sd(pct_return) / sqrt(.N),
    mean_total_profit = mean(total_profit),
    sd_total_profit = stats::sd(total_profit),
    mean_total_expand = mean(total_expand),
    mean_shortfall_rate = mean(shortfall_rate)
  )]
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

cat("Loaded", uniqueN(trips$tripID), "trips\n")
```

    ## Loaded 23054 trips

``` r
ts_obj <- build_trips_stat(trips, trips_record, model = "trip-specific")
pop_obj <- build_trips_stat(trips, trips_record, model = "population")

trip_specific_stat <- ts_obj$stat
population_stat <- pop_obj$stat
cat("Trips available in trip-specific:", nrow(trip_specific_stat), "\n")
```

    ## Trips available in trip-specific: 23050

``` r
cat("Trips available in population:", nrow(population_stat), "\n")
```

    ## Trips available in population: 23054

``` r
common_trip_ids <- intersect(trip_specific_stat$tripID, population_stat$tripID)
cat("Common trip pool:", length(common_trip_ids), "\n")
```

    ## Common trip pool: 23050

``` r
kt_cmp <- merge(
  trip_specific_stat[, .(tripID, Kt_ts = Kt)],
  population_stat[, .(tripID, Kt_pop = Kt)],
  by = "tripID"
)
kt_cmp[, diff := Kt_ts - Kt_pop]

kt_summary <- kt_cmp[, .(
  corr = cor(Kt_ts, Kt_pop),
  mean_diff = mean(diff),
  mean_abs_diff = mean(abs(diff)),
  p50_abs_diff = as.numeric(stats::quantile(abs(diff), probs = 0.5)),
  p90_abs_diff = as.numeric(stats::quantile(abs(diff), probs = 0.9)),
  max_abs_diff = max(abs(diff))
)]
kt_summary
```

    ##         corr  mean_diff mean_abs_diff p50_abs_diff p90_abs_diff max_abs_diff
    ##        <num>      <num>         <num>        <num>        <num>        <num>
    ## 1: 0.9874992 -0.1285046      1.434913     1.004892     2.878975     44.16779

``` r
design_dt <- make_repeat_design(
  common_trip_ids = common_trip_ids,
  n_repeats = N_REPEATS,
  trips_per_rider = TRIPS_PER_RIDER,
  q_ref = Q_REF
)

# Keep a compact view of the matched repeat design (same for both models).
design_preview <- unique(design_dt[, .(repeat_time, rider, q, M)])
design_preview
```

    ##    repeat_time rider     q     M
    ##          <int> <int> <num> <int>
    ## 1:           1     1   0.5     0

``` r
ts_sampled <- attach_contract_terms(design_dt, trip_specific_stat)
set.seed(SEED + 100L)
ts_priced <- simulate_trip_prices(ts_sampled, trips[, .(tripID, linkID)], timebin_x_edges)
ts_repeat_metrics <- summarize_repeat_metrics(ts_priced)
ts_repeat_metrics[, model := "trip-specific"]
ts_repeat_metrics
```

    ##    repeat_time rider n_trips total_profit total_expand pct_return total_Rt
    ##          <int> <int>   <int>        <num>        <num>      <num>    <num>
    ## 1:           1     1     100    -347.6202     2213.323   -15.7058 49.54591
    ##    total_Kt mean_real_price sd_real_price shortfall_rate         model
    ##       <num>           <num>         <num>          <num>        <char>
    ## 1: 2460.469        22.13323      13.31976           0.33 trip-specific

``` r
pop_sampled <- attach_contract_terms(design_dt, population_stat)
set.seed(SEED + 100L)
pop_priced <- simulate_trip_prices(pop_sampled, trips[, .(tripID, linkID)], timebin_x_edges)
pop_repeat_metrics <- summarize_repeat_metrics(pop_priced)
pop_repeat_metrics[, model := "population"]
pop_repeat_metrics
```

    ##    repeat_time rider n_trips total_profit total_expand pct_return total_Rt
    ##          <int> <int>   <int>        <num>        <num>      <num>    <num>
    ## 1:           1     1     100    -308.8432     2213.323  -13.95382 84.20599
    ##    total_Kt mean_real_price sd_real_price shortfall_rate      model
    ##       <num>           <num>         <num>          <num>     <char>
    ## 1: 2472.945        22.13323      13.31976           0.33 population

``` r
summary_ts <- summarize_model(ts_repeat_metrics, "trip-specific")
summary_pop <- summarize_model(pop_repeat_metrics, "population")
summary_table <- rbind(summary_ts, summary_pop)
summary_table
```

    ##            model repeats mean_pct_return sd_pct_return se_pct_return
    ##           <char>   <int>           <num>         <num>         <num>
    ## 1: trip-specific       1       -15.70580            NA            NA
    ## 2:    population       1       -13.95382            NA            NA
    ##    mean_total_profit sd_total_profit mean_total_expand mean_shortfall_rate
    ##                <num>           <num>             <num>               <num>
    ## 1:         -347.6202              NA          2213.323                0.33
    ## 2:         -308.8432              NA          2213.323                0.33

``` r
repeat_compare <- merge(
  ts_repeat_metrics[, .(repeat_time, pct_return_ts = pct_return, profit_ts = total_profit, expand_ts = total_expand, shortfall_ts = shortfall_rate)],
  pop_repeat_metrics[, .(repeat_time, pct_return_pop = pct_return, profit_pop = total_profit, expand_pop = total_expand, shortfall_pop = shortfall_rate)],
  by = "repeat_time"
)
repeat_compare[, `:=`(
  delta_pct_return = pct_return_ts - pct_return_pop,
  delta_profit = profit_ts - profit_pop,
  delta_expand = expand_ts - expand_pop,
  delta_shortfall = shortfall_ts - shortfall_pop
)]
repeat_compare
```

    ## Key: <repeat_time>
    ##    repeat_time pct_return_ts profit_ts expand_ts shortfall_ts pct_return_pop
    ##          <int>         <num>     <num>     <num>        <num>          <num>
    ## 1:           1      -15.7058 -347.6202  2213.323         0.33      -13.95382
    ##    profit_pop expand_pop shortfall_pop delta_pct_return delta_profit
    ##         <num>      <num>         <num>            <num>        <num>
    ## 1:  -308.8432   2213.323          0.33         -1.75198    -38.77698
    ##    delta_expand delta_shortfall
    ##           <num>           <num>
    ## 1:            0               0

``` r
diag_table <- repeat_compare[, .(
  mean_abs_delta_pct_return = mean(abs(delta_pct_return)),
  sd_delta_pct_return = stats::sd(delta_pct_return),
  mean_abs_delta_profit = mean(abs(delta_profit)),
  sd_delta_profit = stats::sd(delta_profit),
  mean_abs_delta_shortfall = mean(abs(delta_shortfall)),
  corr_pct_return = cor(pct_return_ts, pct_return_pop),
  corr_profit = cor(profit_ts, profit_pop)
)]
diag_table
```

    ##    mean_abs_delta_pct_return sd_delta_pct_return mean_abs_delta_profit
    ##                        <num>               <num>                 <num>
    ## 1:                   1.75198                  NA              38.77698
    ##    sd_delta_profit mean_abs_delta_shortfall corr_pct_return corr_profit
    ##              <num>                    <num>           <num>       <num>
    ## 1:              NA                        0              NA          NA

``` r
cat("\nIf `mean_abs_delta_pct_return` and `sd_delta_pct_return` are near zero,\n")
```

    ## 
    ## If `mean_abs_delta_pct_return` and `sd_delta_pct_return` are near zero,

``` r
cat("the two models are behaving very similarly under abuse_ratio = 0 in this setup.\n")
```

    ## the two models are behaving very similarly under abuse_ratio = 0 in this setup.

``` r
plot_dt <- rbind(
  ts_repeat_metrics[, .(repeat_time, model, pct_return, total_profit)],
  pop_repeat_metrics[, .(repeat_time, model, pct_return, total_profit)]
)

p1 <- ggplot(plot_dt, aes(repeat_time, pct_return, color = model)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.8) +
  labs(title = "No-abuse pct return by repeat", x = "Repeat", y = "Pct return") +
  theme_minimal(base_size = 12)

p2 <- ggplot(plot_dt, aes(repeat_time, total_profit, color = model)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.8) +
  labs(title = "No-abuse total profit by repeat", x = "Repeat", y = "Total profit") +
  theme_minimal(base_size = 12)

p1
```

![](debug_abuse_ratio0_compare_files/figure-gfm/quick-plots-1.png)<!-- -->

``` r
p2
```

![](debug_abuse_ratio0_compare_files/figure-gfm/quick-plots-2.png)<!-- -->

``` r
dir.create("plot_abuse_debug", showWarnings = FALSE, recursive = TRUE)
fwrite(summary_table, "plot_abuse_debug/no_abuse_summary_1rider_10repeats.csv")
fwrite(repeat_compare, "plot_abuse_debug/no_abuse_repeat_compare_1rider_10repeats.csv")
fwrite(kt_cmp, "plot_abuse_debug/no_abuse_kt_trip_level_compare.csv")
```

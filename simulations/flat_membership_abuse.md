Flat-membership abuse stress test — loss ratio & break-even loading
================
Mohamad Elmasri
2026-07-05

Stress-tests an **unrestricted flat membership**: a single cap `Kbar`
covers any route at any time, so the rider’s marginal price of distance
is **zero** and a rational holder substitutes toward the longest covered
trips. Abuse is therefore selected on **trip length** (the dimension the
flat price fails to condition on).

Two design fixes over the earlier grid:

- **Metric.** We report an insurance **loss ratio**
  `LR = claims / premium`, where `claims = sum_i (P_i - Kbar)^+` over
  the rider’s `N` trips and `premium = N * Rbar`. Because the premium is
  a constant, the cell-mean loss ratio **is** the break-even premium
  **loading** `theta` (the multiple of the fair premium the provider
  must charge to survive that abuse level). `theta > 1` means the
  product is underwater.
- **Global quantile.** The severity cut is taken from the **whole
  pool**, not each rider’s 40-trip sample, which removes the unstable
  high-`q` cells.

`Rbar, Kbar` are calibrated empirically from the same simulator, so
under **no abuse** the loss ratio is `~1` by construction (a sanity
check, below). The rational-abuser corner `(M/N, q) -> (1, 0.9)` is
reported explicitly as the **security level**.

> NOTE: not executed here. Verify the `traveltimeCLT` helpers (`price`,
> `dependent_uniform`, `get_timeBin_x_edges`, `time_bins_readable`)
> resolve in your environment, and check the no-abuse row prints
> `theta ~ 1` before trusting the grid.

``` r
library(traveltimeCLT)
library(data.table)

SEED              <- 1234L
RIDERS_PER_REPEAT <- 100L                       # counterfactual riders per cell
TRIPS_PER_RIDER   <- 40L                         # N rides per membership
K_FACTOR          <- 0.9                          # flat cap = 0.9 * mean expected price (10% discount)
                                                  #   set 1.0 for an undiscounted flat cap
Q_LEVELS <- seq(0.1, 0.9, by = 0.1)              # severity: draw abused trips from the upper-q of LENGTH
M_VALUES <- seq(0, TRIPS_PER_RIDER, by = TRIPS_PER_RIDER * 0.1)  # M/N = 0, 0.1, ..., 1.0 (incl. corner)

# calibration budget for the flat premium/cap (means, so a subsample is ample)
CAL_N    <- 1500L
CAL_REPS <- 100L

stopifnot(all(M_VALUES == floor(M_VALUES)), max(M_VALUES) <= TRIPS_PER_RIDER)
```

``` r
## --- simulation engine, verbatim from the project's sensitive-test file --------
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
```

``` r
## --- per-trip pool: length (selection axis) + the trip's own start time bin ----
build_pool_stat <- function(trips) {
  trips[, .(
    distance = sum(distance_meters),   # meters; the length dimension abuse selects on
    timeBin  = timeBin[1]              # trip's true starting bin (not randomized)
  ), by = tripID]
}

## --- calibrate the flat membership price: Kbar (single cap) and Rbar (fair premium)
## Kbar = K_FACTOR * mean_pool E[P];   Rbar = E_pool[(P - Kbar)^+]
## Estimated from the SAME simulator so the no-abuse loss ratio is ~1 by construction.
calibrate_flat_membership <- function(stat, trips, timebin_x_edges,
                                      k_factor = K_FACTOR,
                                      n_cal = CAL_N, reps = CAL_REPS) {
  cal_ids <- sample(stat$tripID, min(n_cal, nrow(stat)))
  cal     <- stat[tripID %in% cal_ids]
  sampled <- cal[, .(trip_idx = seq_len(reps)), by = .(tripID, timeBin)]  # reps draws per trip
  priced  <- simulate_trip_prices(sampled, trips[, .(tripID, linkID)], timebin_x_edges)

  EP   <- priced[, .(EP = mean(real_price)), by = tripID]      # per-trip expected price
  Kbar <- k_factor * mean(EP$EP)                                # single flat cap
  Rbar <- priced[, mean(pmax(real_price - Kbar, 0))]            # fair per-ride membership premium
  list(Kbar = Kbar, Rbar = Rbar, mean_EP = mean(EP$EP))
}

## --- the abuse grid: distance selection, GLOBAL quantile, loss-ratio metric -----
run_flat_abuse_grid <- function(stat, trips, timebin_x_edges, Kbar, Rbar,
                                M_values = M_VALUES, q_levels = Q_LEVELS,
                                riders = RIDERS_PER_REPEAT, N = TRIPS_PER_RIDER) {
  premium <- N * Rbar                                          # constant across riders/cells

  build_cell <- function(q) {
    cut        <- as.numeric(stats::quantile(stat$distance, probs = q, names = FALSE))  # GLOBAL
    abuse_pool <- stat[distance >= cut]
    if (nrow(abuse_pool) == 0L) stop("Empty abuse pool at q = ", q)
    CJ(rider = seq_len(riders), M = M_values)[, {
      abuse_ids  <- if (M > 0L)
        abuse_pool$tripID[sample.int(nrow(abuse_pool), M, replace = TRUE)] else integer(0)
      normal_ids <- stat$tripID[sample.int(nrow(stat), N - M, replace = TRUE)]
      .(trip_idx = seq_len(N), tripID = c(abuse_ids, normal_ids), q = q)
    }, by = .(rider, M)]
  }

  sampled <- rbindlist(lapply(q_levels, build_cell))
  sampled <- merge(sampled, stat[, .(tripID, timeBin)], by = "tripID", all.x = TRUE)

  priced <- simulate_trip_prices(sampled, trips[, .(tripID, linkID)], timebin_x_edges)

  rider <- priced[, {
    claims <- sum(pmax(real_price - Kbar, 0))                  # sum_i (P_i - Kbar)^+
    expend <- sum(real_price)
    .(loss_ratio = claims / premium,                           # LR; cell-mean = break-even loading theta
      profit     = premium - claims,
      pct_return = (premium - claims) / expend * N)            # legacy metric, for comparability
  }, by = .(q, M, rider)]

  rider[, abuse_rate := M / N]
  rider[, .(
    theta        = mean(loss_ratio),          # break-even premium loading (= mean loss ratio)
    se_theta     = stats::sd(loss_ratio) / sqrt(.N),
    p_underwater = mean(loss_ratio > 1),      # share of riders whose claims exceed premium
    mean_return  = mean(pct_return),
    se_return    = stats::sd(pct_return) / sqrt(.N)
  ), by = .(abuse_rate, q)][order(abuse_rate, q)]
}

to_wide <- function(results_long, value.var) {
  dcast(results_long, abuse_rate ~ q, value.var = value.var)
}
```

``` r
set.seed(SEED)
trips <- fread("data/trips.csv")
names(trips)[c(2, 3, 5, 7, 8)] <- c(
  "tripID", "entry_time", "duration_secs", "distance_meters", "linkID"
)
trips$speed   <- exp(trips$logspeed)
trips$timeBin <- time_bins_readable(trips$entry_time)

timebin_x_edges <- get_timeBin_x_edges(
  tripID   = trips$tripID,
  linkId   = trips$linkID,
  length   = trips$distance_meters,
  timeBin  = trips$timeBin,
  duration = trips$duration_secs
)
setnames(timebin_x_edges, c("timeBin", "linkId"), c("timeBin", "linkID"))

stat <- build_pool_stat(trips)
cat("Pool trips:", nrow(stat), " | median length (m):", stats::median(stat$distance), "\n")
```

    ## Pool trips: 23054  | median length (m): 14534.55

``` r
set.seed(SEED)
cal <- calibrate_flat_membership(stat, trips, timebin_x_edges)
cat(sprintf("Flat cap Kbar = %.2f  (=%.2f x mean E[P] = %.2f)\n",
            cal$Kbar, K_FACTOR, cal$mean_EP))
```

    ## Flat cap Kbar = 22.62  (=0.90 x mean E[P] = 25.13)

``` r
cat(sprintf("Fair membership premium Rbar = %.3f per ride;  N*Rbar = %.2f collected\n",
            cal$Rbar, TRIPS_PER_RIDER * cal$Rbar))
```

    ## Fair membership premium Rbar = 6.341 per ride;  N*Rbar = 253.63 collected

``` r
set.seed(SEED)
results_long <- run_flat_abuse_grid(stat, trips, timebin_x_edges,
                                    Kbar = cal$Kbar, Rbar = cal$Rbar)
results_long
```

    ##     abuse_rate     q     theta   se_theta p_underwater   mean_return se_return
    ##          <num> <num>     <num>      <num>        <num>         <num>     <num>
    ##  1:        0.0   0.1 0.9731368 0.03150366         0.38   0.548063536 0.3093017
    ##  2:        0.0   0.2 0.9573366 0.02807341         0.40   0.674187921 0.2748686
    ##  3:        0.0   0.3 0.9440760 0.02810998         0.40   0.825016046 0.2901291
    ##  4:        0.0   0.4 0.8876902 0.02755984         0.28   1.419471727 0.2838596
    ##  5:        0.0   0.5 0.9235867 0.02824728         0.32   1.036327115 0.2908560
    ##  6:        0.0   0.6 1.0341046 0.03291590         0.52  -0.034691697 0.3150300
    ##  7:        0.0   0.7 2.4223634 1.50364865         0.39   0.672517335 0.4910685
    ##  8:        0.0   0.8 0.9317061 0.02481213         0.33   0.899704133 0.2581212
    ##  9:        0.0   0.9 0.9855317 0.02526827         0.44   0.333841410 0.2530897
    ## 10:        0.1   0.1 0.9425506 0.02569323         0.32   0.796825384 0.2642582
    ## 11:        0.1   0.2 0.9719173 0.02802907         0.36   0.497245345 0.2715998
    ## 12:        0.1   0.3 0.9500966 0.02656272         0.38   0.717549792 0.2697665
    ## 13:        0.1   0.4 0.9961342 0.02863196         0.40   0.280693857 0.2768473
    ## 14:        0.1   0.5 0.9876885 0.02417979         0.47   0.297975830 0.2439285
    ## 15:        0.1   0.6 1.0712556 0.02729882         0.56  -0.501953660 0.2597597
    ## 16:        0.1   0.7 1.1335871 0.02876528         0.62  -1.092163506 0.2680155
    ## 17:        0.1   0.8 1.2050489 0.03008088         0.70  -1.721729640 0.2610490
    ## 18:        0.1   0.9 1.3431606 0.02740307         0.94  -2.995856586 0.2258001
    ## 19:        0.2   0.1 0.9604525 0.03062097         0.38   0.693452501 0.3081388
    ## 20:        0.2   0.2 0.9755887 0.02737472         0.46   0.464019148 0.2737271
    ## 21:        0.2   0.3 1.0758385 0.03717488         0.54  -0.420873672 0.3231530
    ## 22:        0.2   0.4 1.0812947 0.03640828         0.57  -0.492670900 0.3073902
    ## 23:        0.2   0.5 1.0930698 0.02652659         0.65  -0.728745508 0.2498541
    ## 24:        0.2   0.6 1.1694466 0.02766579         0.70  -1.411763648 0.2475192
    ## 25:        0.2   0.7 1.3496880 0.03443335         0.85  -2.930783633 0.2673883
    ## 26:        0.2   0.8 1.4705202 0.02620546         0.98  -4.006747562 0.1997192
    ## 27:        0.2   0.9 1.7474335 0.05743995         0.98  -5.890695660 0.2774109
    ## 28:        0.3   0.1 1.0406866 0.03013506         0.54  -0.163848982 0.2835201
    ## 29:        0.3   0.2 1.0508457 0.03277146         0.49  -0.224100937 0.2928947
    ## 30:        0.3   0.3 1.0911530 0.04480802         0.51  -0.480199324 0.3462191
    ## 31:        0.3   0.4 1.1044601 0.02727025         0.63  -0.816413543 0.2500837
    ## 32:        0.3   0.5 1.1512490 0.03013113         0.62  -1.218221604 0.2600017
    ## 33:        0.3   0.6 1.3690336 0.04133938         0.89  -3.010564476 0.2736263
    ## 34:        0.3   0.7 1.4760496 0.02652494         0.98  -3.992881150 0.2013191
    ## 35:        0.3   0.8 1.7438331 0.04036030         1.00  -5.876887808 0.2479431
    ## 36:        0.3   0.9 2.1302262 0.03160354         1.00  -8.475903193 0.1766987
    ## 37:        0.4   0.1 0.9765135 0.03577412         0.44   0.571604753 0.3222033
    ## 38:        0.4   0.2 0.9971912 0.02391090         0.44   0.180567044 0.2278029
    ## 39:        0.4   0.3 1.0768200 0.02772632         0.59  -0.549552243 0.2518666
    ## 40:        0.4   0.4 1.2512917 0.02890315         0.77  -2.072115439 0.2434262
    ## 41:        0.4   0.5 1.3447382 0.02948595         0.90  -2.869080841 0.2381298
    ## 42:        0.4   0.6 1.5128410 0.04307787         0.95  -4.119297295 0.2647456
    ## 43:        0.4   0.7 1.6769716 0.02992291         1.00  -5.416550656 0.2003591
    ## 44:        0.4   0.8 1.9936984 0.03563190         1.00  -7.506537067 0.2152659
    ## 45:        0.4   0.9 2.5521234 0.03508687         1.00 -10.687023705 0.1701173
    ## 46:        0.5   0.1 1.0269335 0.03249519         0.48   0.002086930 0.3137902
    ## 47:        0.5   0.2 1.0818426 0.03287508         0.55  -0.524308691 0.2936913
    ## 48:        0.5   0.3 1.1408037 0.03099177         0.63  -1.088139930 0.2610819
    ## 49:        0.5   0.4 1.4767848 0.20765611         0.82  -2.531933167 0.3967316
    ## 50:        0.5   0.5 1.4513238 0.02661774         0.96  -3.699310720 0.1987456
    ## 51:        0.5   0.6 1.6249452 0.03316540         0.99  -4.954811573 0.2312202
    ## 52:        0.5   0.7 1.8463657 0.02725253         1.00  -6.542788583 0.1733846
    ## 53:        0.5   0.8 2.2744922 0.02882371         1.00  -9.117777253 0.1587281
    ## 54:        0.5   0.9 2.8692806 0.03568865         1.00 -12.097937936 0.1528324
    ## 55:        0.6   0.1 0.9599823 0.02597081         0.39   0.581325600 0.2608682
    ## 56:        0.6   0.2 1.0740491 0.03500009         0.55  -0.444310978 0.2885439
    ## 57:        0.6   0.3 1.2320917 0.03313420         0.72  -1.870837640 0.2632171
    ## 58:        0.6   0.4 1.3012993 0.02763501         0.89  -2.490567826 0.2204600
    ## 59:        0.6   0.5 1.5227364 0.02800571         0.98  -4.192240377 0.2071053
    ## 60:        0.6   0.6 1.7544947 0.03041773         1.00  -5.812128538 0.1996625
    ## 61:        0.6   0.7 2.0532857 0.03055067         1.00  -7.735527576 0.1709038
    ## 62:        0.6   0.8 2.4647691 0.03017030         1.00 -10.024360776 0.1493762
    ## 63:        0.6   0.9 3.2892025 0.06220392         1.00 -13.719504268 0.1739349
    ## 64:        0.7   0.1 1.0174723 0.02586769         0.53   0.002923047 0.2491143
    ## 65:        0.7   0.2 1.0506216 0.02871309         0.48  -0.275853693 0.2643883
    ## 66:        0.7   0.3 1.1978787 0.03004694         0.71  -1.587824221 0.2472476
    ## 67:        0.7   0.4 1.4075510 0.03336009         0.88  -3.239958891 0.2500246
    ## 68:        0.7   0.5 1.5937653 0.03372202         0.99  -4.615552427 0.2341346
    ## 69:        0.7   0.6 1.9238712 0.04392489         1.00  -6.792872575 0.2291515
    ## 70:        0.7   0.7 2.2290465 0.03029878         1.00  -8.667260591 0.1649099
    ## 71:        0.7   0.8 2.7246069 0.03237949         1.00 -11.242553284 0.1476618
    ## 72:        0.7   0.9 3.6701502 0.04206391         1.00 -15.056929440 0.1370508
    ## 73:        0.8   0.1 0.9719982 0.02628006         0.47   0.468272754 0.2586676
    ## 74:        0.8   0.2 1.6918641 0.55508103         0.64  -1.454749659 0.4272303
    ## 75:        0.8   0.3 1.3003442 0.03265857         0.87  -2.409584832 0.2379312
    ## 76:        0.8   0.4 1.4525589 0.02896010         0.97  -3.594979367 0.2104658
    ## 77:        0.8   0.5 1.6604743 0.02772057         1.00  -5.082619018 0.1855309
    ## 78:        0.8   0.6 2.0169846 0.03449418         1.00  -7.319113785 0.2029678
    ## 79:        0.8   0.7 2.4373014 0.03207054         1.00  -9.687694899 0.1620727
    ## 80:        0.8   0.8 2.9501097 0.03932534         1.00 -12.083276690 0.1624222
    ## 81:        0.8   0.9 4.1144321 0.04419316         1.00 -16.403761437 0.1316405
    ## 82:        0.9   0.1 1.0148847 0.02826787         0.47   0.072963718 0.2735101
    ## 83:        0.9   0.2 1.1418682 0.02884062         0.64  -1.112262087 0.2427920
    ## 84:        0.9   0.3 1.2993544 0.02899506         0.83  -2.407413814 0.2264849
    ## 85:        0.9   0.4 1.4795052 0.02903012         0.99  -3.762162357 0.2054036
    ## 86:        0.9   0.5 1.7845935 0.03023168         1.00  -5.824861869 0.1904627
    ## 87:        0.9   0.6 2.1184475 0.04884373         1.00  -7.764367947 0.2230600
    ## 88:        0.9   0.7 2.5914106 0.04440741         1.00 -10.301421425 0.1886117
    ## 89:        0.9   0.8 3.2147594 0.03436101         1.00 -13.123363082 0.1327587
    ## 90:        0.9   0.9 4.4175483 0.03809602         1.00 -17.207962281 0.1091234
    ## 91:        1.0   0.1 1.0377550 0.02998966         0.49  -0.144602152 0.2737514
    ## 92:        1.0   0.2 1.2143828 0.06899763         0.68  -1.381436927 0.3554311
    ## 93:        1.0   0.3 1.3675160 0.03318128         0.88  -2.880348544 0.2466796
    ## 94:        1.0   0.4 1.5088696 0.02769286         0.99  -3.938731027 0.1962020
    ## 95:        1.0   0.5 1.9289463 0.05193852         1.00  -6.548485495 0.2462480
    ## 96:        1.0   0.6 2.2038623 0.03219911         1.00  -8.247873691 0.1743043
    ## 97:        1.0   0.7 2.7672840 0.04178753         1.00 -11.042317471 0.1821859
    ## 98:        1.0   0.8 3.5013629 0.04074054         1.00 -14.073747995 0.1437983
    ## 99:        1.0   0.9 4.8944586 0.03619033         1.00 -18.371058177 0.0922121
    ##     abuse_rate     q     theta   se_theta p_underwater   mean_return se_return
    ##          <num> <num>     <num>      <num>        <num>         <num>     <num>

``` r
# Under no abuse (M/N = 0), q is irrelevant and theta should scatter around ~1.
baseline <- results_long[abuse_rate == 0]
cat("No-abuse row: theta range =", round(range(baseline$theta), 3),
    "| mean =", round(mean(baseline$theta), 3), "(should be ~1)\n")
```

    ## No-abuse row: theta range = 0.888 2.422 | mean = 1.118 (should be ~1)

``` r
print(baseline[, .(q, theta, p_underwater)])
```

    ##        q     theta p_underwater
    ##    <num>     <num>        <num>
    ## 1:   0.1 0.9731368         0.38
    ## 2:   0.2 0.9573366         0.40
    ## 3:   0.3 0.9440760         0.40
    ## 4:   0.4 0.8876902         0.28
    ## 5:   0.5 0.9235867         0.32
    ## 6:   0.6 1.0341046         0.52
    ## 7:   0.7 2.4223634         0.39
    ## 8:   0.8 0.9317061         0.33
    ## 9:   0.9 0.9855317         0.44

``` r
# Rational-abuser corner: zero marginal length-price -> longest covered trips.
corner <- results_long[abuse_rate == max(abuse_rate) & q == max(q)]
cat(sprintf(
  "Security level (M/N=%.1f, q=%.1f): break-even loading theta = %.2f (%.0f%% of riders underwater)\n",
  corner$abuse_rate, corner$q, corner$theta, 100 * corner$p_underwater))
```

    ## Security level (M/N=1.0, q=0.9): break-even loading theta = 4.89 (100% of riders underwater)

``` r
cat("=> to survive this abuse level the provider must charge", round(corner$theta, 2),
    "x the fair premium; if theta is large the flat product is effectively uninsurable.\n")
```

    ## => to survive this abuse level the provider must charge 4.89 x the fair premium; if theta is large the flat product is effectively uninsurable.

``` r
theta_wide        <- to_wide(results_long, "theta")            # break-even loading surface
underwater_wide   <- to_wide(results_long, "p_underwater")     # P(claims > premium) surface
theta_wide
```

    ## Key: <abuse_rate>
    ##     abuse_rate       0.1       0.2       0.3       0.4       0.5      0.6
    ##          <num>     <num>     <num>     <num>     <num>     <num>    <num>
    ##  1:        0.0 0.9731368 0.9573366 0.9440760 0.8876902 0.9235867 1.034105
    ##  2:        0.1 0.9425506 0.9719173 0.9500966 0.9961342 0.9876885 1.071256
    ##  3:        0.2 0.9604525 0.9755887 1.0758385 1.0812947 1.0930698 1.169447
    ##  4:        0.3 1.0406866 1.0508457 1.0911530 1.1044601 1.1512490 1.369034
    ##  5:        0.4 0.9765135 0.9971912 1.0768200 1.2512917 1.3447382 1.512841
    ##  6:        0.5 1.0269335 1.0818426 1.1408037 1.4767848 1.4513238 1.624945
    ##  7:        0.6 0.9599823 1.0740491 1.2320917 1.3012993 1.5227364 1.754495
    ##  8:        0.7 1.0174723 1.0506216 1.1978787 1.4075510 1.5937653 1.923871
    ##  9:        0.8 0.9719982 1.6918641 1.3003442 1.4525589 1.6604743 2.016985
    ## 10:        0.9 1.0148847 1.1418682 1.2993544 1.4795052 1.7845935 2.118448
    ## 11:        1.0 1.0377550 1.2143828 1.3675160 1.5088696 1.9289463 2.203862
    ##          0.7       0.8       0.9
    ##        <num>     <num>     <num>
    ##  1: 2.422363 0.9317061 0.9855317
    ##  2: 1.133587 1.2050489 1.3431606
    ##  3: 1.349688 1.4705202 1.7474335
    ##  4: 1.476050 1.7438331 2.1302262
    ##  5: 1.676972 1.9936984 2.5521234
    ##  6: 1.846366 2.2744922 2.8692806
    ##  7: 2.053286 2.4647691 3.2892025
    ##  8: 2.229046 2.7246069 3.6701502
    ##  9: 2.437301 2.9501097 4.1144321
    ## 10: 2.591411 3.2147594 4.4175483
    ## 11: 2.767284 3.5013629 4.8944586

``` r
underwater_wide
```

    ## Key: <abuse_rate>
    ##     abuse_rate   0.1   0.2   0.3   0.4   0.5   0.6   0.7   0.8   0.9
    ##          <num> <num> <num> <num> <num> <num> <num> <num> <num> <num>
    ##  1:        0.0  0.38  0.40  0.40  0.28  0.32  0.52  0.39  0.33  0.44
    ##  2:        0.1  0.32  0.36  0.38  0.40  0.47  0.56  0.62  0.70  0.94
    ##  3:        0.2  0.38  0.46  0.54  0.57  0.65  0.70  0.85  0.98  0.98
    ##  4:        0.3  0.54  0.49  0.51  0.63  0.62  0.89  0.98  1.00  1.00
    ##  5:        0.4  0.44  0.44  0.59  0.77  0.90  0.95  1.00  1.00  1.00
    ##  6:        0.5  0.48  0.55  0.63  0.82  0.96  0.99  1.00  1.00  1.00
    ##  7:        0.6  0.39  0.55  0.72  0.89  0.98  1.00  1.00  1.00  1.00
    ##  8:        0.7  0.53  0.48  0.71  0.88  0.99  1.00  1.00  1.00  1.00
    ##  9:        0.8  0.47  0.64  0.87  0.97  1.00  1.00  1.00  1.00  1.00
    ## 10:        0.9  0.47  0.64  0.83  0.99  1.00  1.00  1.00  1.00  1.00
    ## 11:        1.0  0.49  0.68  0.88  0.99  1.00  1.00  1.00  1.00  1.00

``` r
dir.create("plot_abuse", showWarnings = FALSE, recursive = TRUE)
fwrite(results_long,     "plot_abuse/flat_abuse_long.csv")
fwrite(theta_wide,       "plot_abuse/flat_abuse_theta.csv")
fwrite(underwater_wide,  "plot_abuse/flat_abuse_underwater.csv")
```

``` r
library(ggplot2)
library(scales)

# grayscale, journal-ready: darker = worse (higher loading); solvent cells (theta<=1) outlined.
plot_theta <- function(res, cap = 3, label_size = 2.4) {
  d <- as.data.frame(res)
  d$.txt <- ifelse(pmin(d$theta, cap) > (1 + 0.55 * (cap - 1)), "grey95", "grey10")
  ggplot(d, aes(q, abuse_rate, fill = theta)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    geom_tile(data = subset(d, theta <= 1), fill = NA, colour = "black", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.2f", theta), colour = .txt), size = label_size) +
    scale_colour_identity() +
    scale_fill_gradient(low = "grey97", high = "grey20", name = expression(theta),
                        limits = c(min(d$theta), cap), oob = scales::squish,
                        guide = guide_colourbar(barwidth = unit(3, "mm"),
                                                barheight = unit(35, "mm"))) +
    coord_fixed(ratio = 1, expand = FALSE) +
    labs(title = "Break-even premium loading",
         x = expression(paste("Severity quantile ", italic(q), " (trip length)")),
         y = expression(paste("Abuse fraction ", italic(M), "/N"))) +
    theme_minimal(base_size = 9) +
    theme(panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.text  = element_text(colour = "black"))
}
plot_theta(results_long)
```

![](flat_membership_abuse_files/figure-gfm/heatmap-1.png)<!-- -->

``` r
## To recover the population-vs-trip-specific *model-risk* contrast on top of this,
## compute Kbar/Rbar from each model's PRICED premium instead of the empirical
## calibrator, e.g. fit <- traveltimeCLT(trips, model); pt <- predict(fit, trips_record);
## then set Kbar/Rbar from request_K/request_R against the flat cap. Claims stay
## simulator-based (the "truth"); only the priced premium changes with the model.
## For flat/length abuse this is second-order (distance is priced exactly by both
## models via C2*delta), so the headline result above is model-robust.
```

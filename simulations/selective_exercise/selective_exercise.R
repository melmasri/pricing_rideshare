# Selective exercise in the commuter membership (paper Sec. 7.9).
#
# The commuter of Sec. 7.8 holds M = 30 capped rides on a fixed origin-destination
# pair at a fixed departure hour, and Sec. 7.8 drops the rides that are not taken at
# random. A rational holder does not drop at random: on the morning of each occasion
# they observe something about conditions and exercise the membership when the ride
# looks expensive. The rider's private benefit on occasion i is (P_i - K)^+, which is
# exactly the provider's payout, so rider objective and provider loss are one object
# and no separate utility model is needed.
#
# This file is a *wrapper*. Pricing, route sampling, travel-time sampling, and the
# premium all come from the Sec. 7.8 pipeline through its two cached artefacts:
#
#   ../plot/commuter_membership_results.rds      per-member premium and dollar cap K
#   ../commuter_membership/commuter_cohorts.rds  per-member realized fares P_{m,1..M}
#
# Nothing here re-prices anything. The only new logic is the selection rule below,
# plus aggregation.
#
# Foresight lambda in [0, 1] is the rank correlation between the rider's signal and
# the realized fare: lambda = 0 reproduces the random breakage of Sec. 7.8, lambda = 1
# is a rider who always exercises on the worst-traffic days.

suppressPackageStartupMessages({
  library(data.table)
})

SELECTIVE_DEFAULTS <- list(
  lambdas = c(0, 0.25, 0.50, 0.75, 0.90, 1.00),
  # Sec. 7.8's max-loss column grows with h because the eligible OD-pair pool shrinks
  # from 1,001 pairs at h = 2 to 63 at h = 8, not because pricing degrades, so the h
  # axis is not a clean comparison unless the pair set is held fixed across h. Holding
  # it fixed means rebuilding the cohorts, which is exactly what this wrapper does not
  # do, so the headline fixes h = 8. Widening this is allowed but the caveat travels
  # with it.
  history_sizes = 8L,
  # Utilizations at which to ask how good a forecaster the rider must be.
  u0_grid = c(0.6, 0.7, 0.8, 0.9, 1.0),
  n_boot = 1000L,
  ci_level = 0.95,
  # Only the rider's private noise is drawn here; every priced quantity is inherited.
  seed = 20260730L
)

RESULTS_RDS <- "../plot/commuter_membership_results.rds"
COHORT_RDS <- "../commuter_membership/commuter_cohorts.rds"

#' Read the priced memberships and the realized months from the Sec. 7.8 artefacts.
#'
#' `premium` is Pi_m of eq. (24) and `cap` is the month's dollar strike K_m, both
#' already averaged over the sampled history routes by the existing pipeline. Fares
#' are model free and strike free, so one set of realized months serves every config.
load_selective_inputs <- function(results_rds = RESULTS_RDS,
                                  cohort_rds = COHORT_RDS,
                                  history_sizes = SELECTIVE_DEFAULTS$history_sizes) {
  if (!file.exists(results_rds)) {
    stop("Missing ", results_rds, ". Run commuter_membership/run_commuter_membership.R first.")
  }
  if (!file.exists(cohort_rds)) {
    stop("Missing ", cohort_rds, ". Run commuter_membership/run_commuter_membership.R first.")
  }
  results <- readRDS(results_rds)
  cohorts <- readRDS(cohort_rds)$cohorts

  missing <- setdiff(history_sizes, results$settings$history_sizes)
  if (length(missing) > 0) {
    stop("The commuter run holds no cohort at history size(s): ",
         paste(missing, collapse = ", "))
  }
  rides_per_member <- results$settings$rides_per_member

  members <- rbindlist(lapply(results$runs, function(run) {
    if (!run$history_size %in% history_sizes) return(NULL)
    data.table(
      model = run$model,
      strike = run$k_factor,
      h = run$history_size,
      member_id = run$by_rider$rider,
      premium = run$by_rider$premium,
      cap = run$by_rider$strike,
      fares_total = run$by_rider$spend
    )
  }))

  rides <- rbindlist(lapply(history_sizes, function(h) {
    cohort <- cohorts[[paste0("h", h)]]$rides
    data.table(h = h, member_id = cohort$rider, ride = cohort$ride, fare = cohort$real_price)
  }))

  check_inputs(members, rides, rides_per_member)

  list(
    members = members[],
    rides = rides[],
    rides_per_member = rides_per_member,
    commuter_settings = results$settings,
    reference_usage = results$usage[history_size %in% history_sizes]
  )
}

#' Guard the join between the two artefacts: a stale cache would pair a member's
#' premium with somebody else's month, and nothing downstream would notice.
check_inputs <- function(members, rides, rides_per_member) {
  counts <- rides[, .N, by = .(h, member_id)]
  if (any(counts$N != rides_per_member)) {
    stop("Some members do not hold exactly ", rides_per_member, " realized rides.")
  }
  totals <- rides[, .(fares = sum(fare)), by = .(h, member_id)]
  joined <- merge(members, totals, by = c("h", "member_id"))
  if (nrow(joined) != nrow(members)) {
    stop("Priced members and realized months do not line up; the caches are out of step.")
  }
  drift <- joined[, max(abs(fares_total - fares) / pmax(fares, 1e-8))]
  if (drift > 1e-8) {
    stop("Realized fares disagree with the recorded month spend by ", signif(drift, 3),
         "; the results RDS and the cohort cache come from different runs.")
  }
  invisible(TRUE)
}

#' Within-month normal scores of the fare, and one private noise draw per member.
#'
#' Ranking is on the fare P and not on the payout L: L has an atom at zero on the
#' out-of-the-money occasions, which would produce massive ties, while P is continuous
#' and L is monotone in P, so ranking on P induces the correct ranking on L. Normal
#' scores rather than raw fares make lambda scale free and give the signal the same
#' marginal distribution at every lambda.
#'
#' The noise is drawn once per member and reused for every lambda, every k, and every
#' (model, strike) config. Realized fares are model free, so the whole selection order
#' is shared across configs: comparisons are paired throughout.
member_signals <- function(rides, seed = SELECTIVE_DEFAULTS$seed) {
  set.seed(seed)
  signals <- copy(rides)
  setorder(signals, h, member_id, ride)
  signals[, z := qnorm((frank(fare, ties.method = "random") - 0.5) / .N), by = .(h, member_id)]
  signals[, eps := rnorm(.N), by = .(h, member_id)]
  signals[]
}

#' The order in which a rider of foresight lambda exercises the month's occasions.
#'
#' S = lambda * z + sqrt(1 - lambda^2) * eps, taken in descending order. At lambda = 1
#' the noise term vanishes exactly and S = z, whose values are distinct by construction
#' (they are normal scores of the ranks 1..M), so the ordering stays well defined
#' rather than collapsing into ties.
exercise_order <- function(signals, lambda) {
  stopifnot(lambda >= 0, lambda <= 1)
  noise_weight <- sqrt(max(1 - lambda^2, 0))
  ord <- signals[, .(h, member_id, ride, S = lambda * z + noise_weight * eps)]
  setorder(ord, h, member_id, -S)
  ord[, k := seq_len(.N), by = .(h, member_id)]
  ord[, .(h, member_id, ride, k)]
}

#' Payout on each occasion: L_{m,i} = (P_{m,i} - K_m)^+.
#'
#' The rider pays the premium up front and then min(P_i, K) per ride taken, while the
#' provider's cost of supplying ride i is P_i, so for a taken set A the profit is
#' Pi_m - sum_{i in A} L_{m,i}. Utilization enters only through which L_i are summed.
payout_ledger <- function(members, rides) {
  ledger <- merge(members, rides, by = c("h", "member_id"), allow.cartesian = TRUE)
  ledger[, payout_i := pmax(fare - cap, 0)]
  ledger[]
}

#' Member-level curves in k for every lambda: payout, profit, and loss ratio.
#'
#' Because the taken set at k is the first k entries of one order, the curve is nested
#' in k by construction, and because the order is built from noise drawn once per
#' member, curves at different lambda are paired.
selective_curves <- function(ledger, signals, lambdas, rides_per_member) {
  taken_none <- unique(ledger[, .(model, strike, h, member_id, premium, fares_total)])
  taken_none[, `:=`(k = 0L, payout = 0, fares_taken = 0)]

  raw <- rbindlist(lapply(lambdas, function(lam) {
    ord <- exercise_order(signals, lam)
    taken <- merge(ledger, ord, by = c("h", "member_id", "ride"), allow.cartesian = TRUE)
    setorder(taken, model, strike, h, member_id, k)
    taken[, `:=`(
      payout = cumsum(payout_i),
      fares_taken = cumsum(fare)
    ), by = .(model, strike, h, member_id)]
    columns <- c("model", "strike", "h", "member_id", "k",
                 "premium", "payout", "fares_taken", "fares_total")
    both <- rbind(taken[, ..columns], taken_none[, ..columns])
    both[, lambda := lam]
    both
  }))

  raw[, `:=`(
    u = k / rides_per_member,
    profit = premium - payout,
    loss_ratio = payout / premium
  )]
  setcolorder(raw, c("member_id", "model", "strike", "h", "lambda", "k", "u",
                     "premium", "payout", "profit", "fares_taken", "fares_total",
                     "loss_ratio"))
  setorder(raw, model, strike, h, lambda, member_id, k)
  raw[]
}

#' Mean profit and the two loss ratios across members, with marginal standard errors.
#'
#' The loss ratio normalizes by the premium rather than by the fares taken: the premium
#' is fixed within a member while the fares taken move with k, so a "% of fares taken"
#' denominator would contaminate the utilization curve with a trend of its own.
#'
#' Two versions are kept, and they answer different questions. `mean_loss_ratio` is the
#' average over members of each member's own payout-to-premium ratio, so every member
#' counts once: it describes the typical membership. `pooled_loss_ratio` divides total
#' payout by total premium, so members count in proportion to what they pay: it is the
#' provider's book, and it is the one that reconciles with the dollar-profit curve of
#' Sec. 7.8, since it crosses one exactly where mean profit crosses zero. The premium
#' distribution is right skewed enough for the two to differ materially, so the choice
#' is reported rather than made silently.
aggregate_curves <- function(raw) {
  curves <- raw[, .(
    n_members = .N,
    mean_profit = mean(profit),
    se_profit = sd(profit) / sqrt(.N),
    mean_loss_ratio = mean(loss_ratio),
    se_loss_ratio = sd(loss_ratio) / sqrt(.N),
    pooled_loss_ratio = sum(payout) / sum(premium),
    median_loss_ratio = median(loss_ratio)
  ), by = .(model, strike, h, lambda, k, u)]
  setorder(curves, model, strike, h, lambda, k)
  curves[]
}

#' First x at which a monotone-ish y reaches `level`, linearly interpolated.
crossing_point <- function(x, y, level = 1) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  if (length(x) == 0L || all(y < level)) return(NA_real_)
  i <- which(y >= level)[1L]
  if (i == 1L) return(x[1L])
  if (y[i] == y[i - 1L]) return(x[i])
  x[i - 1L] + (level - y[i - 1L]) * (x[i] - x[i - 1L]) / (y[i] - y[i - 1L])
}

#' u*(lambda): the utilization at which the mean loss ratio first reaches one.
#'
#' NA means the product never breaks even within the month, i.e. u* > 1.
break_even_utilization <- function(curves) {
  out <- curves[order(k), .(
    u_star = crossing_point(u, mean_loss_ratio),
    u_star_pooled = crossing_point(u, pooled_loss_ratio)
  ), by = .(model, strike, h, lambda)]
  setorder(out, model, strike, h, lambda)
  # How far selection moves the threshold, which is the quantity of interest even where
  # the level itself is only loosely pinned down.
  out[, shift := u_star - u_star[lambda == 0], by = .(model, strike, h)]
  out[, shift_pooled := u_star_pooled - u_star_pooled[lambda == 0],
      by = .(model, strike, h)]
  out[]
}

#' lambda*(u0): how good a forecaster the rider must be before the product loses money.
#'
#' Inverting the question this way avoids having to calibrate the rider's true
#' foresight, which no data here can identify.
foresight_threshold <- function(curves, u0_grid = SELECTIVE_DEFAULTS$u0_grid) {
  at_u0 <- rbindlist(lapply(u0_grid, function(target) {
    curves[order(k), .(
      u0 = target,
      loss_ratio = approx(u, mean_loss_ratio, xout = target, rule = 2)$y,
      pooled = approx(u, pooled_loss_ratio, xout = target, rule = 2)$y
    ), by = .(model, strike, h, lambda)]
  }))
  out <- at_u0[order(lambda), .(
    lambda_star = crossing_point(lambda, loss_ratio),
    lambda_star_pooled = crossing_point(lambda, pooled)
  ), by = .(model, strike, h, u0)]
  setorder(out, model, strike, h, u0)
  out[]
}

#' Member-level nonparametric bootstrap for u*(lambda) and lambda*(u0).
#'
#' Members are resampled, not (member, lambda) cells, so the pairing induced by the
#' common random numbers survives: the lambda contrasts are far tighter than their
#' marginal standard errors suggest.
bootstrap_thresholds <- function(raw, lambdas,
                                 u0_grid = SELECTIVE_DEFAULTS$u0_grid,
                                 n_boot = SELECTIVE_DEFAULTS$n_boot,
                                 ci_level = SELECTIVE_DEFAULTS$ci_level,
                                 seed = SELECTIVE_DEFAULTS$seed,
                                 rides_per_member = 30L) {
  probs <- c((1 - ci_level) / 2, 1 - (1 - ci_level) / 2)
  configs <- unique(raw[, .(model, strike, h)])
  u_grid <- seq(0, 1, length.out = rides_per_member + 1L)

  set.seed(seed)
  results <- lapply(seq_len(nrow(configs)), function(i) {
    config <- configs[i]
    cell <- raw[model == config$model & strike == config$strike & h == config$h]
    n <- uniqueN(cell$member_id)

    # One members x (M + 1) payout matrix per lambda, in a shared member order, plus the
    # premium each member paid. Both loss ratios are recovered from these, so a resample
    # is drawn once and priced under both definitions.
    wide_payout <- lapply(lambdas, function(lam) {
      wide <- dcast(cell[lambda == lam], member_id ~ k, value.var = "payout")
      setorder(wide, member_id)
      m <- as.matrix(wide[, -1L, with = FALSE])
      m[, order(as.integer(colnames(m))), drop = FALSE]
    })
    premium <- cell[lambda == lambdas[1L] & k == 0L][order(member_id), premium]

    u_star <- matrix(NA_real_, n_boot, length(lambdas))
    u_star_pooled <- matrix(NA_real_, n_boot, length(lambdas))
    lambda_star <- matrix(NA_real_, n_boot, length(u0_grid))
    lambda_star_pooled <- matrix(NA_real_, n_boot, length(u0_grid))
    for (b in seq_len(n_boot)) {
      # One draw of members serves every lambda, so differences taken within a row are
      # paired and the common random numbers are not thrown away.
      draw <- sample.int(n, n, replace = TRUE)
      paid <- sum(premium[draw])
      member_mean <- lapply(wide_payout, function(m) colMeans(m[draw, ] / premium[draw]))
      pooled <- lapply(wide_payout, function(m) colSums(m[draw, ]) / paid)
      u_star[b, ] <- vapply(member_mean, function(y) crossing_point(u_grid, y), numeric(1))
      u_star_pooled[b, ] <- vapply(pooled, function(y) crossing_point(u_grid, y), numeric(1))
      at_u0 <- function(curve_set) {
        vapply(u0_grid, function(u0) {
          vapply(curve_set, function(y) approx(u_grid, y, xout = u0, rule = 2)$y,
                 numeric(1))
        }, numeric(length(lambdas)))
      }
      lambda_star[b, ] <- apply(at_u0(member_mean), 2L,
                                function(y) crossing_point(lambdas, y))
      lambda_star_pooled[b, ] <- apply(at_u0(pooled), 2L,
                                       function(y) crossing_point(lambdas, y))
    }

    # Replicates in which the threshold is never reached carry no value to average, so
    # they are reported as a share rather than silently dropped into the interval.
    spread <- function(x) if (sum(is.finite(x)) < 2L) NA_real_ else sd(x[is.finite(x)])
    edge <- function(x, side) {
      finite <- x[is.finite(x)]
      if (length(finite) == 0L) return(NA_real_)
      unname(quantile(finite, probs[side]))
    }
    # The shift away from the no-foresight case is the quantity the section rests on,
    # and it is identified far better than either level: the same members carry both
    # curves, so the member heterogeneity that dominates the marginal interval cancels.
    shift <- u_star - u_star[, 1L]
    shift_pooled <- u_star_pooled - u_star_pooled[, 1L]

    list(
      u_star = data.table(
        config,
        lambda = lambdas,
        u_star_se = apply(u_star, 2L, spread),
        u_star_lo = apply(u_star, 2L, edge, side = 1L),
        u_star_hi = apply(u_star, 2L, edge, side = 2L),
        p_no_break_even = apply(u_star, 2L, function(x) mean(!is.finite(x))),
        shift_se = apply(shift, 2L, spread),
        shift_lo = apply(shift, 2L, edge, side = 1L),
        shift_hi = apply(shift, 2L, edge, side = 2L),
        u_star_pooled_lo = apply(u_star_pooled, 2L, edge, side = 1L),
        u_star_pooled_hi = apply(u_star_pooled, 2L, edge, side = 2L),
        p_no_break_even_pooled = apply(u_star_pooled, 2L, function(x) mean(!is.finite(x))),
        shift_pooled_lo = apply(shift_pooled, 2L, edge, side = 1L),
        shift_pooled_hi = apply(shift_pooled, 2L, edge, side = 2L)
      ),
      lambda_star = data.table(
        config,
        u0 = u0_grid,
        lambda_star_se = apply(lambda_star, 2L, spread),
        lambda_star_lo = apply(lambda_star, 2L, edge, side = 1L),
        lambda_star_hi = apply(lambda_star, 2L, edge, side = 2L),
        p_never_loses = apply(lambda_star, 2L, function(x) mean(!is.finite(x))),
        lambda_star_pooled_lo = apply(lambda_star_pooled, 2L, edge, side = 1L),
        lambda_star_pooled_hi = apply(lambda_star_pooled, 2L, edge, side = 2L),
        p_never_loses_pooled = apply(lambda_star_pooled, 2L,
                                     function(x) mean(!is.finite(x)))
      )
    )
  })

  list(
    u_star = rbindlist(lapply(results, `[[`, "u_star")),
    lambda_star = rbindlist(lapply(results, `[[`, "lambda_star"))
  )
}

#' Structural checks that must hold before any figure is drawn.
#'
#' Test 2 is the strongest of them: taking all M rides pays the same regardless of the
#' order they were taken in, so if it fails the selection indexing is wrong. Test 7 is
#' the regression test protecting the published Sec. 7.8 breakage result.
run_acceptance_tests <- function(raw, signals, curves, reference_usage, rides_per_member) {
  M <- rides_per_member
  lambdas <- sort(unique(raw$lambda))
  result <- function(id, description, statistic, tolerance, pass) {
    data.table(test = id, description = description,
               statistic = statistic, tolerance = tolerance, pass = pass)
  }

  # 1. Nothing is owed on a month in which nothing is exercised.
  t1 <- raw[k == 0L, max(abs(loss_ratio))]

  # 2. The full month costs the same under every foresight level.
  full <- raw[k == M, .(spread = diff(range(loss_ratio))),
              by = .(model, strike, h, member_id)]
  t2 <- full[, max(spread)]

  # 3. Payouts are non-negative, so the loss ratio cannot fall in k.
  steps <- raw[order(k), .(worst = min(diff(loss_ratio))),
               by = .(model, strike, h, lambda, member_id)]
  t3 <- steps[, min(worst)]

  # 4. Perfect foresight exercises in descending order of the realized fare.
  perfect <- merge(
    exercise_order(signals, 1),
    signals[, .(h, member_id, ride, fare)],
    by = c("h", "member_id", "ride")
  )
  t4 <- perfect[, .(misplaced = sum(frank(-fare, ties.method = "first") != k)),
                by = .(h, member_id)][, sum(misplaced)]

  # 5. Without foresight the mean loss ratio is linear in k, with the slope the whole
  # month's mean loss ratio spread evenly over the M occasions. The realized lambda = 0
  # curve is one draw of which subset was taken, so the fitted slope sits near that
  # value rather than on it; the fit's own standard error sets the scale. Residuals of
  # a cumulative sum are autocorrelated, so treat that as a scale and not as a test.
  linear <- curves[lambda == 0, {
    fit <- summary(lm(mean_loss_ratio ~ k))
    .(r_squared = fit$r.squared,
      slope = fit$coefficients[2L, 1L],
      slope_se = fit$coefficients[2L, 2L])
  }, by = .(model, strike, h)]
  expected_slope <- raw[lambda == 0 & k == M,
                        .(expected = mean(loss_ratio) / M), by = .(model, strike, h)]
  linear <- merge(linear, expected_slope, by = c("model", "strike", "h"))
  t5 <- linear[, min(r_squared)]
  t5b <- linear[, max(abs(slope - expected) / slope_se)]

  # 6. More foresight cannot be cheaper for the provider, paired across members. The
  # endpoints are excluded: at k = 0 and k = M every lambda agrees by construction, so
  # including them would let the test pass on two cases it cannot fail.
  paired <- rbindlist(lapply(seq_len(length(lambdas) - 1L), function(j) {
    lo <- raw[lambda == lambdas[j], .(model, strike, h, member_id, k, lr = loss_ratio)]
    hi <- raw[lambda == lambdas[j + 1L], .(model, strike, h, member_id, k, lr = loss_ratio)]
    d <- merge(lo, hi, by = c("model", "strike", "h", "member_id", "k"),
               suffixes = c("_lo", "_hi"))
    d[, .(gap = mean(lr_hi - lr_lo), se = sd(lr_hi - lr_lo) / sqrt(.N)),
      by = .(model, strike, h, k)]
  }))
  t6 <- paired[k > 0 & k < M, min(gap + 2 * se)]

  # 7. The lambda = 0 curve reproduces the Sec. 7.8 breakage figure. That figure takes
  # the first k rides in sampling order and this takes a noise-ordered subset of the
  # same k, so the two agree only up to the Monte-Carlo error of which subset was
  # drawn; the tolerance is two standard errors of the reference profit, which is
  # generous, since the premium heterogeneity it is built from cancels in the pairing.
  n_members <- raw[, uniqueN(member_id)]
  reference <- copy(reference_usage)
  setnames(reference, c("k_factor", "history_size"), c("strike", "h"), skip_absent = TRUE)
  reference[, k := pmin(M, pmax(0L, as.integer(floor(M * usage_rate + 1e-9))))]
  reference <- reference[, .(model, strike, h, k,
                             reference_profit = mean_profit,
                             tolerance = 2 * sd_profit / sqrt(n_members))]
  ours <- curves[lambda == 0, .(model, strike, h, k, mean_profit)]
  compared <- merge(reference, ours, by = c("model", "strike", "h", "k"))
  compared[, gap := abs(mean_profit - reference_profit)]
  t7 <- compared[, max(gap - tolerance)]

  rbind(
    result(1L, "Loss ratio is zero at k = 0", t1, 0, t1 == 0),
    result(2L, "Full-month loss ratio is identical across lambda", t2, 1e-9, t2 <= 1e-9),
    result(3L, "Loss ratio is non-decreasing in k", t3, -1e-12, t3 >= -1e-12),
    result(4L, "Perfect foresight ranks occasions by realized fare", t4, 0, t4 == 0),
    result(5L, "Mean loss ratio is linear in k at lambda = 0 (min R^2)", t5, 0.999, t5 > 0.999),
    result(5L, "Fitted slope matches M E[L] / Pi (max error, in fitted SEs)",
           t5b, 3, t5b < 3),
    result(6L, "Mean loss ratio is non-decreasing in lambda (worst gap + 2 SE)",
           t6, 0, t6 >= 0),
    result(7L, "lambda = 0 reproduces the Sec. 7.8 breakage curve (worst excess)",
           t7, 0, t7 <= 0)
  )
}

#' Run the selective-exercise study on top of the cached Sec. 7.8 results.
run_selective_exercise <- function(results_rds = RESULTS_RDS,
                                   cohort_rds = COHORT_RDS,
                                   settings = SELECTIVE_DEFAULTS,
                                   verbose = TRUE) {
  say <- function(...) if (verbose) cat(..., "\n")

  inputs <- load_selective_inputs(results_rds, cohort_rds, settings$history_sizes)
  M <- inputs$rides_per_member
  say("Members:", inputs$members[, uniqueN(member_id)],
      "| configs:", nrow(unique(inputs$members[, .(model, strike, h)])),
      "| rides per member:", M)
  if (length(settings$history_sizes) > 1L) {
    warning("Sweeping h without holding the OD-pair set fixed: the h axis is not a ",
            "like-for-like comparison. See the note in SELECTIVE_DEFAULTS.")
  }

  say("Drawing rider signals ...")
  signals <- member_signals(inputs$rides, settings$seed)
  ledger <- payout_ledger(inputs$members, inputs$rides)

  say("Building selection curves over", length(settings$lambdas), "foresight levels ...")
  raw <- selective_curves(ledger, signals, settings$lambdas, M)
  curves <- aggregate_curves(raw)

  say("Acceptance tests ...")
  tests <- run_acceptance_tests(raw, signals, curves, inputs$reference_usage, M)
  if (verbose) print(tests[, .(test, description, statistic = signif(statistic, 4), pass)])
  if (!all(tests$pass)) {
    stop("Acceptance tests failed: ",
         paste(tests[pass == FALSE, description], collapse = "; "))
  }

  say("Bootstrapping thresholds over", settings$n_boot, "member resamples ...")
  boot <- bootstrap_thresholds(
    raw, settings$lambdas, settings$u0_grid, settings$n_boot,
    settings$ci_level, settings$seed, M
  )

  break_even <- merge(
    break_even_utilization(curves), boot$u_star,
    by = c("model", "strike", "h", "lambda")
  )
  lambda_star <- merge(
    foresight_threshold(curves, settings$u0_grid), boot$lambda_star,
    by = c("model", "strike", "h", "u0")
  )

  list(
    settings = settings,
    commuter_settings = inputs$commuter_settings,
    raw = raw,
    curves = curves,
    reference_usage = inputs$reference_usage,
    break_even = break_even[order(model, strike, h, lambda)],
    lambda_star = lambda_star[order(model, strike, h, u0)],
    tests = tests
  )
}

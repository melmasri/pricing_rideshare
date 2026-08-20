# Commuter membership (product P3 with fixed origin-destination and departure hour).
#
# A counterfactual commuter is an origin-destination pair whose trips are split into
#   history : trips the platform has already observed, which define the route
#             distribution pi(path) the membership is priced against
#   future  : held-out trips, randomized into the rides the member actually takes
# so the premium is never computed from the routes it is later evaluated against.
#
# Both sides are passed through similar_route(): the sampler reproduces travel time
# well but returns routes that are systematically shorter in metres, so pricing on
# raw routes while realizing on sampled ones would credit the provider with a
# distance discount that is an artefact of the sampler. Averaging the premium over
# sampled routes is also what eq. (24) prescribes, since pi(path) *is* the sampler.
#
# The membership premium is rides_per_member times the average per-ride premium over
# the sampled history routes, all evaluated at the member's fixed departure time
# (pi(t_0) degenerate) with a zero risk-free rate.

suppressPackageStartupMessages({
  library(data.table)
  library(traveltimeCLT)
})

COMMUTER_DEFAULTS <- list(
  n_riders = 100L,
  rides_per_member = 30L,
  history_sizes = c(2L, 4L, 6L, 8L),
  future_size = 1L,
  quotes_per_history_trip = 5L,
  k_factors = c(1.0, 0.9),
  models = c("trip-specific", "population"),
  usage_rates = seq(0, 1, by = 0.05),
  sigma_n = 2,
  significance = 0.05,
  rho = 0.31,
  risk_free = 0,
  zeta = 0,
  # traveltimeCLT() estimates sigma_profile from a random subsample of this many trips.
  # Edge durations are heavy tailed -- the longest single edge in the corpus runs for
  # 79 minutes and the top 0.1% of edges carry ~45% of the total variance -- so at the
  # package default of 500 the estimate swings over roughly 50-375 across seeds on
  # identical data. NULL uses every training trip, which makes the fit deterministic.
  population_nsamples = NULL,
  # Drop edges longer than this quantile of the duration distribution before fitting.
  # NULL keeps the corpus as is; see the note on sigma_profile above.
  edge_duration_cap = NULL,
  # sigma_profile for the population model: "calibrate" replaces the fitted value with
  # one matched to realized travel times, a number overrides it outright, NULL keeps the
  # package estimate. The profile estimator is sqrt(var(per-trip mean edge duration) /
  # E[1/N]); it charges all between-trip variation in mean edge duration to within-trip
  # edge noise and then multiplies by 1/E[1/N] ~ 49, so genuine heterogeneity between a
  # highway trip with 40 km edges and a downtown trip with 100 m edges is amplified into
  # a spurious per-edge variance. On this corpus the fitted value is about 2.6 times too
  # dispersed against realized travel times.
  population_sigma = "calibrate",
  seed = 1234L
)

# Cohort construction is model free, so a cache is keyed only on what feeds the sampler.
COHORT_CACHE_KEYS <- c(
  "n_riders", "rides_per_member", "history_sizes", "future_size",
  "quotes_per_history_trip", "sigma_n", "significance", "rho", "seed"
)

# trips.csv ships with the columns (row, trip, time, timeBins, tt, logspeed, length,
# linkId, src, osm, dst); traveltimeCLT expects its own names downstream.
RAW_TO_MODEL_NAMES <- c(
  trip = "tripID",
  time = "entry_time",
  tt = "duration_secs",
  length = "distance_meters",
  linkId = "linkID"
)

load_commuter_trips <- function(path = "../data/trips.csv") {
  raw <- fread(path)
  missing <- setdiff(names(RAW_TO_MODEL_NAMES), names(raw))
  if (length(missing) > 0) {
    stop("trips file is missing column(s): ", paste(missing, collapse = ", "))
  }
  raw[, time := as.POSIXct(time, tz = "UTC")]
  setorder(raw, trip, time)

  model <- copy(raw)
  setnames(model, names(RAW_TO_MODEL_NAMES), unname(RAW_TO_MODEL_NAMES))
  model[, speed := exp(logspeed)]
  model[, timeBin := time_bins_readable(entry_time)]

  list(raw = raw, model = model)
}

# One row per trip: terminal links, departure, and length.
summarise_trips <- function(raw) {
  raw[, .(
    start = linkId[1],
    end = linkId[.N],
    depart = time[1],
    distance = sum(length)
  ), by = trip]
}

#' Assign counterfactual commuters to origin-destination pairs.
#'
#' Each commuter needs `history_size + future_size` trips on one pair. Held-out
#' trips are allocated first and without replacement, so every one of them is
#' realized by exactly one member and the whole set can be dropped from training.
#' History trips are then drawn from what is left on the pair, and may be shared
#' across commuters: the data has at most 13 trips per pair, too few to give every
#' commuter a private history at the larger history sizes.
assign_commuters <- function(trip_index, history_size, future_size, n_riders) {
  needed <- history_size + future_size
  pair_counts <- trip_index[, .(count = .N), by = .(start, end)]
  eligible <- pair_counts[count >= needed]
  if (nrow(eligible) == 0) {
    stop("No origin-destination pair has ", needed, " trips.")
  }

  pool_of <- function(pair) trip_index[start == pair$start & end == pair$end]

  held_out <- integer(0)
  pairs <- vector("list", n_riders)
  for (i in seq_len(n_riders)) {
    picked <- NULL
    for (attempt in seq_len(200L)) {
      pair <- eligible[sample.int(.N, 1L)]
      if (nrow(pool_of(pair)[!trip %in% held_out]) >= needed) {
        picked <- pair
        break
      }
    }
    if (is.null(picked)) {
      stop(
        "Ran out of unused trips at history size ", history_size,
        " after ", i - 1L, " commuters; lower n_riders or future_size."
      )
    }
    pool <- pool_of(picked)
    future <- pool[!trip %in% held_out][sample.int(.N, future_size)]
    held_out <- c(held_out, future$trip)
    pairs[[i]] <- list(pool = pool, future = future)
  }

  assignments <- rbindlist(lapply(seq_along(pairs), function(i) {
    pool <- pairs[[i]]$pool
    future <- pairs[[i]]$future
    history <- pool[!trip %in% held_out][sample.int(.N, history_size)]
    data.table(
      rider = i,
      role = rep(c("future", "history"), c(nrow(future), nrow(history))),
      trip = c(future$trip, history$trip),
      # The commuter's habitual departure, shared by every ride in the month.
      depart = pool$depart[sample.int(nrow(pool), 1L)]
    )
  }))

  list(assignments = assignments, n_eligible_pairs = nrow(eligible))
}

#' Restate each assigned trip as a synthetic trip departing at the member's hour.
#'
#' A trip can serve several commuters, so every (rider, trip) pair gets its own id
#' and its own time shift. The synthetic trips are appended to the corpus because
#' similar_route() derives its edge statistics from the table it is handed.
build_route_requests <- function(raw, assignments) {
  requests <- copy(assignments)
  requests[, request := .I]
  requests[, syn_trip := max(raw$trip) + request]

  rows <- raw[requests, on = .(trip), allow.cartesian = TRUE, nomatch = 0]
  setorder(rows, request, time)
  rows[, time := time + as.numeric(difftime(depart[1], time[1], units = "secs")), by = request]
  rows[, trip := syn_trip]

  list(
    work = rbind(raw, rows[, names(raw), with = FALSE]),
    requests = requests[]
  )
}

#' Randomize every requested trip into `r` alternative routes.
sample_commuter_routes <- function(work, requests, r, sigma_n, significance) {
  simulated <- similar_route(
    requests$syn_trip, work,
    r = r,
    sigma_n = sigma_n,
    significance = significance,
    Ftest_sd = FALSE
  )
  setnames(simulated, "trip", "syn_trip")
  # similar_route() returns edges in route order; keep that order explicit.
  simulated[, edge_seq := seq_len(.N), by = .(syn_trip, newtrip)]
  simulated[, route := rleid(newtrip), by = syn_trip]
  merge(
    simulated,
    requests[, .(syn_trip, request, rider, role, depart)],
    by = "syn_trip"
  )
}

#' Realize the rides a member takes: sampled routes with sampled travel times.
#'
#' Prices here do not involve the fitted travel-time model, so they are shared
#' across models and strikes.
realize_rides <- function(routes, rides_per_member, rho) {
  rides <- routes[, .(
    duration = sum(exp(mean + qnorm(dependent_uniform(.N, rho)) * sd)),
    distance = sum(length)
  ), by = .(rider, request, route)]
  rides[, real_price := price(duration, distance)[, 1]]

  setorder(rides, rider, request, route)
  short <- rides[, .N, by = rider][N < rides_per_member]
  if (nrow(short) > 0) {
    warning(
      nrow(short), " commuter(s) produced fewer than ", rides_per_member,
      " routes and were dropped"
    )
    rides <- rides[!rider %in% short$rider]
  }

  rides <- rides[, .SD[seq_len(rides_per_member)], by = rider]
  rides[, ride := seq_len(.N), by = rider]
  rides[]
}

#' Premium and strike on each sampled history route, at the member's departure time.
quote_routes <- function(fit, routes, k_factor, risk_free, zeta) {
  quotes <- routes[order(rider, request, route, edge_seq), .(
    tripID = .GRP,
    entry_time = depart,
    linkID = linkId,
    distance_meters = length
  ), by = .(rider, request, route)]

  predicted <- as.data.table(predict(fit, quotes))
  totals <- quotes[, .(
    rider = rider[1],
    entry_time = entry_time[1],
    distance = sum(distance_meters)
  ), by = tripID]
  totals <- merge(totals, predicted[, .(tripID, ETA, variance)], by = "tripID")

  totals[, `:=`(
    expected_price = request_K(.SD, distance, discount_factor = 1),
    strike = request_K(.SD, distance, discount_factor = k_factor),
    premium = request_R(
      .SD, entry_time, entry_time, distance,
      K = k_factor, risk_free = risk_free, zeta = zeta
    )
  )]
  totals[]
}

#' Collapse quotes and realized rides into one membership per commuter.
#'
#' `premium` is eq. (24) with r = 0: M times the mean per-ride premium over the
#' sampled history routes. The member is promised a single cap, so the strike is
#' averaged the same way. Provider profit is the premium less realized overruns.
membership_ledger <- function(quotes, rides, rides_per_member, usage_rates) {
  member <- quotes[, .(
    premium = rides_per_member * mean(premium),
    strike = mean(strike),
    expected_price = mean(expected_price)
  ), by = rider]

  ledger <- merge(rides, member, by = "rider")
  ledger[, overrun := pmax(real_price - strike, 0)]

  by_rider <- ledger[, .(
    premium = premium[1],
    strike = strike[1],
    expected_price = expected_price[1],
    spend = sum(real_price),
    overrun = sum(overrun)
  ), by = rider]
  by_rider[, profit := premium - overrun]
  by_rider[, pct_return := 100 * profit / spend]
  by_rider[, premium_over_price := 100 * premium / spend]

  # Breakage: the member pays the premium up front but may not take every ride.
  usage <- rbindlist(lapply(usage_rates, function(u) {
    taken <- min(
      rides_per_member,
      max(0L, as.integer(floor(rides_per_member * u + 1e-9)))
    )
    used <- if (taken == 0L) {
      ledger[, .(premium = premium[1], spend = 0, overrun = 0), by = rider]
    } else {
      ledger[ride <= taken, .(
        premium = premium[1],
        spend = sum(real_price),
        overrun = sum(overrun)
      ), by = rider]
    }
    used[, profit := premium - overrun]
    used[, .(
      usage_rate = u,
      mean_profit = mean(profit),
      sd_profit = sd(profit),
      mean_pct_return = if (taken == 0L) NA_real_ else 100 * mean(profit / spend),
      max_loss = min(profit),
      win_rate = mean(profit > 0)
    )]
  }))

  list(by_rider = by_rider[], usage = usage[])
}

summarise_members <- function(by_rider) {
  by_rider[, .(
    mean_pct_return = mean(pct_return),
    se_pct_return = sd(pct_return) / sqrt(.N),
    mean_profit = mean(profit),
    max_loss = min(profit),
    premium_over_price = mean(premium_over_price),
    win_rate = mean(profit > 0)
  )]
}

#' Build the commuter cohorts: assignment, route sampling, and realized rides.
#'
#' This is the expensive half of the study and none of it depends on the travel-time
#' model, the strike, or the fitting options, so it is separated out and cached.
#' Route sampling depends only on the history size, so each cohort is simulated once
#' and later reused across models and strikes.
build_commuter_cohorts <- function(trips_path = "../data/trips.csv",
                                   settings = COMMUTER_DEFAULTS,
                                   verbose = TRUE) {
  say <- function(...) if (verbose) cat(..., "\n")

  set.seed(settings$seed)
  data <- load_commuter_trips(trips_path)
  trip_index <- summarise_trips(data$raw)
  say("Loaded", nrow(trip_index), "trips over",
      uniqueN(trip_index[, .(start, end)]), "OD pairs")

  routes_per_ride_trip <- as.integer(ceiling(settings$rides_per_member / settings$future_size))
  routes_per_request <- max(routes_per_ride_trip, settings$quotes_per_history_trip)

  cohorts <- lapply(settings$history_sizes, function(h) {
    say("Cohort with history size", h, "...")
    commuters <- assign_commuters(trip_index, h, settings$future_size, settings$n_riders)
    say("  eligible OD pairs:", commuters$n_eligible_pairs)
    requested <- build_route_requests(data$raw, commuters$assignments)
    say("  randomizing", nrow(requested$requests), "trips into",
        routes_per_request, "routes each ...")
    routes <- sample_commuter_routes(
      requested$work, requested$requests, routes_per_request,
      settings$sigma_n, settings$significance
    )
    list(
      history_size = h,
      n_eligible_pairs = commuters$n_eligible_pairs,
      held_out = commuters$assignments[role == "future", unique(trip)],
      quote_routes = routes[role == "history" & route <= settings$quotes_per_history_trip],
      rides = realize_rides(
        routes[role == "future" & route <= routes_per_ride_trip],
        settings$rides_per_member, settings$rho
      )
    )
  })
  names(cohorts) <- paste0("h", settings$history_sizes)

  list(cohorts = cohorts, keys = settings[COHORT_CACHE_KEYS])
}

#' Read the cohorts from `path`, rebuilding them if the cache is absent or stale.
cached_commuter_cohorts <- function(path = "commuter_cohorts.rds",
                                    trips_path = "../data/trips.csv",
                                    settings = COMMUTER_DEFAULTS,
                                    refresh = FALSE,
                                    verbose = TRUE) {
  say <- function(...) if (verbose) cat(..., "\n")
  if (!refresh && file.exists(path)) {
    cached <- readRDS(path)
    if (identical(cached$keys, settings[COHORT_CACHE_KEYS])) {
      say("Reusing cohorts from", path)
      return(cached$cohorts)
    }
    say("Cohort cache at", path, "was built with different settings; rebuilding")
  }
  built <- build_commuter_cohorts(trips_path, settings, verbose)
  saveRDS(built, path)
  say("Wrote cohorts to", path)
  built$cohorts
}

#' Fit one travel-time model on the training corpus.
#'
#' traveltimeCLT() adds columns to its input by reference under the trip-specific
#' model, so every fit gets its own copy.
fit_travel_time_model <- function(model, training, settings) {
  data <- copy(training)
  if (!is.null(settings$edge_duration_cap)) {
    keep <- quantile(data$duration_secs, settings$edge_duration_cap, na.rm = TRUE)
    data <- data[duration_secs <= keep]
  }
  nsamples <- if (model == "population") {
    settings$population_nsamples %||% uniqueN(data$tripID)
  } else {
    500L
  }
  fit <- suppressWarnings(traveltimeCLT(data, model, nsamples = nsamples))
  if (model == "population" && !is.null(settings$population_sigma)) {
    fit$sigma.prof <- if (identical(settings$population_sigma, "calibrate")) {
      calibrate_population_sigma(fit, data)
    } else {
      settings$population_sigma
    }
  }
  fit
}

#' Rescale sigma_profile so the predicted spread matches realized travel times.
#'
#' The predictor sets Var[T] = sigma^2 N (1 + 1/m), so the sigma under which the
#' standardized residuals have unit variance is the standard deviation of the residual
#' per root-edge. Calibrating on the fitting corpus is safe here because the population
#' model carries two parameters and cannot meaningfully overfit.
calibrate_population_sigma <- function(fit, data) {
  realized <- data[, .(realized = sum(duration_secs), n_edges = .N), by = tripID]
  predicted <- as.data.table(predict(fit, data))
  d <- na.omit(merge(realized, predicted[, .(tripID, ETA)], by = "tripID"))
  sd((d$realized - d$ETA) / sqrt(d$n_edges * (1 + 1 / fit$nsamples)))
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Run the commuter membership study over models, strikes, and history sizes.
run_commuter_membership <- function(trips_path = "../data/trips.csv",
                                    settings = COMMUTER_DEFAULTS,
                                    cohort_cache = "commuter_cohorts.rds",
                                    refresh_cohorts = FALSE,
                                    verbose = TRUE) {
  say <- function(...) if (verbose) cat(..., "\n")

  cohorts <- cached_commuter_cohorts(
    cohort_cache, trips_path, settings, refresh_cohorts, verbose
  )

  # Both models estimate their parameters from a random subsample of trips, so the seed
  # is reset here: pricing must not depend on whether the cohorts were just sampled or
  # read back from the cache.
  set.seed(settings$seed)

  data <- load_commuter_trips(trips_path)
  # Held-out trips must not inform the travel-time model that prices them.
  held_out <- unique(unlist(lapply(cohorts, `[[`, "held_out")))
  training <- data$model[!tripID %in% held_out]
  say("Fitting on", uniqueN(training$tripID), "training trips,",
      length(held_out), "held out")

  fits <- lapply(settings$models, function(m) {
    say("Fitting", m, "model ...")
    fit <- fit_travel_time_model(m, training, settings)
    if (m == "population") say("  sigma_profile =", round(fit$sigma.prof, 2))
    fit
  })
  names(fits) <- settings$models

  grid <- CJ(
    model = settings$models,
    k_factor = settings$k_factors,
    history_size = settings$history_sizes,
    sorted = FALSE
  )
  runs <- lapply(seq_len(nrow(grid)), function(i) {
    row <- grid[i]
    cohort <- cohorts[[paste0("h", row$history_size)]]
    say("Pricing:", row$model, "| K =", row$k_factor, "P | history", row$history_size)
    quotes <- quote_routes(
      fits[[row$model]], cohort$quote_routes,
      row$k_factor, settings$risk_free, settings$zeta
    )
    ledger <- membership_ledger(
      quotes, cohort$rides, settings$rides_per_member, settings$usage_rates
    )
    c(as.list(row), list(
      n_eligible_pairs = cohort$n_eligible_pairs,
      by_rider = ledger$by_rider,
      usage = ledger$usage,
      summary = summarise_members(ledger$by_rider)
    ))
  })

  keys <- function(r) {
    data.table(
      model = r$model, k_factor = r$k_factor,
      history_size = r$history_size
    )
  }
  summary <- rbindlist(lapply(runs, function(r) {
    cbind(keys(r), n_eligible_pairs = r$n_eligible_pairs, r$summary)
  }))
  usage <- rbindlist(lapply(runs, function(r) cbind(keys(r), r$usage)))

  list(settings = settings, runs = runs, summary = summary[], usage = usage[])
}

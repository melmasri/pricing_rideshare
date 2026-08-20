# Commuter membership (paper Sec. 7.8)

A 30-ride monthly membership on a fixed origin–destination pair at a fixed departure hour,
priced from eq. (24) under both travel-time models and both strikes (`K = P` and `K = 0.9P`).

| File | Role |
|------|------|
| `commuter_membership.R` | Simulation engine: commuter construction, route sampling, pricing, ledger |
| `run_commuter_membership.R` | Batch driver; writes `../plot/commuter_membership_results.rds` and `results/*.csv` |
| `commuter_membership.Rmd` | Report: summary table, premium and profit figures, breakage heatmaps |
| `commuter_heatmap.R` | Figure and table helpers used by the report |

## How the commuter rides are sampled

The data contains no commuters. It is 23,054 individual trips spread over 18,051
origin–destination pairs, and no pair carries more than 13 trips. A commuter who takes the same
trip 30 times a month therefore has to be constructed, and the construction has to satisfy two
requirements: the premium must be computed without seeing the rides it will later be judged
against, and both sides must live in the same space of routes so that no artefact of the
sampler is booked as profit. The pipeline below runs once per history size `h` and produces one
*cohort*, which is what gets cached.

### 1. Assign commuters to origin–destination pairs

A pair is eligible if it carries at least `h + future_size` trips, since each commuter needs `h`
trips of history plus one to hold out. Eligibility is what makes `h` the binding constraint on
the study — 1,001 pairs qualify at `h = 2`, then 361, 151, and only 63 at `h = 8` — and it is why
the grid stops at 8 and why `future_size` is 1.

For each of the `n_riders = 100` commuters in turn, `assign_commuters()` draws an eligible pair
at random, retrying up to 200 times until it finds one that still has enough unused trips, and
allocates the trips in two passes:

- **Held-out trips first**, `future_size = 1` per commuter, drawn *without replacement across
  the whole cohort*. Each held-out trip therefore belongs to exactly one commuter, which is what
  allows the entire held-out set to be removed from the travel-time model fit in one step.
- **History trips second**, `h` of them drawn from what remains on that pair. These *may* be
  shared between commuters. With at most 13 trips on a pair there is no way to give 100
  commuters a private history at `h = 8`, and sharing history is harmless because history is
  only ever used to price, never to settle.

### 2. Fix each commuter's habitual departure

One departure time is drawn from the pair's observed departures and becomes that commuter's
habitual hour. `build_route_requests()` then rewrites every trip assigned to that commuter —
history and held out alike — so its whole edge sequence is shifted to start at that time, with
the relative spacing between edges preserved. This makes `π(t₀)` degenerate, so the month is
priced and settled in one traffic regime rather than averaged across the day.

Because a single trip can serve several commuters at different hours, each `(rider, trip)` pair
becomes its own synthetic trip with a fresh id. The synthetic trips are appended to the corpus
before sampling, since `similar_route()` derives its edge statistics from the table it is given.

### 3. Randomize each trip into alternative routes

`sample_commuter_routes()` hands the synthetic trips to `traveltimeCLT::similar_route()`, which
replaces each edge of a route with a statistically similar one — matched on log travel time
within `sigma_n = 2` standard deviations at `significance = 0.05`, with the F-test on the
standard deviation disabled — keeping the edge count intact.

Every request is randomized into the same number of routes, `max(rides_per_member / future_size,
quotes_per_history_trip)`, which is 30 under the defaults: the held-out side needs 30 routes to
fill a month from a single trip, and the history side needs only 10, so 30 covers both. This step
dominates the runtime — 72,000 routes across the four cohorts, about 2.5 hours.

### 4. Realize the rides

For the held-out side, `realize_rides()` draws a travel time on each sampled route by summing
lognormal edge times, `exp(mean + z·sd)`, where the `z` are correlated across consecutive edges
through `dependent_uniform()` at `rho = 0.31`. Route length and sampled duration then give the
realized fare through `price()`. The first 30 routes become the member's 30 rides, in order, so
the same ledger can be truncated later to study breakage. Nothing in this step touches a fitted
model, which is why realized fares are shared across all models and strikes.

### 5. Which routes are priced and which are settled

| Side | Source | Routes used | Per cohort |
|------|--------|-------------|------------|
| Pricing | the `h` history trips | first `quotes_per_history_trip = 10` of the 30 | 10 × `h` × 100 quotes |
| Settlement | the 1 held-out trip | all 30 | 30 × 100 = 3,000 rides |

The premium is `rides_per_member` times the mean per-ride premium over the sampled history
routes, which is eq. (24) at `r = 0`; the member is promised one cap, so the strike is averaged
the same way.

### Two properties this protects

**The premium never sees the rides it is judged against.** Held-out trips are unique across
commuters, and their union across all four cohorts — 385 trips — is dropped before either
travel-time model is fitted.

**Both sides are sampled, not just one.** `similar_route()` matches edges on log travel time
rather than on length, and returns routes roughly 21% shorter in metres than the originals.
Pricing observed routes while settling sampled ones would hand the provider that distance
difference as pure profit, so history routes are passed through the sampler too. Averaging the
premium over sampled routes is also what eq. (24) asks for, since the sampler *is* `π(path)`.

## The population `sigma_profile`

`traveltimeCLT`'s profile estimator is `sqrt(var(per-trip mean edge duration) / E[1/N])`. It
charges every between-trip difference in mean edge duration to within-trip edge noise and then
rescales by `1/E[1/N]` ≈ 49. Edge lengths here run from a median of 109 m to a maximum of
46.6 km, so trips are nowhere near exchangeable and the estimate is set by however many
multi-kilometre links land in the subsample: it ranges from 48.7 to 375.4 over twenty draws of
the default 500 trips, and the deterministic all-trips fit of 142.9 leaves predicted travel
times about 2.6× too dispersed. `population_sigma = "calibrate"` therefore replaces it with the
value matching predicted and realized spread (≈ 61 s), which reproduces the premiums in the
paper. The trip-specific model needs no such treatment; its premium moves under 1% across
subsamples.

## Running

```bash
cd simulations/commuter_membership
Rscript run_commuter_membership.R                          # ~2.5 h on a cold cache
Rscript run_commuter_membership.R --refresh                # discard the cache and resample
Rscript -e 'rmarkdown::render("commuter_membership.Rmd")'  # fast, reads the saved RDS
```

Cohort construction — assignment, route sampling, realized rides — depends on neither the
travel-time model nor the strike, so it is cached in `commuter_cohorts.rds`. Later runs reprice
off the cache in seconds. The cache key is the set of settings that feed the sampler
(`COHORT_CACHE_KEYS`: rider count, rides per member, history sizes, `future_size`, quotes per
history trip, `sigma_n`, `significance`, `rho`, and the seed), so changing a strike or a model
reuses the cache while changing anything upstream rebuilds it.

`seed = 1234` fixes commuter assignment, route randomization, and the travel-time draws. It is
also reset before the models are fitted, because both of them estimate their parameters from a
random subsample of trips: without that reset, pricing would depend on whether the cohorts had
just been sampled or read back from disk.

Requires `../data/trips.csv` and the `traveltimeCLT` package. Adjust `COMMUTER_DEFAULTS` in
`commuter_membership.R` (or override in the driver) to change the grid; `n_riders` is bounded by
how many trips the eligible pairs can supply, which is tight at the larger history sizes.

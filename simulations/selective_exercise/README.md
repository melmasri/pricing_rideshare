# Selective exercise (paper Sec. 7.9)

The commuter membership of Sec. 7.8 assumes rides that are not taken are dropped **at
random**, which makes unused rides pure profit. This section asks what happens when they
are not dropped at random: the holder observes something about the morning — weather, a
traffic app, the day of the week — and exercises the membership when the ride looks
expensive, using an outside option when it looks cheap.

That is the one abuse channel left to a commuter whose route and departure hour are
already fixed by the contract, which is what separates this from the membership abuse of
Sec. 7.7. Sec. 7.7 asks which *trips* a flat-membership rider substitutes into; this asks
which *days* a commuter exercises.

The rider's private benefit on occasion `i` is the amount the cap saves them,
`(P_i − K)⁺`, which is exactly the provider's payout, so rider objective and provider
loss are the same object and no separate utility model is needed.

| File | Role |
|------|------|
| `selective_exercise.R` | Engine: selection rule, curves, thresholds, bootstrap, acceptance tests |
| `run_selective_exercise.R` | Driver; writes `results/*.csv` and `../plot/selective_exercise_results.rds` |
| `selective_exercise_figures.R` | Figure and table helpers, reusing the commuter study's theme |
| `selective_exercise.Rmd` | Report: acceptance tests, curves, break-even table, foresight table |

The specification this implements is [`../selective_exercise_spec.md`](../selective_exercise_spec.md).

## What is and is not computed here

Nothing is priced here. The study is a wrapper that reads two artefacts the commuter
study already produced and re-sums payouts under a selection rule:

| Input | Supplies |
|-------|----------|
| `../plot/commuter_membership_results.rds` | per-member premium `Π_m` and dollar cap `K_m`, per model and strike |
| `../commuter_membership/commuter_cohorts.rds` | per-member realized fares `P_{m,1..30}` |

Route sampling, travel-time sampling, model fitting, and eq. (24) are untouched. A full
run takes about three seconds, against the two and a half hours the commuter study
needs. `check_inputs()` refuses to proceed if the two artefacts come from different runs,
which would otherwise pair one member's premium with another's month in silence.

## The selection rule

Within each member's own month, and never pooled across members:

```
r_{m,i}    = rank of P_{m,i} among the month's 30 fares, ties broken at random
z_{m,i}    = Φ⁻¹((r_{m,i} − 0.5) / 30)          within-month normal scores
ε_{m,i}    ~ N(0,1)                              drawn ONCE per member
S_{m,i}(λ) = λ·z_{m,i} + sqrt(1 − λ²)·ε_{m,i}
```

The member exercises in descending order of `S`, so the taken set at `k` rides is the
first `k` of that order and the payout is `Λ_m(k,λ) = Σ L_{m,i}` over it.

Three choices are worth keeping in mind when reading the code:

- **Ranking is on the fare `P`, not on the payout `L`.** `L` has an atom at zero on the
  out-of-the-money occasions, which would produce massive ties. `P` is continuous and `L`
  is monotone in `P`, so ranking on `P` induces the correct ranking on `L`.
- **Normal scores, not raw fares.** This makes `λ` scale free and gives the signal the
  same marginal distribution at every `λ`.
- **Common random numbers throughout.** The noise is drawn once per member and reused
  across every `λ`, every `k`, and every (model, strike) config; realized fares are model
  free, so even the selection *order* is shared across configs. The `k` curve is then
  nested by construction and every `λ` comparison is paired, which is why the bootstrap
  resamples members rather than (member, `λ`) cells.

## Grid

| Dimension | Values |
|-----------|--------|
| `λ` | 0, 0.25, 0.50, 0.75, 0.90, 1.00 |
| `k` | 0, 1, …, 30, reported as `u = k/30` |
| model | population, trip-specific |
| strike | `K = P`, `K = 0.9P` |
| `h` | 8 |
| members | 100 |

**Why `h = 8` only.** Sec. 7.8's max-loss column grows with `h` because the eligible
OD-pair pool shrinks from 1,001 pairs at `h = 2` to 63 at `h = 8`, not because pricing
degrades, so the `h` axis is only a like-for-like comparison if the pair set is held
fixed across `h`. Holding it fixed means rebuilding the cohorts, which is what this
wrapper deliberately does not do. `--sweep-h` runs the other cohorts anyway and warns.

**Why 100 members and not the 500 the spec asks for.** The commuter cohort is capped by
the data, not by choice: each commuter needs one held-out trip plus `h` of history on the
same pair, and at `h = 8` the whole corpus supports at most 141 of them (395 at `h = 6`,
997 at `h = 4`). Raising the count also means rebuilding and re-pricing the cohorts. The
run therefore uses the 100 commuters already priced by Sec. 7.8, which has the side
benefit of making the `λ = 0` comparison against the published curve exact in its
members, and reports member-level bootstrap intervals so the resulting width is visible
rather than assumed away.

## Outputs

| File | Contents |
|------|----------|
| `results/selective_exercise_raw.csv` | one row per member × config × `λ` × `k`, the long format the spec asks for |
| `results/selective_exercise_curves.csv` | means and marginal SEs across members |
| `results/break_even_utilization.csv` | `u*(λ)` with bootstrap intervals |
| `results/foresight_threshold.csv` | `λ*(u₀)` with bootstrap intervals |
| `results/acceptance_tests.csv` | the eight structural checks and their statistics |
| `../plot/selective_exercise_curves.png` | profit and loss ratio against utilization, one line per `λ` |
| `../plot/selective_exercise_surface.png` | loss-ratio surface on the `(u, λ)` plane with the break-even contour |
| `../plot/selective_exercise_break_even.png` | `u*(λ)` with bootstrap bands |

Nothing is aggregated inside the simulation loop; every table and figure is produced
downstream from the raw file.

**Loss ratio, not "% of fares".** `Π_m` is fixed within a member while the fares taken
move with `k`, so a "% of fares taken" denominator would contaminate the utilization
curve with a trend of its own. The headline is `LR = payout / premium`, with dollar
profit as the secondary column.

## Acceptance tests

Eight checks run before any figure is drawn and stop the run if any fails.

| # | Check |
|---|-------|
| 1 | The loss ratio is zero at `k = 0` |
| 2 | The full-month loss ratio is identical across every `λ` |
| 3 | The loss ratio is non-decreasing in `k` |
| 4 | At `λ = 1` the exercise order is the fares in descending order |
| 5 | `mean LR(k, 0)` is linear in `k`, with the expected slope |
| 6 | `mean LR(k, λ)` is non-decreasing in `λ`, paired across members |
| 7 | The `λ = 0` curve reproduces the published Sec. 7.8 breakage curve |

Test 2 is the strongest structural check: taking all 30 rides pays the same regardless of
the order they were taken in, so if it fails the selection indexing is wrong. Test 7 is
the regression test protecting the published result — the two curves take different
subsets of the same 30 rides, so they agree up to the Monte-Carlo error of which subset
was drawn, and the tolerance is two standard errors of the reference profit.

## Running

```bash
cd simulations/commuter_membership
Rscript run_commuter_membership.R                          # only if the caches are absent

cd ../selective_exercise
Rscript run_selective_exercise.R                           # ~3 s
Rscript run_selective_exercise.R --boot=200 --sweep-h      # fewer resamples, every h
Rscript -e 'rmarkdown::render("selective_exercise.Rmd")'
```

`seed = 20260730` fixes the rider's private noise and the bootstrap resamples. It is
deliberately not the commuter study's seed: nothing priced is redrawn here.

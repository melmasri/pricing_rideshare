Selective exercise in the commuter membership
================
2026-07-30

Reports the selective-exercise section of the paper
(`sec:selective-exercise`), which follows the commuter membership of
Sec. 7.8. The commuter holds 30 capped rides on a fixed
origin–destination pair at a fixed departure hour. Sec. 7.8 drops the
rides that are not taken **at random**, which makes unused rides pure
profit. A rational holder does not drop at random: they exercise the
membership on the mornings the ride looks expensive and use an outside
option when it looks cheap.

The rider’s private benefit on occasion *i* is the amount the cap saves
them, $(P_i - K)^+$, which is exactly the provider’s payout, so rider
objective and provider loss are the same object and no separate utility
model is needed. Foresight $\lambda \in [0,1]$ is the rank correlation
between the rider’s signal and the realized fare: $\lambda = 0$
reproduces Sec. 7.8, $\lambda = 1$ is a rider who always exercises on
the worst-traffic days.

Nothing here is re-priced. Premiums, caps, and realized fares are read
from the commuter study’s cached artefacts. Run
`Rscript run_selective_exercise.R` first; it saves
`../plot/selective_exercise_results.rds`, which this report reads.

    ## List of 6
    ##  $ lambdas      : num [1:6] 0 0.25 0.5 0.75 0.9 1
    ##  $ history_sizes: int 8
    ##  $ u0_grid      : num [1:5] 0.6 0.7 0.8 0.9 1
    ##  $ n_boot       : int 1000
    ##  $ ci_level     : num 0.95
    ##  $ seed         : int 20260730

## Acceptance tests

These run inside `run_selective_exercise()` and hold up the whole run if
any fails. Test 2 is the structural one: taking all 30 rides pays the
same regardless of the order they were taken in. Test 7 is the
regression test protecting the published Sec. 7.8 breakage result.

| \# | Check | Statistic | Tolerance | Pass |
|:--:|:---|---:|---:|:--:|
| 1 | Loss ratio is zero at k = 0 | 0 | 0 | TRUE |
| 2 | Full-month loss ratio is identical across lambda | 7.105e-15 | 1e-09 | TRUE |
| 3 | Loss ratio is non-decreasing in k | 0 | -1e-12 | TRUE |
| 4 | Perfect foresight ranks occasions by realized fare | 0 | 0 | TRUE |
| 5 | Mean loss ratio is linear in k at lambda = 0 (min R^2) | 0.9996 | 0.999 | TRUE |
| 5 | Fitted slope matches M E\[L\] / Pi (max error, in fitted SEs) | 2.658 | 3 | TRUE |
| 6 | Mean loss ratio is non-decreasing in lambda (worst gap + 2 SE) | 0.0004388 | 0 | TRUE |
| 7 | lambda = 0 reproduces the Sec. 7.8 breakage curve (worst excess) | -1.206 | 0 | TRUE |

## Profit and loss ratio against utilization

The $\lambda = 0$ line is the published breakage curve. Every line above
it is what the same month costs once the rider chooses which days to
exercise. The claim to read off the lower panel: the loss ratio is
**linear** in utilization at $\lambda = 0$ and **concave** for
$\lambda > 0$, because the provider’s exposure is front-loaded onto the
occasions the rider picks. The gap between the two is the cost of
selective exercise.

Open circles on the upper panel are the published Sec. 7.8 breakage
curve. They sit on the $\lambda = 0$ line, which is acceptance test 7 by
eye.

![](selective_exercise_files/figure-gfm/curves-1.png)<!-- -->

At full utilization every curve meets, whatever the foresight: taking
all 30 rides costs the provider the same total regardless of the order
they are taken in. Selection only moves *when* the exposure is paid,
which is why it matters exactly to the extent that the member does not
use the whole month.

![](selective_exercise_files/figure-gfm/surface-1.png)<!-- -->

## Break-even utilization

$u^*(\lambda)$ is the first utilization at which the loss ratio reaches
one, linearly interpolated between adjacent ride counts. `NA (>1)` means
the product never breaks even inside the month. Intervals are
member-level nonparametric bootstrap percentiles, resampling members
rather than (member, $\lambda$) cells so the pairing induced by the
common random numbers survives.

Two loss ratios are carried through, and they answer different
questions. The **member-average** ratio averages each member’s own
payout-to-premium ratio, so every member counts once and it describes
the typical membership. The **pooled** ratio divides total payout by
total premium, so members count in proportion to what they pay; it is
the provider’s book, and it crosses one exactly where mean profit
crosses zero, which is what Sec. 7.8 reports. The premium distribution
is right skewed enough for the two to part company — at full use under
route conditioning at $K = P$ the member average is 1.30 while the book
is 1.08 and the median member is at 0.70 — so the paper headlines the
pooled version.

| Model | Strike | $\lambda$ | $u^*$ pooled | 95% CI | $u^*$ member avg. | 95% CI |
|:---|:--:|:--:|---:|---:|---:|---:|
| Population | $0.9\,P$ | 0.00 | NA (\>1) | – | NA (\>1) | – |
| Population | $0.9\,P$ | 0.25 | NA (\>1) | – | NA (\>1) | – |
| Population | $0.9\,P$ | 0.50 | NA (\>1) | – | NA (\>1) | – |
| Population | $0.9\,P$ | 0.75 | NA (\>1) | – | NA (\>1) | – |
| Population | $0.9\,P$ | 0.90 | NA (\>1) | – | NA (\>1) | – |
| Population | $0.9\,P$ | 1.00 | NA (\>1) | – | NA (\>1) | – |
| Population | $P$ | 0.00 | NA (\>1) | – | NA (\>1) | – |
| Population | $P$ | 0.25 | NA (\>1) | – | NA (\>1) | – |
| Population | $P$ | 0.50 | NA (\>1) | – | NA (\>1) | – |
| Population | $P$ | 0.75 | NA (\>1) | – | NA (\>1) | – |
| Population | $P$ | 0.90 | NA (\>1) | – | NA (\>1) | – |
| Population | $P$ | 1.00 | NA (\>1) | – | NA (\>1) | – |
| Trip-specific | $0.9\,P$ | 0.00 | NA (\>1) | – | 0.91 | \[0.79, 0.99\] |
| Trip-specific | $0.9\,P$ | 0.25 | NA (\>1) | – | 0.89 | \[0.75, 0.99\] |
| Trip-specific | $0.9\,P$ | 0.50 | NA (\>1) | – | 0.86 | \[0.72, 0.99\] |
| Trip-specific | $0.9\,P$ | 0.75 | NA (\>1) | – | 0.83 | \[0.67, 0.98\] |
| Trip-specific | $0.9\,P$ | 0.90 | NA (\>1) | – | 0.80 | \[0.64, 0.97\] |
| Trip-specific | $0.9\,P$ | 1.00 | NA (\>1) | – | 0.78 | \[0.62, 0.97\] |
| Trip-specific | $P$ | 0.00 | 0.91 | \[0.63, 0.99\] | 0.76 | \[0.52, 0.97\] |
| Trip-specific | $P$ | 0.25 | 0.88 | \[0.57, 0.99\] | 0.69 | \[0.47, 0.95\] |
| Trip-specific | $P$ | 0.50 | 0.83 | \[0.50, 0.98\] | 0.61 | \[0.41, 0.92\] |
| Trip-specific | $P$ | 0.75 | 0.79 | \[0.43, 0.98\] | 0.53 | \[0.34, 0.89\] |
| Trip-specific | $P$ | 0.90 | 0.75 | \[0.39, 0.97\] | 0.48 | \[0.31, 0.87\] |
| Trip-specific | $P$ | 1.00 | 0.72 | \[0.36, 0.97\] | 0.44 | \[0.28, 0.86\] |

An interval is withheld where most resamples never break even inside the
month, since it would describe the minority that did rather than the
quantity asked for. With 100 commuters the intervals are wide: the data
caps the h = 8 cohort at 141 possible commuters, so this axis cannot be
sharpened by resampling more of it.

![](selective_exercise_files/figure-gfm/break-even-figure-1.png)<!-- -->

## How good a forecaster the rider must be

Rather than estimating the rider’s true foresight, which no data here
identifies, the question is inverted: at an assumed utilization $u_0$,
how good a forecaster must the rider be before the product loses money?

| Model         |  Strike  | $u_0$ | $\lambda^*$ |         95% CI | Never loses |
|:--------------|:--------:|:-----:|------------:|---------------:|------------:|
| Population    | $0.9\,P$ |  0.6  |   NA (none) |              – |        100% |
| Population    | $0.9\,P$ |  0.7  |   NA (none) |              – |        100% |
| Population    | $0.9\,P$ |  0.8  |   NA (none) |              – |        100% |
| Population    | $0.9\,P$ |  0.9  |   NA (none) |              – |         99% |
| Population    | $0.9\,P$ |  1.0  |   NA (none) |              – |         99% |
| Population    |   $P$    |  0.6  |   NA (none) |              – |        100% |
| Population    |   $P$    |  0.7  |   NA (none) |              – |         99% |
| Population    |   $P$    |  0.8  |   NA (none) |              – |         98% |
| Population    |   $P$    |  0.9  |   NA (none) |              – |         96% |
| Population    |   $P$    |  1.0  |   NA (none) |              – |         93% |
| Trip-specific | $0.9\,P$ |  0.6  |   NA (none) |              – |         99% |
| Trip-specific | $0.9\,P$ |  0.7  |   NA (none) |              – |         80% |
| Trip-specific | $0.9\,P$ |  0.8  |        0.89 | \[0.00, 0.98\] |         48% |
| Trip-specific | $0.9\,P$ |  0.9  |        0.19 | \[0.00, 0.93\] |         25% |
| Trip-specific | $0.9\,P$ |  1.0  |        0.00 | \[0.00, 0.00\] |         15% |
| Trip-specific |   $P$    |  0.6  |        0.54 | \[0.00, 0.95\] |         22% |
| Trip-specific |   $P$    |  0.7  |        0.21 | \[0.00, 0.87\] |         16% |
| Trip-specific |   $P$    |  0.8  |        0.00 | \[0.00, 0.73\] |         13% |
| Trip-specific |   $P$    |  0.9  |        0.00 | \[0.00, 0.51\] |         10% |
| Trip-specific |   $P$    |  1.0  |        0.00 | \[0.00, 0.00\] |          9% |

At $u_0 = 1$ the answer is degenerate by construction — the whole month
costs the same under every $\lambda$ — so that row reports whether the
product is under water at full use, not a foresight threshold.

## Export

    ## [1] TRUE

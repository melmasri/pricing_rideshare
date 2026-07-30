Commuter membership — fixed origin, destination, and departure hour
================
2026-07-29

Reports the commuter membership of paper Sec. 7.8 (`sec:commuter`): 100
counterfactual commuters, each holding a 30-ride monthly membership on
one origin–destination pair at one departure hour, priced under both
travel-time models and both strikes.

Each commuter’s trips on the pair are split into a **history** set,
which defines the route distribution the membership is priced against,
and a **held-out** trip, randomized into the 30 rides the member
actually takes. Held-out trips are excluded from the travel-time model
fit, and both sides of the ledger pass through `similar_route()`, so the
sampler’s length bias cannot show up as provider profit.

Run `Rscript run_commuter_membership.R` first; it saves
`../plot/commuter_membership_results.rds`, which this report reads.

    ## List of 17
    ##  $ n_riders               : int 100
    ##  $ rides_per_member       : int 30
    ##  $ history_sizes          : int [1:4] 2 4 6 8
    ##  $ future_size            : int 1
    ##  $ quotes_per_history_trip: int 10
    ##  $ k_factors              : num [1:2] 1 0.9
    ##  $ models                 : chr [1:2] "trip-specific" "population"
    ##  $ usage_rates            : num [1:21] 0 0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 ...
    ##  $ sigma_n                : num 2
    ##  $ significance           : num 0.05
    ##  $ rho                    : num 0.31
    ##  $ risk_free              : num 0
    ##  $ zeta                   : num 0
    ##  $ population_nsamples    : NULL
    ##  $ edge_duration_cap      : NULL
    ##  $ population_sigma       : chr "calibrate"
    ##  $ seed                   : int 1234

## Membership performance

Returns are per member: the collected premium less realized overruns, as
a percentage of the fares the member generates over the month.

| Model | Strike | History | Avg. % return | SE | Avg. profit | Max. loss | Premium/P (%) | Win rate |
|:---|:--:|---:|---:|---:|---:|---:|---:|---:|
| Trip-specific | $P$ | 2 | 0.00 | 0.40 | -0.54 | -130.70 | 3.74 | 0.65 |
| Trip-specific | $P$ | 4 | -0.12 | 0.50 | 1.84 | -193.65 | 3.74 | 0.67 |
| Trip-specific | $P$ | 6 | -0.39 | 0.50 | -3.32 | -210.49 | 3.53 | 0.66 |
| Trip-specific | $P$ | 8 | -0.31 | 0.59 | -1.96 | -258.27 | 3.73 | 0.64 |
| Trip-specific | $0.9\,P$ | 2 | 0.23 | 0.57 | 0.78 | -155.19 | 10.88 | 0.56 |
| Trip-specific | $0.9\,P$ | 4 | -0.17 | 0.63 | 2.36 | -205.38 | 10.79 | 0.51 |
| Trip-specific | $0.9\,P$ | 6 | -0.55 | 0.60 | -4.37 | -225.13 | 10.58 | 0.55 |
| Trip-specific | $0.9\,P$ | 8 | -0.08 | 0.74 | 0.42 | -278.30 | 10.88 | 0.55 |
| Population | $P$ | 2 | 2.15 | 0.46 | 10.86 | -86.79 | 5.40 | 0.77 |
| Population | $P$ | 4 | 1.71 | 0.60 | 9.62 | -199.52 | 5.28 | 0.78 |
| Population | $P$ | 6 | 2.07 | 0.53 | 10.57 | -157.21 | 5.38 | 0.83 |
| Population | $P$ | 8 | 2.13 | 0.63 | 10.91 | -238.66 | 5.38 | 0.79 |
| Population | $0.9\,P$ | 2 | 3.57 | 0.73 | 18.92 | -110.51 | 12.16 | 0.71 |
| Population | $0.9\,P$ | 4 | 2.79 | 0.83 | 15.34 | -213.69 | 11.95 | 0.68 |
| Population | $0.9\,P$ | 6 | 3.09 | 0.76 | 16.17 | -177.72 | 12.01 | 0.73 |
| Population | $0.9\,P$ | 8 | 3.94 | 0.88 | 22.85 | -252.12 | 12.21 | 0.75 |

![](commuter_membership_files/figure-gfm/premium-share-1.png)<!-- -->

![](commuter_membership_files/figure-gfm/profit-distribution-1.png)<!-- -->

## Breakage

A member pays the premium up front but need not take all 30 rides.
Columns give the share of the month’s rides actually taken.

![](commuter_membership_files/figure-gfm/usage-lines-1.png)<!-- -->

![](commuter_membership_files/figure-gfm/usage-heatmap-profit-1.png)<!-- -->

![](commuter_membership_files/figure-gfm/usage-heatmap-return-1.png)<!-- -->

## Data availability

The number of origin–destination pairs able to supply a commuter shrinks
quickly with the history size, which bounds how far the information axis
can be pushed on this data.

| History size | Eligible OD pairs |
|-------------:|------------------:|
|            2 |              1001 |
|            4 |               361 |
|            6 |               151 |
|            8 |                63 |

    ## [1] TRUE

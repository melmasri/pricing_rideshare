Commuter membership — fixed origin, destination, and departure hour
================
2026-07-25

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

    ## List of 14
    ##  $ n_riders               : int 100
    ##  $ rides_per_member       : int 30
    ##  $ history_sizes          : int [1:4] 2 4 6 8
    ##  $ future_size            : int 1
    ##  $ quotes_per_history_trip: int 10
    ##  $ k_factors              : num [1:2] 1 0.9
    ##  $ models                 : chr [1:2] "trip-specific" "population"
    ##  $ usage_rates            : num [1:5] 1 0.9 0.8 0.7 0.6
    ##  $ sigma_n                : num 2
    ##  $ significance           : num 0.05
    ##  $ rho                    : num 0.31
    ##  $ risk_free              : num 0
    ##  $ zeta                   : num 0
    ##  $ seed                   : int 1234

## Membership performance

Returns are per member: the collected premium less realized overruns, as
a percentage of the fares the member generates over the month.

| Model | Strike | History | Avg. % return | SE | Avg. profit | Max. loss | Premium/P (%) | Win rate |
|:---|:--:|---:|---:|---:|---:|---:|---:|---:|
| trip-specific | $P$ | 2 | -0.01 | 0.40 | -0.60 | -130.80 | 3.73 | 0.64 |
| trip-specific | $P$ | 4 | -0.13 | 0.50 | 1.77 | -193.70 | 3.73 | 0.67 |
| trip-specific | $P$ | 6 | -0.40 | 0.50 | -3.39 | -210.55 | 3.52 | 0.66 |
| trip-specific | $P$ | 8 | -0.32 | 0.59 | -2.03 | -258.35 | 3.72 | 0.64 |
| trip-specific | $0.9\,P$ | 2 | 0.22 | 0.57 | 0.75 | -155.23 | 10.87 | 0.56 |
| trip-specific | $0.9\,P$ | 4 | -0.18 | 0.63 | 2.33 | -205.40 | 10.78 | 0.51 |
| trip-specific | $0.9\,P$ | 6 | -0.56 | 0.60 | -4.40 | -225.15 | 10.57 | 0.55 |
| trip-specific | $0.9\,P$ | 8 | -0.09 | 0.74 | 0.38 | -278.32 | 10.87 | 0.55 |
| population | $P$ | 2 | 1.17 | 0.49 | 4.29 | -99.10 | 4.96 | 0.71 |
| population | $P$ | 4 | 0.70 | 0.62 | 2.77 | -206.30 | 4.85 | 0.68 |
| population | $P$ | 6 | 1.04 | 0.55 | 3.78 | -169.82 | 4.95 | 0.72 |
| population | $P$ | 8 | 1.20 | 0.65 | 4.62 | -244.82 | 4.95 | 0.76 |
| population | $0.9\,P$ | 2 | 2.04 | 0.74 | 8.69 | -122.35 | 11.71 | 0.63 |
| population | $0.9\,P$ | 4 | 1.24 | 0.84 | 4.85 | -219.96 | 11.51 | 0.59 |
| population | $0.9\,P$ | 6 | 1.50 | 0.77 | 5.74 | -189.50 | 11.56 | 0.61 |
| population | $0.9\,P$ | 8 | 2.40 | 0.89 | 12.16 | -257.78 | 11.76 | 0.72 |

![](commuter_membership_files/figure-gfm/premium-share-1.png)<!-- -->

![](commuter_membership_files/figure-gfm/profit-distribution-1.png)<!-- -->

## Breakage

A member pays the premium up front but need not take all 30 rides.
Columns give the share of the month’s rides actually taken.

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

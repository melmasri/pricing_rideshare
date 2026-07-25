# Commuter membership (fixed origin–destination)

Simulates the **commuter membership** case from the paper: repeated trips with the same start and end link, route randomization via `similar_route()`, and discount pricing (`K = 0.9`).

| File | Model |
|------|-------|
| `case_study.rmd` | Trip-specific |
| `case_study_population.rmd` | Population |
| `case_study_population_heatmap.Rmd` | Replot heatmaps from saved RDS |

Outputs go to `../plot/`. Requires `../data/trips.csv`.

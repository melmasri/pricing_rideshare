# Pricing Rideshare

Research code and simulations for pricing complex transportation products (on-demand rides, scheduled rides, and memberships) under travel-time uncertainty.

## Layout

```
pricing_rideshare/
├── latex/
│   ├── paper/          # Main manuscript (main.tex, figures, references)
│   └── preprint/       # Preprint / submission variant (add sources here)
├── simulations/
│   ├── case_study/     # Commuter membership — fixed origin/destination
│   ├── membership_abuse/  # Discount & flat membership stress tests
│   ├── plot/           # Shared figure and RDS outputs
│   ├── sql/            # Trip extraction queries
│   └── data/           # Local trip data (gitignored)
```

## Paper ↔ simulations

| Paper section | Simulation |
|---------------|------------|
| On-demand pricing (Sec. upfront) | `simulations/On-Demand.Rmd`, `table1.Rmd`–`table3.Rmd` |
| Discount membership abuse | `simulations/membership_abuse/sensitive_test_generate.Rmd` |
| Flat membership abuse | `simulations/membership_abuse/flat_membership_abuse.Rmd` |
| Commuter membership (fixed OD) | `simulations/case_study/case_study.rmd` (trip-specific), `case_study_population.rmd` (population) |

## Running simulations

Knit R Markdown from each notebook's directory (paths assume `../data/trips.csv` and `../plot/`).

```bash
cd simulations/case_study && Rscript -e 'rmarkdown::render("case_study.rmd")'
cd simulations/membership_abuse && Rscript -e 'rmarkdown::render("sensitive_test_generate.Rmd")'
```

## Building the paper

```bash
cd latex/paper && latexmk -pdf main.tex
```

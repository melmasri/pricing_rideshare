# Pricing Rideshare

Research code and simulations for pricing complex transportation products (on-demand rides, scheduled rides, and memberships) under travel-time uncertainty.

## Layout

```
pricing_rideshare/
├── latex/
│   ├── paper/          # Main manuscript (main.tex, figures, references)
│   └── preprint/       # Preprint / submission variant (add sources here)
├── simulations/
│   ├── commuter_membership/  # Commuter membership — fixed origin/destination and hour
│   ├── membership_abuse/     # Discount & flat membership stress tests
│   ├── plot/           # Shared figure and RDS outputs
│   ├── sql/            # Trip extraction queries
│   └── data/           # Local trip data (gitignored)
```

## Paper ↔ simulations

| Paper section | Simulation |
|---------------|------------|
| Edge × time-bin descriptives (Fig. 4) | `simulations/edge_timebin_figure.R` |
| Sampler / route fidelity (Fig. 5) | `simulations/fidelity_traveltime_figure.R` |
| On-demand pricing (Sec. upfront) | `simulations/On-Demand.Rmd`, `table1.Rmd`–`table3.Rmd` |
| On-demand premium vs time/distance (Fig. route-specific-R) | `simulations/on_demand_premium_figure.R` |
| On-demand realized profit and loss | `simulations/on_demand_pnl.R` |
| Discount membership abuse | `simulations/membership_abuse/sensitive_test_generate.Rmd` |
| Flat membership abuse | `simulations/membership_abuse/flat_membership_abuse.Rmd` |
| Commuter membership (fixed OD) | `simulations/commuter_membership/` |

## Running simulations

Knit R Markdown from each notebook's directory (paths assume `../data/trips.csv` and `../plot/`).

```bash
cd simulations && Rscript edge_timebin_figure.R
cd simulations && Rscript fidelity_traveltime_figure.R
cd simulations && Rscript on_demand_premium_figure.R
cd simulations && Rscript on_demand_pnl.R
cd simulations/commuter_membership && Rscript run_commuter_membership.R
cd simulations/membership_abuse && Rscript -e 'rmarkdown::render("sensitive_test_generate.Rmd")'
```

## Building the paper

```bash
cd latex/paper && latexmk -pdf main.tex
```

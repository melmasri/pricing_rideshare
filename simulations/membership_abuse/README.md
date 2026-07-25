# Membership abuse stress tests

| File | Description |
|------|-------------|
| `sensitive_test_generate.Rmd` | Discount membership (10% cap, `K = 0.9P`) — 100 riders × 40 trips grid |
| `sensitive_test_heatmap.Rmd` | Replot heatmaps from saved RDS |
| `flat_membership_abuse.Rmd` | Unrestricted flat membership (single cap `Kbar`) |
| `debug_abuse_ratio0_compare.Rmd` | Debug notebook for zero-abuse baseline |

Grid CSV outputs: `plot_abuse/`. Shared RDS/figures: `../plot/`. Requires `../data/trips.csv`.

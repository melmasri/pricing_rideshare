Plot4
================
Mingze Li 300137754
2025-06-24

``` r
library(ggplot2)

travel_time <- read.csv("../plot_data/plot4_data.csv")

model_colors <- c(
  "Real Data" = "red",
  "Non-dependent" = "#1f77b4",
  "Full-dependent" = "#ff7f0e",
  "1st-order" = "#2ca02c",
  "2nd-order" = "#d19f9f",
  "Population" = "#7c48ac",
  "HMM" = "#7d3f33"
)
model_linetypes <- c(
  "Real Data" = "solid",
  "Non-dependent" = "dashed",
  "Full-dependent" = "dotted",
  "1st-order" = "dotdash",
  "2nd-order" = "longdash",
  "Population" = "twodash",
  "HMM" = "F2"
)

plot_theme <- function(legend_pos, legend_just) {
  theme(
    panel.background = element_rect(fill = "white", colour = "black", linewidth = 1),
    panel.grid = element_line(colour = "white"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    legend.position = legend_pos,
    legend.justification = legend_just,
    legend.key = element_rect(fill = "white"),
    text = element_text(size = 14),
    plot.title = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12)
  )
}
```

``` r
cdf <- ggplot(travel_time) +
  stat_ecdf(aes(x = sampled_time, color = "Real Data", linetype = "Real Data")) +
  stat_ecdf(aes(x = non_dependent_time, color = "Non-dependent", linetype = "Non-dependent")) +
  stat_ecdf(aes(x = dependent_time, color = "Full-dependent", linetype = "Full-dependent")) +
  stat_ecdf(aes(x = first_order_time, color = "1st-order", linetype = "1st-order")) +
  stat_ecdf(aes(x = second_order_time, color = "2nd-order", linetype = "2nd-order")) +
  stat_ecdf(aes(x = population_time, color = "Population", linetype = "Population")) +
  stat_ecdf(aes(x = HMM, color = "HMM", linetype = "HMM")) +
  labs(
    title = "Empirical CDF Comparison",
    x = "Total Travel Time (seconds)",
    y = "Cumulative Probability"
  ) +
  coord_cartesian(xlim = c(0, 4000), ylim = c(0, 1)) +
  scale_color_manual(name = "Models", values = model_colors) +
  scale_linetype_manual(name = "Models", values = model_linetypes) +
  plot_theme(legend_pos = c(0.98, 0.05), legend_just = c(1, 0))
cdf
```

![](plot4_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
smooth_pdf <- ggplot(travel_time) +
  stat_density(aes(x = sampled_time, color = "Real Data", linetype = "Real Data"), geom = "line", bw = 50) +
  stat_density(aes(x = non_dependent_time, color = "Non-dependent", linetype = "Non-dependent"), geom = "line", bw = 50) +
  stat_density(aes(x = dependent_time, color = "Full-dependent", linetype = "Full-dependent"), geom = "line", bw = 50) +
  stat_density(aes(x = first_order_time, color = "1st-order", linetype = "1st-order"), geom = "line", bw = 50) +
  stat_density(aes(x = second_order_time, color = "2nd-order", linetype = "2nd-order"), geom = "line", bw = 50) +
  stat_density(aes(x = population_time, color = "Population", linetype = "Population"), geom = "line", bw = 50) +
  stat_density(aes(x = HMM, color = "HMM", linetype = "HMM"), geom = "line", bw = 50) +
  labs(
    title = "Empirical PDF Comparison",
    x = "Total Travel Time (seconds)",
    y = "Density"
  ) +
  scale_color_manual(name = "Models", values = model_colors) +
  scale_linetype_manual(name = "Models", values = model_linetypes) +
  plot_theme(legend_pos = c(0.98, 0.95), legend_just = c(1, 1))
smooth_pdf
```

![](plot4_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

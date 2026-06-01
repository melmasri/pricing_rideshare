Figure 4 right
================
Mingze Li 300137754
2025-07-02

``` r
library(data.table)
library(ggplot2)

data <- fread("../plot_data/figure4_data_right.csv")

model_colors <- c(
  "real_data" = "red",
  "normal_5" = "#e76a10",
  "normal_50" = "#e9ce00",
  "normal_5F" = "#0ff1bc"
)
model_linetypes <- c(
  "real_data" = "solid",
  "normal_5" = "dashed",
  "normal_50" = "twodash",
  "normal_5F" = "twodash"
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
plot4_right <- ggplot(data) +
  stat_ecdf(aes(x = real, color = "real_data", linetype = "real_data"), linewidth = 1.2) +
  stat_ecdf(aes(x = normal_5, color = "normal_5", linetype = "normal_5"), linewidth = 0.75) +
  stat_ecdf(aes(x = normal_50, color = "normal_50", linetype = "normal_50"), linewidth = 0.75) +
  stat_ecdf(aes(x = normal_F, color = "normal_5F", linetype = "normal_5F"), linewidth = 0.75) +
  labs(
    title = "Empirical CDF Comparison",
    x = "Total Travel Time (seconds)",
    y = "Cumulative Probability"
  ) +
  coord_cartesian(xlim = c(0, 4000), ylim = c(0, 1)) +
  scale_color_manual(name = "Models", values = model_colors) +
  scale_linetype_manual(name = "Models", values = model_linetypes) +
  plot_theme(legend_pos = c(0.98, 0.05), legend_just = c(1, 0))
plot4_right
```

![](plot4right_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
plot4_right <- ggplot(data) +
  stat_density(aes(x = real, color = "real_data", linetype = "real_data"), geom = "line", linewidth = 1.2) +
  stat_density(aes(x = normal_5, color = "normal_5", linetype = "normal_5"), geom = "line", linewidth = 0.8) +
  stat_density(aes(x = normal_50, color = "normal_50", linetype = "normal_50"), geom = "line", linewidth = 0.8) +
  stat_density(aes(x = normal_F, color = "normal_5F", linetype = "normal_5F"), geom = "line", linewidth = 0.8) +
  labs(
    title = "Empirical PDF Comparison",
    x = "Total Travel Time (seconds)",
    y = "Density"
  ) +
  scale_color_manual(name = "Models", values = model_colors) +
  scale_linetype_manual(name = "Models", values = model_linetypes) +
  plot_theme(legend_pos = c(0.98, 0.5), legend_just = c(1, 0))
plot4_right
```

![](plot4right_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Plot6
================
Mingze Li 300137754
2025-06-24

``` r
library(ggplot2)

user_data <- read.csv("../plot_data/plot6_data.csv")

breaks <- c(0, 4500, 9000, 12500, 15000, 18000, 22000, 28000, Inf)
user_data$dist_group <- cut(
  user_data$distance,
  breaks = breaks,
  include.lowest = TRUE,
  right = FALSE
)

dist_labels <- vapply(levels(user_data$dist_group), function(level) {
  interval <- unlist(strsplit(gsub("[\\[\\)]", "", level), ","))
  start <- format(as.numeric(interval[1]), scientific = FALSE, big.mark = "")
  end <- ifelse(interval[2] == "Inf]", "∞", format(as.numeric(interval[2]), scientific = FALSE, big.mark = ""))
  paste0("[", start, ",", end, ")")
}, character(1))
```

``` r
ggplot(user_data) +
  geom_boxplot(
    aes(x = dist_group, y = profit_population, fill = "Population"),
    alpha = 0.5,
    outlier.colour = "#2682c3",
    outlier.shape = 19,
    outlier.alpha = 1
  ) +
  geom_boxplot(
    aes(x = dist_group, y = profit_trip_specific, fill = "Trip-specific"),
    alpha = 0.5,
    outlier.colour = "#ff8b25",
    outlier.shape = 17,
    outlier.alpha = 1
  ) +
  labs(x = "distance(meter),230~260 trips per box", y = "profit(CAD)") +
  scale_fill_manual(
    name = "Model",
    values = c("Population" = "#83bce6", "Trip-specific" = "#ff7f0e")
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_discrete(labels = dist_labels) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
    text = element_text(size = 13),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12),
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "white", colour = "black"),
    legend.margin = margin(5, 5, 5, 5),
    panel.background = element_rect(fill = "white", colour = "black"),
    panel.grid = element_line(colour = "white")
  )
```

![](plot6_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

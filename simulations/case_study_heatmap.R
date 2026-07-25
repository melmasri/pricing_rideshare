# Helpers for case-study profit_real heatmaps (case_study*.rmd).

aggregate_profit_real <- function(results) {
  if (!is.list(results) || length(results) == 0) {
    stop("results must be a non-empty list from run_case()")
  }

  rbindlist(lapply(results, function(res) {
    if (is.null(res$profit_real) || is.null(res$group_size)) {
      stop("Each result needs group_size and profit_real")
    }
    copy(res$profit_real)[, group_size := res$group_size]
  }))[, sd := sqrt(var)]
}

prepare_heatmap_data <- function(results) {
  dt <- aggregate_profit_real(results)
  dt[, used_trip_pct := used_trip * 100]
  dt[, group_size := factor(group_size, levels = sort(unique(group_size)))]
  dt[, used_trip_pct := factor(
    used_trip_pct,
    levels = sort(unique(used_trip_pct), decreasing = TRUE)
  )]
  dt
}

plot_profit_mean_heatmap <- function(data, title = "Mean profit") {
  ggplot2::ggplot(data, ggplot2::aes(x = used_trip_pct, y = group_size, fill = mean)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.4) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", mean)),
      size = 3.2,
      color = "black"
    ) +
    ggplot2::scale_fill_gradient2(
      low = "#b2182b",
      mid = "white",
      high = "#1a9850",
      midpoint = 0,
      name = "Mean"
    ) +
    ggplot2::labs(
      title = title,
      x = "Share of bundled trips taken",
      y = "Group size"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.grid = ggplot2::element_blank()
    )
}

plot_profit_sd_heatmap <- function(data, title = "Profit SD") {
  ggplot2::ggplot(data, ggplot2::aes(x = used_trip_pct, y = group_size, fill = sd)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.4) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", sd)),
      size = 3.2,
      color = "black"
    ) +
    ggplot2::scale_fill_viridis_c(option = "C", name = "SD") +
    ggplot2::labs(
      title = title,
      x = "Share of bundled trips taken",
      y = "Group size"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.grid = ggplot2::element_blank()
    )
}

plot_profit_combined_heatmap <- function(data, title = "Mean profit (fill) with SD (labels)") {
  ggplot2::ggplot(data, ggplot2::aes(x = used_trip_pct, y = group_size, fill = mean)) +
    ggplot2::geom_tile(color = "grey30", linewidth = 0.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f\n± %.2f", mean, sd)),
      size = 3,
      color = "black",
      lineheight = 0.9
    ) +
    ggplot2::scale_fill_gradient2(
      low = "#b2182b",
      mid = "white",
      high = "#1a9850",
      midpoint = 0,
      name = "Mean"
    ) +
    ggplot2::labs(
      title = title,
      subtitle = "Cell fill = mean profit; label = mean ± SD",
      x = "Share of bundled trips taken",
      y = "Group size"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10),
      panel.grid = ggplot2::element_blank()
    )
}

plot_profit_facet_heatmap <- function(data) {
  long <- data.table::melt(
    data,
    id.vars = c("group_size", "used_trip_pct", "used_trip"),
    measure.vars = c("mean", "sd"),
    variable.name = "metric",
    value.name = "value"
  )
  long[, metric := factor(metric, levels = c("mean", "sd"), labels = c("Mean profit", "SD"))]

  ggplot2::ggplot(long, ggplot2::aes(x = used_trip_pct, y = group_size, fill = value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.4) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", value)),
      size = 2.8,
      color = "black"
    ) +
    ggplot2::facet_wrap(~metric, scales = "free") +
    ggplot2::scale_fill_viridis_c(option = "D", name = NULL) +
    ggplot2::labs(
      title = "Case study profit tables",
      x = "Share of bundled trips taken",
      y = "Group size"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.grid = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

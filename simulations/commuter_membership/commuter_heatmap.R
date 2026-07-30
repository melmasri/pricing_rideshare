# Figures for the commuter membership study, from run_commuter_membership().

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

strike_label <- function(k_factor) {
  label <- fifelse(k_factor == 1, "K = P", sprintf("K = %.2g P", k_factor))
  factor(label, levels = unique(label[order(-k_factor)]))
}

#' Model names as they appear in the paper, in a stable panel order.
model_label <- function(model) {
  label <- sub("^(.)", "\\U\\1", model, perl = TRUE)
  factor(label, levels = sort(unique(label)))
}

prepare_usage_grid <- function(results) {
  dt <- copy(results$usage)
  dt[, model := model_label(model)]
  dt[, strike := strike_label(k_factor)]
  dt[, history_size := factor(history_size, levels = sort(unique(history_size)))]
  dt[, usage_pct := factor(
    100 * usage_rate,
    levels = sort(unique(100 * usage_rate), decreasing = TRUE)
  )]
  dt[]
}

#' Mean per-member profit (CAD) against the share of rides taken, by model and h.
prepare_usage_lines_data <- function(usage_grid, k_factors = c(1, 0.9)) {
  dt <- as.data.frame(usage_grid[usage_grid$k_factor %in% k_factors, , drop = FALSE])
  dt$model <- ifelse(dt$model %in% c("trip-specific", "Trip-specific"),
                     "Trip-specific", "Population")
  dt$model <- factor(dt$model, levels = c("Trip-specific", "Population"))
  dt$history_size <- factor(dt$history_size, levels = c(2, 4, 6, 8))
  dt$strike <- ifelse(dt$k_factor == 1, "K = P", sprintf("K = %.1g P", dt$k_factor))
  strike_levels <- vapply(
    k_factors,
    function(k) if (k == 1) "K = P" else sprintf("K = %.1g P", k),
    character(1)
  )
  dt$strike <- factor(dt$strike, levels = strike_levels)
  dt
}

usage_lines_scales <- function() {
  list(
    scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)),
    scale_color_grey(
      name = expression(italic(h)),
      start = 0.75,
      end = 0.25,
      breaks = c(2, 4, 6, 8),
      labels = c(2, 4, 6, 8)
    ),
    scale_linetype_manual(
      values = c("Trip-specific" = "solid", "Population" = "22"),
      name = NULL
    )
  )
}

plot_usage_lines <- function(usage_grid, strike = 1) {
  dt <- prepare_usage_lines_data(usage_grid, k_factors = strike)

  Reduce(
    `+`,
    c(
      list(
        ggplot(dt, aes(
          x = 100 * usage_rate,
          y = mean_profit,
          color = history_size,
          linetype = model,
          group = interaction(model, history_size)
        )) +
          geom_hline(yintercept = 0, linewidth = 0.3, color = "grey55") +
          geom_line(linewidth = 0.7) +
          labs(
            x = "Share of the 30 rides taken (%)",
            y = "Mean profit per member (CAD)"
          ) +
          theme_minimal(base_size = 11) +
          theme(
            legend.position = "right",
            legend.box = "vertical",
            panel.grid.minor = element_blank()
          )
      ),
      usage_lines_scales()
    )
  )
}

#' Side-by-side breakage curves for multiple strikes with one shared legend.
plot_usage_lines_panels <- function(usage_grid, k_factors = c(1, 0.9)) {
  dt <- prepare_usage_lines_data(usage_grid, k_factors = k_factors)

  Reduce(
    `+`,
    c(
      list(
        ggplot(dt, aes(
          x = 100 * usage_rate,
          y = mean_profit,
          color = history_size,
          linetype = model,
          group = interaction(model, history_size)
        )) +
          geom_hline(yintercept = 0, linewidth = 0.3, color = "grey55") +
          geom_line(linewidth = 0.7) +
          facet_wrap(~strike, ncol = length(k_factors)) +
          labs(
            x = "Share of the 30 rides taken (%)",
            y = "Mean profit per member (CAD)"
          ) +
          theme_minimal(base_size = 11) +
          theme(
            legend.position = "right",
            legend.box = "vertical",
            panel.grid.minor = element_blank(),
            strip.text = element_text(face = "bold")
          )
      ),
      usage_lines_scales()
    )
  )
}

#' Mean per-member profit against history size and the share of rides taken.
plot_usage_heatmap <- function(usage_grid, value = c("mean_profit", "mean_pct_return")) {
  value <- match.arg(value)
  label <- if (value == "mean_profit") "Mean profit (CAD)" else "Mean profit (% of fares)"

  ggplot(usage_grid, aes(x = usage_pct, y = history_size, fill = get(value))) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = sprintf("%.2f", get(value))), size = 2.9) +
    facet_grid(strike ~ model) +
    scale_fill_gradient2(
      low = "#b2182b", mid = "white", high = "#1a9850",
      midpoint = 0, name = label
    ) +
    labs(
      x = "Share of the 30 rides taken (%)",
      y = "History size (trips observed before pricing)"
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(), strip.text = element_text(face = "bold"))
}

#' Distribution of per-member profit across counterfactual commuters.
plot_profit_distribution <- function(results) {
  riders <- rbindlist(lapply(results$runs, function(r) {
    cbind(
      data.table(model = r$model, k_factor = r$k_factor, history_size = r$history_size),
      r$by_rider[, .(profit, pct_return)]
    )
  }))
  riders[, model := model_label(model)]
  riders[, strike := strike_label(k_factor)]
  riders[, history_size := factor(history_size, levels = sort(unique(history_size)))]

  ggplot(riders, aes(x = history_size, y = pct_return, fill = strike)) +
    geom_hline(yintercept = 0, linewidth = 0.3, color = "grey40") +
    geom_boxplot(outlier.size = 0.5, alpha = 0.8, position = position_dodge(0.8)) +
    facet_wrap(~model) +
    scale_fill_manual(values = c("#4575b4", "#d73027"), name = NULL) +
    labs(
      x = "History size (trips observed before pricing)",
      y = "Per-member profit (% of fares)"
    ) +
    theme_minimal(base_size = 11) +
    theme(strip.text = element_text(face = "bold"), legend.position = "top")
}

#' Premium charged, as a share of the fares the member ends up generating.
plot_premium_share <- function(results) {
  dt <- copy(results$summary)
  dt[, model := model_label(model)]
  dt[, strike := strike_label(k_factor)]

  ggplot(dt, aes(x = history_size, y = premium_over_price, color = strike, group = strike)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 2) +
    facet_wrap(~model) +
    scale_color_manual(values = c("#4575b4", "#d73027"), name = NULL) +
    labs(
      x = "History size (trips observed before pricing)",
      y = "Membership premium (% of fares)"
    ) +
    theme_minimal(base_size = 11) +
    theme(strip.text = element_text(face = "bold"), legend.position = "top")
}

#' LaTeX-ready summary rows, matching the columns of the discount-membership table.
format_summary_table <- function(results) {
  dt <- copy(results$summary)
  dt[, .(
    Model = model_label(model),
    Strike = fifelse(k_factor == 1, "$P$", sprintf("$%.2g\\,P$", k_factor)),
    History = history_size,
    `Avg. % return` = round(mean_pct_return, 2),
    `SE` = round(se_pct_return, 2),
    `Avg. profit` = round(mean_profit, 2),
    `Max. loss` = round(max_loss, 2),
    `Premium/P (%)` = round(premium_over_price, 2),
    `Win rate` = round(win_rate, 2)
  )]
}

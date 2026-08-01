# Figures for the selective-exercise study, from run_selective_exercise().
#
# Panels reuse the commuter study's theme and labels so the lambda = 0 line can be laid
# over the published breakage figure without a second visual language.

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

# commuter_heatmap.R sources ../figure_theme.R, which resolves against the working
# directory; this directory sits at the same depth as commuter_membership, so it works.
source("../commuter_membership/commuter_heatmap.R")

#' Foresight as an ordered factor, darkest at perfect foresight.
lambda_label <- function(lambda) {
  factor(sprintf("%.2f", lambda), levels = sprintf("%.2f", sort(unique(lambda))))
}

prepare_selective_curves <- function(curves) {
  dt <- copy(curves)
  dt[, model := factor(
    fifelse(model == "trip-specific", "Trip-specific", "Population"),
    levels = c("Trip-specific", "Population")
  )]
  dt[, strike := strike_label(strike)]
  dt[, foresight := lambda_label(lambda)]
  dt[]
}

selective_scales <- function() {
  list(
    scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)),
    scale_color_grey(
      name = expression(lambda),
      start = 0.8,
      end = 0
    ),
    scale_linetype_manual(
      values = c("Trip-specific" = "solid", "Population" = "22"),
      name = NULL
    )
  )
}

selective_base <- function(dt, y) {
  ggplot(dt, aes(
    x = 100 * u,
    y = get(y),
    color = foresight,
    linetype = model,
    group = interaction(model, foresight)
  )) +
    geom_line(linewidth = 0.7) +
    facet_wrap(~strike, ncol = 2) +
    commuter_panel_theme() +
    theme(legend.position = "right", legend.box = "vertical")
}

#' The Sec. 7.8 breakage curve on this study's axes, for overlay.
prepare_reference_usage <- function(reference_usage) {
  dt <- copy(reference_usage)
  setnames(dt, c("k_factor", "history_size"), c("strike", "h"), skip_absent = TRUE)
  dt[, model := factor(
    fifelse(model == "trip-specific", "Trip-specific", "Population"),
    levels = c("Trip-specific", "Population")
  )]
  dt[, strike := strike_label(strike)]
  dt[, u := usage_rate]
  dt[]
}

#' Mean dollar profit per member against utilization, one line per foresight level.
#'
#' The lambda = 0 line is the published breakage curve of Sec. 7.8; every line above it
#' is what the same month costs once the rider chooses which days to exercise. Passing
#' the published curve in draws it as points, which is acceptance test 7 by eye.
plot_selective_profit <- function(curves, reference_usage = NULL) {
  dt <- prepare_selective_curves(curves)
  layers <- list(
    selective_base(dt, "mean_profit") +
      geom_hline(yintercept = 0, linewidth = 0.3, color = "grey55") +
      labs(x = "Share of the 30 rides taken (%)", y = "Mean profit per member ($)")
  )
  if (!is.null(reference_usage)) {
    layers <- c(layers, list(geom_point(
      data = prepare_reference_usage(reference_usage),
      aes(x = 100 * u, y = mean_profit),
      inherit.aes = FALSE, shape = 21, size = 1.6, stroke = 0.4,
      colour = "grey20", fill = "white"
    )))
  }
  Reduce(`+`, c(layers, selective_scales()))
}

#' Mean loss ratio against utilization, with the break-even reference at one.
#'
#' Loss ratio is the headline normalization: the premium is fixed within a member while
#' the fares taken move with k, so "% of fares" would carry a trend of its own.
plot_selective_loss_ratio <- function(curves) {
  dt <- prepare_selective_curves(curves)
  Reduce(`+`, c(
    list(
      selective_base(dt, "mean_loss_ratio") +
        geom_hline(yintercept = 1, linewidth = 0.4, color = "grey30", linetype = "dotted") +
        labs(x = "Share of the 30 rides taken (%)", y = "Mean loss ratio (payout / premium)")
    ),
    selective_scales()
  ))
}

#' The two panels stacked, sharing the utilization axis.
plot_selective_panels <- function(curves, reference_usage = NULL) {
  top <- plot_selective_profit(curves, reference_usage) +
    labs(x = NULL) +
    theme(axis.text.x = element_blank())
  bottom <- plot_selective_loss_ratio(curves) +
    theme(strip.text = element_blank())
  if (requireNamespace("patchwork", quietly = TRUE)) {
    patchwork::wrap_plots(top, bottom, ncol = 1) +
      patchwork::plot_layout(guides = "collect")
  } else {
    list(top, bottom)
  }
}

#' The loss-ratio surface on an evenly spaced foresight axis.
#'
#' The simulated lambda grid is deliberately coarse and unevenly spaced, which a raster
#' cannot render; interpolating linearly in lambda is the same rule used to read
#' lambda*(u0) off the curves.
interpolate_lambda <- function(curves, lambda_out = seq(0, 1, by = 0.02)) {
  curves[order(lambda), {
    .(lambda = lambda_out,
      mean_loss_ratio = approx(lambda, mean_loss_ratio, xout = lambda_out)$y)
  }, by = .(model, strike, h, k, u)]
}

#' Mean loss ratio over the (utilization, foresight) plane, with the break-even contour.
#'
#' The contour is the whole result in one line: everything above and to the right of it
#' is a month the provider loses on.
plot_loss_ratio_surface <- function(curves) {
  dt <- prepare_selective_curves(interpolate_lambda(curves))
  ggplot(dt, aes(x = 100 * u, y = lambda, z = mean_loss_ratio)) +
    geom_raster(aes(fill = mean_loss_ratio), interpolate = TRUE) +
    geom_contour(breaks = 1, color = "black", linewidth = 0.8) +
    facet_grid(model ~ strike) +
    scale_fill_gradient2(
      low = "#1a9850", mid = "white", high = "#b2182b",
      midpoint = 1, name = "Loss ratio"
    ) +
    scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 1, 0.25), expand = c(0, 0)) +
    labs(
      x = "Share of the 30 rides taken (%)",
      y = expression(paste("Rider foresight  ", lambda))
    ) +
    commuter_panel_theme() +
    theme(
      panel.grid = element_blank(),
      panel.spacing = grid::unit(1, "lines"),
      legend.position = "right"
    )
}

#' Break-even utilization against foresight, with member-level bootstrap intervals.
plot_break_even <- function(break_even) {
  dt <- copy(break_even)
  dt[, model := factor(
    fifelse(model == "trip-specific", "Trip-specific", "Population"),
    levels = c("Trip-specific", "Population")
  )]
  dt[, strike := strike_label(strike)]

  # drop = FALSE keeps a panel for a product that never breaks even: an empty panel is
  # the result, not a missing one.
  ggplot(dt[!is.na(u_star)], aes(x = lambda, y = 100 * u_star, color = strike, group = strike)) +
    geom_ribbon(
      aes(ymin = 100 * u_star_lo, ymax = 100 * u_star_hi, fill = strike),
      alpha = 0.15, colour = NA
    ) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2) +
    facet_wrap(~model, drop = FALSE) +
    scale_color_manual(values = c("K = P" = "grey20", "K = 0.9 P" = "grey60"), name = NULL) +
    scale_fill_manual(values = c("K = P" = "grey20", "K = 0.9 P" = "grey60"), name = NULL) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    labs(
      x = expression(paste("Rider foresight  ", lambda)),
      y = "Break-even utilization (% of rides)"
    ) +
    commuter_panel_theme() +
    theme(legend.position = "top")
}

#' An interval built from a minority of resamples describes those resamples and not the
#' quantity, so it is withheld once most of them never reach the threshold at all.
interval_or_dash <- function(lo, hi, p_missing) {
  fifelse(
    is.na(lo) | p_missing > 0.5, "--",
    sprintf("[%.2f, %.2f]", lo, hi)
  )
}

#' LaTeX-ready break-even rows under both normalizations.
format_break_even_table <- function(break_even) {
  dt <- copy(break_even)
  dt[, .(
    Model = model_label(model),
    Strike = fifelse(strike == 1, "$P$", sprintf("$%.2g\\,P$", strike)),
    `$\\lambda$` = sprintf("%.2f", lambda),
    `$u^*$ pooled` = fifelse(is.na(u_star_pooled), "NA (>1)",
                             sprintf("%.2f", u_star_pooled)),
    `95\\% CI` = interval_or_dash(u_star_pooled_lo, u_star_pooled_hi,
                                  p_no_break_even_pooled),
    `$u^*$ member avg.` = fifelse(is.na(u_star), "NA (>1)", sprintf("%.2f", u_star)),
    `95\\% CI ` = interval_or_dash(u_star_lo, u_star_hi, p_no_break_even)
  )]
}

#' LaTeX-ready inverted-calibration rows: the foresight the product can absorb.
format_foresight_table <- function(lambda_star) {
  dt <- copy(lambda_star)
  dt[, .(
    Model = model_label(model),
    Strike = fifelse(strike == 1, "$P$", sprintf("$%.2g\\,P$", strike)),
    `$u_0$` = sprintf("%.1f", u0),
    `$\\lambda^*$ pooled` = fifelse(is.na(lambda_star_pooled), "NA (none)",
                                    sprintf("%.2f", lambda_star_pooled)),
    `95\\% CI` = interval_or_dash(lambda_star_pooled_lo, lambda_star_pooled_hi,
                                  p_never_loses_pooled),
    `$\\lambda^*$ member avg.` = fifelse(is.na(lambda_star), "NA (none)",
                                         sprintf("%.2f", lambda_star)),
    `95\\% CI ` = interval_or_dash(lambda_star_lo, lambda_star_hi, p_never_loses)
  )]
}

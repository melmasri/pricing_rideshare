# Heatmaps for membership-abuse grids (sensitive_test_generate.Rmd).

baseline_x_label <- function(
  baseline,
  label = "Severity quantile q",
  return_digits = 1,
  se_digits = 2
) {
  if (is.null(baseline) || is.na(baseline$mean_return)) {
    return(label)
  }
  sprintf(
    "%s (no-abuse baseline %.*f%%; SE %.*f%%)",
    label,
    return_digits,
    baseline$mean_return,
    se_digits,
    baseline$sd_return_se
  )
}

prepare_sensitive_heatmap_data <- function(
  return_dt,
  sd_dt,
  row_var = NULL,
  col_prefix = NULL
) {
  return_dt <- data.table::copy(return_dt)
  sd_dt <- data.table::copy(sd_dt)

  if (is.null(row_var)) {
    row_var <- intersect(c("abuse_rate", "abuse_ratio"), names(return_dt))[1]
  } else if (!row_var %in% names(return_dt) && row_var == "abuse_rate" && "abuse_ratio" %in% names(return_dt)) {
    row_var <- "abuse_ratio"
  } else if (!row_var %in% names(return_dt) && row_var == "abuse_ratio" && "abuse_rate" %in% names(return_dt)) {
    row_var <- "abuse_rate"
  }
  if (is.null(row_var) || !row_var %in% names(return_dt)) {
    stop("return_dt must contain abuse_rate or abuse_ratio.")
  }

  if (is.null(col_prefix)) {
    col_prefix <- if (any(grepl("^q_", names(return_dt), perl = TRUE))) {
      "q_"
    } else if (any(grepl("^lambda_", names(return_dt), perl = TRUE))) {
      "lambda_"
    } else {
      stop("return_dt must contain q_* or lambda_* value columns.")
    }
  } else if (col_prefix == "q_" && !any(grepl("^q_", names(return_dt), perl = TRUE))) {
    col_prefix <- "lambda_"
  } else if (col_prefix == "lambda_" && !any(grepl("^lambda_", names(return_dt), perl = TRUE))) {
    col_prefix <- "q_"
  }

  value_cols <- grep(paste0("^", col_prefix), names(return_dt), value = TRUE)
  is_baseline <- abs(return_dt[[row_var]]) < 1e-9
  baseline <- list(mean_return = NA_real_, sd_return_se = NA_real_)
  if (any(is_baseline)) {
    baseline$mean_return <- mean(as.numeric(unlist(return_dt[is_baseline, ..value_cols])), na.rm = TRUE)
    baseline$sd_return_se <- mean(as.numeric(unlist(sd_dt[is_baseline, ..value_cols])), na.rm = TRUE)
    return_dt <- return_dt[!is_baseline]
    sd_dt <- sd_dt[!is_baseline]
  }

  return_long <- data.table::melt(
    return_dt,
    id.vars = row_var,
    measure = value_cols,
    variable.name = "q_col",
    value.name = "mean_return"
  )
  sd_long <- data.table::melt(
    sd_dt,
    id.vars = row_var,
    measure = value_cols,
    variable.name = "q_col",
    value.name = "sd_return_se"
  )
  data <- merge(return_long, sd_long, by = c(row_var, "q_col"))
  data[, q_num := as.numeric(sub(paste0("^", col_prefix), "", q_col))]
  setnames(data, row_var, "abuse_rate", skip_absent = TRUE)
  data[, abuse_rate := factor(abuse_rate, levels = sort(unique(abuse_rate), decreasing = TRUE))]
  data[, q_num := factor(q_num, levels = sort(unique(as.numeric(as.character(q_num)))))]
  list(data = data, baseline = baseline)
}
library(ggplot2)
library(scales)

source("../figure_theme.R")

theme_compact_heatmap <- function(base_size = AXIS_TEXT_PT) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title        = element_text(hjust = 0.5, margin = margin(b = 3), size = AXIS_TITLE_PT),
      plot.subtitle     = element_text(hjust = 0.5, size = AXIS_TEXT_PT),
      plot.margin       = margin(2, 2, 2, 2, "mm"),
      panel.grid        = element_blank(),
      axis.title        = element_text(size = AXIS_TITLE_PT),
      axis.text         = element_text(size = AXIS_TEXT_PT, colour = "black"),
      axis.ticks        = element_line(linewidth = 0.3, colour = "black"),
      legend.title      = element_text(size = LEGEND_TEXT_PT),
      legend.text       = element_text(size = LEGEND_TEXT_PT),
      strip.text        = element_text(size = AXIS_TEXT_PT, face = "bold")
    )
}

# ---- Return heatmap: grayscale, darker = more negative (worse) -------------
# expects columns: q_num, abuse_rate, mean_return
plot_sensitive_return_heatmap <- function(
  data,
  title   = "Mean return (%)",
  x_label = expression(paste("Severity quantile ", italic(q))),
  y_label = expression(paste("Abuse fraction ", italic(M)[1], "/40")),
  digits  = 1,
  label_size = 2.4
) {
  d   <- as.data.frame(data)
  rng <- range(d$mean_return, na.rm = TRUE)
  cut <- rng[1] + 0.40 * diff(rng)                 # tiles below this are dark
  d$.txt <- ifelse(d$mean_return < cut, "grey95", "grey10")

  ggplot(d, aes(q_num, abuse_rate, fill = mean_return)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    geom_tile(data = subset(d, mean_return >= 0),   # mark the profitable region
              fill = NA, colour = "black", linewidth = 0.5) +
    geom_text(aes(label = sprintf(paste0("%.", digits, "f"), mean_return),
                  colour = .txt), size = 8) +
    scale_colour_identity() +
    scale_fill_gradient(
      low = "grey20", high = "grey97", name = "Return (%)",
      guide = guide_colourbar(barwidth = unit(3, "mm"),
                              barheight = unit(35, "mm"))
    ) +
    coord_fixed(ratio = 1, expand = FALSE) +
    labs(title = title, x = x_label, y = y_label) +
    theme_compact_heatmap()
}

# ---- SD heatmap: grayscale, darker = noisier, scale capped at `cap` ---------
# expects columns: q_num, abuse_rate, sd_return
plot_sensitive_sd_heatmap <- function(
  data,
  title   = "Standard error (%)",
  x_label = expression(paste("Severity quantile ", italic(q))),
  y_label = expression(paste("Abuse fraction ", italic(M)[1], "/40")),
  cap     = 2,
  digits  = 1,
  label_size = 3.6
) {
  d   <- as.data.frame(data)
  lo  <- min(d$sd_return, na.rm = TRUE)
  cut <- lo + 0.55 * (cap - lo)                    # tiles above this are dark
  d$.txt <- ifelse(pmin(d$sd_return, cap) > cut, "grey95", "grey10")

  ggplot(d, aes(q_num, abuse_rate, fill = sd_return)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    geom_text(aes(label = sprintf(paste0("%.", digits, "f"), sd_return),
                  colour = .txt), size = label_size) +
    scale_colour_identity() +
    scale_fill_viridis(discrete=FALSE) +
    # scale_fill_gradient(
    #   low = "grey97", high = "grey20", name = "SE (%)",
    #   limits = c(lo, cap), oob = scales::squish,
    #   guide = guide_colourbar(barwidth = unit(3, "mm"),
    #                           barheight = unit(35, "mm"))
    # ) +
    coord_fixed(ratio = 1, expand = FALSE) +
    labs(title = title, x = x_label, y = y_label) +
    theme_compact_heatmap()
}

# # ---- export at single-column width with embedded fonts ----------------------
# ggsave("fig_abuse_return.pdf", plot_sensitive_return_heatmap(return_df),
#        width = 90, height = 85, units = "mm", device = cairo_pdf)
# ggsave("fig_abuse_sd.pdf", plot_sensitive_sd_heatmap(sd_df),
#        width = 90, height = 85, units = "mm", device = cairo_pdf)
plot_sensitive_sd_heatmap <- function(
  data,
  title = "SE of mean return (%)",
  x_label = "Severity quantile q",
  y_label = "Abuse rate M1/40"
) {
  ggplot2::ggplot(data, ggplot2::aes(x = q_num, y = abuse_rate, fill = sd_return_se)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.35) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", sd_return_se)),
      size = 2.6,
      color = "black"
    ) +
    ggplot2::scale_fill_viridis_c(option = "C", name = "SE %") +
    ggplot2::labs(title = title, x = x_label, y = y_label) +
    theme_compact_heatmap()
}

plot_sensitive_combined_heatmap <- function(
  data,
  title = NULL,
  subtitle = NULL,
  x_label = "Severity quantile q",
  y_label = expression(paste("Abuse Fraction ", italic(M)[1], "/40"))
) {
  ggplot2::ggplot(data, ggplot2::aes(x = q_num, y = abuse_rate, fill = mean_return)) +
    ggplot2::geom_tile(color = NA, linewidth = 0) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f\n±%.2f", mean_return, sd_return_se)),
      size = 3.6,
      color = "black",
      lineheight = 0.85
    ) +
    ggplot2::scale_fill_gradient2(
      low = "#b2182b",
      mid = "white",
      high = "#1a9850",
      midpoint = 0,
      name = "Profit\n(% of fares)"
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label
    ) +
    theme_compact_heatmap()
}

plot_sensitive_facet_heatmap <- function(
  data,
  title = "Membership abuse grid",
  x_label = "Severity quantile q",
  y_label = "Abuse rate M1/40"
) {
  long <- data.table::melt(
    data,
    id.vars = c("abuse_rate", "q_col", "q_num"),
    measure.vars = c("mean_return", "sd_return_se"),
    variable.name = "metric",
    value.name = "value"
  )
  long[, metric := factor(
    metric,
    levels = c("mean_return", "sd_return_se"),
    labels = c("Mean profit (% of fares)", "SE of mean profit (%)")
  )]

  ggplot2::ggplot(long, ggplot2::aes(x = q_num, y = abuse_rate, fill = value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.35) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", value)),
      size = 2.3,
      color = "black"
    ) +
    ggplot2::facet_wrap(~metric, scales = "free") +
    ggplot2::scale_fill_viridis_c(option = "D", name = NULL) +
    ggplot2::labs(title = title, x = x_label, y = y_label) +
    theme_compact_heatmap()
}

plot_scenario_heatmaps <- function(scenario, model_label) {
  prepared <- prepare_sensitive_heatmap_data(scenario$return_dt, scenario$sd_dt)
  heatmap_data <- prepared$data
  x_label <- baseline_x_label(prepared$baseline)
  y_expr <- expression(paste("Abuse fraction ", italic(M)[1], "/40"))

  list(
    prepared = prepared,
    return = plot_sensitive_return_heatmap(
      heatmap_data,
      title = model_label,
      x_label = x_label,
      y_label = y_expr
    ),
    sd = plot_sensitive_sd_heatmap(
      heatmap_data,
      title = paste(model_label, "— SE of mean profit (%)"),
      x_label = x_label,
      y_label = expression(M[1]/40)
    ),
    combined = plot_sensitive_combined_heatmap(
      heatmap_data,
      title = model_label,
      x_label = x_label
    ),
    facet = plot_sensitive_facet_heatmap(
      heatmap_data,
      title = paste(model_label, "— membership abuse grid"),
      x_label = x_label,
      y_label = expression(M[1]/40)
    )
  )
}

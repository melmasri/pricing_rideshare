# Shared typography for paper figures (matches Fig. 5 fidelity panels).

PANEL_FONT_PT <- 14
AXIS_TITLE_PT <- 15
AXIS_TEXT_PT <- 14
LEGEND_TEXT_PT <- 15

standard_panel_theme <- function(legend_inside = FALSE) {
  theme <- ggplot2::theme_minimal(base_size = PANEL_FONT_PT, base_family = "sans") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      text = ggplot2::element_text(family = "sans", size = PANEL_FONT_PT),
      axis.title = ggplot2::element_text(size = AXIS_TITLE_PT),
      axis.text = ggplot2::element_text(size = AXIS_TEXT_PT),
      legend.text = ggplot2::element_text(size = LEGEND_TEXT_PT),
      legend.key = ggplot2::element_rect(fill = "white", colour = NA),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
  if (legend_inside) {
    theme + ggplot2::theme(
      legend.position = c(0.98, 0.05),
      legend.justification = c(1, 0),
      legend.box = "vertical"
    )
  } else {
    theme + ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical"
    )
  }
}

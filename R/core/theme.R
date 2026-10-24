# Visual identity: colours, ggplot theme, plotly styling and map palettes.
# Every chart in the app uses these, so changing the look happens here.

GR <- list(
  ink = "#111827", text = "#374151", muted = "#6B7280", line = "#E5E7EB", soft = "#F3F6F4",
  green = "#16A34A", green_dark = "#0F3D2E", mint = "#DCFCE7", orange = "#F59E0B",
  red = "#DC2626", blue = "#2563EB", purple = "#7C3AED",
  font = "Inter, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif",
  class_cols = c("Very Low" = "#1A9850", "Low" = "#91CF60", "Medium" = "#FEE08B",
                 "High" = "#FC8D59", "Very High" = "#D73027"),
  lisa_cols = c("High-High" = "#D73027", "Low-Low" = "#4575B4", "High-Low" = "#FC8D59",
                "Low-High" = "#91BFDB", "Not significant" = "#E5E7EB"),
  gi_cols = c("Hot spot 99%" = "#B2182B", "Hot spot 95%" = "#EF8A62", "Hot spot 90%" = "#FDDBC7",
              "Not significant" = "#E5E7EB", "Cold spot 90%" = "#D1E5F0", "Cold spot 95%" = "#67A9CF",
              "Cold spot 99%" = "#2166AC"),
  seq = c("#1A9850", "#91CF60", "#D9EF8B", "#FEE08B", "#FC8D59", "#D73027"),
  div = c("#2166AC", "#67A9CF", "#F7F7F7", "#EF8A62", "#B2182B"),
  pillar_cols = c("Hazard" = "#F59E0B", "Vulnerability" = "#DC2626", "Coping" = "#2563EB", "Risk" = "#111827")
)

theme_georisk <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.2), colour = GR$ink, hjust = 0,
                                         margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(colour = GR$muted, hjust = 0, margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(colour = GR$muted, size = ggplot2::rel(0.75), hjust = 0,
                                           margin = ggplot2::margin(t = 10)),
      plot.title.position = "plot", plot.caption.position = "plot",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#EDF1EF", linewidth = 0.4),
      axis.title = ggplot2::element_text(colour = GR$text, size = ggplot2::rel(0.9)),
      axis.text = ggplot2::element_text(colour = GR$muted),
      legend.position = "bottom", legend.title = ggplot2::element_text(size = ggplot2::rel(0.85), colour = GR$text),
      legend.text = ggplot2::element_text(colour = GR$text),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0, colour = GR$ink),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(10, 14, 10, 10)
    )
}

theme_map <- function(base_size = 11) {
  theme_georisk(base_size) +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "#F1F4F2", linewidth = 0.3),
                   axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
                   legend.key.width = grid::unit(2.2, "lines"))
}

# Turn a ggplot into a clean interactive plotly chart.
gr_plotly <- function(p, tooltip = "text", legend_bottom = TRUE, source = NULL) {
  pl <- if (is.null(source)) plotly::ggplotly(p, tooltip = tooltip) else
    plotly::event_register(plotly::ggplotly(p, tooltip = tooltip, source = source), "plotly_click")
  pl <- plotly::config(pl, displaylogo = FALSE,
                       modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "toggleSpikelines",
                                                  "hoverClosestCartesian", "hoverCompareCartesian"),
                       toImageButtonOptions = list(format = "png", scale = 3))
  # ggplot legends switched off with guide = "none" are not switched off by
  # ggplotly, which then shows junk entries such as "(Africa,1)". Hide them too.
  if (inherits(p, "ggplot")) {
    sc <- Filter(function(s) !any(s$aesthetics %in% c("x", "y", "xmin", "xmax", "ymin", "ymax", "xend", "yend")), p$scales$scales)
    if (length(sc) && all(vapply(sc, function(s) identical(s$guide, "none"), logical(1))))
      pl <- plotly::layout(pl, showlegend = FALSE)
  }
  plotly::layout(pl,
    font = list(family = GR$font, size = 12, color = GR$text),
    hoverlabel = list(bgcolor = "white", bordercolor = GR$line, font = list(family = GR$font, color = GR$ink)),
    legend = if (legend_bottom) list(orientation = "h", x = 0, y = 1.02, yanchor = "bottom", title = list(text = "")) else list(),
    margin = list(l = 10, r = 10, t = 10, b = 10),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Leaflet palettes keyed by variable type.
pal_for <- function(var, values, dict = NULL) {
  if (var %in% c("risk_class")) return(leaflet::colorFactor(unname(GR$class_cols), levels = names(GR$class_cols), na.color = "#D1D5DB"))
  if (var == "lisa") return(leaflet::colorFactor(unname(GR$lisa_cols), levels = names(GR$lisa_cols), na.color = "#D1D5DB"))
  if (var == "gi") return(leaflet::colorFactor(unname(GR$gi_cols), levels = names(GR$gi_cols), na.color = "#D1D5DB"))
  rng <- range(values, na.rm = TRUE)
  if (!all(is.finite(rng))) rng <- c(0, 1)
  if (rng[1] < 0 && rng[2] > 0) {
    m <- max(abs(rng)); return(leaflet::colorNumeric(GR$div, domain = c(-m, m), na.color = "#D1D5DB"))
  }
  leaflet::colorNumeric(GR$seq, domain = rng, na.color = "#D1D5DB")
}

scale_fill_inform <- function(name = NULL, limits = c(0, 10)) {
  ggplot2::scale_fill_gradientn(colours = GR$seq, limits = limits, name = name, na.value = "#D1D5DB",
                                guide = ggplot2::guide_colourbar(barwidth = 14, barheight = 0.6, title.position = "top"))
}

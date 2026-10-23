# Double burden: where hazard exposure and social vulnerability coincide.
# Bivariate classes (terciles), bivariate spatial association (Lee's L) and
# summary statistics.

BIVAR_COLS <- c("1-1" = "#E8E8E8", "2-1" = "#E4ACAC", "3-1" = "#C85A5A",
                "1-2" = "#B0D5DF", "2-2" = "#AD9EA5", "3-2" = "#985356",
                "1-3" = "#64ACBE", "2-3" = "#627F8C", "3-3" = "#574249")

tercile <- function(x) {
  q <- stats::quantile(x, c(1 / 3, 2 / 3), na.rm = TRUE)
  as.integer(cut(x, c(-Inf, q, Inf), labels = 1:3))
}

bivariate_classes <- function(hazard, vuln) {
  h <- tercile(hazard); v <- tercile(vuln)
  cls <- ifelse(is.na(h) | is.na(v), NA, paste0(h, "-", v))
  lab <- ifelse(is.na(cls), NA, paste0(c("Low", "Medium", "High")[h], " hazard, ", c("low", "medium", "high")[v], " vulnerability"))
  data.frame(h = h, v = v, cls = factor(cls, levels = names(BIVAR_COLS)), label = lab, double = cls %in% "3-3",
             stringsAsFactors = FALSE)
}

# Lee's L (Lee, 2001): combines Pearson correlation with spatial smoothing.
# Large positive L means high hazard and high vulnerability co-occur and
# cluster in the same neighbourhoods.
lee_test <- function(x, y, lw, nsim = 499) {
  ok <- !is.na(x) & !is.na(y); lw2 <- subset_lw(lw, ok)
  set.seed(2026)
  mc <- spdep::lee.mc(x[ok], y[ok], lw2, nsim = nsim, zero.policy = TRUE, alternative = "greater")
  loc <- spdep::lee(x[ok], y[ok], lw2, n = sum(ok), zero.policy = TRUE)
  local <- rep(NA_real_, length(x)); local[ok] <- loc$localL
  list(L = unname(mc$statistic), p = mc$p.value, local = local,
       pearson = stats::cor(x[ok], y[ok]), spearman = stats::cor(x[ok], y[ok], method = "spearman"))
}

# Small 3 x 3 legend for static maps.
bivariate_legend <- function(xlab = "Hazard", ylab = "Vulnerability") {
  g <- expand.grid(h = 1:3, v = 1:3); g$fill <- BIVAR_COLS[paste0(g$h, "-", g$v)]
  ggplot2::ggplot(g, ggplot2::aes(h, v, fill = fill)) + ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::scale_fill_identity() + ggplot2::coord_equal() +
    ggplot2::labs(x = paste(xlab, "->"), y = paste(ylab, "->")) +
    ggplot2::theme_void(base_size = 8) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 3)),
                   axis.title.y = ggplot2::element_text(angle = 90, margin = ggplot2::margin(r = 3)))
}

bivariate_legend_html <- function(xlab = "Hazard", ylab = "Vulnerability") {
  cell <- function(h, v) sprintf("<div style='width:18px;height:18px;background:%s'></div>", BIVAR_COLS[[paste0(h, "-", v)]])
  rows <- vapply(3:1, function(v) paste0("<div style='display:flex;gap:2px'>", paste(vapply(1:3, function(h) cell(h, v), character(1)), collapse = ""), "</div>"), character(1))
  sprintf("<div style='font:11px Inter,sans-serif;color:#374151'><div style='display:flex;gap:6px;align-items:center'>
           <div style='writing-mode:vertical-rl;transform:rotate(180deg)'>%s &rarr;</div>
           <div style='display:flex;flex-direction:column;gap:2px'>%s</div></div>
           <div style='margin-left:20px;margin-top:2px'>%s &rarr;</div></div>", ylab, paste(rows, collapse = ""), xlab)
}

# Interactive and static bivariate maps, shared by analyst and public views.
bivariate_leaflet <- function(store, cls, labels, hz_lab, vu_lab, extra = NULL) {
  sf <- store$inform_sf
  fill <- ifelse(is.na(cls), "#D1D5DB", BIVAR_COLS[as.character(cls)])
  tip <- sprintf("<div class='map-tip'><b>%s</b><br/>%s%s</div>", sf$country, ifelse(is.na(labels), "<i>No data: excluded from this analysis</i>", labels), if (is.null(extra)) "" else extra)
  leaflet_base() |> leaflet::setView(15, 20, 1.6) |>
    leaflet::addPolygons(data = sf, layerId = ~iso3, fillColor = fill, fillOpacity = 0.85, color = "#FFFFFF", weight = 0.6,
                         label = lapply(tip, htmltools::HTML),
                         highlightOptions = leaflet::highlightOptions(weight = 2, color = GR$ink, bringToFront = TRUE)) |>
    leaflet::addControl(htmltools::HTML(bivariate_legend_html(hz_lab, vu_lab)), position = "bottomleft", className = "info legend")
}

bivariate_static <- function(store, cls, hz_lab, vu_lab) {
  p <- static_choropleth(store, factor(as.character(cls), levels = names(BIVAR_COLS)), NULL, "factor", BIVAR_COLS) +
    ggplot2::theme(legend.position = "none")
  out <- if (requireNamespace("patchwork", quietly = TRUE))
    p + patchwork::inset_element(bivariate_legend(hz_lab, vu_lab), left = 0.02, bottom = 0.05, right = 0.2, top = 0.38) else p
  attr(out, "export_data") <- data.frame(iso3 = store$inform_sf$iso3, country = store$inform_sf$country, class = as.character(cls))
  out
}

# Publication figures and descriptive statistics.
# Every figure is a plain function returning a ggplot, used both on screen
# (through plotly) and for the PNG/PDF/SVG downloads, so what is shown is
# what is published. export_all_figures() writes the numbered set for a report.

CONT_COLS <- c("Africa" = "#D97706", "Asia" = "#DC2626", "Europe" = "#2563EB",
               "North America" = "#7C3AED", "South America" = "#16A34A", "Oceania" = "#0891B2")

has_repel <- function() requireNamespace("ggrepel", quietly = TRUE)

# ----------------------------------------------------------- statistics --
skewness_of <- function(x) { x <- x[!is.na(x)]; n <- length(x); if (n < 3) return(NA); m <- mean(x); s <- sqrt(mean((x - m)^2)); if (s == 0) NA else mean((x - m)^3) / s^3 }
kurtosis_of <- function(x) { x <- x[!is.na(x)]; n <- length(x); if (n < 4) return(NA); m <- mean(x); s <- sqrt(mean((x - m)^2)); if (s == 0) NA else mean((x - m)^4) / s^4 - 3 }

describe_variables <- function(d, vi) {
  rows <- lapply(seq_len(nrow(vi)), function(i) {
    v <- vi$var[i]; x <- d[[v]]; x <- x[is.finite(x) | is.na(x)]; ok <- x[!is.na(x)]
    q <- if (length(ok)) stats::quantile(ok, c(0, 0.25, 0.5, 0.75, 1), names = FALSE) else rep(NA, 5)
    sw <- if (length(unique(ok)) > 3 && length(ok) >= 3 && length(ok) <= 5000) tryCatch(stats::shapiro.test(ok)$p.value, error = function(e) NA) else NA
    data.frame(Variable = vi$label[i], Group = vi$group[i], Unit = vi$unit[i], n = length(ok), Missing = sum(is.na(x)),
               Mean = if (length(ok)) mean(ok) else NA, SD = if (length(ok) > 1) stats::sd(ok) else NA,
               Min = q[1], Q1 = q[2], Median = q[3], Q3 = q[4], Max = q[5], IQR = q[4] - q[2],
               Skewness = skewness_of(ok), Kurtosis = kurtosis_of(ok), `Shapiro-Wilk p` = sw,
               var = v, check.names = FALSE, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# --------------------------------------------------------------- figures --
fig_workflow <- function() {
  s <- STEPS; s$x <- seq_len(nrow(s)) * 2.2
  wrap <- function(x, w) vapply(x, function(z) paste(strwrap(z, w), collapse = "\n"), character(1))
  ggplot2::ggplot(s) +
    ggplot2::geom_segment(data = s[-nrow(s), ], ggplot2::aes(x = x + 0.92, xend = x + 1.28, y = 0, yend = 0),
                          arrow = grid::arrow(length = grid::unit(0.16, "cm"), type = "closed"), colour = "#6B7280", linewidth = 0.6) +
    ggplot2::geom_tile(ggplot2::aes(x = x, y = 0), width = 1.8, height = 2.3, fill = "#F3F6F4", colour = "#0F3D2E", linewidth = 0.4) +
    ggplot2::geom_text(ggplot2::aes(x = x, y = 0.9, label = paste0(seq_len(nrow(s)), ". ", toupper(verb))), fontface = "bold", size = 3.1, colour = "#16A34A") +
    ggplot2::geom_text(ggplot2::aes(x = x, y = 0.45, label = wrap(question, 20)), fontface = "bold", size = 3, colour = "#111827", lineheight = 0.9) +
    ggplot2::geom_text(ggplot2::aes(x = x, y = -0.25, label = wrap(method, 24)), size = 2.5, colour = "#374151", lineheight = 0.9) +
    ggplot2::geom_text(ggplot2::aes(x = x, y = -0.85, label = wrap(output, 24)), size = 2.4, colour = "#6B7280", fontface = "italic", lineheight = 0.9) +
    ggplot2::coord_cartesian(xlim = c(1.1, max(s$x) + 1.1), ylim = c(-1.25, 1.25), expand = FALSE) +
    ggplot2::theme_void() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                                           plot.title = ggplot2::element_text(face = "bold", size = 12, margin = ggplot2::margin(b = 6)),
                                           plot.caption = ggplot2::element_text(colour = "#6B7280", size = 8, hjust = 0),
                                           plot.margin = ggplot2::margin(10, 10, 10, 10))
}

fig_distributions <- function(d, vars, labels) {
  long <- do.call(rbind, lapply(seq_along(vars), function(i) data.frame(indicator = labels[i], value = d[[vars[i]]], country = d$country)))
  long <- long[!is.na(long$value), ]
  long$indicator <- factor(long$indicator, levels = labels)
  med <- stats::aggregate(value ~ indicator, long, stats::median)
  ggplot2::ggplot(long, ggplot2::aes(value)) +
    ggplot2::geom_histogram(bins = 20, fill = "#86B9A0", colour = "white", linewidth = 0.2) +
    ggplot2::geom_vline(data = med, ggplot2::aes(xintercept = value), colour = GR$ink, linetype = 2, linewidth = 0.5) +
    ggplot2::facet_wrap(~indicator, scales = "free", ncol = min(4, length(vars))) +
    ggplot2::scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE, drop0trailing = TRUE)) +
    ggplot2::labs(x = "Value", y = "Number of countries") + theme_georisk(10)
}

fig_box_continent <- function(d, var, label) {
  dd <- d[!is.na(d[[var]]) & !is.na(d$continent), ]; dd$v <- dd[[var]]
  n <- table(dd$continent)
  dd$text <- sprintf("%s (%s)<br>%s: %.2f", dd$country, dd$continent, label, dd$v)
  ggplot2::ggplot(dd, ggplot2::aes(continent, v)) +
    ggplot2::geom_boxplot(ggplot2::aes(fill = continent), alpha = 0.35, outlier.shape = NA, width = 0.6, colour = "#374151") +
    ggplot2::geom_jitter(ggplot2::aes(colour = continent, text = text), width = 0.15, height = 0, size = 1.6, alpha = 0.8) +
    ggplot2::scale_fill_manual(values = CONT_COLS, guide = "none") + ggplot2::scale_colour_manual(values = CONT_COLS, guide = "none") +
    ggplot2::scale_x_discrete(labels = function(x) sprintf("%s\n(n = %d)", x, as.integer(n[x]))) +
    ggplot2::labs(x = NULL, y = label) + theme_georisk(10)
}

fig_corr_matrix <- function(d, vars, labels, method = "spearman") {
  m <- d[, vars, drop = FALSE]
  r <- stats::cor(m, use = "pairwise.complete.obs", method = method)
  p <- outer(seq_along(vars), seq_along(vars), Vectorize(function(i, j)
    if (i == j) NA else tryCatch(stats::cor.test(m[[i]], m[[j]], method = method, exact = FALSE)$p.value, error = function(e) NA)))
  g <- expand.grid(i = seq_along(vars), j = seq_along(vars))
  g <- g[g$i > g$j, ]
  g$r <- r[cbind(g$i, g$j)]; g$p <- p[cbind(g$i, g$j)]
  g$x <- factor(labels[g$j], levels = labels); g$y <- factor(labels[g$i], levels = rev(labels))
  g$lab <- sprintf("%.2f%s", g$r, ifelse(is.na(g$p), "", ifelse(g$p < 0.001, "***", ifelse(g$p < 0.01, "**", ifelse(g$p < 0.05, "*", "")))))
  g$text <- sprintf("%s and %s<br>%s = %.2f, p %s<br><i>Click to see the countries</i>", labels[g$i], labels[g$j],
                    if (method == "spearman") "Spearman rho" else "Pearson r", g$r, fmt_p(g$p))
  g$key <- paste(vars[g$j], vars[g$i], sep = "|")
  out <- ggplot2::ggplot(g, ggplot2::aes(x, y, fill = r)) +
    ggplot2::geom_tile(ggplot2::aes(text = text, key = key), colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = lab), size = if (length(vars) > 10) 2.1 else 2.8, colour = "#111827") +
    ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B", limits = c(-1, 1), name = if (method == "spearman") "Spearman rho" else "Pearson r",
                                  guide = ggplot2::guide_colourbar(barwidth = 12, barheight = 0.6, title.position = "top")) +
    ggplot2::labs(x = NULL, y = NULL) + theme_georisk(10) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1), panel.grid = ggplot2::element_blank())
  attr(out, "export_data") <- data.frame(variable_1 = labels[g$i], variable_2 = labels[g$j], r = g$r, p = g$p)
  out
}

# Scatter of two variables, one panel per continent, with a fitted line and the
# correlation in each panel title. Highlighted countries (iso3) are labelled.
fig_scatter_continent <- function(d, x, y, xlab, ylab, highlight = character(0), continents = CONTINENT_LEVELS,
                                  log_x = FALSE, method = "spearman", ncol = 3, repel = TRUE) {
  dd <- d[!is.na(d[[x]]) & !is.na(d[[y]]) & d$continent %in% continents, ]
  dd$xv <- dd[[x]]; dd$yv <- dd[[y]]
  st <- do.call(rbind, lapply(split(dd, droplevels(dd$continent)), function(z) {
    r <- if (nrow(z) >= 4) suppressWarnings(stats::cor.test(z$xv, z$yv, method = method, exact = FALSE)) else NULL
    data.frame(continent = z$continent[1], n = nrow(z), r = if (is.null(r)) NA else unname(r$estimate), p = if (is.null(r)) NA else r$p.value)
  }))
  strip <- stats::setNames(ifelse(is.na(st$r), sprintf("%s (n = %d, too few for a correlation)", st$continent, st$n),
                                   sprintf("%s (n = %d, %s = %.2f%s)", st$continent, st$n, if (method == "spearman") "rho" else "r", st$r,
                                           ifelse(st$p < 0.05, ", p < 0.05", ", not significant"))), st$continent)
  dd$panel <- factor(strip[as.character(dd$continent)], levels = strip[intersect(CONTINENT_LEVELS, names(strip))])
  dd$hl <- dd$iso3 %in% highlight
  dd$text <- sprintf("%s (%s)<br>%s: %s<br>%s: %.2f<br><i>Click to label</i>", dd$country, dd$continent, xlab,
                     if (log_x) format(round(dd$xv), big.mark = ",") else sprintf("%.2f", dd$xv), ylab, dd$yv)
  fit_d <- dd[dd$continent %in% st$continent[st$n >= 4], ]
  p <- ggplot2::ggplot(dd, ggplot2::aes(xv, yv)) +
    ggplot2::geom_smooth(data = fit_d, method = "lm", formula = y ~ x, se = TRUE, colour = GR$ink, fill = "#E5E7EB", linewidth = 0.7) +
    ggplot2::geom_point(ggplot2::aes(colour = continent, text = text, key = iso3, size = hl, alpha = hl)) +
    ggplot2::scale_colour_manual(values = CONT_COLS, guide = "none") +
    ggplot2::scale_size_manual(values = c(`FALSE` = 1.8, `TRUE` = 3.4), guide = "none") +
    ggplot2::scale_alpha_manual(values = c(`FALSE` = if (length(highlight)) 0.45 else 0.8, `TRUE` = 1), guide = "none") +
    ggplot2::facet_wrap(~panel, ncol = min(ncol, length(unique(dd$panel))), scales = "free_x") +
    ggplot2::labs(x = xlab, y = ylab) + theme_georisk(10)
  if (log_x) p <- p + ggplot2::scale_x_log10(labels = function(v) format(v, big.mark = ",", scientific = FALSE))
  if (any(dd$hl)) {
    lab <- dd[dd$hl, ]
    p <- p + ggplot2::geom_point(data = lab, shape = 21, size = 3.6, colour = GR$ink, fill = NA, stroke = 0.9) +
      if (repel && has_repel()) ggrepel::geom_text_repel(data = lab, ggplot2::aes(label = country), size = 3, colour = GR$ink, min.segment.length = 0, seed = 1)
      else ggplot2::geom_text(data = lab, ggplot2::aes(label = country), size = 3.2, colour = GR$ink, fontface = "bold", nudge_y = 0.45)
  }
  attr(p, "export_data") <- data.frame(country = dd$country, iso3 = dd$iso3, continent = dd$continent, x = dd$xv, y = dd$yv)
  attr(p, "panel_stats") <- st
  p
}

fig_moran_scatter <- function(l, countries, label) {
  d <- data.frame(z = l$z, lag = l$lag, cluster = l$cluster, country = countries)
  d <- d[!is.na(d$z), ]
  d$text <- sprintf("%s<br>%s<br>Own value (standardised): %.2f<br>Neighbours' average: %.2f", d$country, d$cluster, d$z, d$lag)
  ggplot2::ggplot(d, ggplot2::aes(z, lag)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#D1D5DB") + ggplot2::geom_vline(xintercept = 0, colour = "#D1D5DB") +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = GR$ink, linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(fill = cluster, text = text), shape = 21, colour = "white", size = 2.6, stroke = 0.3) +
    ggplot2::scale_fill_manual(values = GR$lisa_cols, name = NULL, drop = TRUE) +
    ggplot2::labs(x = paste("Country's own", lc(label), "(standardised)"), y = "Average of its neighbours (spatial lag)") + theme_georisk(10)
}

fig_uncertainty <- function(u, top = 30) {
  u <- utils::head(u[order(-u$risk), ], top)
  u$country <- factor(u$country, levels = rev(u$country))
  u$text <- sprintf("%s<br>Published %.1f (%s)<br>90%% range %.1f to %.1f<br>Lack of reliability %.1f", u$country, u$risk, u$risk_class, u$risk_lo, u$risk_hi, u$reliability)
  bands <- data.frame(xmin = c(5.35, 6.95), xmax = c(6.95, 10), cls = c("High", "Very High"))
  ggplot2::ggplot(u, ggplot2::aes(y = country)) +
    ggplot2::geom_rect(data = bands, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = cls), inherit.aes = FALSE, alpha = 0.15) +
    ggplot2::scale_fill_manual(values = GR$class_cols, name = "Risk class") +
    ggplot2::geom_segment(ggplot2::aes(x = risk_lo, xend = risk_hi, yend = country, colour = reliability), linewidth = 2.2, alpha = 0.75) +
    ggplot2::geom_point(ggplot2::aes(x = risk, text = text), size = 2.2, colour = GR$ink) +
    ggplot2::scale_colour_gradientn(colours = c("#16A34A", "#FDE68A", "#F59E0B", "#DC2626"), name = "Lack of reliability") +
    ggplot2::labs(x = "INFORM Risk (0-10)", y = NULL) + theme_georisk(10) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8))
}

# ------------------------------------------------------ export all figures --
# Writes the numbered figure set (PNG at 300 dpi and PDF) and the tables, with
# the default settings of each page, into dir. Returns the file paths.
export_all_figures <- function(store, dir, progress = function(i, n, label) NULL) {
  d <- store$all; sfd <- store$inform_sf
  src <- paste("Source:", store$sources$inform)
  src2 <- paste0("Sources: ", store$sources$inform, "; ", store$sources$owid, ".")
  cw <- analysis_data(store, "inform_risk", "INFORM Risk")
  lisa <- local_moran(cw$data$inform_risk, cw$w$lw, 0.05, "fdr"); gm <- global_moran(cw$data$inform_risk, cw$w$lw, 999)
  cy <- analysis_data(store, c("inform_risk", "log_gdp_pc"), c("INFORM Risk", "GDP per person"), d = d)
  fit <- spatial_regression(cy$data, "inform_risk", "log_gdp_pc", cy$w$lw, standardise = TRUE)
  cs <- analysis_data(store, c("inform_risk", "hazard_and_exposure", "vulnerability", "lack_of_coping_capacity", "reliability_index"))
  sim <- simulate_risk(cs$data, 1, 1000)
  ca <- analysis_data(store, c("hazard_and_exposure", "vulnerability"))
  bc <- bivariate_classes(ca$data$hazard_and_exposure, ca$data$vulnerability); lee <- lee_test(ca$data$hazard_and_exposure, ca$data$vulnerability, ca$w$lw)
  core <- c("inform_risk", "hazard_and_exposure", "vulnerability", "lack_of_coping_capacity")
  haz <- c("earthquake", "river_flood", "tsunami", "tropical_cyclone", "coastal_flood", "drought", "epidemic", "projected_conflict_probability", "current_conflict_intensity")
  cm <- c(core, "drought", "river_flood", "food_security", "governance", "log_gdp_pc", "reliability_index")
  L <- function(v) unname(store$labels[v])
  figs <- list(
    list("Analytical workflow: five steps from data to decision", fig_workflow(), "Each step answers one question with the method shown; its output feeds the next step.", 10, 3.6),
    list("INFORM Risk class by country, 2026", static_choropleth(store, sfd$risk_class, "Risk class", "factor", GR$class_cols), src, 10, 5.6),
    list("Distribution of INFORM Risk and its three parts", fig_distributions(d, core, L(core)), paste("Dashed line: median. n = 191 countries.", src), 9, 5),
    list("Distribution of the hazard indicators", fig_distributions(d, haz, L(haz)), paste("Distribution of each hazard score across 191 countries; dashed line: median.", src), 10, 7),
    list("INFORM Risk by continent", fig_box_continent(d, "inform_risk", "INFORM Risk (0-10)"), paste("Boxes: median and interquartile range; points: countries.", src), 8, 5),
    list("Correlations between the main variables", fig_corr_matrix(d, cm, L(cm)), paste("Spearman rank correlations, pairwise complete cases. * p < 0.05, ** p < 0.01, *** p < 0.001.", src2), 8.5, 7.5),
    list("Clusters of INFORM Risk (Local Moran's I)", static_choropleth(store, cw$expand(lisa$cluster), "Local Moran's I cluster", "factor", GR$lisa_cols),
         sprintf("Global Moran's I = %.3f (permutation p %s); local clusters p < 0.05 after false discovery rate correction; 5 nearest-neighbour weights. %s", gm$I, fmt_p(gm$p_perm), src), 10, 5.6),
    list("Moran scatterplot of INFORM Risk", fig_moran_scatter(lisa, cw$data$country, "INFORM Risk"), sprintf("Slope = Moran's I = %.3f. %s", gm$I, src), 6.5, 5.5),
    list("INFORM Risk against GDP per person, by continent", fig_scatter_continent(d, "gdp_per_capita", "inform_risk", "GDP per person (US dollars, log scale)", "INFORM Risk (0-10)", log_x = TRUE),
         paste("Line: least-squares fit with 95% confidence band in each continent. Countries without GDP data are excluded.", src2), 10, 6.5),
    list("INFORM Risk not explained by wealth (regression residuals)", static_choropleth(store, cy$expand(unname(stats::residuals(fit$ols))), "Residual (observed minus predicted)", "diverging"),
         sprintf("Ordinary regression of INFORM Risk on log GDP per person (n = %d, R squared = %.2f); residual Moran's I = %.2f, p %s; selected model: %s. Grey: no GDP data. %s",
                 fit$n, fit$r2, unname(fit$residual_moran$estimate[1]), fmt_p(fit$residual_moran$p.value), lc(fit$choice$model), src2), 10, 5.6),
    list("INFORM Risk with 90% uncertainty ranges, 30 highest-risk countries", fig_uncertainty(sim), paste("Maximum error 1 index point at worst reliability; 1,000 simulations.", src), 8, 8),
    list("Where danger meets vulnerability (double burden)", bivariate_static(store, ca$expand(bc$cls), "Hazard and exposure", "Vulnerability"),
         sprintf("Terciles of hazard and exposure and of vulnerability; Lee's L = %.2f, p %s. %s", lee$L, fmt_p(lee$p), src), 10, 5.6)
  )
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  files <- character(0)
  for (i in seq_along(figs)) {
    f <- figs[[i]]; progress(i, length(figs), f[[1]])
    p <- f[[2]]; title <- sprintf("Figure %d. %s", i, f[[1]])
    cap <- paste(strwrap(f[[3]], width = floor(f[[4]] * 15)), collapse = "\n")
    p <- if (inherits(p, "patchwork")) p + patchwork::plot_annotation(title = title, caption = cap, theme = theme_georisk())
         else p + ggplot2::labs(title = title, caption = cap)
    base <- file.path(dir, sprintf("Figure_%02d_%s", i, gsub("[^A-Za-z0-9]+", "_", substr(f[[1]], 1, 40))))
    for (ext in c("png", "pdf")) { save_plot(p, paste0(base, ".", ext), ext, f[[4]], f[[5]]); files <- c(files, paste0(base, ".", ext)) }
  }
  t1 <- describe_variables(d, store$vars); t1$var <- NULL
  utils::write.csv(t1, file.path(dir, "Table_1_Descriptive_statistics.csv"), row.names = FALSE)
  utils::write.csv(robustness_table(subset_store(store, cs$data$iso3), "inform_risk", 1, "world", 4.5), file.path(dir, "Table_2_Robustness.csv"), row.names = FALSE)
  utils::write.csv(store$vars[, c("var", "label", "group", "unit", "description", "source", "steps")], file.path(dir, "Table_3_Variable_dictionary.csv"), row.names = FALSE)
  c(files, file.path(dir, c("Table_1_Descriptive_statistics.csv", "Table_2_Robustness.csv", "Table_3_Variable_dictionary.csv")))
}

# Interactive version of the correlation matrix for the screen: a native plotly
# heatmap, so clicking a cell reliably returns the two variables.
corr_plotly <- function(d, vars, labels, source, method = "spearman") {
  m <- d[, vars, drop = FALSE]
  r <- stats::cor(m, use = "pairwise.complete.obs", method = method)
  n <- length(vars)
  z <- r; z[upper.tri(z, diag = TRUE)] <- NA
  txt <- matrix(ifelse(is.na(z), "", sprintf("%.2f", z)), n)
  hover <- matrix(sprintf("%s and %s<br>rho = %.2f<br><i>Click to see the countries</i>", rep(labels, n), rep(labels, each = n), r), n)
  hover[is.na(z)] <- ""
  key <- matrix(paste(rep(vars, each = n), rep(vars, n), sep = "|"), n)   # column var | row var
  rev_i <- rev(seq_len(n))
  p <- plotly::plot_ly(x = labels, y = labels[rev_i], z = z[rev_i, , drop = FALSE], type = "heatmap", source = source,
                       zmin = -1, zmax = 1, colorscale = list(c(0, "#2166AC"), c(0.5, "#F7F7F7"), c(1, "#B2182B")),
                       text = hover[rev_i, , drop = FALSE], hoverinfo = "text", customdata = key[rev_i, , drop = FALSE],
                       xgap = 2, ygap = 2, colorbar = list(title = "rho", thickness = 12, len = 0.6))
  ann <- which(!is.na(z), arr.ind = TRUE)
  p <- plotly::add_annotations(p, x = labels[ann[, 2]], y = labels[ann[, 1]], text = txt[ann], showarrow = FALSE,
                               font = list(size = if (n > 10) 9 else 11, color = "#111827"))
  p <- plotly::layout(p, xaxis = list(title = "", tickangle = -35, showgrid = FALSE), yaxis = list(title = "", showgrid = FALSE),
                      font = list(family = GR$font, size = 11, color = GR$text), margin = list(l = 10, r = 10, t = 10, b = 10),
                      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  p <- plotly::config(p, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
  plotly::event_register(p, "plotly_click")
}

# Interactive Moran scatterplot drawn directly in plotly (much faster than
# converting the ggplot version, which is kept for downloads).
moran_plotly <- function(l, countries, label) {
  d <- data.frame(z = l$z, lag = l$lag, cluster = l$cluster, country = countries)
  d <- d[!is.na(d$z), ]
  fit <- stats::lm(lag ~ z, d); xs <- range(d$z)
  cols <- GR$lisa_cols[intersect(names(GR$lisa_cols), unique(d$cluster))]
  p <- plotly::plot_ly()
  p <- plotly::add_lines(p, x = xs, y = stats::predict(fit, data.frame(z = xs)), line = list(color = GR$ink, width = 2), hoverinfo = "skip", showlegend = FALSE)
  for (k in names(cols)) {
    dk <- d[d$cluster == k, ]
    p <- plotly::add_markers(p, x = dk$z, y = dk$lag, name = k, marker = list(color = cols[[k]], size = 9, line = list(color = "white", width = 0.6)),
                             text = sprintf("%s<br>%s<br>Own value (standardised): %.2f<br>Neighbours' average: %.2f", dk$country, k, dk$z, dk$lag), hoverinfo = "text")
  }
  p <- plotly::layout(p, xaxis = list(title = paste("Country's own", lc(label), "(standardised)"), zeroline = TRUE, zerolinecolor = "#D1D5DB", gridcolor = "#EDF1EF"),
                      yaxis = list(title = "Average of its neighbours (spatial lag)", zeroline = TRUE, zerolinecolor = "#D1D5DB", gridcolor = "#EDF1EF"),
                      legend = list(orientation = "h", x = 0, y = 1.06), font = list(family = GR$font, size = 12, color = GR$text),
                      margin = list(l = 10, r = 10, t = 10, b = 10), paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  plotly::config(p, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

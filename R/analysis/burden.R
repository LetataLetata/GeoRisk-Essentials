# Double burden: where hazard exposure and social vulnerability coincide.
# Bivariate classes (terciles), bivariate spatial association (Lee's L) and
# summary statistics.

BIVAR_COLS <- c("1-1" = "#E8E8E8", "2-1" = "#E4ACAC", "3-1" = "#C85A5A",
                "1-2" = "#B0D5DF", "2-2" = "#AD9EA5", "3-2" = "#985356",
                "1-3" = "#64ACBE", "2-3" = "#627F8C", "3-3" = "#574249")

# Terciles are a coarse summary and should be stable even when values are
# repeated or heavily concentrated at the same cut point.
tercile <- function(x) {
  x <- as.numeric(x)
  out <- rep(NA_integer_, length(x))
  ok <- !is.na(x)
  if (!any(ok)) return(out)

  q <- stats::quantile(x[ok], probs = c(1 / 3, 2 / 3), na.rm = TRUE,
                       names = FALSE, type = 1)

  if (all(is.na(q)) || isTRUE(all.equal(q[1], q[2], tolerance = .Machine$double.eps^0.5))) {
    out[ok] <- 2L
    return(out)
  }

  # Cut points can be repeated when the distribution is skewed or has ties.
  # If a value falls exactly on a threshold, include.lowest keeps it in the lower
  # class, which is more stable and easier to interpret.
  out[ok] <- as.integer(cut(x[ok], c(-Inf, q, Inf), labels = 1:3,
                            include.lowest = TRUE, right = TRUE))
  out[ok][is.na(out[ok])] <- 2L
  out
}

bivariate_classes <- function(hazard, vuln) {
  h <- tercile(hazard)
  v <- tercile(vuln)
  cls <- ifelse(is.na(h) | is.na(v), NA_character_, paste0(h, "-", v))
  lab <- ifelse(is.na(cls), NA_character_,
                paste0(c("Low", "Medium", "High")[h], " hazard, ",
                       c("low", "medium", "high")[v], " vulnerability"))
  data.frame(h = h, v = v, cls = factor(cls, levels = names(BIVAR_COLS)),
             label = lab, double = !is.na(cls) & cls == "3-3",
             stringsAsFactors = FALSE)
}

# Lee's L (Lee, 2001): combines Pearson correlation with spatial smoothing.
# Large positive L means high hazard and high vulnerability co-occur and
# cluster in the same neighbourhoods.
lee_test <- function(x, y, lw, nsim = 499) {
  ok <- !is.na(x) & !is.na(y)
  lw2 <- subset_lw(lw, ok)
  if (sum(ok) < 3 || nrow(lw2$neighbours) == 0) {
    return(list(L = NA_real_, p = NA_real_, local = rep(NA_real_, length(x)),
                pearson = if (sum(ok) > 1) stats::cor(x[ok], y[ok]) else NA_real_,
                spearman = if (sum(ok) > 1) stats::cor(x[ok], y[ok], method = "spearman") else NA_real_))
  }
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


bivariate_static <- function(store, cls, hz_lab, vu_lab) {
  p <- static_choropleth(store, factor(as.character(cls), levels = names(BIVAR_COLS)), NULL, "factor", BIVAR_COLS) +
    ggplot2::theme(legend.position = "none")
  out <- if (requireNamespace("patchwork", quietly = TRUE))
    p + patchwork::inset_element(bivariate_legend(hz_lab, vu_lab), left = 0.02, bottom = 0.05, right = 0.2, top = 0.38) else p
  attr(out, "export_data") <- data.frame(iso3 = store$inform_sf$iso3, country = store$inform_sf$country, class = as.character(cls))
  out
}


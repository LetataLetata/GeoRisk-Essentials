# Change over time (INFORM 2017-2026): Mann-Kendall trend test and
# Theil-Sen slope per country, and Moran's I per year.

mann_kendall <- function(y, t = seq_along(y)) {
  ok <- !is.na(y); y <- y[ok]; t <- t[ok]; n <- length(y)
  if (n < 4) return(c(S = NA, z = NA, p = NA, tau = NA, sen = NA))
  s <- 0
  for (i in 1:(n - 1)) s <- s + sum(sign(y[(i + 1):n] - y[i]))
  ties <- table(y); ties <- ties[ties > 1]
  v <- (n * (n - 1) * (2 * n + 5) - sum(ties * (ties - 1) * (2 * ties + 5))) / 18
  z <- if (s > 0) (s - 1) / sqrt(v) else if (s < 0) (s + 1) / sqrt(v) else 0
  slopes <- unlist(lapply(1:(n - 1), function(i) (y[(i + 1):n] - y[i]) / (t[(i + 1):n] - t[i])))
  c(S = s, z = z, p = 2 * stats::pnorm(-abs(z)), tau = s / (n * (n - 1) / 2), sen = stats::median(slopes))
}

country_trends <- function(trend, var) {
  d <- trend[trend$var == var, ]
  out <- do.call(rbind, lapply(split(d, d$iso3), function(x) {
    x <- x[order(x$year), ]
    r <- mann_kendall(x$score, x$year)
    data.frame(iso3 = x$iso3[1], first = x$score[1], last = x$score[nrow(x)],
               change = x$score[nrow(x)] - x$score[1], sen_per_year = r[["sen"]],
               tau = r[["tau"]], p = r[["p"]], n_years = sum(!is.na(x$score)))
  }))
  out$direction <- ifelse(out$p < 0.05 & out$sen_per_year > 0, "Rising",
                   ifelse(out$p < 0.05 & out$sen_per_year < 0, "Falling", "No significant trend"))
  out
}

moran_by_year <- function(store, trend, var, w) {
  years <- sort(unique(trend$year))
  do.call(rbind, lapply(years, function(yr) {
    d <- trend[trend$var == var & trend$year == yr, ]
    x <- d$score[match(store$inform_sf$iso3, d$iso3)]
    g <- global_moran(x, w$lw, nsim = 199)
    data.frame(year = yr, I = g$I, p = g$p_perm, z = g$z)
  }))
}

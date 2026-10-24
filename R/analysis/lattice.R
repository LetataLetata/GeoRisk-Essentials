# Lattice (areal) methods for country data: spatial weights, global and local
# autocorrelation, Getis-Ord Gi*, correlograms and spatial regression.
# Plain functions: no Shiny code, so they can be reused in reports.

# Spatial weights between countries. Many countries are islands, so pure
# contiguity leaves gaps; options combine contiguity with nearest neighbours
# or use great-circle distances between country centroids.
country_weights <- function(store, iso3, type = c("knn", "distance", "contiguity"), k = 5, dist_km = 2000) {
  type <- match.arg(type)
  cent <- store$geo$centroids[match(iso3, store$geo$centroids$iso3), ]
  xy <- as.matrix(cent[, c("lon", "lat")])
  if (type == "knn") {
    nb <- spdep::make.sym.nb(spdep::knn2nb(spdep::knearneigh(xy, k = k, longlat = TRUE)))
  } else if (type == "distance") {
    nb <- spdep::dnearneigh(xy, 0, dist_km, longlat = TRUE)
    iso <- which(spdep::card(nb) == 0)
    if (length(iso)) {
      nn <- spdep::knn2nb(spdep::knearneigh(xy, k = 1, longlat = TRUE))
      for (i in iso) nb[[i]] <- nn[[i]]
      nb <- spdep::make.sym.nb(nb)
    }
  } else {
    poly <- sf::st_transform(store$geo$countries[match(iso3, store$geo$countries$iso3), ], EQUAL_EARTH)
    nb <- spdep::poly2nb(poly, queen = TRUE, snap = 20000)
    iso <- which(spdep::card(nb) == 0)
    if (length(iso)) {   # islands get their nearest country as neighbour
      nn <- spdep::knn2nb(spdep::knearneigh(xy, k = 1, longlat = TRUE))
      for (i in iso) nb[[i]] <- nn[[i]]
      nb <- spdep::make.sym.nb(nb)
    }
  }
  list(nb = nb, lw = spdep::nb2listw(nb, style = "W", zero.policy = TRUE), type = type, k = k, dist_km = dist_km,
       mean_links = mean(spdep::card(nb)), n = length(nb))
}


subset_lw <- function(lw, ok) if (all(ok)) lw else spdep::subset.listw(lw, ok, zero.policy = TRUE)

global_moran <- function(x, lw, nsim = 999) {
  ok <- !is.na(x); lw2 <- subset_lw(lw, ok)
  set.seed(2026)
  mc <- spdep::moran.mc(x[ok], lw2, nsim = nsim, zero.policy = TRUE)
  an <- spdep::moran.test(x[ok], lw2, zero.policy = TRUE, randomisation = TRUE)
  gc <- spdep::geary.test(x[ok], lw2, zero.policy = TRUE)
  list(I = unname(mc$statistic), p_perm = mc$p.value, expected = unname(an$estimate[2]),
       variance = unname(an$estimate[3]), z = unname(an$statistic), p_norm = an$p.value,
       geary_c = unname(gc$estimate[1]), geary_p = gc$p.value,
       perm = mc$res[-length(mc$res)], n = sum(ok), nsim = nsim)
}

local_moran <- function(x, lw, alpha = 0.05, adjust = "none") {
  ok <- !is.na(x); lw2 <- subset_lw(lw, ok)
  lm <- spdep::localmoran(x[ok], lw2, zero.policy = TRUE)
  p <- stats::p.adjust(lm[, grep("^Pr\\(", colnames(lm))[1]], method = adjust)
  q <- as.character(attr(lm, "quadr")$mean)
  q[is.na(p) | p >= alpha] <- "Not significant"
  out <- data.frame(Ii = NA_real_, p = NA_real_, cluster = NA_character_, z = NA_real_, lag = NA_real_)[rep(1, length(x)), ]
  z <- as.numeric(scale(x[ok]))
  out$Ii[ok] <- lm[, 1]; out$p[ok] <- p; out$cluster[ok] <- q
  out$z[ok] <- z; out$lag[ok] <- spdep::lag.listw(lw2, z, zero.policy = TRUE)
  out$cluster <- factor(out$cluster, levels = names(GR$lisa_cols))
  rownames(out) <- NULL
  out
}



# ------------------------------------------------------------ regression --
vif <- function(X) {
  if (ncol(X) < 2) return(stats::setNames(rep(1, ncol(X)), colnames(X)))
  vapply(colnames(X), function(v) {
    r2 <- summary(stats::lm(X[, v] ~ X[, setdiff(colnames(X), v), drop = FALSE]))$r.squared
    1 / (1 - r2)
  }, numeric(1))
}

lm_tests <- function(ols, lw) {
  res <- if (exists("lm.RStests", envir = asNamespace("spdep"))) spdep::lm.RStests(ols, lw, test = "all", zero.policy = TRUE)
         else spdep::lm.LMtests(ols, lw, test = "all", zero.policy = TRUE)
  data.frame(test = c("LM error", "LM lag", "Robust LM error", "Robust LM lag", "SARMA")[seq_along(res)],
             statistic = vapply(res, function(t) unname(t$statistic), numeric(1)),
             df = vapply(res, function(t) unname(t$parameter), numeric(1)),
             p_value = vapply(res, function(t) unname(t$p.value), numeric(1)), row.names = NULL)
}

choose_model <- function(tests, alpha = 0.05) {
  sig <- stats::setNames(tests$p_value < alpha, tests$test); st <- stats::setNames(tests$statistic, tests$test)
  if (!sig["LM error"] && !sig["LM lag"]) return(list(model = "OLS", why = "Neither LM test is significant, so OLS is adequate."))
  if (sig["LM lag"] && !sig["LM error"]) return(list(model = "Spatial lag", why = "Only the LM lag test is significant."))
  if (sig["LM error"] && !sig["LM lag"]) return(list(model = "Spatial error", why = "Only the LM error test is significant."))
  if (sig["Robust LM lag"] && !sig["Robust LM error"]) return(list(model = "Spatial lag", why = "Both LM tests are significant; only robust LM lag remains significant."))
  if (sig["Robust LM error"] && !sig["Robust LM lag"]) return(list(model = "Spatial error", why = "Both LM tests are significant; only robust LM error remains significant."))
  if (st["Robust LM lag"] >= st["Robust LM error"]) return(list(model = "Spatial lag", why = "Both robust tests are significant; robust LM lag is larger."))
  list(model = "Spatial error", why = "Both robust tests are significant; robust LM error is larger.")
}

# Diagnostics computed directly so no extra packages are needed.
breusch_pagan <- function(ols) {
  e2 <- stats::residuals(ols)^2; X <- stats::model.matrix(ols)[, -1, drop = FALSE]
  aux <- stats::lm(e2 ~ X); stat <- length(e2) * summary(aux)$r.squared
  c(statistic = stat, df = ncol(X), p = stats::pchisq(stat, ncol(X), lower.tail = FALSE))
}
jarque_bera <- function(e) {
  n <- length(e); s <- mean((e - mean(e))^3) / stats::sd(e)^3; k <- mean((e - mean(e))^4) / stats::sd(e)^4
  stat <- n / 6 * (s^2 + (k - 3)^2 / 4)
  c(statistic = stat, df = 2, p = stats::pchisq(stat, 2, lower.tail = FALSE))
}

spatial_regression <- function(d, response, predictors, lw, standardise = TRUE, alpha = 0.05) {
  vars <- c(response, predictors)
  ok <- stats::complete.cases(d[, vars])
  if (sum(ok) < 30) stop("At least 30 countries with complete data are needed (found ", sum(ok), ").")
  dd <- d[ok, , drop = FALSE]
  if (standardise) for (v in predictors) dd[[v]] <- as.numeric(scale(dd[[v]]))
  lw2 <- subset_lw(lw, ok)
  f <- stats::as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
  ols <- stats::lm(f, data = dd)
  rm <- spdep::lm.morantest(ols, lw2, zero.policy = TRUE)
  tests <- lm_tests(ols, lw2)
  choice <- choose_model(tests, alpha)
  lag <- tryCatch(spatialreg::lagsarlm(f, data = dd, listw = lw2, zero.policy = TRUE, quiet = TRUE), error = function(e) NULL)
  err <- tryCatch(spatialreg::errorsarlm(f, data = dd, listw = lw2, zero.policy = TRUE, quiet = TRUE), error = function(e) NULL)
  ct <- function(fit, name) {
    if (is.null(fit)) return(NULL)
    s <- summary(fit); m <- if (inherits(fit, "lm")) s$coefficients else s$Coef
    data.frame(model = name, term = rownames(m), estimate = m[, 1], se = m[, 2], p = m[, 4],
               lo = m[, 1] - 1.96 * m[, 2], hi = m[, 1] + 1.96 * m[, 2], row.names = NULL)
  }
  coefs <- rbind(ct(ols, "OLS"), ct(lag, "Spatial lag"), ct(err, "Spatial error"))
  rmp <- function(fit) if (is.null(fit)) NA else spdep::moran.test(stats::residuals(fit), lw2, zero.policy = TRUE)$p.value
  fits <- data.frame(model = c("OLS", "Spatial lag", "Spatial error"),
                     AIC = c(stats::AIC(ols), if (is.null(lag)) NA else stats::AIC(lag), if (is.null(err)) NA else stats::AIC(err)),
                     logLik = c(as.numeric(stats::logLik(ols)), if (is.null(lag)) NA else as.numeric(stats::logLik(lag)),
                                if (is.null(err)) NA else as.numeric(stats::logLik(err))),
                     spatial_parameter = c(NA, if (is.null(lag)) NA else lag$rho, if (is.null(err)) NA else err$lambda),
                     residual_moran_p = c(rm$p.value, rmp(lag), rmp(err)))
  impacts <- NULL
  if (!is.null(lag)) {
    im <- tryCatch(spatialreg::impacts(lag, listw = lw2), error = function(e) NULL)
    if (!is.null(im)) impacts <- data.frame(term = attr(im, "bnames"), direct = im$direct, indirect = im$indirect, total = im$total)
  }
  chosen <- switch(choice$model, "OLS" = ols, "Spatial lag" = lag, "Spatial error" = err)
  if (is.null(chosen)) chosen <- ols
  resid <- rep(NA_real_, nrow(d)); resid[ok] <- stats::residuals(chosen)
  diag <- rbind(
    data.frame(test = "Moran's I of OLS residuals", statistic = unname(rm$estimate[1]), p = rm$p.value),
    data.frame(test = "Breusch-Pagan (heteroscedasticity)", statistic = breusch_pagan(ols)[["statistic"]], p = breusch_pagan(ols)[["p"]]),
    data.frame(test = "Jarque-Bera (normality of residuals)", statistic = jarque_bera(stats::residuals(ols))[["statistic"]], p = jarque_bera(stats::residuals(ols))[["p"]]))
  X <- stats::model.matrix(ols)[, -1, drop = FALSE]
  list(formula = f, n = sum(ok), ols = ols, r2 = summary(ols)$r.squared, adj_r2 = summary(ols)$adj.r.squared,
       residual_moran = rm, tests = tests, choice = choice, coefs = coefs, fits = fits,
       impacts = impacts, residuals = resid, vif = vif(X), diagnostics = diag, standardised = standardise)
}


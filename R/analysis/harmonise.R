# Making country scores comparable when data quality differs.
#
# INFORM already puts every indicator on the same 0-10 scale, using the same
# international sources and method for every country. What still differs is
# the quality of the data behind each score: some countries have complete,
# recent statistics; others rely on old, missing or imputed values. INFORM
# measures this with the Lack of Reliability index (LoR, 0-10).
#
# Three complementary ways of putting countries on an equal footing:
#   1. Reliability adjustment (empirical Bayes shrinkage). A score backed by
#      weak data is pulled towards a reference value (the world, regional or
#      neighbourhood average) in proportion to how unreliable it is. Scores
#      backed by good data hardly move. The adjusted INFORM Risk is then
#      recomputed from the adjusted pillars with the INFORM formula.
#   2. Percentile ranks. Each country's position among all countries (0-100).
#      Ranks are unaffected by any distortion that keeps the order of
#      countries, and by skewed distributions.
#   3. Robustness checks. Repeat the key statistics on each version, and on
#      reliable-data countries only, to see whether the conclusions survive.
#
# Assumption (shared with the Confidence page): the error standard deviation
# of a score is max_error * LoR / 10 index points. INFORM does not publish
# error estimates, so this is a stated, adjustable assumption.

error_sd <- function(inform, max_error = 1) {
  lor <- inform$reliability_index
  lor[is.na(lor)] <- stats::median(lor, na.rm = TRUE)
  pmax(max_error * lor / 10, 1e-6)
}

# Empirical Bayes shrinkage of x towards a prior mean.
# prior: "world" (overall mean), "region" (mean of the World Bank region) or
#        "neighbours" (mean of the spatial neighbours, needs lw).
# Between-country variance is estimated by the method of moments.
eb_shrink <- function(x, sd, prior = c("world", "region", "neighbours"), region = NULL, lw = NULL) {
  prior <- match.arg(prior)
  ok <- !is.na(x)
  m <- switch(prior,
    world = rep(mean(x, na.rm = TRUE), length(x)),
    region = stats::ave(x, region, FUN = function(z) rep(mean(z, na.rm = TRUE), length(z))),
    neighbours = {
      xf <- x; xf[!ok] <- mean(x, na.rm = TRUE)
      spdep::lag.listw(lw, xf, zero.policy = TRUE)
    })
  dev <- x - m
  tau2 <- max(stats::var(dev, na.rm = TRUE) - mean(sd[ok]^2), 0.05 * stats::var(dev, na.rm = TRUE))
  B <- sd^2 / (sd^2 + tau2)
  adj <- pmin(10, pmax(0, B * m + (1 - B) * x))
  data.frame(adjusted = adj, shrink = B, prior_mean = m)
}

percentile_rank <- function(x) {
  r <- rank(x, na.last = "keep", ties.method = "average")
  100 * (r - 1) / (sum(!is.na(x)) - 1)
}

# Adjusted pillars and INFORM Risk for every country.
harmonise_inform <- function(inform, max_error = 1, prior = "world", lw = NULL) {
  sd <- error_sd(inform, max_error)
  pill <- c("hazard_and_exposure", "vulnerability", "lack_of_coping_capacity")
  out <- data.frame(iso3 = inform$iso3, country = inform$country, region = inform$wb_region,
                    reliability = inform$reliability_index, sd = sd)
  for (p in pill) {
    e <- eb_shrink(inform[[p]], sd, prior, inform$wb_region, lw)
    out[[p]] <- inform[[p]]; out[[paste0(p, "_adj")]] <- e$adjusted
    if (p == "hazard_and_exposure") out$shrink <- e$shrink
  }
  out$inform_risk <- inform$inform_risk
  out$inform_risk_adj <- (out$hazard_and_exposure_adj * out$vulnerability_adj * out$lack_of_coping_capacity_adj)^(1 / 3)
  out$risk_class <- inform$risk_class
  out$risk_class_adj <- risk_classes(out$inform_risk_adj)
  out$rank <- inform$rank
  out$rank_adj <- rank(-out$inform_risk_adj, ties.method = "min")
  out$change <- out$inform_risk_adj - out$inform_risk
  out
}

# One indicator in every version, aligned with the input rows.
indicator_versions <- function(inform, var, max_error = 1, prior = "world", lw = NULL) {
  if (var == "inform_risk") {
    h <- harmonise_inform(inform, max_error, prior, lw)
    adj <- h$inform_risk_adj; shrink <- h$shrink
  } else {
    e <- eb_shrink(inform[[var]], error_sd(inform, max_error), prior, inform$wb_region, lw)
    adj <- e$adjusted; shrink <- e$shrink
  }
  data.frame(iso3 = inform$iso3, country = inform$country, region = inform$wb_region,
             reliability = inform$reliability_index, published = inform[[var]], adjusted = adj,
             percentile = percentile_rank(inform[[var]]), shrink = shrink, change = adj - inform[[var]])
}

# Do the main conclusions survive? Global Moran's I, local High-High clusters
# and the top-20 list for each version of the data, compared with the
# published scores.
robustness_table <- function(store, var, max_error = 1, prior = "world", lor_cut = 4.5, nsim = 499, adjust = "fdr") {
  inf <- store$inform
  w_all <- country_weights(store, inf$iso3, "knn", 5)
  v <- indicator_versions(inf, var, max_error, prior, w_all$lw)
  rel <- inf$reliability_index <= lor_cut & !is.na(inf$reliability_index)
  w_rel <- country_weights(store, inf$iso3[rel], "knn", 5)

  hh_of <- function(x, lw) { l <- local_moran(x, lw, 0.05, adjust); which(l$cluster %in% "High-High") }
  top_of <- function(x, iso) iso[order(-x)][1:20]
  recall <- function(found, base) if (!length(base)) NA else length(intersect(found, base)) / length(base)

  base_hh <- inf$iso3[hh_of(v$published, w_all$lw)]
  row <- function(name, x, lw, iso, note = "") {
    g <- global_moran(x, lw, nsim)
    hh <- iso[hh_of(x, lw)]; top <- top_of(x, iso)
    # Compare with the published top 20 among the same countries.
    base_top <- top_of(v$published[match(iso, inf$iso3)], iso)
    same_values <- isTRUE(all.equal(x, v$published[match(iso, inf$iso3)]))
    data.frame(Version = name, Countries = sum(!is.na(x)), `Moran's I` = g$I, `p (permutation)` = g$p_perm,
               `High-High clusters` = length(hh), `Published clusters that reappear` = recall(hh, intersect(base_hh, iso)),
               `New clusters` = length(setdiff(hh, base_hh)),
               `Top 20 shared with published` = length(intersect(top, base_top)) / 20,
               `Rank correlation with published` = if (same_values && length(iso) < nrow(inf)) NA else
                 stats::cor(x, v$published[match(iso, inf$iso3)], method = "spearman", use = "complete.obs"),
               Note = note, check.names = FALSE)
  }
  rbind(
    row("Published scores", v$published, w_all$lw, inf$iso3),
    row(sprintf("Reliability-adjusted (%s prior)", prior), v$adjusted, w_all$lw, inf$iso3,
        if (prior == "neighbours") "Clustering inflated by design: scores were pulled towards neighbours" else
        if (prior == "region") "Regional prior can strengthen regional clustering" else ""),
    row("Percentile ranks", v$percentile, w_all$lw, inf$iso3, "Moran's I on ranks: robust to skew and outliers"),
    row(sprintf("Reliable data only (LoR <= %s)", lor_cut), v$published[rel], w_rel$lw, inf$iso3[rel],
        "Weights rebuilt for the subset"))
}

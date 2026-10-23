# How sure are we? Sensitivity of INFORM risk classes to data error.
#
# INFORM Risk is the geometric mean of three pillars (hazard and exposure,
# vulnerability, lack of coping capacity). We perturb each pillar with
# random error whose size grows with the country's Lack of Reliability index
# (sd = max_error * LoR / 10 index points), recompute risk, and record how
# often the country's risk class or rank changes.
#
# This is a sensitivity analysis under a stated assumption, not a measured
# error distribution: INFORM does not publish confidence intervals.

risk_classes <- function(x) cut(x, c(-Inf, 2.35, 3.55, 5.35, 6.95, Inf),
                                labels = c("Very Low", "Low", "Medium", "High", "Very High"))

simulate_risk <- function(inform, max_error = 1, n = 1000, seed = 2026) {
  set.seed(seed)
  h <- inform$hazard_and_exposure; v <- inform$vulnerability; cc <- inform$lack_of_coping_capacity
  sd <- max_error * ifelse(is.na(inform$reliability_index), stats::median(inform$reliability_index, na.rm = TRUE),
                           inform$reliability_index) / 10
  k <- length(h)
  base_cls <- as.character(inform$risk_class)
  sims <- matrix(NA_real_, k, n)
  for (s in seq_len(n)) {
    hp <- pmin(10, pmax(0, h + stats::rnorm(k, 0, sd)))
    vp <- pmin(10, pmax(0, v + stats::rnorm(k, 0, sd)))
    cp <- pmin(10, pmax(0, cc + stats::rnorm(k, 0, sd)))
    sims[, s] <- (hp * vp * cp)^(1 / 3)
  }
  cls <- apply(sims, 2, function(x) as.character(risk_classes(x)))
  ranks <- apply(sims, 2, function(x) rank(-x, ties.method = "min"))
  hi <- c("High", "Very High")
  data.frame(iso3 = inform$iso3, country = inform$country, region = inform$wb_region, risk = inform$inform_risk,
             risk_class = inform$risk_class, reliability = inform$reliability_index, sd = sd,
             risk_lo = apply(sims, 1, stats::quantile, 0.05), risk_hi = apply(sims, 1, stats::quantile, 0.95),
             p_change = rowMeans(cls != base_cls), p_high = rowMeans(matrix(cls %in% hi, k)),
             rank = inform$rank, rank_lo = apply(ranks, 1, stats::quantile, 0.05), rank_hi = apply(ranks, 1, stats::quantile, 0.95),
             stringsAsFactors = FALSE)
}

# Blind spots: high risk but weak evidence, where decisions are most exposed
# to inaccurate estimates.
blind_spots <- function(inform, lor_cut = 5, imputed_cut = 0.2) {
  hi <- inform$risk_class %in% c("High", "Very High")
  weak <- (inform$reliability_index > lor_cut) | (inform$imputed_share > imputed_cut)
  inform[hi & weak %in% TRUE, ]
}

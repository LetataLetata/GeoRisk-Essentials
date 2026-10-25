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
# The implementation below keeps the idea but makes the propagation step more
# stable when reliability is missing or extremely low/high.

risk_classes <- function(x) cut(x, c(-Inf, 2.35, 3.55, 5.35, 6.95, Inf),
                                labels = c("Very Low", "Low", "Medium", "High", "Very High"))

simulate_risk <- function(inform, max_error = 1, n = 1000, seed = 2026) {
  required <- c("hazard_and_exposure", "vulnerability", "lack_of_coping_capacity",
                "reliability_index", "inform_risk", "risk_class", "iso3", "country", "wb_region")
  missing <- setdiff(required, names(inform))
  if (length(missing)) stop(sprintf("simulate_risk() missing columns: %s", paste(missing, collapse = ", ")))

  set.seed(seed)
  h <- inform$hazard_and_exposure
  v <- inform$vulnerability
  cc <- inform$lack_of_coping_capacity

  reliability <- inform$reliability_index
  fallback <- stats::median(reliability, na.rm = TRUE)
  reliability <- ifelse(is.na(reliability), fallback, reliability)
  # Keep the simulation numerically stable; zero or negative reliability would
  # otherwise collapse all simulations to a point estimate.
  reliability <- pmax(reliability, 0.1, na.rm = FALSE)
  sd <- (max_error * reliability) / 10

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
             risk_lo = apply(sims, 1, stats::quantile, 0.05, na.rm = TRUE),
             risk_hi = apply(sims, 1, stats::quantile, 0.95, na.rm = TRUE),
             p_change = rowMeans(cls != base_cls),
             p_high = rowMeans(matrix(cls %in% hi, nrow = nrow(cls), ncol = ncol(cls))),
             rank = inform$rank,
             rank_lo = apply(ranks, 1, stats::quantile, 0.05, na.rm = TRUE),
             rank_hi = apply(ranks, 1, stats::quantile, 0.95, na.rm = TRUE),
             stringsAsFactors = FALSE)
}

# Blind spots: high risk but weak evidence, where decisions are most exposed
# to inaccurate estimates. The logical check is left explicit so it is easier to
# review and explain. 
blind_spots <- function(inform, lor_cut = 5, imputed_cut = 0.2) {
  hi <- inform$risk_class %in% c("High", "Very High")
  weak <- (inform$reliability_index > lor_cut) | (inform$imputed_share > imputed_cut)
  inform[which(hi & weak), , drop = FALSE]
}


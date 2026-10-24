# App-wide cache. Results that depend only on the data and the chosen setting
# (for example Moran's I for one variable) are computed once and shared by
# every page and every visitor, instead of being recalculated each time.
# cached("key", expression): the expression is evaluated only the first time.

APP_CACHE <- new.env()

cached <- function(key, expr) {
  if (!exists(key, envir = APP_CACHE, inherits = FALSE)) assign(key, expr, envir = APP_CACHE)
  get(key, envir = APP_CACHE, inherits = FALSE)
}

# Shared building blocks used by several pages.
cached_check <- function(store, vars, labels = vars, d = NULL, tag = "inform") {
  cached(paste("chk", tag, paste(vars, collapse = "+"), sep = ":"), analysis_data(store, vars, labels, d = d))
}

# Global Moran's I for every INFORM variable (Where page), each on the
# countries with data for that variable.
moran_all_variables <- function(store, groups = c("Risk and its three parts", "Hazards", "Vulnerability", "Coping capacity")) {
  vv <- store$vars$var[store$vars$group %in% groups]
  do.call(rbind, lapply(vv, function(v) {
    c0 <- tryCatch(cached_check(store, v, var_label(store, v)), error = function(e) NULL); if (is.null(c0)) return(NULL)
    g <- global_moran(c0$data[[v]], c0$w$lw, 499)
    data.frame(var = v, label = var_label(store, v), group = store$vars$group[store$vars$var == v], I = g$I, p = g$p_perm, n = c0$n)
  }))
}

# Run once when the app starts, so the first visitor does not wait for the
# default results of each page. Later settings are cached on first use.
warm_cache <- function(store) {
  t0 <- Sys.time()
  ok <- tryCatch({
    cw <- cached_check(store, "inform_risk", "INFORM Risk")
    cached("gm:inform_risk", global_moran(cw$data$inform_risk, cw$w$lw, 999))
    cached("lisa:inform_risk", local_moran(cw$data$inform_risk, cw$w$lw, 0.05, "fdr"))
    cached("where:all_moran", moran_all_variables(store))
    cy <- cached_check(store, c("inform_risk", "log_gdp_pc"), c("INFORM Risk", "GDP per person"), d = store$all, tag = "all")
    cached("fit:inform_risk", spatial_regression(cy$data, "inform_risk", "log_gdp_pc", cy$w$lw, standardise = TRUE))
    cs <- cached_check(store, c("inform_risk", "hazard_and_exposure", "vulnerability", "lack_of_coping_capacity", "reliability_index"),
                       c("INFORM Risk", "hazard and exposure", "vulnerability", "lack of coping capacity", "the Lack of Reliability index"))
    cached("sure:sim", simulate_risk(cs$data, 1, 1000))
    cached("sure:rob", robustness_table(subset_store(store, cs$data$iso3), "inform_risk", 1, "world", 4.5))
    ca <- cached_check(store, c("hazard_and_exposure", "vulnerability"), c("All hazards", "vulnerability"))
    cached("lee:hazard_and_exposure", lee_test(ca$data$hazard_and_exposure, ca$data$vulnerability, ca$w$lw))
    TRUE
  }, error = function(e) { message("Cache warm-up skipped: ", conditionMessage(e)); FALSE })
  if (ok) message(sprintf("Results prepared in %.1f seconds.", as.numeric(Sys.time() - t0, units = "secs")))
  invisible(ok)
}

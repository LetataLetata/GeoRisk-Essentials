# Data store: loads every processed dataset once at start-up.
# A missing or broken file becomes NULL, and views that need it show a
# "data not available" note instead of failing.

load_store <- function(dir = "data") {
  rd <- function(f) tryCatch(readRDS(file.path(dir, f)), error = function(e) { message("Could not load ", f); NULL })
  s <- list(
    inform = rd("inform.rds"), dict = rd("inform_dictionary.rds"), quality = rd("inform_quality.rds"),
    trend = rd("inform_trend.rds"), owid = rd("owid_energy.rds"), geo = rd("geography.rds"),
    # The measured facts behind the scores, and recorded earthquakes (scripts/prepare_indicators.R).
    indicators = rd("inform_indicators.rds"), quakes = rd("earthquakes.rds")
  )
  # Comparable versions (R/analysis/harmonise.R): reliability-adjusted scores
  # with a world-average prior, and percentile ranks.
  if (!is.null(s$inform) && exists("harmonise_inform")) {
    h <- tryCatch(harmonise_inform(s$inform), error = function(e) { message("Harmonisation failed: ", conditionMessage(e)); NULL })
    if (!is.null(h)) for (v in c("inform_risk", "hazard_and_exposure", "vulnerability", "lack_of_coping_capacity")) {
      s$inform[[paste0(v, "_adj")]] <- h[[paste0(v, "_adj")]]
      s$inform[[paste0(v, "_pct")]] <- percentile_rank(s$inform[[v]])
    }
  }
  if (!is.null(s$inform) && !is.null(s$geo)) {
    s$inform_sf <- merge(s$geo$display, s$inform[, setdiff(names(s$inform), "country")], by = "iso3")
    s$labels <- stats::setNames(s$dict$label, s$dict$var)
    s$labels <- c(s$labels, reliability_index = "Lack of reliability (INFORM)", missing_share = "Missing indicators (share)",
                  mean_data_age_years = "Mean data age (years)", imputed_share = "Imputed indicators (share)",
                  inform_risk_adj = "INFORM Risk, reliability-adjusted", hazard_and_exposure_adj = "Hazard and exposure, reliability-adjusted",
                  vulnerability_adj = "Vulnerability, reliability-adjusted", lack_of_coping_capacity_adj = "Lack of coping capacity, reliability-adjusted",
                  inform_risk_pct = "INFORM Risk, percentile rank", hazard_and_exposure_pct = "Hazard and exposure, percentile rank",
                  vulnerability_pct = "Vulnerability, percentile rank", lack_of_coping_capacity_pct = "Lack of coping capacity, percentile rank")
  }
  # One table of every analysis variable, the continent of each country, and the variable dictionary.
  if (!is.null(s$inform_sf)) {
    s$labels <- c(s$labels, gdp_per_capita = "GDP per person", electricity_per_capita = "Electricity per person",
                  energy_per_capita = "Energy use per person", renewables_share_elec = "Renewable share of electricity",
                  population = "Population", log_gdp_pc = "GDP per person (log10)")
    s$all <- build_all_variables(s)
    s$inform_sf$continent <- s$all$continent
    s$inform$continent <- s$all$continent[match(s$inform$iso3, s$all$iso3)]
    s$vars <- VARIABLE_INFO
    s$vars$label <- unname(s$labels[s$vars$var])
  }
  s$sources <- list(
    inform = "INFORM Risk Index Mid-2026 (v0.7.3), European Commission Joint Research Centre",
    trend = "INFORM Risk Index trend 2017-2026, European Commission Joint Research Centre",
    owid = "Our World in Data, energy dataset (Energy Institute, Ember, EIA and others)",
    boundaries = "Natural Earth 1:50m admin-0 boundaries",
    basemaps = "Background maps: CARTO, OpenStreetMap contributors, Esri World Imagery, OpenTopoMap (free tile services)",
    indicators = "INFORM Risk Mid-2026 indicator data (EC JRC), compiled from GEM, UNDRR, EM-DAT (CRED), FAO, WHO, UNHCR, IDMC, UNDP, World Bank and others",
    quakes = "Significant earthquakes M5.5 and stronger, 1965-2016 (US Geological Survey catalogue)",
    facts = "Country facts: REST Countries (restcountries.com) and Wikipedia page summaries, fetched live when online"
  )
  s
}

var_label <- function(store, v) {
  l <- unname(store$labels[v])
  ifelse(is.na(l), v, l)
}

# Variable choices grouped by pillar for select inputs.
var_choices <- function(store, include_quality = FALSE, include_comparable = FALSE) {
  d <- store$dict
  out <- lapply(split(d, factor(d$pillar, levels = c("Risk", "Hazard", "Vulnerability", "Coping"))),
                function(x) stats::setNames(x$var, x$label))
  if (include_quality) out[["Data quality"]] <- c("Lack of reliability (INFORM)" = "reliability_index",
                                                  "Mean data age (years)" = "mean_data_age_years",
                                                  "Imputed indicators (share)" = "imputed_share")
  if (include_comparable && "inform_risk_adj" %in% names(store$inform)) {
    pv <- c("inform_risk", "hazard_and_exposure", "vulnerability", "lack_of_coping_capacity")
    out[["Comparable: adjusted for data reliability"]] <- stats::setNames(paste0(pv, "_adj"), unname(store$labels[paste0(pv, "_adj")]))
    out[["Comparable: percentile rank (0-100)"]] <- stats::setNames(paste0(pv, "_pct"), unname(store$labels[paste0(pv, "_pct")]))
  }
  out
}

need_data <- function(store, ...) {
  miss <- c(...)[vapply(c(...), function(k) is.null(store[[k]]), logical(1))]
  if (length(miss)) stop("Data not available: ", paste(miss, collapse = ", "), ". Run scripts/prepare_data.R.")
  invisible(TRUE)
}

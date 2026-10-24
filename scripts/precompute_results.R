# GEORISK Observatory: precompute every analysis result the pages can show.
#
# The dashboard computes results on first use and keeps them (R/core/cache.R).
# Running this script once computes all of them ahead of time and saves them
# in data/results_cache.rds, which the app loads at start-up. Then nothing is
# calculated live while presenting: every option on Where, Why and Where to
# act, and the How sure results, appear instantly.
#
# The file records a fingerprint of the data; if the data change, the app
# ignores the old file and computes results on first use instead. Run again
# after changing the data or the analysis code:
#
#   Rscript scripts/precompute_results.R      (from the app folder)

suppressPackageStartupMessages(library(shiny))
sf::sf_use_s2(FALSE)
for (f in c(list.files("R/core", full.names = TRUE), list.files("R/analysis", full.names = TRUE))) source(f, local = FALSE)
store <- load_store("data")
t0 <- Sys.time()
say <- function(...) cat(sprintf("[%5.0fs] ", as.numeric(Sys.time() - t0, units = "secs")), ..., "\n", sep = "")

# Defaults first (same code the app runs at start-up).
warm_cache(store, use_file = FALSE)

# Where: Moran's I and local clusters for every INFORM variable.
groups <- c("Risk and its three parts", "Hazards", "Vulnerability", "Coping capacity")
for (v in store$vars$var[store$vars$group %in% groups]) {
  c0 <- cached_check(store, v, var_label(store, v))
  cached(paste0("gm:", v), global_moran(c0$data[[v]], c0$w$lw, 999))
  cached(paste0("lisa:", v), local_moran(c0$data[[v]], c0$w$lw, 0.05, "fdr"))
}
say("Where: ", length(ls(APP_CACHE, pattern = "^gm:")), " variables")

# Why: the regression for every outcome on the page.
for (v in store$vars$var[store$vars$group %in% c("Risk and its three parts", "Vulnerability", "Coping capacity")]) {
  c0 <- cached_check(store, c(v, "log_gdp_pc"), c(var_label(store, v), "GDP per person"), d = store$all, tag = "all")
  cached(paste0("fit:", v), suppressWarnings(spatial_regression(c0$data, v, "log_gdp_pc", c0$w$lw, standardise = TRUE)))
}
say("Why: ", length(ls(APP_CACHE, pattern = "^fit:")), " outcomes")

# Where to act: Lee's L for every hazard choice.
for (h in c("hazard_and_exposure", "natural", "drought", "river_flood", "coastal_flood", "tropical_cyclone", "earthquake",
            "tsunami", "epidemic", "current_conflict_intensity", "projected_conflict_probability", "human")) {
  c0 <- cached_check(store, c(h, "vulnerability"), c(var_label(store, h), "vulnerability"))
  cached(paste0("lee:", h), lee_test(c0$data[[h]], c0$data$vulnerability, c0$w$lw))
}
say("Where to act: ", length(ls(APP_CACHE, pattern = "^lee:")), " hazards")

save_results_cache(store)
say("Saved ", RESULTS_FILE, " (", format(structure(file.size(RESULTS_FILE), class = "object_size"), units = "KB"), ")")

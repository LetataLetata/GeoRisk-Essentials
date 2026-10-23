# Missing-data contingency.
#
# Before each analysis runs, the countries are checked for the variables that
# analysis needs. Only countries with a value for every one of them are
# analysed (complete-case analysis), and the spatial weights are rebuilt for
# exactly those countries, so a country with missing data never enters a
# statistic, not even as somebody's neighbour. Excluded countries are kept on
# the maps in grey and listed on the page.
#
# Variables each analysis needs:
#   Where         the indicator being mapped
#   Why           the outcome and GDP per person
#   How sure      the three INFORM pillars, INFORM Risk and Lack of Reliability
#   Where to act  the chosen hazard and vulnerability

check_complete <- function(d, vars, labels = vars) {
  labels <- stats::setNames(labels, vars)
  miss <- is.na(as.matrix(d[, vars, drop = FALSE])) |
          !is.finite(suppressWarnings(as.matrix(d[, vars, drop = FALSE])))
  ok <- rowSums(miss) == 0
  why <- if (any(!ok)) apply(miss[!ok, , drop = FALSE], 1, function(r) paste(labels[vars[r]], collapse = ", ")) else character(0)
  list(ok = ok, n = sum(ok), total = nrow(d), vars = vars,
       excluded = data.frame(iso3 = d$iso3[!ok], country = d$country[!ok], missing = unname(why), stringsAsFactors = FALSE))
}

# d: a data frame in the same row order as store$inform_sf (defaults to the
# INFORM attributes). Returns the complete rows, weights for them, and
# expand(), which puts a result back in the order of all countries with NA for
# the excluded ones (for maps).
analysis_data <- function(store, vars, labels = vars, d = NULL, k = 5) {
  if (is.null(d)) d <- sf::st_drop_geometry(store$inform_sf)
  chk <- check_complete(d, vars, labels)
  if (chk$n < 30) stop(sprintf("Only %d countries have complete data for this analysis; at least 30 are needed.", chk$n))
  chk$data <- d[chk$ok, , drop = FALSE]
  chk$w <- country_weights(store, chk$data$iso3, "knn", k)
  idx <- match(seq_len(nrow(d)), which(chk$ok))
  chk$expand <- function(x) x[idx]
  chk
}

# A copy of the store holding only the given countries, for functions that
# take the whole store (such as robustness_table).
subset_store <- function(store, iso) {
  s <- store
  s$inform <- store$inform[store$inform$iso3 %in% iso, , drop = FALSE]
  s$inform_sf <- store$inform_sf[store$inform_sf$iso3 %in% iso, ]
  s
}

# Note shown under the answer on each page.
data_note <- function(chk, on_map = TRUE) {
  if (!nrow(chk$excluded))
    return(div(class = "data-note ok", icon("circle-check"),
               sprintf(" All %d countries have complete data for this analysis.", chk$total)))
  ex <- chk$excluded[order(chk$excluded$country), ]
  groups <- split(ex$country, ex$missing)
  tags$details(class = "data-note",
    tags$summary(icon("filter"), sprintf(" This analysis uses %d of %d countries. %d excluded because of missing data%s. Click to see which.",
                                         chk$n, chk$total, nrow(ex), if (on_map) " (grey on the maps)" else "")),
    div(class = "data-note-body", lapply(names(groups), function(g)
      p(strong(sprintf("Missing %s: ", lc(g))), paste(groups[[g]], collapse = ", ")))))
}

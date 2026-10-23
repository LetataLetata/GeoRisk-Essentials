# General country facts from two free web services that need no key:
#   REST Countries (https://restcountries.com)  capital, area, languages,
#                                               currencies, borders, flag
#   Wikipedia REST API (https://en.wikipedia.org/api/rest_v1)  short summary
# Results are cached for the session. If the internet is not available the
# functions return NULL quickly and the dashboard shows only its own data.

FACTS_CACHE <- new.env()
FACTS_STATE <- new.env()
FACTS_STATE$offline_until <- Sys.time() - 1

fetch_json <- function(u, timeout = 5) {
  if (Sys.time() < FACTS_STATE$offline_until) return(NULL)
  res <- tryCatch({
    if (requireNamespace("curl", quietly = TRUE)) {
      h <- curl::new_handle(useragent = "GeoRisk-Observatory/1.0 (R Shiny student dashboard)",
                            timeout = timeout, connecttimeout = timeout)
      r <- curl::curl_fetch_memory(u, handle = h)
      if (r$status_code != 200) stop("HTTP ", r$status_code)
      jsonlite::fromJSON(rawToChar(r$content), simplifyVector = TRUE)
    } else {
      old <- options(timeout = timeout); on.exit(options(old))
      jsonlite::fromJSON(u, simplifyVector = TRUE)
    }
  }, error = function(e) e)
  if (inherits(res, "error")) {
    # A connection failure (not a missing page) means we are offline: stop trying for 10 minutes.
    if (!grepl("HTTP 4", conditionMessage(res))) FACTS_STATE$offline_until <- Sys.time() + 600
    return(NULL)
  }
  res
}

country_facts <- function(iso3, fallback_name = NULL) {
  key <- paste0("facts_", iso3)
  if (exists(key, envir = FACTS_CACHE)) return(get(key, envir = FACTS_CACHE))
  fields <- "name,capital,subregion,area,languages,currencies,borders,flags,timezones,maps"
  j <- fetch_json(sprintf("https://restcountries.com/v3.1/alpha/%s?fields=%s", iso3, fields))
  out <- NULL
  if (!is.null(j)) {
    if (is.data.frame(j)) j <- as.list(j[1, ])
    flat <- function(x) unique(unlist(x, use.names = FALSE))
    cur <- j$currencies
    out <- list(
      common = flat(j$name$common)[1],
      official = flat(j$name$official)[1],
      capital = flat(j$capital),
      subregion = flat(j$subregion)[1],
      area = suppressWarnings(as.numeric(flat(j$area)[1])),
      languages = flat(j$languages),
      currencies = if (length(cur)) vapply(names(cur), function(k) {
        nm <- flat(cur[[k]]$name)[1]; sy <- flat(cur[[k]]$symbol)[1]
        paste0(nm, " (", k, if (!is.null(sy) && !is.na(sy)) paste0(", ", sy), ")")
      }, character(1)) else character(0),
      borders = flat(j$borders),
      flag = flat(j$flags$png)[1],
      flag_alt = flat(j$flags$alt)[1],
      timezones = flat(j$timezones),
      osm = flat(j$maps$openStreetMaps)[1]
    )
  }
  # Only cache successes, so a country fetched while offline is retried later.
  if (!is.null(out)) assign(key, out, envir = FACTS_CACHE)
  out
}

country_summary <- function(name, alt_name = NULL) {
  key <- paste0("wiki_", name)
  if (exists(key, envir = FACTS_CACHE)) return(get(key, envir = FACTS_CACHE))
  out <- NULL
  for (nm in unique(c(alt_name, name))) {
    if (is.null(nm) || is.na(nm) || !nzchar(nm)) next
    title <- utils::URLencode(gsub(" ", "_", nm), reserved = TRUE)
    j <- fetch_json(paste0("https://en.wikipedia.org/api/rest_v1/page/summary/", title))
    if (!is.null(j) && !is.null(j$extract) && nzchar(j$extract) && !identical(j$type, "disambiguation")) {
      out <- list(text = j$extract, url = j$content_urls$desktop$page, title = j$title)
      break
    }
  }
  if (!is.null(out)) assign(key, out, envir = FACTS_CACHE)
  out
}

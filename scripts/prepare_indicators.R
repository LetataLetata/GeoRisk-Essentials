# GEORISK Observatory: the measured facts behind each INFORM score.
#
# INFORM scores (0-10) are built from raw indicators: people exposed to
# floods each year, people affected by drought, people affected by
# disasters in recent years, displaced people, child mortality and so on.
# This script takes those raw values from the INFORM workbook, with their
# unit, year and source, and saves them for the Explore page, which uses
# them to explain why a country gets its rating. It also copies the record
# of significant earthquakes (M5.5+, 1965-2016), which lists real events.
#
# Inputs (not stored in this repository because of their size):
#   raw/INFORM_Risk_Mid_2026_v073.xlsx   INFORM Risk Mid-2026 workbook (EC JRC)
#   raw/earthquakes.rds                  earthquakes with the nearest country
#                                        within 100 km (from the full GeoRisk app)
# Output:
#   data/inform_indicators.rds
#   data/earthquakes.rds
#
# Run from the app folder:  Rscript scripts/prepare_indicators.R [raw folder]

args <- commandArgs(trailingOnly = TRUE)
raw <- if (length(args)) args[1] else "raw"
xl <- file.path(raw, "INFORM_Risk_Mid_2026_v073.xlsx")
stopifnot(file.exists(xl))

read_sheet <- function(sheet) suppressMessages(readxl::read_excel(xl, sheet = sheet, col_names = FALSE, .name_repair = "minimal"))
dat <- read_sheet("Indicator Data")
yrs <- read_sheet("Indicator Date")
src <- read_sheet("Indicator Source")

# Header rows are found by their label in the first column (the reader drops
# blank rows at the top, so fixed row numbers would be off).
hrow <- function(d, label) which(trimws(as.character(d[[1]])) == label)[1]
r_name <- hrow(dat, "COUNTRY"); r_year <- hrow(dat, "Survey Year"); r_id <- hrow(dat, "IndicatorId"); r_unit <- hrow(dat, "Unit of Measurament")
cols <- 3:ncol(dat)
ids <- as.character(unlist(dat[r_id, cols])); keep <- !is.na(ids) & nzchar(ids)
cols <- cols[keep]; ids <- ids[keep]
meta <- data.frame(id = ids, name = trimws(as.character(unlist(dat[r_name, cols]))), unit = as.character(unlist(dat[r_unit, cols])),
                   years = as.character(unlist(dat[r_year, cols])), stringsAsFactors = FALSE)

rows <- which(!is.na(dat[[2]]) & grepl("^[A-Z]{3}$", dat[[2]]))
iso <- dat[[2]][rows]
num <- function(x) suppressWarnings(as.numeric(x))
values <- sapply(cols, function(j) num(unlist(dat[rows, j])))
colnames(values) <- ids; rownames(values) <- iso

# Year of each value, per country (sheet "Indicator Date", same layout, ids on row 4).
y_ids <- as.character(unlist(yrs[hrow(yrs, "IndicatorId"), ])); y_rows <- match(iso, yrs[[2]])
year <- sapply(ids, function(k) { j <- match(k, y_ids); if (is.na(j)) rep(NA, length(iso)) else as.character(unlist(yrs[y_rows, j])) })
rownames(year) <- iso

# Source: the most common provider per indicator. The source sheet has the
# same columns as the data sheet but may stop earlier; check the header name.
s_rows <- which(grepl("^[A-Z]{3}$", src[[2]]))
s_names <- trimws(as.character(unlist(src[hrow(src, "COUNTRY"), ])))
source_of <- vapply(seq_along(cols), function(i) {
  j <- cols[i]
  if (j > ncol(src) || !identical(s_names[j], meta$name[i])) return(NA_character_)
  v <- as.character(unlist(src[s_rows, j])); v <- v[!is.na(v) & nzchar(v) & !grepl("^=", v)]
  if (length(v)) names(sort(table(v), decreasing = TRUE))[1] else NA_character_
}, character(1))
meta$source <- source_of

out <- list(meta = meta, values = values, year = year,
            note = "INFORM Risk Mid-2026 (v0.7.3), sheet 'Indicator Data'. European Commission Joint Research Centre.")
saveRDS(out, "data/inform_indicators.rds")
cat("Saved data/inform_indicators.rds:", nrow(values), "countries x", ncol(values), "indicators\n")

eq_in <- file.path(raw, "earthquakes.rds")
if (file.exists(eq_in)) {
  eq <- readRDS(eq_in)
  # All events are kept: the world point pattern uses every one; iso3 (the nearest
  # country within 100 km) is empty for events far out at sea.
  eq <- eq[, c("date", "year", "lat", "lon", "magnitude", "iso3", "dist_to_land_km")]
  saveRDS(eq, "data/earthquakes.rds")
  cat("Saved data/earthquakes.rds:", nrow(eq), "earthquakes,", sum(!is.na(eq$iso3)), "linked to a country\n")
}

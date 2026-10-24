# GEORISK Observatory: build the processed data used by the app.
#
# Reads the raw files in raw/ and writes one .rds file per dataset to data/.
# The app only reads data/, so this script is the single place where raw
# formats are handled. Rerun it whenever a raw file is replaced.
#
# Run from the GeoRisk folder:   Rscript scripts/prepare_data.R
#
# Raw inputs
#   raw/INFORM_Risk_Mid_2026_v073.xlsx          INFORM Risk Index, mid-2026 release
#   raw/INFORM2026_TREND_2017_2026_v72_ALL.xlsx INFORM indicators 2017-2026
#   raw/owid_energy.csv                         Our World in Data energy dataset
#   raw/emdat_country_profile_zaf.xlsx          EM-DAT country profile, South Africa
#   raw/ne_50m_admin_0_countries.geojson        Natural Earth country boundaries
#   raw/earthquakes_1965_2016.csv               Significant earthquakes M5.5+ (USGS-derived)
#   raw/volcanoes_holocene.csv                  Holocene volcanoes (Smithsonian GVP-derived)
#   fields::NorthAmericanRainfall               GHCN v2 summer rainfall, 1720 stations

suppressPackageStartupMessages({ library(readxl); library(sf) })
sf_use_s2(FALSE)
dir.create("data", showWarnings = FALSE)

snake <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("&", "and", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}
num <- function(x) suppressWarnings(as.numeric(as.character(x)))

# readxl drops leading empty rows, so header rows are found by content
# rather than by position. This also survives small layout changes
# between INFORM releases.
read_raw <- function(sheet) read_excel(inform_file, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
find_row <- function(raw, pattern, col = 1) {
  v <- toupper(trimws(as.character(unlist(raw[[col]]))))
  i <- which(v == toupper(pattern))[1]
  if (is.na(i)) stop("Row '", pattern, "' not found")
  i
}

inform_file <- "raw/INFORM_Risk_Mid_2026_v073.xlsx"

# ------------------------------------------------------------ INFORM -----
raw <- read_raw("INFORM Risk 2026 (a-z)")
h <- find_row(raw, "COUNTRY")
hdr <- as.character(unlist(raw[h, ]))
inform <- as.data.frame(raw[-(1:(h + 1)), ], stringsAsFactors = FALSE)
names(inform) <- snake(hdr)
inform <- inform[!is.na(inform$iso3) & nchar(inform$iso3) == 3, ]
names(inform)[names(inform) == "lack_of_reliability"] <- "reliability_index"
names(inform)[names(inform) == "of_missing_indicators"] <- "missing_share"
chr_cols <- c("country", "iso3", "risk_class")
for (v in setdiff(names(inform), chr_cols)) inform[[v]] <- num(inform[[v]])
inform$risk_class <- factor(inform$risk_class, levels = c("Very Low", "Low", "Medium", "High", "Very High"))

rr <- read_raw("Regions"); h <- find_row(rr, "COUNTRY")
reg <- as.data.frame(rr[-(1:h), ], stringsAsFactors = FALSE); names(reg) <- snake(as.character(unlist(rr[h, ])))
reg <- reg[!is.na(reg$iso3), c("iso3", "wb_region", "wb_incomegroup", "un_ocha_region")]
inform <- merge(inform, reg, by = "iso3", all.x = TRUE)

pop <- read_raw("Population Data"); h <- find_row(pop, "Reference Year")
yrs <- num(unlist(pop[h, ]))
last_col <- max(which(!is.na(yrs)))
pop_df <- data.frame(iso3 = unlist(pop[-(1:h), 2]), population = num(unlist(pop[-(1:h), last_col])))
pop_df <- pop_df[!is.na(pop_df$iso3), ]
inform <- merge(inform, pop_df, by = "iso3", all.x = TRUE)
attr(inform, "population_year") <- yrs[last_col]

# Column dictionary: pillar structure, used for labels and to stop
# circular regressions (an index explained by its own components).
dict <- data.frame(
  var = c("inform_risk", "hazard_and_exposure", "natural", "earthquake", "river_flood", "tsunami",
          "tropical_cyclone", "coastal_flood", "drought", "epidemic", "human",
          "projected_conflict_probability", "current_conflict_intensity",
          "vulnerability", "socio_economic_vulnerability", "development_and_deprivation", "inequality",
          "economic_dependency", "vulnerable_groups", "uprooted_people", "health_conditions",
          "children_u5", "recent_shocks", "food_security", "other_vulnerable_groups",
          "lack_of_coping_capacity", "institutional", "drr", "governance", "infrastructure",
          "communication", "physical_infrastructure", "access_to_health_care"),
  label = c("INFORM Risk", "Hazard and exposure", "Natural hazard", "Earthquake", "River flood", "Tsunami",
            "Tropical cyclone", "Coastal flood", "Drought", "Epidemic", "Human hazard",
            "Projected conflict probability", "Current conflict intensity",
            "Vulnerability", "Socio-economic vulnerability", "Development and deprivation", "Inequality",
            "Economic dependency", "Vulnerable groups", "Uprooted people", "Health conditions",
            "Children under 5", "Recent shocks", "Food security", "Other vulnerable groups",
            "Lack of coping capacity", "Institutional", "Disaster risk reduction", "Governance", "Infrastructure",
            "Communication", "Physical infrastructure", "Access to health care"),
  parent = c(NA, "inform_risk", "hazard_and_exposure", "natural", "natural", "natural", "natural", "natural",
             "natural", "natural", "hazard_and_exposure", "human", "human",
             "inform_risk", "vulnerability", "socio_economic_vulnerability", "socio_economic_vulnerability",
             "socio_economic_vulnerability", "vulnerability", "vulnerable_groups", "other_vulnerable_groups",
             "other_vulnerable_groups", "other_vulnerable_groups", "other_vulnerable_groups", "vulnerable_groups",
             "inform_risk", "lack_of_coping_capacity", "institutional", "institutional", "lack_of_coping_capacity",
             "infrastructure", "infrastructure", "infrastructure"),
  pillar = c("Risk", rep("Hazard", 12), rep("Vulnerability", 12), rep("Coping", 8)),
  stringsAsFactors = FALSE)
dict <- dict[dict$var %in% names(inform), ]

# Thresholds used by INFORM for the five classes.
thresholds <- data.frame(class = c("Very High", "High", "Medium", "Low", "Very Low"),
                         min = c(7, 5.4, 3.6, 2.4, 0), max = c(10, 6.9, 5.3, 3.5, 2.3))   # INFORM Risk class thresholds

# --------------------------------------------------- INFORM data quality --
imp <- read_excel(inform_file, sheet = "Imputed and missing data hidden")
imp <- as.data.frame(imp)
imp <- imp[!is.na(imp$ISO3) & nchar(imp$ISO3) == 3, ]
ind_cols <- setdiff(names(imp)[-1], names(imp)[grepl("MISSING", toupper(names(imp)))])   # drop summary columns
imputed_long <- do.call(rbind, lapply(ind_cols, function(ind) {
  data.frame(iso3 = imp$ISO3, indicator = ind, imputed = num(imp[[ind]]) >= 1)
}))

dt <- read_raw("Indicator Date"); h <- find_row(dt, "COUNTRY"); hi <- find_row(dt, "IndicatorId")
ind_names <- as.character(unlist(dt[h, ]))
ind_ids <- as.character(unlist(dt[hi, ]))
dt_body <- as.data.frame(dt[-(1:hi), ], stringsAsFactors = FALSE)
dates_long <- do.call(rbind, lapply(3:ncol(dt_body), function(j) {
  data.frame(iso3 = dt_body[[2]], indicator = ind_names[j], indicator_id = ind_ids[j], data_year = num(dt_body[[j]]))
}))
dates_long <- dates_long[!is.na(dates_long$iso3) & nchar(dates_long$iso3) == 3, ]

src <- read_raw("Indicator Source"); h <- find_row(src, "COUNTRY"); hu <- find_row(src, "Unit of Measurament")
src_names <- as.character(unlist(src[h, ]))
src_body <- as.data.frame(src[-(1:hu), ], stringsAsFactors = FALSE)
sources_long <- do.call(rbind, lapply(3:ncol(src_body), function(j) {
  data.frame(iso3 = src_body[[2]], indicator = src_names[j], source = as.character(src_body[[j]]))
}))
sources_long <- sources_long[!is.na(sources_long$iso3) & nchar(sources_long$iso3) == 3, ]

mr <- read_raw("Indicator Metadata"); h <- find_row(mr, "Dimension")
meta <- as.data.frame(mr[-(1:h), ], stringsAsFactors = FALSE); names(meta) <- snake(as.character(unlist(mr[h, ])))
meta <- meta[!is.na(meta$indicator_name), ]

# Mean age of the underlying indicator data per country (years before 2026).
age <- stats::aggregate(data_year ~ iso3, data = dates_long[!is.na(dates_long$data_year) & dates_long$data_year > 1990, ],
                        FUN = function(y) mean(2026 - y))
names(age)[2] <- "mean_data_age_years"
inform <- merge(inform, age, by = "iso3", all.x = TRUE)
imp_share <- stats::aggregate(imputed ~ iso3, data = imputed_long, FUN = mean)
names(imp_share)[2] <- "imputed_share"
inform <- merge(inform, imp_share, by = "iso3", all.x = TRUE)

inform_quality <- list(imputed = imputed_long, dates = dates_long, sources = sources_long,
                       metadata = as.data.frame(meta), thresholds = thresholds,
                       release = "INFORM Risk Mid-2026 (v0.7.3, 1 September 2026)")

# ------------------------------------------------------- INFORM trend ----
tr <- read_excel("raw/INFORM2026_TREND_2017_2026_v72_ALL.xlsx")
names(tr) <- snake(names(tr))
keep_ids <- c(INFORM = "inform_risk", HA = "hazard_and_exposure", VU = "vulnerability",
              CC = "lack_of_coping_capacity", HA.NAT = "natural", HA.HUM = "human",
              VU.SEV = "socio_economic_vulnerability", VU.VGR = "vulnerable_groups",
              CC.INS = "institutional", CC.INF = "infrastructure")
trend <- as.data.frame(tr[tr$indicatorid %in% names(keep_ids), c("iso3", "indicatorid", "indicatorscore", "informyear")])
trend$var <- unname(keep_ids[trend$indicatorid])
trend <- data.frame(iso3 = trend$iso3, year = as.integer(trend$informyear), var = trend$var,
                    score = num(trend$indicatorscore))
trend <- trend[!is.na(trend$score), ]
trend <- trend[!duplicated(trend[, c("iso3", "year", "var")]), ]

# ---------------------------------------------------------- OWID energy --
ow <- utils::read.csv("raw/owid_energy.csv", stringsAsFactors = FALSE)
ow <- ow[!is.na(ow$iso_code) & nchar(ow$iso_code) == 3 & !grepl("^OWID", ow$iso_code), ]
# Column names follow the current OWID energy release (units in the name).
rename <- c(total_energy_per_capita_kwh = "energy_per_capita",
            electricity_demand_per_capita_kwh = "electricity_per_capita",
            fossil_fuels_energy_share_pct = "fossil_share_energy",
            renewables_energy_share_pct = "renewables_share_energy",
            low_carbon_energy_share_pct = "low_carbon_share_energy",
            fossil_fuels_electricity_share_pct = "fossil_share_elec",
            renewables_electricity_share_pct = "renewables_share_elec",
            electricity_carbon_intensity_gco2eq_per_kwh = "carbon_intensity_elec",
            total_energy_per_gdp_kwh_per_dollar = "energy_per_gdp",
            coal_energy_share_pct = "coal_share_energy")
for (k in intersect(names(rename), names(ow))) names(ow)[names(ow) == k] <- rename[[k]]
owid_vars <- intersect(c("population", "gdp", unname(rename)), names(ow))
owid <- ow[ow$year >= 2000, c("country", "iso_code", "year", owid_vars)]
names(owid)[names(owid) == "iso_code"] <- "iso3"
owid$gdp_per_capita <- owid$gdp / owid$population

# Most recent non-missing value per country (up to 2024) for regression.
latest <- function(v, max_year = 2024) {
  d <- owid[owid$year <= max_year & !is.na(owid[[v]]), c("iso3", "year", v)]
  d <- d[order(d$iso3, -d$year), ]
  d <- d[!duplicated(d$iso3), ]
  names(d) <- c("iso3", paste0(v, "_year"), v)
  d
}
owid_latest <- Reduce(function(a, b) merge(a, b, by = "iso3", all = TRUE),
                      lapply(c("gdp_per_capita", "energy_per_capita", "electricity_per_capita",
                               "renewables_share_elec", "fossil_share_elec", "carbon_intensity_elec"), latest))

owid_completeness <- do.call(rbind, lapply(c(owid_vars, "gdp_per_capita"), function(v) {
  a <- stats::aggregate(owid[[v]], by = list(year = owid$year), FUN = function(z) mean(!is.na(z)))
  data.frame(variable = v, year = a$year, complete = a$x)
}))

# -------------------------------------------------------------- EM-DAT --
em <- read_excel("raw/emdat_country_profile_zaf.xlsx")
em <- as.data.frame(em[!grepl("^#", em$Year), ])
names(em) <- snake(names(em))
for (v in c("year", "total_events", "total_affected", "total_deaths", "total_damage_usd_original", "total_damage_usd_adjusted")) em[[v]] <- num(em[[v]])

# ---------------------------------------------------------- boundaries --
ne <- st_read("raw/ne_50m_admin_0_countries.geojson", quiet = TRUE)
ne$iso3 <- ifelse(ne$ISO_A3 %in% c("-99", "", NA), ne$ISO_A3_EH, ne$ISO_A3)
ne$iso3 <- ifelse(ne$iso3 %in% c("-99", "", NA), ne$ADM0_A3, ne$iso3)
ne <- ne[, c("iso3", "NAME", "CONTINENT")]
names(ne)[2:3] <- c("ne_name", "continent")
ne <- st_make_valid(ne)
ne <- ne[ne$iso3 != "ATA", ]                                 # Antarctica
# Dissolve any duplicate iso3 codes into one shape.
if (anyDuplicated(ne$iso3)) {
  ne <- stats::aggregate(ne[, "iso3"], by = list(iso3k = ne$iso3), FUN = function(x) x[1])
  ne$iso3 <- ne$iso3k; ne$iso3k <- NULL
}
missing_shapes <- setdiff(inform$iso3, ne$iso3)
countries <- ne[ne$iso3 %in% inform$iso3, ]
countries <- merge(countries, inform[, c("iso3", "country")], by = "iso3")
countries <- countries[, c("iso3", "country")]
world_outline <- st_union(st_make_valid(st_simplify(st_transform(ne, "+proj=eqearth"), dTolerance = 5000)))

# Display geometry for the browser (lighter).
countries_display <- st_transform(st_simplify(st_transform(countries, "+proj=eqearth"), preserveTopology = TRUE, dTolerance = 6000), 4326)
countries_display <- st_make_valid(countries_display)

# Centroids for distance-based weights: point on surface of the largest part.
largest_part <- function(g) {
  p <- st_cast(st_geometry(g), "POLYGON")
  p[which.max(st_area(p))]
}
cent <- do.call(rbind, lapply(seq_len(nrow(countries)), function(i) {
  lp <- largest_part(countries[i, ])
  data.frame(iso3 = countries$iso3[i], st_coordinates(st_point_on_surface(lp)))
}))
names(cent) <- c("iso3", "lon", "lat")

# --------------------------------------------------------- earthquakes --
eq <- utils::read.csv("raw/earthquakes_1965_2016.csv", stringsAsFactors = FALSE)
eq$date <- as.Date(eq$Date, format = "%m/%d/%Y")
eq <- data.frame(date = eq$date, year = as.integer(format(eq$date, "%Y")),
                 lat = num(eq$Latitude), lon = num(eq$Longitude), magnitude = num(eq$Magnitude))
eq <- eq[stats::complete.cases(eq), ]
# Assign each event to the country it falls in, or the nearest country
# within 100 km (offshore events), otherwise "Offshore".
eq_sf <- st_as_sf(eq, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
cty_eq <- st_transform(countries, "+proj=eqearth"); eq_p <- st_transform(eq_sf, "+proj=eqearth")
near <- st_nearest_feature(eq_p, cty_eq)
dist_km <- as.numeric(st_distance(eq_p, cty_eq[near, ], by_element = TRUE)) / 1000
eq$iso3 <- ifelse(dist_km <= 100, cty_eq$iso3[near], NA)
eq$dist_to_land_km <- round(dist_km, 1)

vo <- utils::read.csv("raw/volcanoes_holocene.csv", stringsAsFactors = FALSE, check.names = FALSE)
volcanoes <- data.frame(name = vo[["Volcano Name"]], country = vo$Country, lat = num(vo$Latitude),
                        lon = num(vo$Longitude), elevation_m = num(vo$Elev), type = vo$Type,
                        status = vo$Status, last_known = vo[["Last Known"]], stringsAsFactors = FALSE)
volcanoes <- volcanoes[stats::complete.cases(volcanoes[, c("lat", "lon")]), ]

# ------------------------------------------------------ rainfall stations --
utils::data("NorthAmericanRainfall", package = "fields", envir = environment())
nar <- NorthAmericanRainfall
stations <- data.frame(station = seq_along(nar$longitude), lon = nar$longitude, lat = nar$latitude,
                       elevation_m = nar$elevation,
                       precip_mm = nar$precip / 10, precip_se_mm = nar$precipSE / 10,
                       trend_mm_per_decade = nar$trend / 10 * 10, trend_se = nar$trendSE / 10 * 10,
                       type = as.character(nar$type))
attr(stations, "description") <- "Mean June-August rainfall (mm) and its linear trend, 1950-2010, GHCN v2 (via fields::NorthAmericanRainfall)"

# ---------------------------------------------------------------- save --
saveRDS(inform, "data/inform.rds")
saveRDS(dict, "data/inform_dictionary.rds")
saveRDS(inform_quality, "data/inform_quality.rds")
saveRDS(trend, "data/inform_trend.rds")
saveRDS(list(series = owid, latest = owid_latest, completeness = owid_completeness), "data/owid_energy.rds")
saveRDS(em, "data/emdat_zaf.rds")
saveRDS(list(countries = countries, display = countries_display, centroids = cent,
             outline = world_outline, missing = missing_shapes), "data/geography.rds")
saveRDS(eq, "data/earthquakes.rds")
saveRDS(volcanoes, "data/volcanoes.rds")
saveRDS(stations, "data/rain_stations.rds")

cat("INFORM countries:", nrow(inform), " with shapes:", nrow(countries),
    " missing shapes:", paste(missing_shapes, collapse = ", "), "\n")
cat("Trend rows:", nrow(trend), " years:", paste(range(trend$year), collapse = "-"), "\n")
cat("OWID country-years:", nrow(owid), "\n")
cat("Earthquakes:", nrow(eq), " assigned to a country:", sum(!is.na(eq$iso3)), "\n")
cat("Volcanoes:", nrow(volcanoes), " Stations:", nrow(stations), "\n")

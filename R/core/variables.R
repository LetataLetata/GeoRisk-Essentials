# Every variable used in the analysis, in one table, with a dictionary that
# says what each one measures, where it comes from and which step uses it.
# Also assigns each country to a continent (the data only has World Bank
# regions, which mix continents, e.g. Middle East & North Africa).

CONTINENT_LEVELS <- c("Africa", "Asia", "Europe", "North America", "South America", "Oceania")

continent_of <- function(iso3, wb_region) {
  out <- rep(NA_character_, length(iso3))
  out[wb_region == "Sub-Saharan Africa"] <- "Africa"
  out[wb_region == "South Asia"] <- "Asia"
  out[wb_region == "North America"] <- "North America"
  mena_africa <- c("DZA", "DJI", "EGY", "LBY", "MAR", "TUN")
  out[wb_region == "Middle East & North Africa"] <- ifelse(iso3[wb_region == "Middle East & North Africa"] %in% mena_africa, "Africa", "Asia")
  out[iso3 == "MLT"] <- "Europe"
  # Central Asia and the Caucasus, Turkey and Cyprus are in Asia in the United Nations scheme.
  eca_asia <- c("ARM", "AZE", "CYP", "GEO", "KAZ", "KGZ", "TJK", "TKM", "TUR", "UZB")
  sel <- wb_region == "Europe & Central Asia"
  out[sel] <- ifelse(iso3[sel] %in% eca_asia, "Asia", "Europe")
  oceania <- c("AUS", "FJI", "FSM", "KIR", "MHL", "NRU", "NZL", "PLW", "PNG", "SLB", "TON", "TUV", "VUT", "WSM")
  sel <- wb_region == "East Asia & Pacific"
  out[sel] <- ifelse(iso3[sel] %in% oceania, "Oceania", "Asia")
  south_america <- c("ARG", "BOL", "BRA", "CHL", "COL", "ECU", "GUY", "PER", "PRY", "SUR", "URY", "VEN")
  sel <- wb_region == "Latin America & Caribbean"
  out[sel] <- ifelse(iso3[sel] %in% south_america, "South America", "North America")
  factor(out, levels = CONTINENT_LEVELS)
}

# group, label, unit, description, source, steps (which pages use it)
VARIABLE_INFO <- as.data.frame(matrix(ncol = 6, byrow = TRUE, dimnames = list(NULL, c("var", "group", "unit", "description", "source", "steps")), data = c(
  "inform_risk", "Risk and its three parts", "0-10", "Overall disaster risk: geometric mean of the three parts below", "INFORM", "Data, Where, Why, How sure, Explore",
  "hazard_and_exposure", "Risk and its three parts", "0-10", "How likely hazards are and how many people and assets are exposed", "INFORM", "Data, Where, Why, How sure, Where to act",
  "vulnerability", "Risk and its three parts", "0-10", "How susceptible people are to harm: poverty, health, food security, displacement", "INFORM", "Data, Where, Why, How sure, Where to act",
  "lack_of_coping_capacity", "Risk and its three parts", "0-10", "Weakness of institutions and infrastructure that help people cope and recover", "INFORM", "Data, Where, Why, How sure",
  "natural", "Hazards", "0-10", "All natural hazards combined", "INFORM", "Data, Where, Where to act",
  "earthquake", "Hazards", "0-10", "People exposed to strong earthquake shaking", "INFORM", "Data, Where, Where to act",
  "river_flood", "Hazards", "0-10", "People exposed to river floods", "INFORM", "Data, Where, Where to act",
  "tsunami", "Hazards", "0-10", "People exposed to tsunamis", "INFORM", "Data, Where, Where to act",
  "tropical_cyclone", "Hazards", "0-10", "People exposed to tropical cyclone winds", "INFORM", "Data, Where, Where to act",
  "coastal_flood", "Hazards", "0-10", "People exposed to coastal flooding and storm surge", "INFORM", "Data, Where, Where to act",
  "drought", "Hazards", "0-10", "Exposure to drought, mainly of agriculture and people", "INFORM", "Data, Where, Where to act",
  "epidemic", "Hazards", "0-10", "Likelihood and spread of infectious disease outbreaks", "INFORM", "Data, Where, Where to act",
  "human", "Hazards", "0-10", "Human hazards combined: conflict now and projected", "INFORM", "Data, Where, Where to act",
  "projected_conflict_probability", "Hazards", "0-10", "Predicted probability of violent conflict", "INFORM", "Data, Where, Where to act",
  "current_conflict_intensity", "Hazards", "0-10", "Intensity of current violent conflict", "INFORM", "Data, Where, Where to act",
  "socio_economic_vulnerability", "Vulnerability", "0-10", "Poverty, development, inequality and aid dependency combined", "INFORM", "Data, Where, Why",
  "development_and_deprivation", "Vulnerability", "0-10", "Human development and multidimensional poverty", "INFORM", "Data, Where, Why",
  "inequality", "Vulnerability", "0-10", "Income and gender inequality", "INFORM", "Data, Where, Why",
  "economic_dependency", "Vulnerability", "0-10", "Dependence on aid and remittances", "INFORM", "Data, Where, Why",
  "vulnerable_groups", "Vulnerability", "0-10", "Displaced people and other vulnerable groups combined", "INFORM", "Data, Where, Why",
  "uprooted_people", "Vulnerability", "0-10", "Refugees, returnees and internally displaced people", "INFORM", "Data, Where, Why",
  "health_conditions", "Vulnerability", "0-10", "Burden of HIV, tuberculosis and malaria", "INFORM", "Data, Where, Why",
  "children_u5", "Vulnerability", "0-10", "Child mortality and underweight children under 5", "INFORM", "Data, Where, Why",
  "recent_shocks", "Vulnerability", "0-10", "People affected by disasters in the last three years", "INFORM", "Data, Where, Why",
  "food_security", "Vulnerability", "0-10", "Food availability, access and use", "INFORM", "Data, Where, Why",
  "other_vulnerable_groups", "Vulnerability", "0-10", "Health, children, recent shocks and food security combined", "INFORM", "Data, Where, Why",
  "institutional", "Coping capacity", "0-10", "Disaster risk reduction and governance combined (higher = weaker)", "INFORM", "Data, Where, Why",
  "drr", "Coping capacity", "0-10", "Implementation of disaster risk reduction (higher = weaker)", "INFORM", "Data, Where, Why",
  "governance", "Coping capacity", "0-10", "Government effectiveness and corruption (higher = weaker)", "INFORM", "Data, Where, Why",
  "infrastructure", "Coping capacity", "0-10", "Communication, physical infrastructure and health access combined (higher = weaker)", "INFORM", "Data, Where, Why",
  "communication", "Coping capacity", "0-10", "Access to electricity, internet and mobile phones (higher = weaker)", "INFORM", "Data, Where, Why",
  "physical_infrastructure", "Coping capacity", "0-10", "Roads, water and sanitation (higher = weaker)", "INFORM", "Data, Where, Why",
  "access_to_health_care", "Coping capacity", "0-10", "Health workers, spending and vaccination (higher = weaker)", "INFORM", "Data, Where, Why",
  "gdp_per_capita", "Wealth and energy", "US dollars", "Gross domestic product per person (latest year to 2022)", "Our World in Data", "Data, Why",
  "electricity_per_capita", "Wealth and energy", "kWh per person", "Electricity demand per person", "Our World in Data", "Data",
  "energy_per_capita", "Wealth and energy", "kWh per person", "Primary energy use per person", "Our World in Data", "Data",
  "renewables_share_elec", "Wealth and energy", "%", "Share of electricity from renewables", "Our World in Data", "Data",
  "population", "Wealth and energy", "people", "Population", "INFORM", "Data, Where to act",
  "reliability_index", "Data quality", "0-10", "INFORM Lack of Reliability: missing, old and estimated data (higher = weaker data)", "INFORM", "Data, How sure",
  "imputed_share", "Data quality", "share", "Share of a country's indicators estimated by INFORM", "INFORM", "Data, How sure",
  "mean_data_age_years", "Data quality", "years", "Average age of the data behind a country's scores", "INFORM", "Data, How sure"
)), stringsAsFactors = FALSE)

VARIABLE_GROUPS <- c("Risk and its three parts", "Hazards", "Vulnerability", "Coping capacity", "Wealth and energy", "Data quality")

# All analysis variables for every country, in the row order of store$inform_sf.
build_all_variables <- function(store) {
  d <- sf::st_drop_geometry(store$inform_sf)
  if (!is.null(store$owid)) {
    ow <- store$owid$latest[, intersect(names(store$owid$latest), c("iso3", "gdp_per_capita", "electricity_per_capita", "energy_per_capita", "renewables_share_elec"))]
    d <- merge(d, ow, by = "iso3", all.x = TRUE)
    d <- d[match(store$inform_sf$iso3, d$iso3), ]
  }
  d$log_gdp_pc <- log10(d$gdp_per_capita)
  d$continent <- continent_of(d$iso3, d$wb_region)
  d
}

# Grouped choices for select inputs: list(group = c(label = var)).
variable_choices <- function(store, groups = VARIABLE_GROUPS, exclude = NULL) {
  vi <- store$vars[store$vars$group %in% groups & !store$vars$var %in% exclude, ]
  out <- lapply(split(vi, factor(vi$group, levels = groups)), function(x) stats::setNames(x$var, x$label))
  out[lengths(out) > 0]
}

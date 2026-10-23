# Plain-language insight for the public mode: turns INFORM scores into
# sentences and concrete actions. All wording lives here so it can be
# reviewed and edited in one place.

level_word <- function(x) cut(x, c(-Inf, 2.4, 3.6, 5.4, 7, Inf), right = FALSE,
                              labels = c("very low", "low", "medium", "high", "very high"))

HAZARD_ACTIONS <- list(
  earthquake = list(name = "Earthquakes", why = "strong ground shaking can damage buildings and infrastructure",
    gov = "Enforce earthquake-resistant building codes and retrofit schools and hospitals.",
    people = "Secure heavy furniture, know safe spots indoors, and keep an emergency kit."),
  river_flood = list(name = "River floods", why = "rivers can overflow onto homes, farms and roads",
    gov = "Restrict building on floodplains, maintain drainage and run flood early-warning systems.",
    people = "Know your evacuation route and keep documents in waterproof storage."),
  tsunami = list(name = "Tsunamis", why = "coastal areas can be hit by large waves after undersea earthquakes",
    gov = "Maintain coastal warning sirens, signposted evacuation routes and drills.",
    people = "If the ground shakes strongly near the coast, move to high ground immediately."),
  tropical_cyclone = list(name = "Tropical cyclones", why = "storms bring destructive wind, rain and storm surge",
    gov = "Prepare shelters, pre-position supplies and issue timely cyclone warnings.",
    people = "Follow warnings, secure roofs and loose objects, and evacuate when told."),
  coastal_flood = list(name = "Coastal floods", why = "storm surge and high tides can flood low-lying coasts",
    gov = "Protect and restore dunes and wetlands, and plan for sea-level rise.",
    people = "Know whether your home is in a low-lying coastal zone and have a plan to leave."),
  drought = list(name = "Drought", why = "long dry periods reduce water, crops and livestock",
    gov = "Build water storage, monitor rainfall and vegetation, and prepare drought relief early.",
    people = "Save water, diversify crops or income, and plan livestock feed ahead of dry seasons."),
  epidemic = list(name = "Epidemics", why = "infectious disease outbreaks can spread quickly",
    gov = "Strengthen disease surveillance, vaccination and clinic readiness.",
    people = "Keep vaccinations up to date and follow public health advice."),
  projected_conflict_probability = list(name = "Projected conflict risk", why = "violence can displace people and block help",
    gov = "Invest in conflict prevention and protect humanitarian access.",
    people = "Stay informed through trusted sources and know where to seek protection."),
  current_conflict_intensity = list(name = "Ongoing conflict", why = "active violence disrupts services and livelihoods",
    gov = "Protect civilians, keep health and food systems running, and support displaced people.",
    people = "Follow guidance from humanitarian organisations and keep essential documents safe.")
)

WEAKNESS_ACTIONS <- list(
  food_security = "Strengthen food reserves and social protection so shocks do not become hunger.",
  health_conditions = "Expand basic health services, nutrition and disease prevention.",
  uprooted_people = "Support displaced people with shelter, documents and access to services.",
  development_and_deprivation = "Improve access to basic services: water, sanitation, education and income.",
  inequality = "Target support to the poorest households and regions, who are hit hardest.",
  economic_dependency = "Reduce dependence on aid and single sources of income.",
  children_u5 = "Protect young children through nutrition and health programmes.",
  recent_shocks = "Help communities still recovering from recent disasters rebuild resilience.",
  drr = "Update national and local disaster risk reduction plans and test them with drills.",
  governance = "Strengthen institutions and accountability so disaster funds reach people.",
  communication = "Make sure warnings reach everyone: mobile phones, radio and community networks.",
  physical_infrastructure = "Invest in roads, water supply and electricity that keep working in disasters.",
  access_to_health_care = "Improve access to clinics and emergency medical care."
)

country_profile <- function(store, iso) {
  r <- store$inform[store$inform$iso3 == iso, ]
  if (!nrow(r)) return(NULL)
  hz <- intersect(names(HAZARD_ACTIONS), names(r))
  hz_scores <- sort(unlist(r[, hz]), decreasing = TRUE)
  top_hz <- names(hz_scores)[hz_scores >= 4.5][1:min(3, sum(hz_scores >= 4.5))]
  if (!length(top_hz) || all(is.na(top_hz))) top_hz <- names(hz_scores)[1:2]
  wk <- intersect(names(WEAKNESS_ACTIONS), names(r))
  wk_scores <- sort(unlist(r[, wk]), decreasing = TRUE)
  top_wk <- names(wk_scores)[wk_scores >= 5][1:min(3, sum(wk_scores >= 5))]
  top_wk <- top_wk[!is.na(top_wk)]

  pillars <- c(hazard_and_exposure = r$hazard_and_exposure, vulnerability = r$vulnerability,
               lack_of_coping_capacity = r$lack_of_coping_capacity)
  main <- names(which.max(pillars))
  main_txt <- c(hazard_and_exposure = "exposure to hazards", vulnerability = "the vulnerability of its people",
                lack_of_coping_capacity = "limited capacity to cope")[[main]]

  tr <- store$trend[store$trend$iso3 == iso & store$trend$var == "inform_risk", ]
  tr <- tr[order(tr$year), ]
  trend_txt <- if (nrow(tr) >= 2) {
    d <- tr$score[nrow(tr)] - tr$score[1]
    if (abs(d) < 0.3) sprintf("Risk has stayed about the same since %d (%.1f to %.1f).", tr$year[1], tr$score[1], tr$score[nrow(tr)])
    else sprintf("Risk has %s since %d, from %.1f to %.1f in the INFORM trend series.", ifelse(d > 0, "risen", "fallen"), tr$year[1], tr$score[1], tr$score[nrow(tr)])
  } else ""

  rel <- r$reliability_index
  rel_txt <- if (is.na(rel)) "Reliability information is not available." else if (rel <= 3)
    "The data behind this score is fairly complete and recent." else if (rel <= 5)
    "Some of the data behind this score is missing or several years old, so treat it as a guide." else
    "Much of the data behind this score is missing or old; use it with caution."

  list(
    row = r, top_hazards = top_hz, hazard_scores = hz_scores, top_weak = top_wk, main = main,
    headline = sprintf("%s has %s disaster risk: %.1f out of 10, %s highest of %d countries.",
                       r$country, as.character(level_word(r$inform_risk)), r$inform_risk,
                       ordinal(r$rank), nrow(store$inform)),
    driver = sprintf("The biggest contributor is %s (%.1f out of 10).", main_txt, max(pillars)),
    trend = trend_txt, reliability = rel_txt,
    hazard_lines = lapply(top_hz, function(h) list(name = HAZARD_ACTIONS[[h]]$name, score = r[[h]],
                                                   why = HAZARD_ACTIONS[[h]]$why, gov = HAZARD_ACTIONS[[h]]$gov,
                                                   people = HAZARD_ACTIONS[[h]]$people)),
    weak_lines = lapply(top_wk, function(w) list(name = var_label(store, w), score = r[[w]], action = WEAKNESS_ACTIONS[[w]]))
  )
}

neighbours_of <- function(store, iso, k = 5) {
  c0 <- store$geo$centroids
  i <- match(iso, c0$iso3); if (is.na(i)) return(character(0))
  d <- sf::st_distance(sf::st_as_sf(c0[i, ], coords = c("lon", "lat"), crs = 4326),
                       sf::st_as_sf(c0, coords = c("lon", "lat"), crs = 4326))
  c0$iso3[order(as.numeric(d))][2:(k + 1)]
}

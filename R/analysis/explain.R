# Why does a country get its rating? For any INFORM score (overall risk, a
# pillar, a hazard or a weakness) this file finds:
#   - the score, rank and level compared with other countries;
#   - the measured facts INFORM builds the score from (people exposed to
#     floods each year, people affected by drought, displaced people ...),
#     each with how the country compares;
#   - the parts the score is made of, for combined scores;
#   - recorded events: people affected by disasters in each of the last three
#     years, drought history, displacement, and the strongest earthquakes on
#     record near the country;
#   - where the score sits in the INFORM structure, up to overall risk.
# Plain functions, no Shiny; the Explore page turns the result into the panel.

# INFORM structure: each score and the score it feeds into.
VAR_PARENT <- c(
  hazard_and_exposure = "inform_risk", vulnerability = "inform_risk", lack_of_coping_capacity = "inform_risk",
  natural = "hazard_and_exposure", human = "hazard_and_exposure",
  earthquake = "natural", river_flood = "natural", tsunami = "natural", tropical_cyclone = "natural",
  coastal_flood = "natural", drought = "natural", epidemic = "natural",
  projected_conflict_probability = "human", current_conflict_intensity = "human",
  socio_economic_vulnerability = "vulnerability", vulnerable_groups = "vulnerability",
  development_and_deprivation = "socio_economic_vulnerability", inequality = "socio_economic_vulnerability",
  economic_dependency = "socio_economic_vulnerability",
  uprooted_people = "vulnerable_groups", other_vulnerable_groups = "vulnerable_groups",
  health_conditions = "other_vulnerable_groups", children_u5 = "other_vulnerable_groups",
  recent_shocks = "other_vulnerable_groups", food_security = "other_vulnerable_groups",
  institutional = "lack_of_coping_capacity", infrastructure = "lack_of_coping_capacity",
  drr = "institutional", governance = "institutional",
  communication = "infrastructure", physical_infrastructure = "infrastructure", access_to_health_care = "infrastructure")

var_children <- function(v) names(VAR_PARENT)[VAR_PARENT == v]
var_path <- function(v) { out <- v; while (!is.na(VAR_PARENT[out[length(out)]])) out <- c(out, unname(VAR_PARENT[out[length(out)]])); out }

# The measured facts behind each score. Columns: INFORM indicator id, plain
# label, display format, and direction (1 = a higher value means more risk,
# -1 = a higher value means less risk).
#   people  a number of people          pct   already a percentage
#   frac    a share from 0 to 1, shown as a percentage
#   num     a plain number (unit in the label)    usd  US dollars
ind <- function(id, label, fmt = "people", dir = 1) data.frame(id = id, label = label, fmt = fmt, dir = dir, stringsAsFactors = FALSE)
INDICATORS <- list(
  earthquake = rbind(ind("NH.EQ.GEM.MMI6.SP", "People exposed each year to strong shaking (intensity VI or more)"),
                     ind("NH.EQ.GEM.MMI8.SP", "People exposed each year to severe shaking (intensity VIII or more)")),
  river_flood = ind("EX_FL", "People expected to be exposed to river floods each year"),
  tsunami = ind("EX_TS", "People expected to be exposed to tsunamis each year"),
  tropical_cyclone = rbind(ind("EX_TC_SS1", "People exposed each year to cyclone winds of category 1 or more"),
                           ind("EX_TC_SS3", "People exposed each year to cyclone winds of category 3 or more")),
  coastal_flood = ind("EX_CFL", "People expected to be exposed to coastal floods each year"),
  drought = rbind(ind("AFF_DR", "People affected by drought in an average year (records since 1991)"),
                  ind("AFF_DR.FREQ", "Recorded drought events per year (since 1991)", "num"),
                  ind("ASI", "Chance in any year that drought hits more than 30% of farmland (FAO)", "frac")),
  epidemic = rbind(ind("POP.EXP.VECT.MAL.ZS", "Share of people at risk of malaria", "frac"),
                   ind("POP.EXP.VECT.DENGUE", "People exposed to dengue"),
                   ind("POP.EXP.ZOON.EVD", "People living where Ebola can pass from animals"),
                   ind("SH.STA.ODFC.ZS", "People with no toilet (open defecation)", "pct"),
                   ind("SH.SAN.HNDWSH.ZS-P2P", "People with a place to wash hands with soap and water", "pct", -1)),
  projected_conflict_probability = ind("HVC.PROB", "Predicted chance of violent conflict in the next 4 years (JRC model)", "frac"),
  current_conflict_intensity = ind("CON_GPI", "Ongoing conflict score (Global Peace Index, 1 = none, 5 = most)", "num"),
  development_and_deprivation = rbind(ind("HDI", "Human Development Index (0 to 1, higher is better)", "num", -1),
                                      ind("MPI", "Multidimensional Poverty Index (0 to 1)", "num")),
  inequality = rbind(ind("GINI", "Income inequality (Gini, 0 to 100)", "num"),
                     ind("GII", "Gender Inequality Index (0 to 1)", "num")),
  economic_dependency = rbind(ind("DT.ODA.ODAT.GN.ZS", "Development aid received (% of national income)", "pct"),
                              ind("BX_TRF_PWKR", "Money sent home by workers abroad (% of GDP)", "pct"),
                              ind("FTS", "Humanitarian aid received this year (US dollars, millions)", "num")),
  uprooted_people = rbind(ind("POP.SEC.IDP", "People forced from their homes within the country"),
                          ind("TRPS", "Refugees and asylum seekers hosted"),
                          ind("RET_REF", "Refugees who have returned")),
  health_conditions = rbind(ind("HIV", "Adults living with HIV", "pct"),
                            ind("TB", "Tuberculosis cases per 100,000 people", "num"),
                            ind("SH_STA_MALR", "Malaria cases per 1,000 people at risk", "num")),
  children_u5 = rbind(ind("CM", "Children who die before age 5, per 1,000 births", "num"),
                      ind("CUW", "Children under 5 who are underweight", "pct")),
  recent_shocks = rbind(ind("VU.VGR.OG.NATDIS-CUR", "People affected by natural disasters, latest year"),
                        ind("VU.VGR.OG.NATDIS-1", "People affected by natural disasters, the year before"),
                        ind("VU.VGR.OG.NATDIS-2", "People affected by natural disasters, two years before")),
  food_security = rbind(ind("SN.ITK.DEFC.ZS", "People who do not get enough to eat", "pct"),
                        ind("FS.ADSA", "Food supply compared with what people need", "pct", -1)),
  drr = ind("SG_DSR_LGRGSR", "Disaster risk reduction plans in place (UN Sendai score, 0 to 1)", "num", -1),
  governance = rbind(ind("GovernmentEffectiveness", "Government effectiveness (World Bank, -2.5 to 2.5)", "num", -1),
                     ind("CPI", "Corruption Perceptions Index (0 = most corrupt, 100 = clean)", "num", -1)),
  communication = rbind(ind("EG.ELC.ACCS.ZS", "People with electricity", "pct", -1),
                        ind("IT.NET.USER.P2", "People using the internet", "pct", -1),
                        ind("SE.ADT.LITR.ZS", "Adults who can read and write", "pct", -1)),
  physical_infrastructure = rbind(ind("SH.H2O.SAFE.ZS", "People with basic drinking water", "pct", -1),
                                  ind("SH.STA.BASS.ZS", "People with basic sanitation", "pct", -1)),
  access_to_health_care = rbind(ind("PHIS", "Doctors per 10,000 people", "num", -1),
                                ind("SH_ACS_MCV2", "Children fully vaccinated against measles", "pct", -1),
                                ind("HEALTH_EXP", "Health spending per person (US dollars, PPP)", "usd", -1),
                                ind("MMR", "Mothers who die in childbirth, per 100,000 births", "num"))
)

# Value of an indicator for one country, and the share of countries it is worse than.
ind_value <- function(store, iso, id) {
  m <- store$indicators; if (is.null(m) || !id %in% colnames(m$values) || !iso %in% rownames(m$values)) return(NA_real_)
  m$values[iso, id]
}
ind_year <- function(store, iso, id) {
  m <- store$indicators; if (is.null(m) || !id %in% colnames(m$year)) return(NA_character_)
  y <- m$year[iso, id]; mm <- m$meta$years[m$meta$id == id][1]
  if (!is.na(mm) && grepl("-", mm)) mm else y
}
worse_than <- function(x, all, dir = 1) {
  all <- all[!is.na(all)]; if (is.na(x) || !length(all)) return(NA_real_)
  100 * if (dir > 0) mean(all < x) else mean(all > x)
}
fmt_ind <- function(x, fmt) {
  if (is.na(x)) return("no data")
  switch(fmt,
    people = if (x >= 1e6) sprintf("%.1f million", x / 1e6) else if (x >= 1000) format(round(x, -2), big.mark = ",") else format(round(x), big.mark = ","),
    pct = sprintf("%.0f%%", x), frac = sprintf("%.0f%%", 100 * x),
    usd = paste0("$", format(round(x), big.mark = ",")),
    num = if (abs(x) >= 100) format(round(x), big.mark = ",") else if (abs(x) >= 10) sprintf("%.1f", x) else sprintf("%.2f", x),
    format(x))
}

# Lower-case the first letter of a label, unless it starts with an acronym
# ("FAO", "HIV") or a name ("Global Peace Index").
lcfirst_word <- function(x) {
  keep <- grepl("^[A-Z]{2}", x) | grepl("^(Global|Human|Gender|Multidimensional|Corruption|INFORM)", x)
  ifelse(keep, x, paste0(tolower(substr(x, 1, 1)), substring(x, 2)))
}

# Level compared with other countries (fifths of the ranking).
level_of <- function(pct) {
  if (is.na(pct)) return("No data")
  if (pct >= 80) "Very high" else if (pct >= 60) "High" else if (pct >= 40) "Medium" else if (pct >= 20) "Low" else "Very low"
}
LEVEL_COLS <- c("Very low" = "#1A9850", "Low" = "#91CF60", "Medium" = "#FEE08B", "High" = "#FC8D59", "Very high" = "#D73027", "No data" = "#D1D5DB")

# The measured facts for one score; for combined scores, those of its parts.
facts_for <- function(store, iso, var, depth = 0) {
  if (!is.null(INDICATORS[[var]])) {
    tb <- INDICATORS[[var]]
    tb$value <- vapply(tb$id, function(k) ind_value(store, iso, k), numeric(1))
    tb$shown <- mapply(fmt_ind, tb$value, tb$fmt)
    tb$worse <- mapply(function(k, x, d) worse_than(x, store$indicators$values[, k], d), tb$id, tb$value, tb$dir)
    tb$better <- mapply(function(k, x, d) worse_than(x, store$indicators$values[, k], -d), tb$id, tb$value, tb$dir)
    tb$year <- vapply(tb$id, function(k) ind_year(store, iso, k), character(1))
    tb$part <- var
    return(tb[!is.na(tb$value), , drop = FALSE])
  }
  if (depth > 2) return(NULL)
  do.call(rbind, lapply(var_children(var), function(ch) facts_for(store, iso, ch, depth + 1)))
}

# Recorded events behind a score.
events_for <- function(store, iso, var) {
  out <- list()
  path <- var_path(var)
  r <- store$inform[store$inform$iso3 == iso, ]
  v <- function(id) ind_value(store, iso, id)
  if ("natural" %in% path || var %in% c("recent_shocks", "inform_risk", "hazard_and_exposure")) {
    yrs <- c(`VU.VGR.OG.NATDIS-2` = NA, `VU.VGR.OG.NATDIS-1` = NA, `VU.VGR.OG.NATDIS-CUR` = NA)
    vals <- vapply(names(yrs), v, numeric(1)); ys <- vapply(names(yrs), function(k) ind_year(store, iso, k), character(1))
    if (any(!is.na(vals))) out$disasters <- data.frame(year = ys, people = vals, stringsAsFactors = FALSE)
  }
  if (var %in% c("drought", "natural", "hazard_and_exposure", "inform_risk")) {
    a <- v("AFF_DR"); f <- v("AFF_DR.FREQ")
    if (!is.na(a) && a > 0) out$drought <- sprintf("On average %s people a year affected by drought since 1991; about %d drought events recorded in that time.",
                                                  fmt_ind(a, "people"), max(1L, as.integer(round(f * 35))))
  }
  if (var %in% c("earthquake", "tsunami", "natural", "hazard_and_exposure", "inform_risk") && !is.null(store$quakes)) {
    q <- store$quakes[store$quakes$iso3 == iso, ]
    if (var == "tsunami") q <- q[q$magnitude >= 7, ]
    if (nrow(q)) {
      q <- q[order(-q$magnitude, q$date), ]
      out$quakes <- list(n = nrow(q), n7 = sum(q$magnitude >= 7), top = head(q, 3), tsunami = var == "tsunami")
    }
  }
  if (any(c("human", "uprooted_people", "vulnerable_groups") %in% path) || var %in% c("inform_risk", "vulnerability")) {
    idp <- v("POP.SEC.IDP"); ref <- v("TRPS"); ret <- v("RET_REF")
    if (any(c(idp, ref, ret) > 0, na.rm = TRUE)) out$displaced <- c(
      if (isTRUE(idp > 0)) paste(fmt_ind(idp, "people"), "people forced from their homes inside the country"),
      if (isTRUE(ref > 0)) paste(fmt_ind(ref, "people"), "refugees and asylum seekers hosted"),
      if (isTRUE(ret > 0)) paste(fmt_ind(ret, "people"), "refugees returned"))
  }
  if ("human" %in% path || var %in% c("inform_risk", "hazard_and_exposure")) {
    g <- v("CON_GPI"); p <- v("HVC.PROB")
    if (!is.na(g) && g >= 2.5) out$conflict <- sprintf("Ongoing violent conflict: Global Peace Index conflict score %.1f out of 5.", g)
    else if (!is.na(p) && p >= 0.5 && "projected_conflict_probability" %in% path) out$conflict <- sprintf("No major conflict now, but the model gives a %s chance of violent conflict.", fmt_ind(p, "frac"))
  }
  if (any(c("economic_dependency", "uprooted_people") %in% path) || var %in% c("human", "current_conflict_intensity")) {
    f <- v("FTS"); if (!is.na(f) && f >= 1) out$aid <- sprintf("US$%s million in humanitarian aid received this year.", fmt_ind(f, "num"))
  }
  out
}

# Everything the Explore panel needs to explain one score for one country.
explain_variable <- function(store, iso, var) {
  inf <- store$inform
  r <- inf[inf$iso3 == iso, ]
  x <- r[[var]]; all <- inf[[var]]
  n_ok <- sum(!is.na(all))
  rank <- if (is.na(x)) NA else sum(all > x, na.rm = TRUE) + 1
  pct <- worse_than(x, all, 1)
  facts <- facts_for(store, iso, var)
  kids <- var_children(var)
  parts <- if (length(kids)) {
    d <- data.frame(var = kids, label = vapply(kids, function(k) var_label(store, k), character(1)),
                    score = vapply(kids, function(k) as.numeric(r[[k]]), numeric(1)), stringsAsFactors = FALSE)
    d[order(-d$score), ]
  } else NULL
  # One plain sentence: the score, then what drives it.
  lab <- var_label(store, var)
  lab_s <- if (var == "inform_risk") "overall risk" else lcfirst_word(lab)
  lvl <- if (var == "inform_risk") as.character(r$risk_class) else level_of(pct)
  why <- if (is.na(x)) sprintf("There is no %s score for %s.", lab_s, r$country)
  else if (x == 0 && !is.null(facts) && nrow(facts) && all(facts$value == 0, na.rm = TRUE))
    sprintf("%s scores 0 for %s: the data show nobody exposed (for example, no coastline or no hazard zone).", r$country, lab_s)
  else {
    lead <- sprintf("%s scores %.1f out of 10 for %s, higher than %.0f%% of countries.", r$country, x, lab_s, pct)
    if (!is.null(parts) && nrow(parts) && !is.na(parts$score[1]))
      paste(lead, sprintf("The biggest part is %s (%.1f out of 10).", lcfirst_word(parts$label[1]), parts$score[1]))
    else if (!is.null(facts) && nrow(facts)) {
      high <- !is.na(pct) && pct >= 40
      w <- ifelse(is.na(facts$worse), if (high) -1 else 101, facts$worse)
      f <- facts[if (high) which.max(w) else which.min(w), ]
      ppl <- f$fmt == "people"
      cmp <- if (high) sprintf("%s than in %.0f%% of countries", if (f$dir < 0) "worse" else if (ppl) "more" else "higher", f$worse)
             else sprintf("%s than in %.0f%% of countries", if (f$dir < 0) "better" else if (ppl) "fewer" else "lower", f$better)
      paste(lead, sprintf("%s %s: %s, %s.", if (high) "The main reason:" else "It is low because of", lcfirst_word(f$label), f$shown, cmp))
    } else lead
  }
  list(var = var, label = lab, score = x, rank = rank, n = n_ok, pct = pct, level = lvl,
       median = stats::median(all, na.rm = TRUE), why = why, facts = facts, parts = parts,
       events = events_for(store, iso, var), path = var_path(var),
       description = store$vars$description[match(var, store$vars$var)])
}

# Step 5, Decide: where should action come first? Where danger and
# vulnerability come together (double burden), for every hazard.

ACT_PHRASE <- c(hazard_and_exposure = "exposure to natural and human hazards", natural = "exposure to natural hazards",
                human = "exposure to conflict", drought = "drought danger", river_flood = "river flood danger",
                coastal_flood = "coastal flood danger", tsunami = "tsunami danger", tropical_cyclone = "cyclone danger",
                earthquake = "earthquake danger", current_conflict_intensity = "current conflict",
                projected_conflict_probability = "risk of future conflict", epidemic = "disease outbreak danger")

ACT_HAZARDS <- c("All hazards" = "hazard_and_exposure", "All natural hazards" = "natural", "Drought" = "drought",
                 "River floods" = "river_flood", "Coastal floods" = "coastal_flood", "Tropical cyclones" = "tropical_cyclone",
                 "Earthquakes" = "earthquake", "Tsunamis" = "tsunami", "Disease outbreaks" = "epidemic",
                 "Current conflict" = "current_conflict_intensity", "Projected conflict" = "projected_conflict_probability",
                 "All human hazards (conflict)" = "human")

# National-level actions for each hazard (see R/analysis/insights.R for the wording).
national_actions <- function(hz) {
  map <- c(river_flood = "river_flood", coastal_flood = "coastal_flood", tsunami = "tsunami", tropical_cyclone = "tropical_cyclone",
           earthquake = "earthquake", drought = "drought", epidemic = "epidemic",
           current_conflict_intensity = "current_conflict_intensity", projected_conflict_probability = "projected_conflict_probability",
           human = "current_conflict_intensity")
  if (!hz %in% names(map)) return(NULL)
  HAZARD_ACTIONS[[map[[hz]]]]
}

register_view(
  id = "act", title = "Where to act", mode = "analyst", icon = "hand-holding-heart", order = 40,
  ui = function(id, store) {
    ns <- NS(id)
    tagList(
      step_head("act", "Where should action come first?",
                "A flood or drought does the most harm where people have the least to fall back on. These are the countries where high danger meets high vulnerability.",
                controls(selectInput(ns("hz"), "Which danger?", ACT_HAZARDS, width = "250px"))),
      uiOutput(ns("answer")),
      gr_grid(cols = "1.6fr 1fr",
        map_ui(ns("map"), "Danger and vulnerability together", "Dark purple: high on both. Red: mostly danger. Blue: mostly vulnerability.", height = 460),
        gr_card(title = "Countries carrying both burdens", subtitle = "Top third for danger and for vulnerability", uiOutput(ns("list")))),
      gr_card(title = "What helps at national level", subtitle = "Recommendations for national governments of the countries listed above",
        div(class = "scale-note", icon("flag"), strong(" Country level, not city level. "),
            "These recommendations follow from national scores. They say what national governments and their partners can do; they do not describe conditions in particular cities, towns or communities, where risk can be very different. Local plans need local data."),
        uiOutput(ns("hazard_action")),
        div(class = "gr-grid", style = "grid-template-columns:repeat(4,minmax(0,1fr));margin:0;",
          div(class = "action-card", h4("Guide where people build"), p("National planning rules that keep new housing and services out of floodplains and off unstable slopes, and fund upgrading of settlements already there.")),
          div(class = "action-card", h4("Warn everyone"), p("National early-warning systems that reach people without smartphones: radio, sirens and community networks.")),
          div(class = "action-card", h4("Protect incomes"), p("Social grants, insurance and national food reserves so a shock does not push families into poverty.")),
          div(class = "action-card", h4("Plan across borders"), p("Risk clusters across neighbouring countries, so regional agreements and shared early warning work better than acting alone.")))),
      method_box(
        p(strong("Classes. "), "Countries are split into thirds (terciles) on the chosen danger and on INFORM vulnerability, giving nine combinations. The double burden is the top third on both."),
        p(strong("Lee's L "), "(Lee, 2001) tests whether high danger and high vulnerability not only go together but also cluster in the same neighbourhoods; significance from 499 permutations. Spearman's rho gives the plain correlation."),
        p(strong("Missing data. "), "Countries without a value for the chosen danger or for vulnerability are left out before the thirds are worked out, and neighbours are worked out again among the rest; they appear grey on the map."),
        p(strong("Scale. "), "All results are for whole countries. A country in the double burden can contain safe areas, and a country outside it can contain very exposed communities.")),
      next_step("act")
    )
  },
  server = function(id, store, shared) {
    moduleServer(id, function(input, output, session) {
      need_data(store, "inform")
      hz_lab <- reactive(names(ACT_HAZARDS)[ACT_HAZARDS == input$hz])
      # Missing-data contingency: countries need both the chosen danger and vulnerability.
      chk <- reactive(cached_check(store, c(input$hz, "vulnerability"), c(hz_lab(), "vulnerability")))
      sfd <- reactive(chk()$data)
      bc <- reactive(bivariate_classes(sfd()[[input$hz]], sfd()$vulnerability))
      lee <- reactive(cached(paste0("lee:", input$hz), lee_test(sfd()[[input$hz]], sfd()$vulnerability, chk()$w$lw)))

      output$answer <- renderUI({
        b <- bc(); l <- lee(); pop <- sum(sfd()$population[b$double], na.rm = TRUE)
        reg <- sort(table(sfd()$wb_region[b$double]), decreasing = TRUE)
        txt <- sprintf("<b>%d countries</b>, home to %s people, face high %s and high vulnerability%s",
                       sum(b$double), fmt_pop(pop), ACT_PHRASE[[input$hz]],
                       if (length(reg)) sprintf(", mostly in %s. ", paste(names(reg)[seq_len(min(2, length(reg)))], collapse = " and ")) else ". ")
        txt <- paste0(txt, if (l$p < 0.05) sprintf("The two burdens cluster together in space (Lee's L = %.2f, p %s), so regional programmes can reach many of them at once.", l$L, fmt_p(l$p))
                           else sprintf("The two burdens do not significantly cluster together in space (Lee's L = %.2f, p %s), so each country needs its own plan.", l$L, fmt_p(l$p)))
        tagList(answer_box(HTML(txt)), data_note(chk()))
      })

      fast_map_server("map", store,
        style_r = reactive(bivariate_style(store, chk()$expand(bc()$cls), chk()$expand(bc()$label), hz_lab(), "Vulnerability")),
        static_r = reactive(bivariate_static(store, chk()$expand(bc()$cls), hz_lab(), "Vulnerability")),
        title = reactive(sprintf("Where %s meets vulnerability", tolower(hz_lab()))),
        caption = reactive(sprintf("Terciles across %d countries with data (grey: excluded); Lee's L = %.2f, p %s. Source: %s", chk()$n, lee()$L, fmt_p(lee()$p), store$sources$inform)),
        filename = reactive(paste0("double_burden_", input$hz)))

      output$list <- renderUI({
        b <- bc(); d <- sfd()[b$double, ]
        if (!nrow(d)) return(p("No country is in the top third for both."))
        groups <- split(d$country, d$wb_region)
        groups <- groups[order(-lengths(groups))]
        div(class = "country-list", lapply(names(groups), function(r) tagList(h5(sprintf("%s (%d)", r, length(groups[[r]]))), span(paste(sort(groups[[r]]), collapse = ", ")))))
      })

      output$hazard_action <- renderUI({
        a <- national_actions(input$hz)
        if (is.null(a)) return(NULL)
        div(class = "action-card hazard-action", h4(sprintf("For %s specifically", tolower(a$name))),
            p(a$gov), p(class = "gr-card-sub", sprintf("Why: %s.", a$why)))
      })
    })
  })

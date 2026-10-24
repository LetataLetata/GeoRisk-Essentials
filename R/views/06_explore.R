# Explore page: a free background map where clicking any country opens its
# profile. The profile follows the "Colour countries by" choice: with Overall
# risk it explains the country's INFORM Risk score; with a hazard or weakness
# (coastal flood, drought, conflict, governance ...) it explains that score
# instead: the level, why the country is rated so, the measured facts and the
# recorded events behind it, and where it fits in the overall score
# (R/analysis/explain.R). General facts come from REST Countries and
# Wikipedia when the server is online.

EXPLORE_HAZARDS <- c("earthquake", "river_flood", "tsunami", "tropical_cyclone", "coastal_flood", "drought",
                     "epidemic", "projected_conflict_probability", "current_conflict_intensity")
EXPLORE_WEAK <- c("development_and_deprivation", "inequality", "economic_dependency", "uprooted_people",
                  "health_conditions", "children_u5", "recent_shocks", "food_security",
                  "drr", "governance", "communication", "physical_infrastructure", "access_to_health_care")
OVERALL_CHOICES <- c("inform_risk", "risk_class", "none")

# The largest part of a country, so zooming to Russia or Fiji is not stretched across the date line.
# Areas are compared on the flat longitude-latitude plane (crs removed), which needs only GEOS:
# the spherical version needs the lwgeom package, which hosted servers may not have.
main_part_bbox <- function(g) {
  parts <- suppressWarnings(sf::st_cast(sf::st_geometry(g), "POLYGON"))
  flat <- sf::st_set_crs(parts, NA)
  as.numeric(sf::st_bbox(parts[which.max(as.numeric(sf::st_area(flat)))]))
}

register_view(
  id = "explore", title = "Explore", mode = "analyst", icon = "map-location-dot", order = 60,
  ui = function(id, store) {
    ns <- NS(id)
    inf <- store$inform[order(store$inform$country), ]
    colour_choices <- c(list("Overall" = c("Overall risk (score 0 to 10)" = "inform_risk", "Overall risk class" = "risk_class",
                                           "Nothing, just the map" = "none")),
                        variable_choices(store, c("Risk and its three parts", "Hazards", "Vulnerability", "Coping capacity", "Data quality"),
                                         exclude = "inform_risk"))
    tagList(
      page_head("Explore the world, one country at a time",
                "Click any country on the map to see its risk profile. Choose a hazard or weakness in \"Colour countries by\" and the profile explains that score instead.",
                controls(
                  selectizeInput(ns("find"), "Find a country", choices = c("Type a name" = "", stats::setNames(inf$iso3, inf$country)), width = "220px"),
                  selectInput(ns("colour"), "Colour countries by", colour_choices, selected = "inform_risk", width = "260px"),
                  sliderInput(ns("opacity"), "Colour strength", 0, 1, 0.55, step = 0.05, width = "170px")),
                how = PAGE_HOW$explore),
      div(class = "explore-grid",
        div(class = "gr-card map-card explore-map", leaflet::leafletOutput(ns("map"), height = "100%")),
        div(class = "gr-card explore-panel", uiOutput(ns("panel"))))
    )
  },
  server = function(id, store, shared) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      need_data(store, "inform")
      sfd <- store$inform_sf
      inf <- store$inform
      sel <- reactiveVal(isolate(shared$country()))

      fill_spec <- function(colour) {
        if (colour == "none") return(list(cols = rep("#000000", nrow(sfd)), pal = NULL, values = NULL, title = NULL))
        v <- sfd[[colour]]
        pal <- pal_for(colour, v)
        list(cols = pal(v), pal = pal, values = if (is.numeric(v)) v else factor(levels(v), levels = levels(v)),
             title = if (colour == "risk_class") "Overall risk class" else if (colour == "inform_risk") "Overall risk (0-10)" else var_label(store, colour))
      }

      tip <- function(colour) {
        f <- fill_spec(colour)
        val <- if (colour == "none") "" else {
          v <- sfd[[colour]]; sprintf("<br/>%s: <b>%s</b>", f$title, if (is.numeric(v)) fmt_num(v, 2) else as.character(v))
        }
        sprintf("<div class='map-tip'><b>%s</b>%s<br/><i>Click for the country profile</i></div>", sfd$country, val)
      }

      draw_countries <- function(m, colour, opacity) {
        f <- fill_spec(colour)
        m <- m |>
          leaflet::clearGroup("countries") |>
          leaflet::addPolygons(data = sfd, layerId = ~iso3, group = "countries",
                               fillColor = f$cols, fillOpacity = if (colour == "none") 0.001 else opacity,
                               color = if (colour == "none") "#16A34A" else "#FFFFFF", weight = 0.7, opacity = 0.8,
                               smoothFactor = 0.5, label = lapply(tip(colour), htmltools::HTML),
                               highlightOptions = leaflet::highlightOptions(weight = 2.5, color = "#111827", fillOpacity = min(1, opacity + 0.2),
                                                                            bringToFront = FALSE)) |>
          leaflet::removeControl("legend")
        if (!is.null(f$pal)) m <- leaflet::addLegend(m, "bottomleft", pal = f$pal, values = f$values, title = f$title, bins = 5,
                                                     opacity = 0.95, layerId = "legend")
        m
      }

      draw_selected <- function(m, iso) {
        m <- leaflet::clearGroup(m, "selected")
        if (is.null(iso) || !iso %in% sfd$iso3) return(m)
        leaflet::addPolylines(m, data = sf::st_cast(sfd[sfd$iso3 == iso, ], "MULTILINESTRING"), group = "selected",
                              color = "#111827", weight = 3.5, opacity = 1,
                              options = leaflet::pathOptions(interactive = FALSE))
      }

      fly_to <- function(iso) {
        # Never let a zoom problem end the session: fall back to the whole country's box.
        b <- tryCatch(main_part_bbox(sfd[sfd$iso3 == iso, ]),
                      error = function(e) as.numeric(sf::st_bbox(sfd[sfd$iso3 == iso, ])))
        leaflet::leafletProxy("map") |> leaflet::flyToBounds(b[1], b[2], b[3], b[4], options = list(maxZoom = 5, duration = 0.8))
      }

      output$map <- leaflet::renderLeaflet({
        leaflet_basemaps() |>
          leaflet::setView(20, 10, 2) |>
          draw_countries(isolate(input$colour), isolate(input$opacity)) |>
          draw_selected(isolate(sel()))
      })

      # Changing the colouring only recolours the shapes already on the map; the
      # shapes are redrawn only when switching to or from "just the map" or when
      # the colour strength changes (their hover style depends on both).
      last <- reactiveVal(NULL)
      observeEvent(list(input$colour, input$opacity), {
        prev <- last(); last(list(colour = input$colour, opacity = input$opacity))
        quick <- !is.null(prev) && identical(prev$opacity, input$opacity) && prev$colour != "none" && input$colour != "none"
        if (quick) {
          f <- fill_spec(input$colour)
          session$sendCustomMessage("gr-restyle", list(id = session$ns("map"), ids = sfd$iso3, fill = f$cols, labels = tip(input$colour)))
          leaflet::leafletProxy("map") |> leaflet::removeControl("legend") |>
            leaflet::addLegend("bottomleft", pal = f$pal, values = f$values, title = f$title, opacity = 0.95, layerId = "legend", bins = 5)
        } else {
          leaflet::leafletProxy("map") |> draw_countries(input$colour, input$opacity) |> draw_selected(sel())
        }
      }, ignoreInit = TRUE)
      observeEvent(TRUE, last(list(colour = isolate(input$colour), opacity = isolate(input$opacity))), once = TRUE)

      choose <- function(iso, fly = TRUE) {
        if (is.null(iso) || !nzchar(iso) || !iso %in% sfd$iso3) return()
        sel(iso); shared$country(iso)
        leaflet::leafletProxy("map") |> draw_selected(iso)
        if (fly) fly_to(iso)
      }
      observeEvent(input$map_shape_click, choose(input$map_shape_click$id, fly = FALSE))
      observeEvent(input$find, choose(input$find), ignoreInit = TRUE)
      observeEvent(input$jump, choose(input$jump))
      observeEvent(shared$country(), if (!identical(shared$country(), sel())) sel(shared$country()), ignoreInit = TRUE)
      # Clicking a part or a step in the panel switches the map to that score.
      observeEvent(input$colour_to, updateSelectInput(session, "colour", selected = input$colour_to))

      # The score the panel explains: overall risk unless a single score is chosen.
      focus <- reactive({ col <- input$colour %||% "inform_risk"; if (col %in% OVERALL_CHOICES) "inform_risk" else col })
      trend_var <- reactive(if (focus() %in% unique(store$trend$var)) focus() else "inform_risk")

      output$spark <- renderPlot({
        iso <- sel(); req(iso)
        tv <- trend_var()
        tr <- store$trend[store$trend$iso3 == iso & store$trend$var == tv, ]
        req(nrow(tr) > 1); tr <- tr[order(tr$year), ]
        bands <- data.frame(ymin = c(0, 2.35, 3.55, 5.35, 6.95), ymax = c(2.35, 3.55, 5.35, 6.95, 10), cls = names(GR$class_cols))
        ggplot2::ggplot(tr, ggplot2::aes(year, score)) +
          (if (tv == "inform_risk") ggplot2::geom_rect(data = bands, ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = cls),
                                                       inherit.aes = FALSE, alpha = 0.18)) +
          ggplot2::scale_fill_manual(values = GR$class_cols, guide = "none") +
          ggplot2::geom_line(linewidth = 1.1, colour = GR$ink) + ggplot2::geom_point(size = 1.8, colour = GR$ink) +
          ggplot2::scale_y_continuous(limits = c(0, 10), breaks = c(0, 5, 10), expand = c(0, 0)) +
          ggplot2::scale_x_continuous(breaks = range(tr$year)) +
          ggplot2::labs(x = NULL, y = NULL) + theme_georisk(10) +
          ggplot2::theme(plot.margin = ggplot2::margin(4, 8, 2, 2), panel.grid.major.x = ggplot2::element_blank())
      }, height = 130, res = 96)

      # ---- panel pieces -------------------------------------------------
      goto_score <- function(v) sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("colour_to"), v)
      score_col <- function(v, x, lvl) if (v == "inform_risk") GR$class_cols[[lvl]] else LEVEL_COLS[[lvl]]

      focus_box <- function(r, e) {
        col <- score_col(e$var, e$score, e$level)
        div(class = "xrisk xfocus", style = sprintf("border-color:%s;", col),
          div(class = "xfocus-label", if (e$var == "inform_risk") "Overall risk" else e$label),
          div(class = "xfocus-row",
            div(class = "xrisk-score", if (is.na(e$score)) "n/a" else fmt_num(e$score, 1), span("/10")),
            div(div(class = "xrisk-class", style = sprintf("background:%s;", col),
                    if (e$var == "inform_risk") paste(e$level, "risk") else e$level),
                if (!is.na(e$rank)) div(class = "xsub", sprintf("%s highest of %d countries%s", ordinal(e$rank), e$n,
                                                                if (e$var == "inform_risk") "" else sprintf(", higher than %.0f%%. Level compared with other countries.", e$pct))),
                if (e$var == "inform_risk" && !is.null(r$inform_risk_adj))
                  div(class = "xsub", sprintf("Adjusted for data reliability: %.1f", r$inform_risk_adj)))),
          if (!is.na(e$score)) div(class = "xbar-track xfocus-track",
              div(class = "xbar-fill", style = sprintf("width:%.0f%%;background:%s;", 10 * e$score, col)),
              div(class = "xbar-med", style = sprintf("left:%.0f%%;", 10 * e$median), title = "World median")),
          div(class = "xsub", sprintf("Thin dark line: world median (%.1f). %s", e$median, e$description %||% "")))
      }

      why_box <- function(e) div(class = "xwhy", div(class = "xwhy-tag", icon("circle-question"), " Why this rating"), p(e$why))

      parts_box <- function(e) {
        if (is.null(e$parts) || !nrow(e$parts)) return(NULL)
        tagList(h4(class = "xh", "What it is made of"),
          div(class = "xsub", style = "margin:-2px 0 6px;", "Click a part to show it on the map and explain it."),
          lapply(seq_len(nrow(e$parts)), function(i) {
            pr <- e$parts[i, ]; pct <- worse_than(pr$score, inf[[pr$var]], 1); lvl <- level_of(pct)
            tags$button(type = "button", class = "xpart", onclick = goto_score(pr$var),
              span(class = "xpart-name", pr$label),
              span(class = "xbar-track", span(class = "xbar-fill", style = sprintf("width:%.0f%%;background:%s;", if (is.na(pr$score)) 0 else 10 * pr$score, LEVEL_COLS[[lvl]]))),
              span(class = "xpart-score", if (is.na(pr$score)) "n/a" else sprintf("%.1f", pr$score)))
          }))
      }

      facts_box <- function(e) {
        f <- e$facts
        if (is.null(f) || !nrow(f)) return(NULL)
        if (!is.null(e$parts)) f <- f[order(-ifelse(is.na(f$worse), -1, f$worse)), ][seq_len(min(6, nrow(f))), ]
        tagList(h4(class = "xh", "The measured facts behind it"),
          lapply(seq_len(nrow(f)), function(i) {
            x <- f[i, ]; w <- if (is.na(x$worse)) NA else x$worse
            div(class = "xfactrow",
              div(class = "xfact-top", span(x$label), strong(x$shown)),
              if (!is.na(w)) div(class = "xfact-cmp",
                span(class = "xfact-track", span(class = "xfact-fill", style = sprintf("width:%.0f%%;background:%s;", w, LEVEL_COLS[[level_of(w)]]))),
                span(sprintf("%s than %.0f%% of countries", if (x$dir > 0) "Higher" else "Worse", w))),
              div(class = "xfact-src", paste(c(if (!is.na(x$year)) x$year, if (!is.null(e$parts)) var_label(store, x$part)), collapse = "  |  ")))
          }),
          div(class = "xsub", "Source: INFORM Risk 2026 indicator data (European Commission JRC), compiled from UN agencies, the World Bank, EM-DAT and others."))
      }

      events_box <- function(e) {
        ev <- e$events
        items <- tagList(
          if (!is.null(ev$disasters)) {
            d <- ev$disasters; mx <- max(c(d$people, 1), na.rm = TRUE)
            div(class = "xev",
              div(class = "xev-title", icon("house-flood-water"), " People affected by natural disasters"),
              div(class = "xev-bars", lapply(seq_len(nrow(d)), function(i) div(class = "xev-bar",
                span(class = "xev-year", d$year[i]),
                span(class = "xev-track", span(class = "xev-fill", style = sprintf("width:%.0f%%;", if (is.na(d$people[i])) 0 else 100 * d$people[i] / mx))),
                span(class = "xev-n", fmt_ind(d$people[i], "people"))))),
              div(class = "xfact-src", "All natural disasters recorded in EM-DAT, as used by INFORM"))
          },
          if (!is.null(ev$drought)) div(class = "xev", div(class = "xev-title", icon("sun-plant-wilt"), " Drought history"), p(ev$drought)),
          if (!is.null(ev$quakes)) {
            q <- ev$quakes
            div(class = "xev",
              div(class = "xev-title", icon("house-crack"), if (q$tsunami) " Strong undersea or coastal earthquakes" else " Strongest earthquakes on record"),
              p(sprintf("%d earthquakes of magnitude %s or more recorded on or within 100 km of its land, 1965-2016%s.",
                        q$n, if (q$tsunami) "7" else "5.5", if (!q$tsunami && q$n7) sprintf(", %d of them magnitude 7 or more", q$n7) else "")),
              tags$ul(class = "xev-list", lapply(seq_len(nrow(q$top)), function(i)
                tags$li(strong(sprintf("Magnitude %.1f", q$top$magnitude[i])), format(q$top$date[i], " %d %B %Y")))),
              div(class = "xfact-src", "US Geological Survey catalogue of significant earthquakes"))
          },
          if (!is.null(ev$conflict)) div(class = "xev", div(class = "xev-title", icon("person-rifle"), " Conflict"), p(ev$conflict)),
          if (!is.null(ev$displaced)) div(class = "xev", div(class = "xev-title", icon("people-arrows"), " People displaced"),
                                          tags$ul(class = "xev-list", lapply(ev$displaced, tags$li))),
          if (!is.null(ev$aid)) div(class = "xev", div(class = "xev-title", icon("hand-holding-heart"), " Humanitarian aid"), p(ev$aid)))
        if (!length(Filter(Negate(is.null), items))) return(NULL)
        tagList(h4(class = "xh", "Recorded events"), items)
      }

      path_box <- function(e, r) {
        if (e$var == "inform_risk") return(NULL)
        p <- e$path
        tagList(h4(class = "xh", "Where it fits in the overall score"),
          div(class = "xpath", lapply(seq_along(p), function(i) tagList(
            tags$button(type = "button", class = paste("xpath-step", if (i == 1) "on"), onclick = goto_score(p[i]),
                        span(if (p[i] == "inform_risk") "Overall risk" else var_label(store, p[i])),
                        strong(fmt_num(r[[p[i]]], 1))),
            if (i < length(p)) span(class = "xpath-arrow", icon("arrow-right"))))),
          div(class = "xsub", "Each score feeds the one to its right; click any of them to show it on the map."))
      }

      output$panel <- renderUI({
        iso <- sel()
        if (is.null(iso) || !iso %in% inf$iso3)
          return(div(class = "explore-empty", icon("hand-pointer"), h3("Click a country"), p("Its profile will appear here.")))
        r <- inf[inf$iso3 == iso, ]
        fv <- focus()
        e <- explain_variable(store, iso, fv)
        overall <- fv == "inform_risk"
        pr <- country_profile(store, iso)
        facts <- withProgress(message = "Fetching country facts", value = 0.5, country_facts(iso))
        wiki <- country_summary(r$country, if (!is.null(facts)) facts$common)
        ow <- if (!is.null(store$owid)) store$owid$latest[store$owid$latest$iso3 == iso, ] else NULL
        med <- vapply(c("hazard_and_exposure", "vulnerability", "lack_of_coping_capacity"),
                      function(v) stats::median(inf[[v]], na.rm = TRUE), numeric(1))

        bar <- function(label, v, m, col) div(class = "xbar",
          div(class = "xbar-top", span(label), strong(fmt_num(v, 1))),
          div(class = "xbar-track", div(class = "xbar-fill", style = sprintf("width:%.0f%%;background:%s;", 10 * v, col)),
              div(class = "xbar-med", style = sprintf("left:%.0f%%;", 10 * m), title = "World median")))
        chip <- function(lbl, v, var) tags$button(type = "button", class = "xchip", onclick = goto_score(var), lbl, strong(fmt_num(v, 1)))
        fact <- function(lbl, val) if (length(val) && !all(is.na(val)) && any(nzchar(val))) div(class = "xfact", span(lbl), strong(paste(val, collapse = ", ")))

        hz <- r[, EXPLORE_HAZARDS]; hz <- sort(unlist(hz), decreasing = TRUE)[1:3]
        wk <- r[, EXPLORE_WEAK]; wk <- sort(unlist(wk), decreasing = TRUE)[1:3]

        # Neighbours show the score being explained.
        nb_iso <- if (!is.null(facts) && length(facts$borders)) facts$borders else neighbours_of(store, iso, 5)
        nb_iso <- nb_iso[nb_iso %in% inf$iso3]
        nb <- lapply(nb_iso, function(n) {
          rr <- inf[inf$iso3 == n, ]; sc <- rr[[fv]]
          dot <- if (overall) GR$class_cols[[as.character(rr$risk_class)]] else LEVEL_COLS[[level_of(worse_than(sc, inf[[fv]], 1))]]
          tags$button(class = "xnb", type = "button",
                      onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("jump"), n),
                      span(class = "xdot", style = sprintf("background:%s;", dot)),
                      rr$country, span(class = "xnb-score", fmt_num(sc, 1)))
        })

        # Actions for the score being explained, at national level.
        act_line <- if (!overall) {
          if (!is.null(HAZARD_ACTIONS[[fv]])) HAZARD_ACTIONS[[fv]]$gov else if (!is.null(WEAKNESS_ACTIONS[[fv]])) WEAKNESS_ACTIONS[[fv]] else NULL
        }

        tagList(
          tags$script(HTML("var xp = document.querySelector('.explore-panel'); if (xp) xp.scrollTop = 0;")),
          div(class = "xhead",
            if (!is.null(facts) && isTRUE(nzchar(facts$flag))) tags$img(src = facts$flag, alt = paste("Flag of", r$country), class = "xflag"),
            div(h2(r$country), div(class = "xsub", paste(c(if (!is.null(facts)) facts$subregion else NULL, r$wb_region, r$wb_incomegroup), collapse = "  |  ")))),
          if (!overall) div(class = "xmode", icon("layer-group"), " Showing ", b_(e$label), ". ",
                            tags$button(type = "button", class = "xlinkbtn", onclick = goto_score("inform_risk"),
                                        sprintf("Overall risk: %.1f (%s)", r$inform_risk, as.character(r$risk_class)), icon("arrow-right"))),
          focus_box(r, e),
          why_box(e),
          if (overall) tagList(
            h4(class = "xh", "The three parts of risk"),
            div(class = "xsub", style = "margin:-2px 0 6px;", "Overall risk combines these three. Thin dark line: world median."),
            tags$button(type = "button", class = "xpart-wrap", onclick = goto_score("hazard_and_exposure"), bar("Hazard and exposure", r$hazard_and_exposure, med[1], GR$pillar_cols[["Hazard"]])),
            tags$button(type = "button", class = "xpart-wrap", onclick = goto_score("vulnerability"), bar("Vulnerability", r$vulnerability, med[2], GR$pillar_cols[["Vulnerability"]])),
            tags$button(type = "button", class = "xpart-wrap", onclick = goto_score("lack_of_coping_capacity"), bar("Lack of coping capacity", r$lack_of_coping_capacity, med[3], GR$pillar_cols[["Coping"]])))
          else parts_box(e),
          if (overall) tagList(
            h4(class = "xh", "Highest hazards"), div(lapply(names(hz), function(h) chip(var_label(store, h), hz[[h]], h))),
            h4(class = "xh", "Main weak points"), div(lapply(names(wk), function(w) chip(var_label(store, w), wk[[w]], w))),
            div(class = "xsub", "Click a hazard or weak point to see why it is rated so."))
          else facts_box(e),
          events_box(e),
          path_box(e, r),
          if (trend_var() == fv) tagList(h4(class = "xh", sprintf("%s since 2017", if (overall) "Risk" else e$label)),
                                         plotOutput(ns("spark"), height = "130px")),
          h4(class = "xh", "What helps (national level)"),
          div(class = "scale-note", icon("flag"), b_(" For the country as a whole. "),
              sprintf("Based on %s's national scores. These are actions for the national government and its partners, not an assessment of any particular city, town or community, where conditions can differ a lot.", r$country)),
          tags$ul(class = "xactions",
            if (!is.null(act_line)) tags$li(b_(paste0(e$label, ":")), " ", act_line)
            else tagList(lapply(pr$hazard_lines, function(h) tags$li(b_(paste0(h$name, ":")), " ", h$gov)),
                         lapply(pr$weak_lines, function(w) tags$li(b_(paste0(w$name, ":")), " ", w$action)))),
          h4(class = "xh", "Key facts"),
          div(class = "xfacts",
            fact("Capital", if (!is.null(facts)) facts$capital),
            fact("Population", fmt_pop(r$population)),
            fact("Area", if (!is.null(facts) && isTRUE(facts$area > 0)) paste(format(round(facts$area), big.mark = ","), "km2")),
            fact("GDP per person", if (!is.null(ow) && nrow(ow) && !is.na(ow$gdp_per_capita)) sprintf("$%s (%d)", format(round(ow$gdp_per_capita), big.mark = ","), ow$gdp_per_capita_year)),
            fact("Electricity per person", if (!is.null(ow) && nrow(ow) && !is.na(ow$electricity_per_capita)) sprintf("%s kWh (%d)", format(round(ow$electricity_per_capita), big.mark = ","), ow$electricity_per_capita_year)),
            fact("Renewable electricity", if (!is.null(ow) && nrow(ow) && !is.na(ow$renewables_share_elec)) sprintf("%.0f%% (%d)", ow$renewables_share_elec, ow$renewables_share_elec_year)),
            fact("Languages", if (!is.null(facts)) facts$languages),
            fact("Currency", if (!is.null(facts)) facts$currencies),
            fact("Time zones", if (!is.null(facts)) facts$timezones)),
          if (length(nb)) tagList(h4(class = "xh", sprintf("%s (click to jump)%s",
                                                           if (!is.null(facts) && length(facts$borders)) "Neighbours" else "Nearest countries",
                                                           if (overall) "" else paste0(": ", tolower(e$label)))),
                                  div(class = "xnbs", nb)),
          if (!is.null(wiki)) tagList(h4(class = "xh", "About"), p(class = "xtext", wiki$text),
                                      tags$a(href = wiki$url, target = "_blank", class = "xlink", icon("arrow-up-right-from-square"), " Read more on Wikipedia")),
          if (is.null(facts)) div(class = "gr-note note-info", style = "margin-top:10px;", icon("wifi"),
                                  " Capital, languages, flag and the summary need an internet connection. Everything else comes from the dashboard's own data."),
          h4(class = "xh", "How reliable is this?"),
          p(class = "xtext", pr$reliability, sprintf(" Lack of reliability: %s out of 10; %s of indicators imputed.",
                                                     fmt_num(r$reliability_index, 1), if (is.na(r$imputed_share)) "n/a" else sprintf("%.0f%%", 100 * r$imputed_share))),
          div(class = "xbtns",
            if (!is.null(facts) && isTRUE(nzchar(facts$osm))) tags$a(href = facts$osm, target = "_blank", class = "btn btn-outline-dark btn-sm", icon("map"), " OpenStreetMap"))
        )
      })
    })
  })

# Explore page: a free background map where clicking any country opens its
# profile. Background maps come from free tile services (no key needed) and
# general facts from REST Countries and Wikipedia when the computer is online.

EXPLORE_HAZARDS <- c("earthquake", "river_flood", "tsunami", "tropical_cyclone", "coastal_flood", "drought",
                     "epidemic", "projected_conflict_probability", "current_conflict_intensity")
EXPLORE_WEAK <- c("development_and_deprivation", "inequality", "economic_dependency", "uprooted_people",
                  "health_conditions", "children_u5", "recent_shocks", "food_security",
                  "drr", "governance", "communication", "physical_infrastructure", "access_to_health_care")

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
    colour_choices <- c(list("Classes" = c("INFORM risk class" = "risk_class", "Nothing, just the map" = "none")),
                        variable_choices(store, c("Risk and its three parts", "Hazards", "Vulnerability", "Coping capacity", "Data quality")))
    tagList(
      page_head("Explore the world, one country at a time",
                "Click any country on the map to see its risk profile, key facts and how it has changed. Change the background map with the layers button at the top right of the map.",
                controls(
                  selectizeInput(ns("find"), "Find a country", choices = c("Type a name" = "", stats::setNames(inf$iso3, inf$country)), width = "220px"),
                  selectInput(ns("colour"), "Colour countries by", colour_choices, selected = "risk_class", width = "250px"),
                  sliderInput(ns("opacity"), "Colour strength", 0, 1, 0.55, step = 0.05, width = "170px"))),
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
             title = if (colour == "risk_class") "Risk class" else var_label(store, colour))
      }

      draw_countries <- function(m, colour, opacity) {
        f <- fill_spec(colour)
        val <- if (colour == "none") "" else {
          v <- sfd[[colour]]; sprintf("<br/>%s: <b>%s</b>", f$title, if (is.numeric(v)) fmt_num(v, 2) else as.character(v))
        }
        lab <- sprintf("<div class='map-tip'><b>%s</b>%s<br/><i>Click for the country profile</i></div>", sfd$country, val)
        m <- m |>
          leaflet::clearGroup("countries") |>
          leaflet::addPolygons(data = sfd, layerId = ~iso3, group = "countries",
                               fillColor = f$cols, fillOpacity = if (colour == "none") 0.001 else opacity,
                               color = if (colour == "none") "#16A34A" else "#FFFFFF", weight = 0.7, opacity = 0.8,
                               smoothFactor = 0.5, label = lapply(lab, htmltools::HTML),
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
          f <- fill_spec(input$colour); v <- sfd[[input$colour]]
          lab <- sprintf("<div class='map-tip'><b>%s</b><br/>%s: <b>%s</b><br/><i>Click for the country profile</i></div>", sfd$country, f$title,
                         if (is.numeric(v)) fmt_num(v, 2) else as.character(v))
          session$sendCustomMessage("gr-restyle", list(id = session$ns("map"), ids = sfd$iso3, fill = f$cols, labels = lab))
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

      output$spark <- renderPlot({
        iso <- sel(); req(iso)
        tr <- store$trend[store$trend$iso3 == iso & store$trend$var == "inform_risk", ]
        req(nrow(tr) > 1); tr <- tr[order(tr$year), ]
        bands <- data.frame(ymin = c(0, 2.35, 3.55, 5.35, 6.95), ymax = c(2.35, 3.55, 5.35, 6.95, 10), cls = names(GR$class_cols))
        ggplot2::ggplot(tr, ggplot2::aes(year, score)) +
          ggplot2::geom_rect(data = bands, ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = cls),
                             inherit.aes = FALSE, alpha = 0.18) +
          ggplot2::scale_fill_manual(values = GR$class_cols, guide = "none") +
          ggplot2::geom_line(linewidth = 1.1, colour = GR$ink) + ggplot2::geom_point(size = 1.8, colour = GR$ink) +
          ggplot2::scale_y_continuous(limits = c(0, 10), breaks = c(0, 5, 10), expand = c(0, 0)) +
          ggplot2::scale_x_continuous(breaks = range(tr$year)) +
          ggplot2::labs(x = NULL, y = NULL) + theme_georisk(10) +
          ggplot2::theme(plot.margin = ggplot2::margin(4, 8, 2, 2), panel.grid.major.x = ggplot2::element_blank())
      }, height = 130, res = 96)

      output$panel <- renderUI({
        iso <- sel()
        if (is.null(iso) || !iso %in% inf$iso3)
          return(div(class = "explore-empty", icon("hand-pointer"), h3("Click a country"), p("Its profile will appear here.")))
        r <- inf[inf$iso3 == iso, ]
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
        chip <- function(lbl, v) span(class = "xchip", lbl, strong(fmt_num(v, 1)))
        fact <- function(lbl, val) if (length(val) && !all(is.na(val)) && any(nzchar(val))) div(class = "xfact", span(lbl), strong(paste(val, collapse = ", ")))

        hz <- r[, EXPLORE_HAZARDS]; hz <- sort(unlist(hz), decreasing = TRUE)[1:3]
        wk <- r[, EXPLORE_WEAK]; wk <- sort(unlist(wk), decreasing = TRUE)[1:3]
        cls <- as.character(r$risk_class)

        nb_iso <- if (!is.null(facts) && length(facts$borders)) facts$borders else neighbours_of(store, iso, 5)
        nb_iso <- nb_iso[nb_iso %in% inf$iso3]
        nb <- lapply(nb_iso, function(n) {
          rr <- inf[inf$iso3 == n, ]
          tags$button(class = "xnb", type = "button",
                      onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("jump"), n),
                      span(class = "xdot", style = sprintf("background:%s;", GR$class_cols[[as.character(rr$risk_class)]])),
                      rr$country, span(class = "xnb-score", fmt_num(rr$inform_risk, 1)))
        })

        tagList(
          tags$script(HTML("var xp = document.querySelector('.explore-panel'); if (xp) xp.scrollTop = 0;")),
          div(class = "xhead",
            if (!is.null(facts) && isTRUE(nzchar(facts$flag))) tags$img(src = facts$flag, alt = paste("Flag of", r$country), class = "xflag"),
            div(h2(r$country), div(class = "xsub", paste(c(if (!is.null(facts)) facts$subregion else NULL, r$wb_region, r$wb_incomegroup), collapse = "  |  ")))),
          div(class = "xrisk", style = sprintf("border-color:%s;", GR$class_cols[[cls]]),
            div(class = "xrisk-score", fmt_num(r$inform_risk, 1), span("/10")),
            div(div(class = "xrisk-class", style = sprintf("background:%s;", GR$class_cols[[cls]]), paste(cls, "risk")),
                div(class = "xsub", sprintf("%s highest of %d countries (INFORM 2026)", ordinal(r$rank), nrow(inf))),
                if (!is.null(r$inform_risk_adj)) div(class = "xsub", sprintf("Adjusted for data reliability: %.1f  |  Higher than %.0f%% of countries",
                                                                        r$inform_risk_adj, r$inform_risk_pct)))),
          p(class = "xtext", pr$driver, " ", pr$trend),
          h4(class = "xh", "The three parts of risk"),
          bar("Hazard and exposure", r$hazard_and_exposure, med[1], GR$pillar_cols[["Hazard"]]),
          bar("Vulnerability", r$vulnerability, med[2], GR$pillar_cols[["Vulnerability"]]),
          bar("Lack of coping capacity", r$lack_of_coping_capacity, med[3], GR$pillar_cols[["Coping"]]),
          div(class = "xsub", style = "margin-top:-4px;", "Thin dark line: world median."),
          h4(class = "xh", "Risk since 2017"),
          plotOutput(ns("spark"), height = "130px"),
          h4(class = "xh", "Highest hazards"), div(lapply(names(hz), function(h) chip(var_label(store, h), hz[[h]]))),
          h4(class = "xh", "Main weak points"), div(lapply(names(wk), function(w) chip(var_label(store, w), wk[[w]]))),
          h4(class = "xh", "What helps (national level)"),
          div(class = "scale-note", icon("flag"), strong(" For the country as a whole. "),
              sprintf("Based on %s's national scores. These are actions for the national government and its partners, not an assessment of any particular city, town or community, where conditions can differ a lot.", r$country)),
          tags$ul(class = "xactions",
            lapply(pr$hazard_lines, function(h) tags$li(strong(h$name, ": "), h$gov)),
            lapply(pr$weak_lines, function(w) tags$li(strong(w$name, ": "), w$action))),
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
          if (length(nb)) tagList(h4(class = "xh", if (!is.null(facts) && length(facts$borders)) "Neighbours (click to jump)" else "Nearest countries (click to jump)"),
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

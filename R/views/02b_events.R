# Step 3, Events: are disaster events themselves clustered? Point pattern
# analysis of every significant earthquake (magnitude 5.5 and above,
# 1965-2016) against complete spatial randomness: kernel density, the quadrat
# test, the nearest-neighbour index, and the G and L functions. Results are
# precomputed (scripts/precompute_points.R); this page only displays them.

register_view(
  id = "events", title = "Events", mode = "analyst", icon = "house-crack", order = 15,
  ui = function(id, store) {
    ns <- NS(id)
    pp <- store$points
    tagList(
      step_head("events", "Are disaster events themselves clustered?",
                "Risk scores might cluster only because of how the index is built. So this step tests the raw events: every recorded earthquake of magnitude 5.5 or more, 1965-2016, against complete spatial randomness.",
                controls(selectInput(ns("region"), "Region", if (is.null(pp)) "World" else pp$regions, selected = "World", width = "190px"),
                         selectInput(ns("mag"), "Magnitude at least", c("5.5" = 5.5, "6.0" = 6, "6.5" = 6.5, "7.0" = 7), selected = 5.5, width = "150px"))),
      uiOutput(ns("answer")),
      gr_grid(cols = "1.55fr 1fr",
        image_chart_ui(ns("density"), "Where earthquakes happen: kernel density", "Colour: earthquakes per 10,000 km2 (Gaussian kernel). Dots: the events. Equal-area projection.", height = 430),
        gr_card(title = "Two tests of complete spatial randomness", subtitle = "Random would give a ratio of 1 and an index of 1",
                uiOutput(ns("tests")))),
      gr_grid(cols = "1fr 1fr",
        chart_ui(ns("G"), "Nearest-neighbour distances: the G function", "Red: observed. Dashed line and grey band: what 39 random patterns give. Red above the band = clustered.", height = 360),
        chart_ui(ns("L"), "Clustering at every scale: the L function", "Ripley's K, made linear (L(d) - d). Above the grey band = more neighbours within d than random.", height = 360)),
      chart_ui(ns("link"), "Do the events match the country scores?", uiOutput(ns("link_sub"), inline = TRUE), height = 430),
      method_box(
        p(b_("Complete spatial randomness "), "is the null model: events fall independently and with the same intensity everywhere (a homogeneous Poisson process). Each test asks whether the earthquakes depart from it."),
        p(b_("Kernel density "), "estimates the intensity, the expected number of events per unit area, by placing a Gaussian bump on each event and adding them up. The bandwidth is a third of Scott's rule; the full rule smooths the plate boundaries away."),
        p(b_("Quadrat test. "), "The region is divided into equal-area cells and the events in each are counted. The variance-to-mean ratio is 1 under randomness, above 1 when clustered and below 1 when regular. The test statistic (m - 1) x ratio follows a chi-square distribution with m - 1 degrees of freedom. Only whole cells are used; cells cut by the edge of the region are left out. Cell size matters (the modifiable areal unit problem), so the distance-based tests below check the same question without cells."),
        p(b_("Nearest-neighbour index "), "(Clark and Evans): the mean distance from each event to its nearest neighbour, divided by the mean expected under randomness, 0.5 / square root of the intensity. Below 1 means clustered, above 1 dispersed; the z-test uses a standard error of 0.26136 / square root of (n x intensity)."),
        p(b_("G function: "), "the share of events whose nearest neighbour lies within distance d. Under randomness G(d) = 1 - exp(-intensity x pi x d squared). A curve rising faster than the random band means clustering. ",
          b_("L function: "), "Ripley's K counts the events within distance d of each event, scaled by the intensity; under randomness K(d) = pi x d squared. L(d) = square root of (K(d) / pi) is plotted as L(d) - d, so randomness is the line at 0. For large regions it is computed on a random sample of 3,000 events, with border edge correction."),
        p(b_("Envelopes. "), "The grey bands come from 39 simulated random patterns with the same number of events in the same region: a curve outside the band is significant at about the 5% level."),
        p(b_("Projection and region. "), "Distances and areas are measured in equal-area projections: Mollweide for the whole Earth (the window is the whole globe, sea included) and Lambert azimuthal equal-area for each continent (the window is its land plus 100 km, holding the events linked to its countries)."),
        p(b_("Limits. "), "The catalogue ends in 2016 and records only magnitude 5.5 and above. Aftershocks make events depend on each other, which is part of why they cluster. Earthquakes are the one hazard with a global event list here; floods, cyclones and conflict are covered on the other pages through INFORM's scores.")),
      next_step("events")
    )
  },
  server = function(id, store, shared) {
    moduleServer(id, function(input, output, session) {
      need_data(store, "points")
      reg <- reactive(input$region %||% "World")
      mag <- reactive(as.numeric(input$mag %||% 5.5))
      res <- reactive(pp_result(store, reg(), mag()))
      src <- "Source: significant earthquakes M5.5+, 1965-2016, US Geological Survey; analysis in R (spatstat)."
      few <- reactive({ r <- res(); is.null(r) || isTRUE(r$too_few) || !is.null(r$error) })

      output$answer <- renderUI({
        r <- res()
        if (few()) return(answer_box(HTML(sprintf("Too few earthquakes of magnitude %.1f or more in %s (%d) for a reliable test. Choose a lower magnitude or a larger region.",
                                                  mag(), reg(), r$n %||% 0))))
        q <- r$quadrat; nn <- r$nn; lk <- store$points$link[[as.character(mag())]]
        vq <- pp_verdict_vmr(q); vn <- pp_verdict_nn(nn)
        txt <- sprintf(paste0("The <b>%s</b> earthquakes of magnitude %.1f or more %s are <b>%s</b>: counts per equal-area cell vary %s times as much as their mean (1 if random; chi-square p %s), ",
                              "and the nearest other earthquake is on average %.0f%% %s than in a random pattern (nearest-neighbour index %.2f, z = %.1f, p %s). "),
                       format(r$n, big.mark = ","), mag(), if (reg() == "World") "worldwide" else paste("in", reg()), if (vq == vn) vq else sprintf("%s by cell counts and %s by distances", vq, vn),
                       format(round(q$vmr, 1), big.mark = ","), fmt_p(q$p), abs(100 * (1 - nn$r)), if (nn$r < 1) "closer" else "further", nn$r, nn$z, fmt_p(nn$p))
        txt <- paste0(txt, sprintf("Countries with more recorded earthquakes per area have higher INFORM earthquake scores (Spearman's rho = %.2f, p %s), so the risk scores reflect where events really happen.",
                                   lk$rho, fmt_p(lk$p)))
        answer_box(HTML(txt))
      })

      output$tests <- renderUI({
        r <- res(); if (few()) return(p(class = "gr-card-sub", "Not enough events for the tests."))
        q <- r$quadrat; nn <- r$nn
        row <- function(name, value, note) tags$tr(tags$td(name), tags$td(b_(value)), tags$td(class = "pp-note", note))
        tagList(
          tags$table(class = "pp-table",
            tags$thead(tags$tr(tags$th("Quadrat test"), tags$th("Value"), tags$th("Meaning"))),
            tags$tbody(
              row("Equal-area cells", q$m, "whole cells used"),
              row("Mean events per cell", fmt_num(q$mean, 1), ""),
              row("Variance", format(round(q$var), big.mark = ","), ""),
              row("Variance-to-mean ratio", fmt_num(q$vmr, 1), "1 = random, above 1 = clustered"),
              row("Chi-square", sprintf("%s on %d df", format(round(q$chi), big.mark = ","), q$df), sprintf("p %s: %s", fmt_p(q$p), pp_verdict_vmr(q))))),
          tags$table(class = "pp-table",
            tags$thead(tags$tr(tags$th("Nearest neighbour"), tags$th("Value"), tags$th("Meaning"))),
            tags$tbody(
              row("Observed mean distance", sprintf("%.0f km", nn$dobs_km), ""),
              row("Expected if random", sprintf("%.0f km", nn$dexp_km), "0.5 / square root of intensity"),
              row("Nearest-neighbour index", fmt_num(nn$r, 2), "1 = random, below 1 = clustered"),
              row("z", fmt_num(nn$z, 1), sprintf("p %s: %s", fmt_p(nn$p), pp_verdict_nn(nn))))),
          p(class = "gr-card-sub", style = "margin-top:6px;", sprintf("%s events over %s km2; kernel bandwidth %.0f km.%s",
            format(r$n, big.mark = ","), format(round(r$area_km2, -3), big.mark = ","), r$bandwidth_km,
            if (!is.na(r$cluster_upto_km)) sprintf(" The L function shows clustering at distances up to about %s km.", format(round(r$cluster_upto_km, -1), big.mark = ",")) else "")))
      })

      image_chart_server("density", reactive({ req(!few()); fig_pp_density(store, reg(), mag()) }),
        title = reactive(sprintf("Kernel density of earthquakes, magnitude %.1f and above, %s, 1965-2016", mag(), reg())),
        caption = reactive(paste("Gaussian kernel, bandwidth a third of Scott's rule; equal-area projection.", src)),
        filename = reactive(sprintf("earthquake_density_%s_M%s", gsub(" ", "_", reg()), mag())), width = 10, height = 6)
      chart_server("G", reactive({ req(!few()); fig_pp_G(store, reg(), mag()) }),
        title = reactive(sprintf("G function of earthquakes, M%.1f+, %s", mag(), reg())),
        caption = reactive(paste("Red: observed; dashed: complete spatial randomness; band: 39 simulations.", src)),
        filename = reactive("G_function"), width = 7, height = 5)
      chart_server("L", reactive({ req(!few()); fig_pp_L(store, reg(), mag()) }),
        title = reactive(sprintf("L function of earthquakes, M%.1f+, %s", mag(), reg())),
        caption = reactive(paste("L(d) - d; 0 = complete spatial randomness; band: 39 simulations; border correction.", src)),
        filename = reactive("L_function"), width = 7, height = 5)
      chart_server("link", reactive(fig_pp_link(store, mag())),
        title = reactive(sprintf("Recorded earthquakes (M%.1f+) per land area against the INFORM earthquake score", mag())),
        caption = reactive(paste("Each point is a country; earthquakes linked to the nearest country within 100 km.", src)),
        filename = reactive("earthquakes_vs_score"), width = 8, height = 5.5)
      output$link_sub <- renderUI({
        lk <- store$points$link[[as.character(mag())]]
        sprintf("Each point is a country. Spearman's rho = %.2f (p %s, %d countries): the point pattern and the areal scores agree.", lk$rho, fmt_p(lk$p), lk$n)
      })
    })
  })

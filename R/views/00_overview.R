# Overview: the whole story on one screen, for people who are not
# statisticians. Numbers stay (they are the point) but each comes with one
# short plain sentence and a simple picture (dots or a bar) instead of a
# paragraph. Technical terms, p-values and methods live on the step pages;
# report downloads and references are folded away at the bottom.

# A row of small dots, `on` of them coloured: a picture of "x out of n".
dot_grid <- function(n, on, col_on, col_off = "#E5E7EB", hollow = FALSE) {
  div(class = "dots", lapply(seq_len(n), function(i)
    span(class = if (i <= on && hollow) "dot dot-ring" else "dot",
         style = sprintf("%s:%s;", if (i <= on && hollow) "border-color" else "background", if (i <= on) col_on else col_off))))
}

# A bar filled to a share, with the share written on it.
share_bar <- function(share, col) {
  div(class = "sbar", div(class = "sbar-fill", style = sprintf("width:%.0f%%;background:%s;", 100 * share, col)))
}

# One of the four headline cards.
big_card <- function(target, step, number, headline, picture, caption, tone = "light") {
  tags$button(type = "button", class = paste("big-card", paste0("big-", tone)), `data-goto` = target,
    div(class = "big-step", step),
    div(class = "big-num", number),
    div(class = "big-head", headline),
    div(class = "big-pic", picture),
    div(class = "big-cap", caption),
    div(class = "big-go", "How we know", icon("arrow-right")))
}

register_view(
  id = "overview", title = "Overview", mode = "analyst", icon = "house", order = 0,
  ui = function(id, store) {
    ns <- NS(id)
    tagList(
      div(class = "hero hero-simple",
        div(class = "hero-kicker", "191 countries  |  INFORM Risk Index 2026  |  Earthquakes 1965-2016"),
        h1("Global distribution of disasters"),
        p(class = "lead hero-overview", "Six steps take the data to a decision: describe the indicators, map where risk clusters, test whether the disaster events themselves cluster, explain the pattern, check how far it can be trusted, and find where action should come first."),
        tags$img(src = "globe-lines.svg", class = "hero-globe", alt = "", `aria-hidden` = "true"),
        div(class = "hero-actions",
            tags$button(type = "button", class = "hero-btn primary", `data-goto` = "data", icon("play"), " Walk through the analysis"),
            tags$button(type = "button", class = "hero-btn", `data-goto` = "explore", icon("magnifying-glass"), " Look up a country"))),

      div(class = "ov-label", "What the numbers tell us"),
      uiOutput(ns("cards")),

      div(class = "ov-label", "The world at a glance"),
      div(class = "gr-grid ov-world",
        map_ui(ns("map"), "Disaster risk by country, 2026", "Red is highest risk, green is lowest. Hover for the score; click a country for its profile.", height = 470),
        div(class = "gr-card ov-side", uiOutput(ns("side")))),

      div(class = "ov-label", "How we found this"),
      div(class = "ov-path", lapply(seq_len(nrow(STEPS)), function(i) tagList(
        tags$button(type = "button", class = "ov-step", `data-goto` = STEPS$id[i],
                    span(class = "pill-n", i), span(STEPS$question[i])),
        if (i < nrow(STEPS)) span(class = "ov-arrow", icon("chevron-right"))))),

      tags$details(class = "method-box ov-report",
        tags$summary(icon("file-arrow-down"), " For reports: all figures, tables, methods and references"),
        div(
          div(class = "ov-dl",
            downloadButton(ns("figs"), "Download all figures and tables (zip)", class = "btn-dark"),
            downloadLink(ns("dl_workflow"), tagList(icon("download"), " Workflow diagram (PNG)"), class = "wf-dl")),
          p(class = "gr-card-sub", "Every figure in publication quality (PNG at 300 dpi and vector PDF), numbered in the order of the analysis with titles, captions and sources, plus the descriptive statistics, robustness and variable dictionary tables. Takes about a minute."),
          p(strong("Data. "), "INFORM Risk Index Mid-2026 and trend 2017-2026 (European Commission Joint Research Centre): risk on a 0-10 scale as the geometric mean of hazard and exposure, vulnerability, and lack of coping capacity. GDP and energy from Our World in Data. Boundaries from Natural Earth."),
          p(strong("Missing data. "), "Before each analysis runs, countries are checked for every variable that analysis needs. Only countries with complete data are analysed, the neighbours are worked out again among them, and the excluded countries are listed on the page and shown grey on the maps."),
          p(strong("Methods. "), "Spatial weights: 5 nearest neighbours by great-circle distance. Where: global and local Moran's I with permutation tests and false discovery rate correction. Why: ordinary regression tested for spatial dependence with Lagrange multiplier tests, then a spatial error model. How sure: Monte Carlo propagation of data error through the INFORM formula, reliability adjustment by empirical Bayes shrinkage, and robustness checks. Where to act: bivariate tercile map and Lee's L."),
          uiOutput(ns("tech")),
          tags$ul(
            tags$li("Anselin, L. (1995). Local indicators of spatial association: LISA. Geographical Analysis, 27(2), 93-115."),
            tags$li("Anselin, L. (2005). Exploring Spatial Data with GeoDa: A Workbook. Center for Spatially Integrated Social Science."),
            tags$li("Efron, B. and Morris, C. (1975). Data analysis using Stein's estimator and its generalizations. Journal of the American Statistical Association, 70(350), 311-319."),
            tags$li("Lee, S.-I. (2001). Developing a bivariate spatial association measure: an integration of Pearson's r and Moran's I. Journal of Geographical Systems, 3, 369-385."),
            tags$li("LeSage, J. and Pace, R. K. (2009). Introduction to Spatial Econometrics. CRC Press."),
            tags$li("Marin Ferrer, M., Vernaccini, L. and Poljansek, K. (2017). INFORM Index for Risk Management: Concept and Methodology. Publications Office of the European Union."))))
    )
  },
  server = function(id, store, shared) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      need_data(store, "inform", "owid")
      sfd <- store$inform_sf
      inf <- store$inform
      # Each finding uses the same missing-data rule as its own page.
      c_where <- cached_check(store, "inform_risk", "INFORM Risk")
      c_why <- cached_check(store, c("inform_risk", "log_gdp_pc"), c("INFORM Risk", "GDP per person"), d = store$all, tag = "all")
      c_sure <- cached_check(store, c("inform_risk", "hazard_and_exposure", "vulnerability", "lack_of_coping_capacity", "reliability_index"))
      c_act <- cached_check(store, c("hazard_and_exposure", "vulnerability"))

      res <- reactive({
        dw <- c_where$data
        gm <- cached("gm:inform_risk", global_moran(dw$inform_risk, c_where$w$lw, 999))
        lisa <- cached("lisa:inform_risk", local_moran(dw$inform_risk, c_where$w$lw, 0.05, "fdr"))
        hh <- lisa$cluster %in% "High-High"
        reg <- names(sort(table(dw$wb_region[hh]), decreasing = TRUE))[1]
        r2 <- summary(stats::lm(inform_risk ~ log_gdp_pc, data = c_why$data))$r.squared
        hi <- c_sure$data$risk_class %in% c("High", "Very High")
        bs <- blind_spots(c_sure$data, 5)
        bc <- bivariate_classes(c_act$data$hazard_and_exposure, c_act$data$vulnerability)
        pop <- sum(c_act$data$population[bc$double], na.rm = TRUE)
        list(gm = gm, n_hh = sum(hh), n_where = nrow(dw), reg = reg, r2 = r2, n_why = c_why$n,
             n_hi = sum(hi), n_bs = nrow(bs), bs_names = bs$country[order(-bs$inform_risk)],
             pop = pop, pop_share = pop / sum(c_act$data$population, na.rm = TRUE), n_double = sum(bc$double),
             pp = pp_result(store, "World", 5.5), pp_rho = if (!is.null(store$points)) store$points$link[["5.5"]]$rho else NA)
      })

      output$cards <- renderUI({
        r <- res()
        div(class = "big-cards",
          big_card("where", step_label("where", "Where"), r$n_hh,
                   "high-risk countries sit right next to each other",
                   dot_grid(r$n_where, r$n_hh, "#DC2626"),
                   sprintf("out of %d countries, mostly in %s. Risk crosses borders.", r$n_where, sub(" & ", " and ", r$reg)), "dark"),
          if (!is.null(r$pp)) big_card("events", step_label("events", "Events"), sprintf("%.0f times", 1 / r$pp$nn$r),
                   "closer together than chance: earthquakes cluster",
                   share_bar(r$pp$nn$r, "#DC2626"),
                   sprintf("The next earthquake is on average %.0f km away; at random it would be %.0f km. Scores agree (rho %.2f).",
                           r$pp$nn$dobs_km, r$pp$nn$dexp_km, r$pp_rho)),
          big_card("why", step_label("why", "Why"), sprintf("%.0f%%", 100 * r$r2),
                   "of the differences in risk line up with how rich a country is",
                   share_bar(r$r2, "#16A34A"),
                   "Wealth matters a lot, but not everything: neighbours still share risk."),
          big_card("sure", step_label("sure", "How sure"), r$n_bs,
                   "high-risk countries have weak data",
                   dot_grid(r$n_hi, r$n_bs, "#F59E0B", hollow = TRUE),
                   sprintf("out of %d high-risk countries. Check these on the ground.", r$n_hi)),
          big_card("act", step_label("act", "Where to act"), sprintf("%.1f billion", r$pop / 1e9),
                   "people live where big dangers meet poverty and weak services",
                   share_bar(r$pop_share, "#7C3AED"),
                   sprintf("About %.0f in every 100 people, in %d countries.", 100 * r$pop_share, r$n_double)))
      })

      output$side <- renderUI({
        cls <- table(factor(as.character(inf$risk_class), levels = names(GR$class_cols)))
        top <- head(inf[order(-inf$inform_risk), ], 10)
        tagList(
          div(class = "gr-card-title", "How many countries in each risk class"),
          div(class = "cls-bar", lapply(names(cls), function(k)
            div(class = "cls-seg", style = sprintf("flex:%d;background:%s;", cls[[k]], GR$class_cols[[k]]),
                title = sprintf("%s: %d countries", k, cls[[k]]), span(cls[[k]])))),
          div(class = "cls-key", lapply(names(cls), function(k)
            span(span(class = "cls-sw", style = sprintf("background:%s;", GR$class_cols[[k]])), k))),
          div(class = "gr-card-title", style = "margin-top:18px;", "The 10 highest-risk countries"),
          div(class = "gr-card-sub", style = "margin-bottom:6px;", "Click one for its profile"),
          div(class = "top10", lapply(seq_len(nrow(top)), function(i) {
            t <- top[i, ]
            tags$button(type = "button", class = "top-row",
                        onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("pick"), t$iso3),
                        span(class = "top-name", t$country),
                        span(class = "top-track", span(class = "top-fill", style = sprintf("width:%.0f%%;background:%s;", 10 * t$inform_risk,
                                                                                         GR$class_cols[[as.character(t$risk_class)]]))),
                        span(class = "top-score", sprintf("%.1f", t$inform_risk)))
          })))
      })

      # The technical version of the four findings, for the report box.
      output$tech <- renderUI({
        r <- res()
        p(strong("The findings in technical terms. "),
          sprintf("Where: global Moran's I = %.3f (permutation p %s); %d countries in significant High-High clusters (local Moran's I, p < 0.05, false discovery rate). ", r$gm$I, fmt_p(r$gm$p_perm), r$n_hh),
          if (!is.null(r$pp)) sprintf("Events: %s earthquakes of magnitude 5.5 and above (1965-2016) depart from complete spatial randomness (quadrat variance-to-mean ratio %.1f, chi-square p %s; Clark-Evans nearest-neighbour index %.2f, z = %.1f); recorded earthquakes per land area correlate with the INFORM earthquake score (Spearman rho = %.2f). ",
                                      format(r$pp$n, big.mark = ","), r$pp$quadrat$vmr, fmt_p(r$pp$quadrat$p), r$pp$nn$r, r$pp$nn$z, r$pp_rho),
          sprintf("Why: log GDP per person explains R squared = %.2f of the variation in INFORM Risk (n = %d); residuals remain spatially autocorrelated, so a spatial error model is preferred. ", r$r2, r$n_why),
          sprintf("How sure: %d of %d High or Very High risk countries have a lack of reliability above 5 or more than 20%% imputed indicators (%s). ", r$n_bs, r$n_hi, paste(r$bs_names, collapse = ", ")),
          sprintf("Where to act: %d countries in the top tercile for both hazard and exposure and vulnerability, home to %s people.", r$n_double, fmt_pop(r$pop)))
      })

      observeEvent(input$pick, { shared$country(input$pick); shared$goto("explore") })

      click <- map_server("map",
        leaflet_r = reactive(leaflet_choropleth(store, c_where$expand(c_where$data$risk_class), "risk_class", "Risk class",
                             label_extra = sprintf("<br/>Score: %.1f out of 10<br/><i>Click for the country profile</i>", sfd$inform_risk))),
        static_r = reactive(static_choropleth(store, c_where$expand(c_where$data$risk_class), "Risk class", "factor", GR$class_cols)),
        title = "INFORM Risk class by country, 2026", caption = paste("Source:", store$sources$inform), filename = "world_risk_classes")
      observeEvent(click(), { shared$country(click()$id); shared$goto("explore") })

      output$dl_workflow <- downloadHandler("Figure_01_workflow.png", function(f)
        save_plot(fig_workflow() + ggplot2::labs(title = "Analytical workflow: six steps from data to decision",
                                                 caption = "Each step answers one question with the method shown; its output feeds the next step."), f, "png", 12, 3.8))

      output$figs <- downloadHandler(
        function() paste0("GEORISK_Observatory_figures_", format(Sys.Date(), "%Y%m%d"), ".zip"),
        function(file) {
          dir <- file.path(tempfile("figs"), "GEORISK_Observatory_figures")
          files <- withProgress(message = "Building figures", value = 0,
            suppressWarnings(export_all_figures(store, dir, function(i, n, label) incProgress(1 / n, detail = label))))
          if (requireNamespace("zip", quietly = TRUE)) zip::zipr(file, dir)
          else { old <- setwd(dirname(dir)); on.exit(setwd(old)); utils::zip(file, basename(dir)) }
        }, contentType = "application/zip")
    })
  })

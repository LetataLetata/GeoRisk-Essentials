# Overview: the question, the chain of analysis (five steps), the main
# findings, the world map, and the full figure set for the report.

register_view(
  id = "overview", title = "Overview", mode = "analyst", icon = "house", order = 0,
  ui = function(id, store) {
    ns <- NS(id)
    tagList(
      div(class = "hero", style = "margin-bottom:18px;",
        h1(style = "color:#fff;font-weight:800;", "Where is disaster risk highest, why, and how sure can we be?"),
        p(class = "lead", "Disasters do the most harm where danger meets disadvantage. This dashboard uses spatial statistics on the INFORM Risk Index for 191 countries to show where risk concentrates, what explains it, how far the results can be trusted, and where action matters most.")),
      gr_card(title = "How the analysis works", subtitle = "Five steps, each answering one question and feeding the next. Click a step to open it.",
              tools = downloadLink(ns("dl_workflow"), tagList(icon("download"), " Figure"), class = "wf-dl"),
        div(class = "workflow", lapply(seq_len(nrow(STEPS)), function(i) {
          s <- STEPS[i, ]
          tags$button(type = "button", class = "wf-step", onclick = sprintf("Shiny.setInputValue('nav_to', '%s', {priority: 'event'})", s$id),
                      div(class = "wf-n", sprintf("Step %d: %s", i, s$verb)), div(class = "wf-q", s$question),
                      div(class = "wf-m", s$method), div(class = "wf-o", s$output))
        }))),
      uiOutput(ns("findings")),
      map_ui(ns("map"), "Disaster risk by country, 2026", "Click a country to open its profile", height = 460),
      gr_card(title = "Figures and tables for the report",
              subtitle = "Every figure in publication quality (PNG at 300 dpi and vector PDF), numbered in the order of the analysis, with titles, captions and sources; plus the descriptive statistics, robustness and variable dictionary tables.",
              downloadButton(ns("figs"), "Download all figures and tables (zip)", class = "btn-dark"),
              p(class = "gr-card-sub", style = "margin-top:8px;", "Takes about a minute. Each chart and map also has its own download menu (PNG, PDF, SVG, CSV) for the settings you choose on its page.")),
      tags$details(class = "method-box",
        tags$summary(icon("book-open"), " Data, methods and references"),
        div(
          p(strong("Data. "), "INFORM Risk Index Mid-2026 and trend 2017-2026 (European Commission Joint Research Centre): risk on a 0-10 scale as the geometric mean of hazard and exposure, vulnerability, and lack of coping capacity. GDP and energy from Our World in Data. Boundaries from Natural Earth."),
          p(strong("Missing data. "), "Before each analysis runs, countries are checked for every variable that analysis needs. Only countries with complete data are analysed, the neighbours are worked out again among them, and the excluded countries are listed on the page and shown grey on the maps."),
          p(strong("Methods. "), "Spatial weights: 5 nearest neighbours by great-circle distance. Where: global and local Moran's I with permutation tests and false discovery rate correction. Why: ordinary regression tested for spatial dependence with Lagrange multiplier tests, then a spatial error model. How sure: Monte Carlo propagation of data error through the INFORM formula, reliability adjustment by empirical Bayes shrinkage, and robustness checks. Where to act: bivariate tercile map and Lee's L."),
          tags$ul(
            tags$li("Anselin, L. (1995). Local indicators of spatial association: LISA. Geographical Analysis, 27(2), 93-115."),
            tags$li("Anselin, L. (2005). Exploring Spatial Data with GeoDa: A Workbook. Center for Spatially Integrated Social Science."),
            tags$li("Efron, B. and Morris, C. (1975). Data analysis using Stein's estimator and its generalizations. Journal of the American Statistical Association, 70(350), 311-319."),
            tags$li("Lee, S.-I. (2001). Developing a bivariate spatial association measure: an integration of Pearson's r and Moran's I. Journal of Geographical Systems, 3, 369-385."),
            tags$li("LeSage, J. and Pace, R. K. (2009). Introduction to Spatial Econometrics. CRC Press."),
            tags$li("Marin Ferrer, M., Vernaccini, L. and Poljansek, K. (2017). INFORM Index for Risk Management: Concept and Methodology. Publications Office of the European Union.")),
          p(class = "gr-card-sub", "Every chart and map has a download menu (PNG 300 dpi, PDF, SVG, CSV).")))
    )
  },
  server = function(id, store, shared) {
    moduleServer(id, function(input, output, session) {
      need_data(store, "inform", "owid")
      sfd <- store$inform_sf
      # Each finding uses the same missing-data rule as its own page.
      c_where <- cached_check(store, "inform_risk", "INFORM Risk")
      c_why <- cached_check(store, c("inform_risk", "log_gdp_pc"), c("INFORM Risk", "GDP per person"), d = store$all, tag = "all")
      c_sure <- cached_check(store, c("inform_risk", "hazard_and_exposure", "vulnerability", "lack_of_coping_capacity", "reliability_index"))
      c_act <- cached_check(store, c("hazard_and_exposure", "vulnerability"))

      output$findings <- renderUI({
        dw <- c_where$data
        gm <- cached("gm:inform_risk", global_moran(dw$inform_risk, c_where$w$lw, 999))
        lisa <- cached("lisa:inform_risk", local_moran(dw$inform_risk, c_where$w$lw, 0.05, "fdr"))
        reg <- names(sort(table(dw$wb_region[lisa$cluster %in% "High-High"]), decreasing = TRUE))[1]
        r2 <- summary(stats::lm(inform_risk ~ log_gdp_pc, data = c_why$data))$r.squared
        bs <- blind_spots(c_sure$data, 5)
        bc <- bivariate_classes(c_act$data$hazard_and_exposure, c_act$data$vulnerability)
        pop <- sum(c_act$data$population[bc$double], na.rm = TRUE)
        div(class = "findings",
          finding_card("where", "Where?", sprintf("%d countries", sum(lisa$cluster %in% "High-High")),
                       sprintf("form high-risk clusters, mostly in %s. Risk is not random: neighbours share it (Moran's I = %.2f, p %s).", reg, gm$I, fmt_p(gm$p_perm)),
                       "location-dot", "dark"),
          finding_card("why", "Why?", sprintf("%.0f%%", 100 * r2),
                       sprintf("of the differences in risk go with national wealth alone (%d countries with GDP data). What is left still clusters across borders, so geography matters beyond income.", c_why$n),
                       "magnifying-glass-chart"),
          finding_card("sure", "How sure?", sprintf("%d blind spots", nrow(bs)),
                       "high-risk countries whose scores rest on weak data. Elsewhere the main patterns survive every test of data quality.",
                       "bullseye"),
          finding_card("act", "Where to act?", fmt_pop(pop),
                       sprintf("people live in the %d countries where high danger and high vulnerability meet.", sum(bc$double)),
                       "hand-holding-heart"))
      })

      click <- map_server("map",
        leaflet_r = reactive(leaflet_choropleth(store, c_where$expand(c_where$data$risk_class), "risk_class", "Risk class",
                             label_extra = sprintf("<br/>Score: %.1f out of 10<br/><i>Click for the country profile</i>", sfd$inform_risk))),
        static_r = reactive(static_choropleth(store, c_where$expand(c_where$data$risk_class), "Risk class", "factor", GR$class_cols)),
        title = "INFORM Risk class by country, 2026", caption = paste("Source:", store$sources$inform), filename = "world_risk_classes")
      observeEvent(click(), { shared$country(click()$id); shared$goto("explore") })

      output$dl_workflow <- downloadHandler("Figure_01_workflow.png", function(f)
        save_plot(fig_workflow() + ggplot2::labs(title = "Analytical workflow: five steps from data to decision",
                                                 caption = "Each step answers one question with the method shown; its output feeds the next step."), f, "png", 10, 3.6))

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


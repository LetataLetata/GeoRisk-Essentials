# Step 2, Locate: where is risk concentrated? Global and local spatial
# autocorrelation, for overall risk or any of its parts (floods, drought,
# conflict, food security and the rest).

INFORM_GROUPS <- c("Risk and its three parts", "Hazards", "Vulnerability", "Coping capacity")

register_view(
  id = "where", title = "Where", mode = "analyst", icon = "location-dot", order = 10,
  ui = function(id, store) {
    ns <- NS(id)
    tagList(
      step_head("where", "Where is risk concentrated?",
                "Do high-risk countries sit next to each other, or is risk scattered at random? Choose overall risk or any hazard or weakness.",
                controls(selectInput(ns("var"), "Look at", variable_choices(store, INFORM_GROUPS), selected = "inform_risk", width = "280px"))),
      uiOutput(ns("answer")),
      gr_grid(cols = "1.6fr 1fr",
        map_ui(ns("map"), "Clusters of high and low values", "Countries that are significantly similar to their neighbours", height = 460),
        chart_ui(ns("scatter"), "Each country against its neighbours", "Points in the upper right are high values surrounded by high values", height = 460)),
      chart_ui(ns("compare"), "Which hazards and weaknesses cluster most?", "Global Moran's I for every INFORM variable; longer bars mean neighbours are more alike. The variable on the map is outlined.", height = 620),
      method_box(
        p(strong("Global Moran's I "), "measures whether neighbouring countries have similar values. It ranges from about -1 (neighbours unlike each other) to +1 (neighbours alike); 0 means no spatial pattern. Significance comes from 999 random rearrangements of the values across the map."),
        p(strong("Local Moran's I "), "finds where the clustering is. High-High: a high-value country surrounded by high-value neighbours (a hot cluster). Low-Low: the reverse. High-Low and Low-High: outliers that differ from their neighbours. p-values are corrected for testing 191 countries at once (false discovery rate)."),
        p(strong("Neighbours "), "are each country's 5 nearest countries by distance between their centres, made symmetric, so islands also have neighbours."),
        p(strong("Missing data. "), "Countries without a value for the indicator are left out before the analysis runs, and the neighbours are worked out again among the remaining countries, so a missing country never counts as anyone's neighbour. They appear grey on the map.")),
      next_step("where")
    )
  },
  server = function(id, store, shared) {
    moduleServer(id, function(input, output, session) {
      need_data(store, "inform", "geo")
      sfd <- store$inform_sf
      lab <- reactive(var_label(store, input$var))
      # Missing-data contingency: only countries with a value are analysed, with weights rebuilt for them.
      chk <- reactive(analysis_data(store, input$var, lab()))
      x <- reactive(chk()$data[[input$var]])
      gm <- reactive(global_moran(x(), chk()$w$lw, 999))
      lisa <- reactive(local_moran(x(), chk()$w$lw, 0.05, "fdr"))
      src <- paste("Source:", store$sources$inform)

      output$answer <- renderUI({
        g <- gm(); l <- lisa(); d <- chk()$data
        hh <- d$country[l$cluster %in% "High-High"]
        reg <- sort(table(d$wb_region[l$cluster %in% "High-High"]), decreasing = TRUE)
        txt <- if (g$p_perm < 0.05) sprintf("Yes. <b>%s</b> is clustered: neighbouring countries have similar values (Moran's I = %.2f, p %s). ", lab(), g$I, fmt_p(g$p_perm))
               else sprintf("No clear pattern. <b>%s</b> is not significantly clustered (Moran's I = %.2f, p %s). ", lab(), g$I, fmt_p(g$p_perm))
        txt <- paste0(txt, if (length(hh)) sprintf("<b>%d countries</b> form high-value clusters, mainly in %s. Problems such as conflict, drought and displacement cross borders, so neighbours benefit from planning together.",
                                                    length(hh), paste(names(reg)[seq_len(min(2, length(reg)))], collapse = " and "))
                           else "No high-value clusters pass the significance test.")
        tagList(answer_box(HTML(txt)), data_note(chk()))
      })

      map_server("map",
        leaflet_r = reactive(leaflet_choropleth(store, chk()$expand(lisa()$cluster), "lisa", "Cluster",
                                                label_extra = sprintf("<br/>%s: %s", lab(), fmt_num(chk()$expand(x()), 1)))),
        static_r = reactive(static_choropleth(store, chk()$expand(lisa()$cluster), "Local Moran's I cluster", "factor", GR$lisa_cols)),
        title = reactive(paste("Clusters of", lc(lab()), "(Local Moran's I)")),
        caption = reactive(sprintf("p < 0.05 after false discovery rate correction; 5 nearest-neighbour weights; %d countries with data (grey: excluded). %s", chk()$n, src)),
        filename = reactive(paste0("clusters_", input$var)))

      chart_server("scatter", reactive(fig_moran_scatter(lisa(), chk()$data$country, lab())), title = reactive(paste("Moran scatterplot:", lc(lab()))),
          caption = reactive(sprintf("Slope of the line = Moran's I = %.3f (permutation p %s); n = %d countries. %s", gm()$I, fmt_p(gm()$p_perm), chk()$n, src)),
          filename = reactive(paste0("moran_scatter_", input$var)), width = 6.5, height = 5.5)

      # Moran's I for every INFORM variable, computed once.
      all_moran <- local({
        vv <- store$vars$var[store$vars$group %in% INFORM_GROUPS]
        do.call(rbind, lapply(vv, function(v) {
          c0 <- tryCatch(analysis_data(store, v), error = function(e) NULL); if (is.null(c0)) return(NULL)
          g <- global_moran(c0$data[[v]], c0$w$lw, 499)
          data.frame(var = v, label = var_label(store, v), group = store$vars$group[store$vars$var == v], I = g$I, p = g$p_perm, n = c0$n)
        }))
      })
      chart_server("compare", reactive({
        d <- all_moran; d$label <- factor(d$label, levels = d$label[order(d$I)])
        d$sel <- d$var == input$var
        d$text <- sprintf("%s<br>Moran's I = %.2f, p %s<br>%d countries", d$label, d$I, fmt_p(d$p), d$n)
        ggplot2::ggplot(d, ggplot2::aes(I, label)) +
          ggplot2::geom_col(ggplot2::aes(fill = group, text = text), width = 0.7, alpha = 0.75) +
          ggplot2::geom_col(data = d[d$sel, ], fill = NA, colour = GR$ink, linewidth = 0.9, width = 0.7) +
          ggplot2::geom_vline(xintercept = 0, colour = GR$muted, linewidth = 0.3) +
          ggplot2::scale_fill_manual(values = c("Risk and its three parts" = "#111827", "Hazards" = "#F59E0B", "Vulnerability" = "#DC2626", "Coping capacity" = "#2563EB"), name = NULL) +
          ggplot2::labs(x = "Global Moran's I (5 nearest neighbours)", y = NULL) + theme_georisk(10)
      }), title = "Spatial clustering of every INFORM variable (global Moran's I)",
          caption = sprintf("%d of %d variables are significantly clustered (permutation p < 0.05, 499 permutations); 5 nearest-neighbour weights. %s",
                            sum(all_moran$p < 0.05), nrow(all_moran), src),
          filename = "moran_all_variables", width = 8, height = 8)
    })
  })

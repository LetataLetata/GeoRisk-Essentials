# Step 2, Locate: where is risk concentrated? Global and local spatial
# autocorrelation, for overall risk or any of its parts (floods, drought,
# conflict, food security and the rest).

INFORM_GROUPS <- c("Risk and its three parts", "Hazards", "Vulnerability", "Coping capacity")

register_view(
  id = "where", title = "Where", mode = "analyst", icon = "location-dot", order = 10,
  ui = function(id, store) {
    ns <- NS(id)
    tagList(
      step_head("where", textOutput(ns("title"), inline = TRUE),
                textOutput(ns("lead"), inline = TRUE),
                controls(selectInput(ns("var"), "Look at", variable_choices(store, INFORM_GROUPS), selected = "inform_risk", width = "280px"))),
      uiOutput(ns("answer")),
      gr_grid(cols = "1.6fr 1fr",
        map_ui(ns("map"), "Clusters of high and low values", "Countries that are significantly similar to their neighbours", height = 460),
        chart_ui(ns("scatter"), "Each country against its neighbours", "Points in the upper right are high values surrounded by high values", height = 460)),
      chart_ui(ns("compare"), "Which hazards and weaknesses cluster most?", "Global Moran's I for every INFORM variable; longer bars mean neighbours are more alike. The variable on the map is outlined for comparison, not as a single verdict.", height = 460),
      method_box(
        p(strong("Global Moran's I "), "measures whether neighbouring countries have similar values. It ranges from about -1 (neighbours unlike each other) to +1 (neighbours alike); 0 means no spatial pattern. Values far from zero and p < 0.05 suggest a non-random pattern, not proof of a single cause."),
        p(strong("Local Moran's I "), "finds where the clustering is. High-High: a high-value country surrounded by high-value neighbours (a hot cluster). Low-Low: the reverse. High-Low and Low-High are spatial outliers."),
        p(strong("Neighbours "), "are each country's 5 nearest countries by distance between their centres, made symmetric, so islands also have neighbours."),
        p(strong("Missing data. "), "Countries without a value for the indicator are left out before the analysis runs, and the neighbours are worked out again among the remaining countries, so a missing country does not distort the pattern.")),
      next_step("where")
    )
  },
  server = function(id, store, shared) {
    moduleServer(id, function(input, output, session) {
      need_data(store, "inform", "geo")
      sfd <- store$inform_sf
      lab <- reactive(var_label(store, input$var))
      # Missing-data contingency: only countries with a value are analysed, with weights rebuilt for them.
      chk <- reactive(cached_check(store, input$var, lab()))
      x <- reactive(chk()$data[[input$var]])
      gm <- reactive(cached(paste0("gm:", input$var), global_moran(x(), chk()$w$lw, 999)))
      lisa <- reactive(cached(paste0("lisa:", input$var), local_moran(x(), chk()$w$lw, 0.05, "fdr")))
      src <- paste("Source:", store$sources$inform)

      output$title <- renderText({
        name <- lc(lab())
        sprintf("Where %s %s concentrated?", is_are(name), name)
      })
      output$lead <- renderText(sprintf("Do countries with high %s scores sit next to each other, or are they scattered at random? Choose overall risk or any hazard or weakness.", lc(lab())))

      output$answer <- renderUI({
        g <- gm(); l <- lisa(); d <- chk()$data
        name <- lc(lab())
        coping <- store$vars$group[match(input$var, store$vars$var)] %in% "Coping capacity"
        regions <- function(cl) {
          r <- sort(table(d$wb_region[l$cluster %in% cl]), decreasing = TRUE)
          paste(gsub(" & ", " and ", names(r)[seq_len(min(2, length(r)))]), collapse = " and ")
        }
        n_hh <- sum(l$cluster %in% "High-High"); n_ll <- sum(l$cluster %in% "Low-Low")
        txt <- if (g$p_perm < 0.05) {
          paste0(sprintf("<b>%s</b> %s evidence of geographic concentration: neighbouring countries tend to have similar %s values (Moran's I = %.2f, p %s). ",
                         cap1(name), is_are(name), name, g$I, fmt_p(g$p_perm)),
                 if (n_hh) sprintf("%s in <b>%d countries</b>, mainly in %s", if (coping) cap1(paste("the weakest", name, "(highest scores) clusters")) else sprintf("The highest %s scores cluster", name),
                                   n_hh, regions("High-High")) else
                   sprintf("No group of high %s scores passes the significance test", name),
                 if (n_ll) sprintf(", while %s cluster in %d countries, mainly in %s. ", if (coping) "the strongest" else "the lowest", n_ll, regions("Low-Low")) else ". ",
                 if (n_hh) sprintf("Because %s %s borders, shared planning for %s can be more effective than acting country by country.", name, if (is_are(name) == "are") "cross" else "crosses", if (is_are(name) == "are") "these areas" else "this risk") else "")
        } else sprintf("<b>%s</b> %s no clear geographic pattern: neighbouring countries are no more alike than chance would give (Moran's I = %.2f, p %s), so the result is better treated as country-by-country variation rather than a stable spatial cluster.",
                       cap1(name), if (is_are(name) == "are") "show" else "shows", g$I, fmt_p(g$p_perm))
        tagList(answer_box(HTML(txt)), data_note(chk()))
      })

      fast_map_server("map", store,
        style_r = reactive(choropleth_style(store, chk()$expand(lisa()$cluster), "lisa", "Cluster",
                                            label_extra = sprintf("<br/>%s: %s", lab(), fmt_num(chk()$expand(x()), 1)))),
        static_r = reactive(static_choropleth(store, chk()$expand(lisa()$cluster), "Local Moran's I cluster", "factor", GR$lisa_cols)),
        title = reactive(paste("Clusters of", lc(lab()), "(Local Moran's I)")),
        caption = reactive(sprintf("p < 0.05 after false discovery rate correction; 5 nearest-neighbour weights; %d countries with data (grey: excluded). %s", chk()$n, src)),
        filename = reactive(paste0("clusters_", input$var)))

      chart_server("scatter", reactive(fig_moran_scatter(lisa(), chk()$data$country, lab())),
          plotly_r = reactive(moran_plotly(lisa(), chk()$data$country, lab())), title = reactive(paste("Moran scatterplot:", lc(lab()))),
          caption = reactive(sprintf("Slope of the line = Moran's I = %.3f (permutation p %s); n = %d countries. %s", gm()$I, fmt_p(gm()$p_perm), chk()$n, src)),
          filename = reactive(paste0("moran_scatter_", input$var)), width = 6.5, height = 5.5)

      # Moran's I for every INFORM variable, computed once.
      all_moran <- cached("where:all_moran", moran_all_variables(store))
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

# Step 1, Describe: what is in the data?
# Descriptive statistics of every analysis variable, distributions, continent
# comparisons, correlations (click a cell to see the countries) and the
# variable dictionary.

register_view(
  id = "data", title = "Data", mode = "analyst", icon = "table", order = 5,
  ui = function(id, store) {
    ns <- NS(id)
    tagList(
      step_head("data", "What is in the data?",
                "Before any spatial analysis: every variable used later, how it is spread, where values are missing, and how the variables relate to each other.",
                controls(selectInput(ns("group"), "Variables", c("All variables" = "all", stats::setNames(VARIABLE_GROUPS, VARIABLE_GROUPS)), width = "230px"),
                         selectInput(ns("cont"), "Countries", c("All continents" = "all", stats::setNames(CONTINENT_LEVELS, CONTINENT_LEVELS)), width = "190px"))),
      uiOutput(ns("answer")),
      table_ui(ns("stats"), "Descriptive statistics", "One row per variable. Skewness above 1 or below -1 means a strongly lopsided distribution; Shapiro-Wilk p below 0.05 means not normally distributed."),
      gr_grid(cols = "1fr 1fr",
        gr_card(title = "One variable in detail", tools = download_menu(NS(ns("dist"))),
          selectInput(ns("var"), NULL, variable_choices(store), selected = "inform_risk", width = "320px"),
          plotly::plotlyOutput(ns("dist-plot"), height = 360)),
        chart_ui(ns("box"), "The same variable by continent", "Box: middle half of countries and the median. Points: countries.", height = 410)),
      chart_ui(ns("all"), "All distributions in this group", "Dashed line: median", height = 520),
      gr_card(title = "How the variables relate", subtitle = "Spearman rank correlation. Click any cell to see the countries behind it, below.",
              tools = download_menu(NS(ns("corr"))), plotly::plotlyOutput(ns("corr-plot"), height = 560)),
      continent_scatter_ui(ns("pair"), "The countries behind a correlation, by continent",
                           "One panel per continent, with the correlation in each title. Click a point to label the country; click again to remove it."),
      gr_card(title = "Variable dictionary", subtitle = "What each variable measures, its source and which step uses it (click to unfold)", class = "folded",
              DT::DTOutput(ns("dict"))),
      method_box(
        p(strong("Statistics. "), "n is the number of countries with a value; Missing is the number without. Q1 and Q3 are the 25th and 75th percentiles and IQR their difference. Skewness measures lopsidedness (0 = symmetric); kurtosis is excess kurtosis (0 = as heavy-tailed as a normal distribution). The Shapiro-Wilk test checks normality."),
        p(strong("Why rank correlation. "), "Many hazard scores are strongly skewed (most countries near 0, a few very high), so Spearman's rank correlation is used rather than Pearson's: it measures whether two variables rise together without assuming a straight-line relationship or normal distributions."),
        p(strong("Continents. "), "The data give World Bank regions, which mix continents; countries were assigned to Africa, Asia, Europe, North America (including Central America and the Caribbean), South America and Oceania following the United Nations geographic scheme.")),
      next_step("data")
    )
  },
  server = function(id, store, shared) {
    moduleServer(id, function(input, output, session) {
      need_data(store, "inform")
      src <- paste0("Sources: ", store$sources$inform, "; ", store$sources$owid, ".")
      vi_r <- reactive(if (input$group == "all") store$vars else store$vars[store$vars$group == input$group, ])
      d_r <- reactive(if (input$cont == "all") store$all else store$all[store$all$continent %in% input$cont, ])
      where_txt <- reactive(if (input$cont == "all") "all continents" else input$cont)
      lab <- function(v) unname(store$labels[v])

      output$answer <- renderUI({
        all <- describe_variables(store$all, store$vars)
        gaps <- all[all$Missing > 0, ]; gaps <- gaps[order(-gaps$Missing), ]
        sk <- sum(abs(all$Skewness) > 1, na.rm = TRUE)
        txt <- sprintf(paste0("The data cover <b>%d countries</b> and <b>%d variables</b> in %d groups. %d variables are complete; the largest gaps are %s. ",
                              "%d variables are strongly skewed, so the analysis uses rank-based and permutation methods that do not assume normal distributions."),
                       nrow(store$all), nrow(all), length(VARIABLE_GROUPS), sum(all$Missing == 0),
                       paste(sprintf("%s (%d missing)", lc(utils::head(gaps$Variable, 3)), utils::head(gaps$Missing, 3)), collapse = ", "), sk)
        answer_box(HTML(txt))
      })

      table_server("stats", reactive({
        t <- describe_variables(d_r(), vi_r()); t$var <- NULL
        t$`Shapiro-Wilk p` <- ifelse(is.na(t$`Shapiro-Wilk p`), "n/a", ifelse(t$`Shapiro-Wilk p` < 0.001, "< 0.001", sprintf("%.3f", t$`Shapiro-Wilk p`)))
        t
      }), filename = "descriptive_statistics", page_length = 50, digits = 2, scroll_y = "440px")

      # One variable: histogram with mean and median.
      dist_plot <- reactive({
        d <- d_r(); v <- input$var; x <- d[[v]]; x <- x[!is.na(x)]
        validate(need(length(x) > 2, "Too few countries with data."))
        dd <- data.frame(value = x)
        ggplot2::ggplot(dd, ggplot2::aes(value)) +
          ggplot2::geom_histogram(bins = 20, fill = "#86B9A0", colour = "white") +
          ggplot2::geom_vline(xintercept = stats::median(x), colour = GR$ink, linetype = 2) +
          ggplot2::geom_vline(xintercept = mean(x), colour = GR$red, linetype = 3) +
          ggplot2::labs(x = lab(v), y = "Number of countries") + theme_georisk()
      })
      output[["dist-plot"]] <- plotly::renderPlotly(suppressWarnings(gr_plotly(dist_plot(), tooltip = c("x", "y"))))
      moduleServer("dist", function(input2, output2, session2) {
        register_downloads(output2, dist_plot, function() paste0("distribution_", input$var),
                           function() sprintf("Distribution of %s, %s", lc(lab(input$var)), where_txt()),
                           function() {
                             x <- d_r()[[input$var]]; x <- x[!is.na(x)]
                             sprintf("n = %d countries; mean = %.2f (dotted), median = %.2f (dashed), skewness = %.2f. %s", length(x), mean(x), stats::median(x), skewness_of(x), src)
                           }, 7, 4.5)
      })

      chart_server("box", reactive(fig_box_continent(store$all, input$var, lab(input$var))),
        title = reactive(sprintf("%s by continent", lab(input$var))),
        caption = paste("Boxes: median and interquartile range; points: countries.", src),
        filename = reactive(paste0("by_continent_", input$var)), width = 8, height = 5)

      chart_server("all", reactive({
        vi <- vi_r(); fig_distributions(d_r(), vi$var, vi$label)
      }), title = reactive(sprintf("Distributions: %s, %s", if (input$group == "all") "all variables" else lc(input$group), where_txt())),
          caption = paste("Dashed line: median.", src), filename = reactive(paste0("distributions_", input$group)), width = 10, height = 8)

      # Correlation matrix; clicking a cell sends the pair to the scatter below.
      corr_vars <- reactive({
        vi <- vi_r()
        v <- vi$var
        if (input$group == "all") v <- c("inform_risk", "hazard_and_exposure", "vulnerability", "lack_of_coping_capacity", "drought", "river_flood",
                                         "tropical_cyclone", "current_conflict_intensity", "food_security", "governance", "gdp_per_capita", "reliability_index")
        if (!"inform_risk" %in% v) v <- c("inform_risk", v)
        v[vapply(v, function(z) sum(!is.na(d_r()[[z]])) > 5 && stats::sd(d_r()[[z]], na.rm = TRUE) > 0, logical(1))]
      })
      corr_plot <- reactive(fig_corr_matrix(d_r(), corr_vars(), lab(corr_vars())))
      csrc <- session$ns("corr")
      output[["corr-plot"]] <- plotly::renderPlotly(corr_plotly(d_r(), corr_vars(), lab(corr_vars()), csrc))
      moduleServer("corr", function(input2, output2, session2) {
        register_downloads(output2, corr_plot, function() paste0("correlations_", input$group),
                           function() sprintf("Spearman correlations between %s, %s", if (input$group == "all") "the main variables" else lc(input$group), where_txt()),
                           function() paste("Spearman rank correlations, pairwise complete cases. * p < 0.05, ** p < 0.01, *** p < 0.001.", src), 8.5, 7.5)
      })
      pair <- reactiveVal(c("gdp_per_capita", "inform_risk"))
      observeEvent(suppressWarnings(plotly::event_data("plotly_click", source = csrc)), {
        e <- suppressWarnings(plotly::event_data("plotly_click", source = csrc))
        if (is.null(e)) return()
        k <- e$customdata
        if (!is.null(k) && length(k) && grepl("\\|", k[1])) { pair(strsplit(k[1], "\\|")[[1]]); return() }
        # Fall back on the axis labels of the clicked cell.
        v <- corr_vars(); l <- lab(v)
        xv <- v[match(e$x, l)]; yv <- v[match(e$y, l)]
        if (!is.na(xv) && !is.na(yv) && xv != yv) pair(c(xv, yv))
      })
      continent_scatter_server("pair", data_r = reactive(store$all), x_r = reactive(pair()[1]), y_r = reactive(pair()[2]),
        xlab_r = reactive(lab(pair()[1])), ylab_r = reactive(lab(pair()[2])),
        log_x_r = reactive(pair()[1] %in% c("gdp_per_capita", "electricity_per_capita", "energy_per_capita", "population")),
        title = reactive(sprintf("%s against %s, by continent", lab(pair()[2]), lc(lab(pair()[1])))),
        caption = paste("Line: least-squares fit with 95% confidence band; panel titles give Spearman's rho. Countries missing either variable are excluded.", src),
        filename = reactive(paste0("scatter_", pair()[2], "_", pair()[1])))

      output$dict <- DT::renderDT(DT::datatable(
        store$vars[, c("label", "group", "unit", "description", "source", "steps")],
        colnames = c("Variable", "Group", "Unit", "What it measures", "Source", "Used in"),
        rownames = FALSE, class = "compact hover", options = list(pageLength = 50, dom = "ft", scrollX = TRUE)))
    })
  })

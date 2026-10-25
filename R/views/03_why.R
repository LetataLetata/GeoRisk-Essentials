# Step 3, Explain: what explains risk, and does geography still matter?
# Ordinary regression on national wealth, tested for spatial dependence,
# followed by the spatial model the Lagrange multiplier tests select.

WHY_GROUPS <- c("Risk and its three parts", "Vulnerability", "Coping capacity")

register_view(
  id = "why", title = "Why", mode = "analyst", icon = "magnifying-glass-chart", order = 20,
  ui = function(id, store) {
    ns <- NS(id)
    tagList(
      step_head("why", "What explains risk, and does geography still matter?",
                "Richer countries can prepare and recover better. But once wealth is taken into account, do neighbouring countries still look alike?",
                controls(
                  selectInput(ns("x"), "X-axis", variable_choices(store, VARIABLE_GROUPS), selected = "gdp_per_capita", width = "260px"),
                  selectInput(ns("y"), "Y-axis", variable_choices(store, VARIABLE_GROUPS), selected = "inform_risk", width = "260px"))),
      uiOutput(ns("answer")),
      continent_scatter_ui(ns("scatter"), "Countries by selected variables",
                           "Each panel is one continent, with its own fitted line and correlation. Click a point to label the country.", height = 600),
      gr_grid(cols = "1.3fr 1fr",
        map_ui(ns("resid"), "What wealth does not explain", "Red: riskier than its wealth predicts. Blue: less risky.", height = 440),
        gr_card(title = "How the model was chosen", class = "interp", uiOutput(ns("steps")))),
      gr_card(title = "Model results", subtitle = "Coefficient of wealth and fit of the three models (click to unfold)", class = "folded",
              tableOutput(ns("models"))),
      method_box(
        p(strong("Model. "), "The outcome is regressed on GDP per person (log scale, latest year up to 2022, standardised so the coefficient is the change per standard deviation of log wealth). A strong relationship suggests wealth matters, but not necessarily enough to explain all risk."),
        p(strong("Testing for geography. "), "If the leftover differences (residuals) of ordinary regression are spatially clustered (Moran's I of residuals), its standard errors are unreliable. Lagrange multiplier tests compare ordinary regression with spatial lag and spatial error alternatives."),
        p(strong("Continents. "), "The panels show whether the selected relationship holds within each continent, not only across the world, which guards against a pattern created only by differences between continents."),
        p(strong("Missing data. "), "Countries without either selected variable are left out before the scatter is drawn, and the neighbours are worked out again among the rest; they are listed under the answer and on the map as grey.")),
      next_step("why")
    )
  },
  server = function(id, store, shared) {
    moduleServer(id, function(input, output, session) {
      need_data(store, "inform", "owid")
      dat <- store$all
      preds <- "log_gdp_pc"
      x_lab <- reactive(var_label(store, input$x))
      y_lab <- reactive(var_label(store, input$y))
      # Missing-data contingency: countries without the outcome or GDP are left out, weights rebuilt for the rest.
      chk <- reactive(cached_check(store, c(input$y, preds), c(y_lab(), "GDP per person"), d = dat, tag = "all"))
      fit <- reactive(cached(paste0("fit:", input$y), spatial_regression(chk()$data, input$y, preds, chk()$w$lw, standardise = TRUE)))
      src <- paste0("Sources: ", store$sources$inform, "; ", store$sources$owid, ".")

      pair_data <- reactive({
        d <- dat[!is.na(dat[[input$x]]) & !is.na(dat[[input$y]]), ]
        if (nrow(d) == 0) return(NULL)
        d
      })

      pair_rho <- reactive({
        d <- pair_data()
        if (is.null(d) || nrow(d) < 3) return(NA_real_)
        stats::cor(d[[input$x]], d[[input$y]], method = "spearman", use = "complete.obs")
      })

      pair_phrase <- reactive({
        r <- pair_rho()
        if (is.na(r)) return("No countries have values for both selected variables.")
        if (abs(r) < 0.15) {
          sprintf("The selected comparison between %s and %s is weak: countries with higher values of %s are not consistently different in %s.",
                  lc(x_lab()), lc(y_lab()), lc(x_lab()), lc(y_lab()))
        } else if (r > 0) {
          sprintf("Higher values of %s tend to coincide with higher values of %s (Spearman rho = %.2f).",
                  lc(x_lab()), lc(y_lab()), r)
        } else {
          sprintf("Higher values of %s tend to coincide with lower values of %s (Spearman rho = %.2f).",
                  lc(x_lab()), lc(y_lab()), abs(r))
        }
      })

      output$answer <- renderUI({
        f <- fit()
        b <- f$coefs[f$coefs$model == f$choice$model & f$coefs$term == "log_gdp_pc", ]
        txt <- if (input$x == "gdp_per_capita") {
          sprintf("The selected comparison of %s against %s shows %s. Wealth explains <b>%.0f%%</b> of the differences in %s between countries: a country one standard deviation richer scores %.1f points %s on the 0-10 scale. ",
                  lc(x_lab()), lc(y_lab()), pair_phrase(), 100 * f$r2, lc(y_lab()), abs(b$estimate), if (b$estimate < 0) "lower" else "higher")
        } else {
          sprintf("The selected comparison of %s against %s shows %s. ", lc(x_lab()), lc(y_lab()), pair_phrase())
        }
        txt <- paste0(txt, if (f$residual_moran$p.value < 0.05)
          sprintf("But what the selected model leaves unexplained still clusters (p %s), so a <b>%s model</b> fits best: neighbouring countries share risk factors that the chosen explanatory variable alone does not capture.",
                  fmt_p(f$residual_moran$p.value), lc(f$choice$model))
          else "What remains unexplained is not clustered, so the simple relationship is adequate for this pair.")
        tagList(answer_box(HTML(txt)), data_note(chk()))
      })

      continent_scatter_server("scatter",
        data_r = reactive(pair_data()),
        x_r = reactive(input$x),
        y_r = reactive(input$y),
        xlab_r = reactive(var_label(store, input$x)),
        ylab_r = reactive(var_label(store, input$y)),
        log_x_r = reactive(input$x %in% c("gdp_per_capita", "electricity_per_capita", "energy_per_capita", "population")),
        title = reactive(sprintf("%s against %s, by continent", var_label(store, input$y), lc(var_label(store, input$x)))),
        caption = reactive(sprintf("Line: least-squares fit with 95%% confidence band in each continent; panel titles give Spearman's rho. n = %d countries with paired data. %s",
                                  nrow(pair_data()), src)),
        filename = reactive(paste0("scatter_", input$y, "_", input$x)))

      rvec <- reactive(chk()$expand(unname(stats::residuals(fit()$ols))))
      fast_map_server("resid", store,
        style_r = reactive(choropleth_style(store, rvec(), "residual", "Unexplained",
                                            label_extra = "<br/><i>Positive: riskier than its wealth predicts</i>")),
        static_r = reactive(static_choropleth(store, rvec(), "Residual (observed minus predicted)", "diverging")),
        title = reactive(sprintf("%s not explained by wealth (regression residuals)", y_lab())),
        caption = reactive(sprintf("Ordinary regression residuals (grey: excluded, missing data); Moran's I of residuals = %.2f, p %s. %s",
                                   unname(fit()$residual_moran$estimate[1]), fmt_p(fit()$residual_moran$p.value), src)),
        filename = reactive(paste0("residuals_", input$y)))

      output$steps <- renderUI({
        f <- fit(); fs <- f$fits; i <- match(f$choice$model, fs$model)
        tags$ol(class = "steps",
          tags$li(strong("Ordinary regression. "), sprintf("Wealth alone explains %.0f%% of the variation (R squared = %.2f, %d countries).", 100 * f$r2, f$r2, f$n)),
          tags$li(strong("Check the leftovers. "), sprintf("The unexplained part is %s (Moran's I = %.2f, p %s).",
                  if (f$residual_moran$p.value < 0.05) "clustered in space" else "not clustered", unname(f$residual_moran$estimate[1]), fmt_p(f$residual_moran$p.value))),
          tags$li(strong("Choose the spatial model. "), "Lagrange multiplier (LM) tests compare the two kinds of spatial model. ", f$choice$why),
          tags$li(HTML(sprintf("<strong>Result: %s model.</strong> ", f$choice$model)),
                  if (i > 1) sprintf("It fits better than ordinary regression (AIC %.0f against %.0f), and its residuals are %s (Moran's I p %s).",
                                     fs$AIC[i], fs$AIC[1], if (isTRUE(fs$residual_moran_p[i] >= 0.05)) "no longer clustered" else "still somewhat clustered", fmt_p(fs$residual_moran_p[i]))
                  else "Ordinary regression is adequate."))
      })

      output$models <- renderTable({
        f <- fit()
        cf <- f$coefs[f$coefs$term == "log_gdp_pc", c("model", "estimate", "se", "p")]
        out <- merge(cf, f$fits[, c("model", "AIC", "spatial_parameter", "residual_moran_p")], by = "model")
        out <- out[match(c("OLS", "Spatial lag", "Spatial error"), out$model), ]
        data.frame(Model = c("Ordinary regression", "Spatial lag", "Spatial error"),
                   `Wealth coefficient` = sprintf("%.3f", out$estimate), `Standard error` = sprintf("%.3f", out$se), `p` = fmt_p(out$p),
                   `Spatial parameter` = ifelse(is.na(out$spatial_parameter), "-", sprintf("%.3f", out$spatial_parameter)),
                   AIC = sprintf("%.1f", out$AIC), `Residual Moran's I p` = fmt_p(out$residual_moran_p),
                   Selected = ifelse(out$model == f$choice$model, "Yes", ""), check.names = FALSE)
      }, striped = TRUE, width = "100%")
    })
  })

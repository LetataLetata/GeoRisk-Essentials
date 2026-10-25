# Step 4, Test: how sure can we be? Data error propagated through the INFORM formula,
# blind spots, and whether the clustering result survives data-quality checks.

register_view(
  id = "sure", title = "How sure", mode = "analyst", icon = "bullseye", order = 30,
  ui = function(id, store) {
    ns <- NS(id)
    tagList(
      step_head("sure", "How sure can we be?",
                "Countries collect data differently: some have complete, recent statistics, others rely on old or estimated values. How much does that change the picture?"),
      uiOutput(ns("answer")),
      gr_grid(cols = "1.5fr 1fr",
        chart_ui(ns("ranges"), "The 30 highest-risk countries, with uncertainty", "Dot: published score. Bar: 90% range if the data contain error. Shading: risk classes.", height = 560),
        div(
          gr_card(title = "Blind spots", subtitle = "High risk, but the evidence is weak", uiOutput(ns("blind"))),
          gr_card(title = "Does the clustering survive?", subtitle = "Moran's I and clusters, recomputed four ways", uiOutput(ns("robust"))))),
      method_box(
        p(strong("Uncertainty ranges. "), "INFORM does not publish error margins, but it does rate each country's Lack of Reliability (0 = complete, recent data; 10 = weak data). In this dashboard, we treat the published score as a point estimate and simulate how much it would move under a simple error model. This is a sensitivity check, not a formal confidence interval."),
        p(strong("Blind spots "), "are countries rated High or Very High risk whose Lack of Reliability is above 5, or with more than 20% of their indicators estimated (imputed)."),
        p(strong("Robustness. "), "Moran's I and the High-High clusters are recomputed on (1) the published scores, (2) scores adjusted for data reliability, (3) scores converted to percentile ranks, and (4) only countries with strong data. The broad pattern is more informative than any single version."),
        p(strong("Use with caution. "), "The idea is to show where uncertainty matters most, not to claim a precise probability of error for every country.")),
      next_step("sure")
    )
  },
  server = function(id, store, shared) {
    moduleServer(id, function(input, output, session) {
      need_data(store, "inform")
      # Missing-data contingency: the simulation needs all three pillars and the Lack of Reliability index.
      chk <- cached_check(store, c("inform_risk", "hazard_and_exposure", "vulnerability", "lack_of_coping_capacity", "reliability_index"),
                           c("INFORM Risk", "hazard and exposure", "vulnerability", "lack of coping capacity", "the Lack of Reliability index"))
      inf <- chk$data
      src <- paste("Source:", store$sources$inform, "| Sensitivity analysis by the authors")
      sim <- reactive(cached("sure:sim", simulate_risk(inf, 1, 1000)))
      rob <- reactive(cached("sure:rob", robustness_table(subset_store(store, inf$iso3), "inform_risk", 1, "world", 4.5)))
      bs <- blind_spots(inf, 5)

      output$answer <- renderUI({
        u <- sim(); r <- rob()
        hi <- u[u$risk_class %in% c("High", "Very High"), ]
        rc <- r$`Published clusters that reappear`[-1]
        tagList(answer_box(HTML(sprintf(paste0("The broad pattern remains fairly stable. Across four versions of the data, Moran's I stays between <b>%.2f and %.2f</b>, and at least %.0f%% of the published high-risk clusters reappear in the adjusted versions. ",
                                        "Of %d high-risk countries, %d keep their class in at least 95%% of simulations. The main caution is in <b>%d blind spots</b>, where risk is high but the evidence is weak and local checks matter more than the headline score."),
                                 min(r$`Moran's I`), max(r$`Moran's I`), 100 * min(rc, na.rm = TRUE), nrow(hi), sum(hi$p_high >= 0.95), nrow(bs)))),
                data_note(chk, on_map = FALSE))
      })

      chart_server("ranges", reactive(fig_uncertainty(sim())), title = "INFORM Risk with 90% uncertainty ranges, 30 highest-risk countries",
          caption = paste("Maximum error 1 index point at worst reliability; 1,000 simulations.", src),
          filename = "risk_uncertainty_ranges", width = 8, height = 8)

      output$blind <- renderUI({
        if (!nrow(bs)) return(p("None."))
        b <- bs[order(-bs$inform_risk), ]
        tagList(
          tags$table(class = "mini-table",
            tags$tr(tags$th("Country"), tags$th("Risk"), tags$th("Lack of reliability")),
            lapply(seq_len(nrow(b)), function(i) tags$tr(tags$td(b$country[i]), tags$td(fmt_num(b$inform_risk[i], 1)), tags$td(fmt_num(b$reliability_index[i], 1))))),
          p(class = "gr-card-sub", style = "margin-top:8px;", "Decisions here need field checks and local data, not just the index."))
      })

      output$robust <- renderUI({
        r <- rob()
        nm <- c("Published", "Adjusted for reliability", "Percentile ranks", "Reliable data only")
        tags$table(class = "mini-table",
          tags$tr(tags$th("Version"), tags$th("Moran's I"), tags$th("Clusters kept")),
          lapply(seq_len(nrow(r)), function(i) tags$tr(tags$td(nm[i]), tags$td(sprintf("%.2f", r$`Moran's I`[i])),
                                                       tags$td(if (i == 1) "-" else sprintf("%.0f%%", 100 * r$`Published clusters that reappear`[i])))))
      })
    })
  })

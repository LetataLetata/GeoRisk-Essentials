# GeoRisk Observatory: Essentials
# A trimmed version of the GeoRisk dashboard built around five questions:
#   What is in the data?                         (descriptive statistics)
#   Where is disaster risk concentrated?        (spatial autocorrelation)
#   What explains it, and does geography matter? (spatial regression)
#   How sure can we be?                          (uncertainty and robustness)
#   Where should action come first?              (double burden)
# plus an Explore map where clicking a country opens its profile.
#
# Run from RStudio with the working directory set to this folder:
#   shiny::runApp(".", launch.browser = TRUE)
#
# Structure (each part can be edited without breaking the others):
#   R/core/       theme, reusable cards and downloads, data store, page registry
#   R/analysis/   statistics as plain R functions (no Shiny)
#   R/views/      one file per page; each registers itself with register_view()
#   data/         processed data

required <- c("shiny", "bslib", "leaflet", "sf", "spdep", "spatialreg", "DT", "ggplot2", "plotly", "htmltools", "htmlwidgets", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install missing packages first:\ninstall.packages(c(",
                          paste(sprintf('"%s"', missing), collapse = ", "), "))")

suppressPackageStartupMessages({ library(shiny); library(bslib) })
sf::sf_use_s2(FALSE)

for (f in c(list.files("R/core", full.names = TRUE), list.files("R/analysis", full.names = TRUE))) source(f, local = FALSE)
load_views("R/views")
STORE <- load_store("data")

pill <- function(v, active = FALSE) {
  tags$button(class = paste("gr-pill", if (active) "active"), `data-target` = v$id, type = "button", icon(v$icon), span(v$title))
}

ui <- function(req) {
  views <- views_for("analyst")
  page(
    title = "GeoRisk Observatory: Essentials",
    theme = bs_theme(version = 5, primary = "#111827", success = "#16A34A", font_scale = 0.95,
                     base_font = font_collection("Inter", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif")),
    tags$head(
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap"),
      tags$link(rel = "stylesheet", href = "styles.css"),
      tags$script(src = "app.js")
    ),
    div(class = "app-shell",
      div(class = "topbar",
        div(class = "brand", span(class = "brand-mark", icon("earth-africa")),
            span(class = "brand-name", "GeoRisk", span(class = "brand-accent", "Essentials"))),
        div(class = "pill-bar pills-analyst", lapply(seq_along(views), function(i) pill(views[[i]], i == 1))),
        div(class = "layout-tools",
            tags$button(id = "fold-all", type = "button", title = "Fold every panel on this page", icon("compress"), " Fold all"),
            tags$button(id = "unfold-all", type = "button", title = "Unfold every panel", icon("expand"), " Unfold"),
            tags$button(id = "compact-toggle", type = "button", title = "Tighter spacing and shorter text", icon("table-cells"), " Compact"))),
      if (length(VIEW_ERRORS))
        div(class = "gr-note note-warning", icon("triangle-exclamation"),
            sprintf(" %d page file(s) could not be loaded and were skipped: %s", length(VIEW_ERRORS),
                    paste(basename(names(VIEW_ERRORS)), collapse = ", "))),
      do.call(navset_hidden, c(list(id = "page"),
        unname(lapply(views, function(v) nav_panel_hidden(value = v$id, div(class = "view", safe_view_ui(v, STORE))))))),
      div(class = "footer",
          "Data: INFORM Risk Index (EC JRC), Our World in Data, Natural Earth. ",
          "Background maps: CARTO, OpenStreetMap contributors, Esri, OpenTopoMap. Country facts: REST Countries, Wikipedia. Built with R and Shiny.")
    )
  )
}

server <- function(input, output, session) {
  shared <- list(country = reactiveVal("ZAF"),
                 goto = function(view) session$sendCustomMessage("goto", view))
  for (v in unname(views_for("analyst"))) safe_view_server(v, STORE, shared)
  observeEvent(input$nav, nav_select("page", input$nav, session = session))
  observeEvent(input$nav_to, shared$goto(input$nav_to))
}

shinyApp(ui, server)

# GEORISK Observatory
# A focused version of the GeoRisk dashboard built around five questions:
#   What is in the data?                         (descriptive statistics)
#   Where is disaster risk concentrated?        (spatial autocorrelation)
#   What explains it, and does geography matter? (spatial regression)
#   How sure can we be?                          (uncertainty and robustness)
#   Where should action come first?              (double burden)
# plus an Explore map where clicking a country opens its profile, and a Help
# page. A short welcome guide opens on a visitor's first visit.
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
warm_cache(STORE)

# Bold text with no stray space after it (so "word," not "word ,").
b_ <- function(...) strong(..., .noWS = "after")

# Top-bar tab. The five analysis steps carry their step number, so the tabs
# themselves show the order to follow.
pill <- function(v, active = FALSE) {
  n <- match(v$id, STEPS$id)
  tags$button(class = paste("gr-pill", if (active) "active", if (v$id == "help") "help-pill"),
              `data-target` = v$id, type = "button",
              title = if (!is.na(n)) sprintf("Step %d of 5: %s", n, STEPS$question[n]) else v$title,
              if (!is.na(n)) span(class = "pill-n", n) else icon(v$icon), span(v$title))
}

# The welcome guide: five short slides shown on the first visit, and again
# from the "Quick tour" button. Plain HTML; www/app.js moves between slides.
guide_slide <- function(icon_name, title, ...) {
  div(class = "guide-slide", div(class = "guide-icon", icon(icon_name)), h2(title), ...)
}
nav_btn <- function(target, label, icon_name, class = "btn-guide") {
  tags$button(type = "button", class = class, `data-goto` = target, icon(icon_name), " ", label)
}
welcome_guide <- function() {
  div(id = "guide", class = "guide-backdrop", role = "dialog", `aria-modal` = "true", `aria-labelledby` = "guide-title",
    div(class = "guide-box",
      tags$button(type = "button", class = "guide-close", title = "Close the guide", `aria-label` = "Close", icon("xmark")),
      div(class = "guide-slides",
        guide_slide("earth-africa", span(id = "guide-title", "Welcome to GEORISK Observatory"),
          p("This dashboard shows ", b_("where disaster risk is highest"), " in the world, ", b_("what explains it"), ", ",
            b_("how sure we can be"), " and ", b_("where help should go first"), "."),
          p("It covers 191 countries, using the INFORM Risk Index 2026 (European Commission). The tour takes about a minute.")),
        guide_slide("list-ol", "Follow the numbered tabs",
          p("The tabs at the top tell one story in order:"),
          div(class = "guide-steps",
              lapply(seq_len(nrow(STEPS)), function(i) div(class = "guide-step", span(class = "pill-n", i), span(STEPS$question[i])))),
          p("Every page ends with a ", b_("Next"), " button, so you can simply keep going. ",
            b_("Overview"), " is the summary, ", b_("Explore"), " is for single countries, and ", b_("Help"), " explains everything.")),
        guide_slide("book-open-reader", "Read each page from top to bottom",
          tags$ol(class = "guide-list",
            tags$li(b_("The question"), " is the page title. The menu at the top right changes what is shown."),
            tags$li(b_("The answer"), " is the green box: one sentence, worked out from the data."),
            tags$li(b_("The evidence"), " is the charts and maps below it."),
            tags$li(b_("The method"), " is folded away at the bottom, for anyone who wants the details."))),
        guide_slide("hand-pointer", "Point, click and download",
          div(class = "guide-keys",
            div(span(class = "gk", icon("arrow-pointer")), span("Hover over a country or point to see its name and value; click a country on the Overview or Explore map to open its profile.")),
            div(span(class = "gk", icon("layer-group")), span("Map buttons on the right: background map, zoom, enlarge and full screen. Drag the corner of a map to resize it.")),
            div(span(class = "gk", icon("download")), span("Download any chart or map as an image or its data.")),
            div(span(class = "gk", icon("chevron-down")), span("Fold a panel away to keep the page tidy.")))),
        guide_slide("flag-checkered", "Where would you like to start?",
          div(class = "guide-choices",
            tags$button(type = "button", class = "guide-choice", `data-goto` = "data",
                        icon("list-ol"), b_("Follow the analysis"), span("Start at step 1 and click Next")),
            tags$button(type = "button", class = "guide-choice", `data-goto` = "explore",
                        icon("map-location-dot"), b_("Look up a country"), span("Click any country for its profile")),
            tags$button(type = "button", class = "guide-choice", `data-goto` = "overview",
                        icon("house"), b_("See the summary"), span("The main findings on one page"))),
          p(class = "guide-small", "You can open this tour again any time with ", b_("Quick tour"), " at the top right."))),
      div(class = "guide-foot",
        tags$button(type = "button", class = "guide-skip", "Skip tour"),
        div(class = "guide-dots"),
        div(tags$button(type = "button", class = "guide-back", icon("arrow-left"), " Back"),
            tags$button(type = "button", class = "guide-next", "Next ", icon("arrow-right"))))))
}

ui <- function(req) {
  views <- views_for("analyst")
  page(
    title = "GEORISK Observatory",
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
            span(class = "brand-name", "GEORISK", span(class = "brand-accent", " Observatory"))),
        div(class = "pill-bar pills-analyst", lapply(seq_along(views), function(i) pill(views[[i]], i == 1))),
        div(class = "layout-tools",
            tags$button(id = "open-guide", type = "button", class = "tour-btn", title = "A one-minute tour of how the dashboard works",
                        icon("compass"), " Quick tour"),
            tags$details(class = "view-menu",
              tags$summary(title = "Tidy the page: fold panels or use tighter spacing", icon("sliders"), " View"),
              div(class = "view-list",
                tags$button(id = "fold-all", type = "button", icon("compress"), span("Fold all panels", tags$small("Show only the titles"))),
                tags$button(id = "unfold-all", type = "button", icon("expand"), span("Unfold all panels", tags$small("Show everything again"))),
                tags$button(id = "compact-toggle", type = "button", icon("table-cells"), span("Compact spacing", tags$small("Fits more on the screen; remembered"))))))),
      if (length(VIEW_ERRORS))
        div(class = "gr-note note-warning", icon("triangle-exclamation"),
            sprintf(" %d page file(s) could not be loaded and were skipped: %s", length(VIEW_ERRORS),
                    paste(basename(names(VIEW_ERRORS)), collapse = ", "))),
      do.call(navset_hidden, c(list(id = "page"),
        unname(lapply(views, function(v) nav_panel_hidden(value = v$id, div(class = "view", safe_view_ui(v, STORE))))))),
      div(class = "footer",
          "Data: INFORM Risk Index (EC JRC), Our World in Data, Natural Earth. ",
          "Background maps: CARTO, OpenStreetMap contributors, Esri, OpenTopoMap. Country facts: REST Countries, Wikipedia. Built with R and Shiny. ",
          tags$a(href = "#", class = "foot-help", `data-goto` = "help", "Help and how to open the app"))
    ),
    welcome_guide()
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

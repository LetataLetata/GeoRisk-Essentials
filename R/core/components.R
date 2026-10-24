# Reusable building blocks: cards, KPI tiles, and chart, map and table cards
# that come with publication-quality downloads (PNG 300 dpi, PDF, SVG, CSV).

# Bold text with no stray space after it (so "word," not "word ,").
b_ <- function(...) tags$strong(..., .noWS = "after")

# ---------------------------------------------------------------- layout --
gr_card <- function(..., title = NULL, subtitle = NULL, tools = NULL, class = NULL) {
  div(class = paste("gr-card", class),
      if (!is.null(title) || !is.null(tools))
        div(class = "gr-card-head",
            div(if (!is.null(title)) div(class = "gr-card-title", title),
                if (!is.null(subtitle)) div(class = "gr-card-sub", subtitle)),
            if (!is.null(tools)) div(class = "gr-card-tools", tools)),
      ...)
}

gr_grid <- function(..., cols = "1fr 1fr", class = NULL) {
  div(class = paste("gr-grid", class), style = paste0("grid-template-columns:", cols, ";"), ...)
}

page_head <- function(title, lead = NULL, right = NULL, how = NULL) {
  tagList(div(class = "page-head", div(h1(title), if (!is.null(lead)) p(class = "lead", lead)),
              if (!is.null(right)) div(class = "page-head-right", right)),
          how_strip(how))
}

# One line under each page title saying what to do on the page.
PAGE_HOW <- list(
  data = "Pick a group of variables or one continent with the menus on the right. Click any coloured cell in the correlation grid to see the countries behind it, and click a point to label a country.",
  where = "Choose what to map with \"Look at\". Red countries are hot spots (high values surrounded by high values), blue are cold spots. Hover over a country or dot to see its name.",
  events = "Choose a region and the smallest magnitude at the top right. Red curves above the grey band mean more clustered than random. Hover over the curves for the values; the last chart links the events to each country's score.",
  why = "Choose what to explain at the top right. Click points in the scatter to label countries. On the map, red means riskier than the country's wealth predicts, blue means safer.",
  sure = "Each bar shows how far a country's score could move if its data contain errors: the longer the bar, the less certain. Blind spots are high-risk countries with weak data.",
  act = "Choose a danger at the top right. Dark purple countries face both high danger and high vulnerability; the list on the right names them by region.",
  explore = "Click any country on the map, or type its name in \"Find a country\". Choose a hazard or weakness in \"Colour countries by\" and the profile explains that score: why the country is rated so, the facts and recorded events behind it. Click a part or a step in the profile to switch the map to it."
)

how_strip <- function(how) {
  if (is.null(how)) return(NULL)
  div(class = "how-strip", span(class = "how-tag", icon("hand-pointer"), " How to use this page"), span(how),
      tags$button(type = "button", class = "how-more", onclick = "Shiny.setInputValue('nav_to', 'help', {priority: 'event'})",
                  "More help", icon("arrow-right")))
}


note <- function(..., tone = "info") div(class = paste("gr-note", paste0("note-", tone)), ...)

method_box <- function(...) tags$details(class = "method-box", tags$summary(icon("book-open"), " Method and interpretation"), div(...))

controls <- function(...) div(class = "gr-controls", ...)

# ------------------------------------------------------------- downloads --
download_menu <- function(ns, csv = TRUE, image = TRUE) {
  tags$details(class = "dl-menu",
    tags$summary(icon("download"), title = "Download this figure or its data"),
    div(class = "dl-list",
        if (image) downloadLink(ns("dl_png"), "PNG (300 dpi)"),
        if (image) downloadLink(ns("dl_pdf"), "PDF (vector)"),
        if (image) downloadLink(ns("dl_svg"), "SVG (vector)"),
        if (csv) downloadLink(ns("dl_csv"), "Data (CSV)")))
}

save_plot <- function(p, file, type, width, height) {
  dev <- switch(type,
    png = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png",
    pdf = if (capabilities("cairo")) grDevices::cairo_pdf else "pdf",
    svg = if (requireNamespace("svglite", quietly = TRUE)) svglite::svglite else "svg")
  ggplot2::ggsave(file, p, device = dev, width = width, height = height, units = "in",
                  dpi = 300, bg = "white")
}

export_data <- function(p) {
  d <- attr(p, "export_data")
  if (is.null(d) && inherits(p, "ggplot") && is.data.frame(p$data)) d <- p$data
  if (inherits(d, "sf")) d <- sf::st_drop_geometry(d)
  d
}

# A plot can set attr(p, "export_size") <- c(width, height) in inches, for
# example a tall map region; otherwise the card's default size is used.
register_downloads <- function(output, plot_fun, filename, title, caption, width, height) {
  dated <- function(ext) function() paste0(filename(), "_", format(Sys.Date(), "%Y%m%d"), ".", ext)
  render <- function(f, type) {
    p <- suppressWarnings(plot_fun())
    size <- attr(p, "export_size"); if (is.null(size)) size <- c(width, height)
    cap <- paste(strwrap(caption(), width = floor(size[1] * 15)), collapse = "\n")
    p <- if (inherits(p, "patchwork")) {
      p + patchwork::plot_annotation(title = title(), caption = cap, theme = theme_georisk())
    } else p + ggplot2::labs(title = title(), caption = cap)
    save_plot(p, f, type, size[1], size[2])
  }
  output$dl_png <- downloadHandler(dated("png"), function(f) render(f, "png"))
  output$dl_pdf <- downloadHandler(dated("pdf"), function(f) render(f, "pdf"))
  output$dl_svg <- downloadHandler(dated("svg"), function(f) render(f, "svg"))
  output$dl_csv <- downloadHandler(dated("csv"), function(f) utils::write.csv(export_data(plot_fun()), f, row.names = FALSE))
}

as_fun <- function(x) if (is.function(x)) x else function() x

# ------------------------------------------------------------ chart card --
chart_ui <- function(id, title, subtitle = NULL, height = 340, csv = TRUE) {
  ns <- NS(id)
  gr_card(title = title, subtitle = if (is.null(subtitle)) uiOutput(ns("sub"), inline = TRUE) else subtitle,
          tools = download_menu(ns, csv = csv), class = "chart-card",
          plotly::plotlyOutput(ns("plot"), height = height))
}

# plot_r: reactive returning a ggplot (with text aesthetics for tooltips).
# plotly_r: optional reactive returning a ready plotly object for the screen.
chart_server <- function(id, plot_r, title, caption = "", filename = id, subtitle_r = NULL,
                         width = 8, height = 5, tooltip = "text", plotly_r = NULL) {
  moduleServer(id, function(input, output, session) {
    output$plot <- plotly::renderPlotly({
      if (!is.null(plotly_r)) return(plotly_r())
      p <- suppressWarnings(plot_r()); req(p)
      suppressWarnings(gr_plotly(p, tooltip = tooltip))
    })
    if (!is.null(subtitle_r)) output$sub <- renderUI(subtitle_r())
    register_downloads(output, plot_r, as_fun(filename), as_fun(title), as_fun(caption), width, height)
  })
}

# -------------------------------------------------------------- map card --
map_ui <- function(id, title, subtitle = NULL, height = 460) {
  ns <- NS(id)
  gr_card(title = title, subtitle = if (is.null(subtitle)) uiOutput(ns("sub"), inline = TRUE) else subtitle,
          tools = download_menu(ns), class = "map-card",
          leaflet::leafletOutput(ns("map"), height = height))
}

# leaflet_r: reactive returning a leaflet map; static_r: reactive returning a
# ggplot map used for the PNG, PDF and SVG downloads.
map_server <- function(id, leaflet_r, static_r, title, caption = "", filename = id, subtitle_r = NULL,
                       width = 10, height = 5.6) {
  moduleServer(id, function(input, output, session) {
    output$map <- leaflet::renderLeaflet(leaflet_r())
    if (!is.null(subtitle_r)) output$sub <- renderUI(subtitle_r())
    register_downloads(output, static_r, as_fun(filename), as_fun(title), as_fun(caption), width, height)
    session$userData[[paste0(id, "_click")]] <- reactive(input$map_shape_click)
    reactive(input$map_shape_click)
  })
}

# ------------------------------------------------------------ table card --
table_ui <- function(id, title, subtitle = NULL) {
  ns <- NS(id)
  gr_card(title = title, subtitle = subtitle,
          tools = tags$details(class = "dl-menu", tags$summary(icon("download"), title = "Download this figure or its data"),
                               div(class = "dl-list", downloadLink(ns("dl_csv"), "Table (CSV)"))),
          class = "table-card", DT::DTOutput(ns("tbl"), fill = FALSE))
}

table_server <- function(id, data_r, filename = id, page_length = 10, digits = 2, scroll_y = NULL) {
  moduleServer(id, function(input, output, session) {
    output$tbl <- DT::renderDT({
      d <- data_r(); req(d)
      num <- names(d)[vapply(d, function(z) is.numeric(z) && any(abs(z - round(z)) > 1e-9, na.rm = TRUE), logical(1))]
      dt <- DT::datatable(d, rownames = FALSE, class = "compact hover", fillContainer = FALSE,
                          options = utils::modifyList(list(pageLength = page_length, scrollX = TRUE, dom = "ftip"),
                                      if (!is.null(scroll_y)) list(scrollY = scroll_y, scrollCollapse = TRUE, dom = "ft", paging = FALSE) else list()))
      if (length(num)) dt <- DT::formatRound(dt, num, digits = digits)
      dt
    })
    output$dl_csv <- downloadHandler(function() paste0(filename, "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
                                     function(f) utils::write.csv(data_r(), f, row.names = FALSE))
  })
}

# ------------------------------------------------------------- formatting --
fmt_num <- function(x, d = 1) ifelse(is.na(x), "n/a", formatC(x, format = "f", digits = d, big.mark = ","))
# Returns the relation and value, used as "p %s": "p < 0.001" or "p = 0.012".
fmt_p <- function(p) ifelse(is.na(p), "= n/a", ifelse(p < 0.001, "< 0.001", paste("=", formatC(p, format = "f", digits = 3))))
# Lower-case the first letter of a label unless it starts with an acronym (INFORM, GDP).
cap1 <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))
# "is" or "are" for a variable name ("uprooted people are", "drought is").
is_are <- function(x) if (grepl("(people|groups|conditions|shocks)$", x)) "are" else "is"
lc <- function(x) ifelse(grepl("^[A-Z]{2,}", x), x, paste0(tolower(substr(x, 1, 1)), substr(x, 2, nchar(x))))
fmt_pop <- function(x) {
  if (is.na(x)) return("n/a")
  if (x >= 1e9) return(sprintf("%.2f billion", x / 1e9))
  if (x >= 1e6) return(sprintf("%.1f million", x / 1e6))
  format(round(x), big.mark = ",")
}
ordinal <- function(n) paste0(n, ifelse(n %% 100 %in% 11:13, "th", c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1]))

# ------------------------------------------------------- essentials layout --
# The one-sentence answer that opens every page.
answer_box <- function(...) div(class = "answer-box", div(class = "answer-label", "The answer"), div(class = "answer-text", ...))


# ------------------------------------------------------ chain of analysis --
# The six steps of the analysis, in order. Each step page shows where it sits
# in the chain and ends with a button to the next step.
STEPS <- data.frame(stringsAsFactors = FALSE,
  id = c("data", "where", "events", "why", "sure", "act"),
  verb = c("Describe", "Locate", "Detect", "Explain", "Test", "Decide"),
  question = c("What is in the data?", "Where is risk concentrated?", "Are the events themselves clustered?", "What explains it?", "How sure can we be?", "Where should action come first?"),
  method = c("Descriptive statistics, distributions, correlations", "Global and local Moran's I", "Point pattern: kernel density, quadrat test, nearest neighbour, G and L functions",
             "Regression, Lagrange multiplier tests, spatial error model",
             "Monte Carlo uncertainty, reliability adjustment, robustness", "Bivariate classes, Lee's L"),
  output = c("Know the variables, their spread and gaps", "Clusters of high and low risk", "Earthquakes cluster far beyond chance",
             "Wealth explains much; geography still matters",
             "Which results are firm, and the blind spots", "Countries where danger meets vulnerability"))

# "Step 3: Events" for a page id, numbered from STEPS so the labels never go out of step.
step_label <- function(id, title) sprintf("Step %d: %s", match(id, STEPS$id), title)


step_head <- function(id, title, lead = NULL, right = NULL) {
  i <- match(id, STEPS$id)
  # The numbered tabs in the top bar already show the six steps, so the page
  # only says which step it is.
  tagList(div(class = "page-head",
              div(div(class = "step-eyebrow", sprintf("Step %d of %d: %s", i, nrow(STEPS), STEPS$verb[i])),
                  h1(title), if (!is.null(lead)) p(class = "lead", lead)),
              if (!is.null(right)) div(class = "page-head-right", right)),
          how_strip(PAGE_HOW[[id]]))
}

next_step <- function(id) {
  i <- match(id, STEPS$id)
  target <- if (i < nrow(STEPS)) STEPS[i + 1, ] else NULL
  div(class = "next-step",
      if (!is.null(target))
        tags$button(type = "button", class = "next-btn", onclick = sprintf("Shiny.setInputValue('nav_to', '%s', {priority: 'event'})", target$id),
                    span(class = "next-label", sprintf("Next: step %d, %s", i + 1, tolower(target$verb))),
                    span(class = "next-q", target$question, icon("arrow-right")))
      else tags$button(type = "button", class = "next-btn", onclick = "Shiny.setInputValue('nav_to', 'explore', {priority: 'event'})",
                       span(class = "next-label", "Then: look at single countries"), span(class = "next-q", "Open the Explore map", icon("arrow-right"))))
}

# ------------------------------------------- continent scatter (clickable) --
# Two variables plotted with one panel per continent. Click a point to label
# that country (click again to remove it), or type names in the box. The
# download uses the same panels and labels.
continent_scatter_ui <- function(id, title, subtitle = NULL, height = 620) {
  ns <- NS(id)
  gr_card(title = title, subtitle = subtitle, tools = download_menu(ns), class = "chart-card",
    div(class = "scatter-tools",
      radioButtons(ns("cont"), NULL, c("All continents" = "all", stats::setNames(CONTINENT_LEVELS, CONTINENT_LEVELS)), inline = TRUE),
      div(class = "scatter-label",
        selectizeInput(ns("lab"), NULL, choices = NULL, multiple = TRUE, width = "100%",
                       options = list(placeholder = "Click points, or type countries to label them")),
        actionLink(ns("clear"), "Clear labels"))),
    div(class = "scatter-now", textOutput(ns("now"), inline = TRUE)),
    plotly::plotlyOutput(ns("plot"), height = height))
}

continent_scatter_server <- function(id, data_r, x_r, y_r, xlab_r, ylab_r, log_x_r = reactive(FALSE), title, caption = "", filename = id,
                                     width = 10, height = 6.5) {
  moduleServer(id, function(input, output, session) {
    src <- session$ns("plot")
    observe({
      d <- data_r(); d <- d[!is.na(d[[x_r()]]) & !is.na(d[[y_r()]]), ]
      d <- d[order(d$country), ]
      updateSelectizeInput(session, "lab", choices = stats::setNames(d$iso3, d$country), selected = isolate(input$lab), server = TRUE)
    })
    observeEvent(suppressWarnings(plotly::event_data("plotly_click", source = src)), {
      k <- suppressWarnings(plotly::event_data("plotly_click", source = src))$key
      if (is.null(k) || !length(k) || is.na(k[1]) || !nzchar(k[1])) return()
      cur <- input$lab
      new <- if (k[1] %in% cur) setdiff(cur, k[1]) else c(cur, k[1])
      updateSelectizeInput(session, "lab", selected = new)
    })
    observeEvent(input$clear, updateSelectizeInput(session, "lab", selected = character(0)))
    conts <- reactive(if (is.null(input$cont) || input$cont == "all") CONTINENT_LEVELS else input$cont)
    make <- function(repel) {
      p <- fig_scatter_continent(data_r(), x_r(), y_r(), xlab_r(), ylab_r(), highlight = input$lab %||% character(0),
                                 continents = conts(), log_x = log_x_r(), ncol = if (length(conts()) == 1) 1 else 3, repel = repel)
      if (length(conts()) == 1) attr(p, "export_size") <- c(7, 5)
      p
    }
    plot_r <- reactive(make(TRUE))           # downloads: labels placed without overlaps
    output$now <- renderText(sprintf("Showing %s (vertical) against %s (horizontal). %d countries labelled.",
                                     ylab_r(), lc(xlab_r()), length(input$lab)))
    output$plot <- plotly::renderPlotly({
      pl <- suppressWarnings(gr_plotly(make(FALSE), tooltip = "text", source = src))
      plotly::layout(pl, showlegend = FALSE, margin = list(l = 60, r = 10, t = 30, b = 60))
    })
    register_downloads(output, plot_r, as_fun(filename), as_fun(title), as_fun(caption), width, height)
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ----------------------------------------------------------- fast map card --
# Same card as map_ui/map_server, but the country shapes are sent once; when a
# setting changes only colours, labels and legend are updated.
fast_map_server <- function(id, store, style_r, static_r, title, caption = "", filename = id, subtitle_r = NULL,
                            width = 10, height = 5.6) {
  moduleServer(id, function(input, output, session) {
    output$map <- leaflet::renderLeaflet(draw_styled_map(store, isolate(style_r())))
    observeEvent(style_r(), restyle_map(session, "map", store, style_r()), ignoreInit = TRUE)
    if (!is.null(subtitle_r)) output$sub <- renderUI(subtitle_r())
    register_downloads(output, static_r, as_fun(filename), as_fun(title), as_fun(caption), width, height)
    reactive(input$map_shape_click)
  })
}

# Static (image) chart card for charts that need no hovering, such as many
# small panels: much faster to draw than an interactive chart.
image_chart_ui <- function(id, title, subtitle = NULL, height = 520) {
  ns <- NS(id)
  gr_card(title = title, subtitle = subtitle, tools = download_menu(ns), class = "chart-card",
          plotOutput(ns("img"), height = height))
}
image_chart_server <- function(id, plot_r, title, caption = "", filename = id, width = 10, height = 8) {
  moduleServer(id, function(input, output, session) {
    # Transparent on screen so the glass panel shows through; downloads stay white.
    output$img <- renderPlot(suppressWarnings(print(plot_r() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = NA, colour = NA),
                                                                         panel.background = ggplot2::element_rect(fill = NA, colour = NA)))),
                             res = 96, bg = "transparent")
    register_downloads(output, plot_r, as_fun(filename), as_fun(title), as_fun(caption), width, height)
  })
}

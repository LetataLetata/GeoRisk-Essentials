# Help page: what the dashboard does, how to open it (web, R, download), how
# to use each page and control, the words it uses, and what to do if
# something goes wrong. Written for someone who has never seen the app.

# Bold text with no stray space after it (so "word," not "word ,").
b_ <- function(...) strong(..., .noWS = "after")

APP_LINKS <- list(
  web = "https://01a16f44-5481-89af-2d69-51579eddc650.share.connect.posit.cloud",
  github = "https://github.com/LetataLetata/GeoRisk-Essentials",
  zip = "https://github.com/LetataLetata/GeoRisk-Essentials/archive/refs/heads/main.zip",
  r_code = paste0(
    'pkgs <- c("shiny", "bslib", "leaflet", "sf", "spdep", "spatialreg", "DT", "ggplot2", "plotly",\n',
    '          "htmltools", "htmlwidgets", "jsonlite", "patchwork", "ragg", "svglite", "ggrepel", "zip")\n',
    'missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]\n',
    'if (length(missing)) install.packages(missing)\n',
    'shiny::runGitHub("GeoRisk-Essentials", "LetataLetata", launch.browser = TRUE)')
)

help_section <- function(id, icon_name, title, subtitle, ...) {
  div(id = paste0("help-", id), class = "help-section",
      gr_card(title = tagList(icon(icon_name), " ", title), subtitle = subtitle, ...))
}

copy_btn <- function(text, label = "Copy") {
  tags$button(type = "button", class = "copy-btn", `data-copy` = text, icon("copy"), span(label))
}

help_go <- function(target, label) {
  tags$button(type = "button", class = "help-go", `data-goto` = target, label, icon("arrow-right"))
}

register_view(
  id = "help", title = "Help", mode = "analyst", icon = "circle-question", order = 90,
  ui = function(id, store) {
    toc <- c(what = "What it does", open = "How to open it", start = "Three ways to use it", pages = "The pages",
             controls = "Buttons and controls", words = "Words used", problems = "If something goes wrong")
    tagList(
      page_head("Help: how to open and use the dashboard",
                "Everything you need, in plain words. Use the list below to jump to a section.",
                right = tags$button(type = "button", class = "btn btn-dark", onclick = "window.grOpenGuide && window.grOpenGuide()",
                                    icon("compass"), " Take the one-minute tour")),
      div(class = "help-toc", lapply(names(toc), function(k)
        tags$a(href = paste0("#help-", k), class = "help-toc-link", toc[[k]]))),

      help_section("what", "earth-africa", "What this dashboard does",
        "One question, answered in six steps",
        div(class = "help-two",
          div(
            p(class = "help-big", "Disasters do the most harm where danger meets disadvantage. This dashboard asks ",
              b_("where disaster risk is highest in the world, why, how sure we can be, and where help should go first.")),
            p("It uses the ", b_("INFORM Risk Index 2026"), " from the European Commission, which scores 191 countries from 0 (very low risk) to 10 (very high risk). ",
              "The score combines three parts: the ", b_("hazards"), " a country faces (floods, drought, earthquakes, conflict and others), how ",
              b_("vulnerable"), " its people are (poverty, health, displacement), and its ", b_("lack of coping capacity"), " (weak government, health care or infrastructure)."),
            p("The dashboard then uses ", b_("spatial statistics"), ", methods that take location into account, to see whether risk is grouped on the map and what that means.")),
          div(class = "help-flow",
            lapply(seq_len(nrow(STEPS)), function(i) div(class = "help-flow-step",
              span(class = "pill-n", i), div(b_(STEPS$question[i]), div(class = "gr-card-sub", STEPS$output[i]))))))),

      help_section("open", "link", "How to open the app",
        "Three ways, depending on what you have. Anyone can use the first one.",
        div(class = "open-grid",
          div(class = "open-card",
            div(class = "open-tag", "Easiest"), h3(icon("globe"), " In a web browser"),
            p("Open this link on any computer or phone. No account and no R needed."),
            tags$a(href = APP_LINKS$web, target = "_blank", class = "open-link", APP_LINKS$web),
            div(class = "open-actions", copy_btn(APP_LINKS$web, "Copy link"),
                tags$a(href = APP_LINKS$web, target = "_blank", class = "copy-btn", icon("arrow-up-right-from-square"), " Open")),
            p(class = "gr-card-sub", "If nobody has used it for a while, the first visit can take up to a minute while the server wakes up.")),
          div(class = "open-card",
            div(class = "open-tag", "Fastest once installed"), h3(icon("r-project"), " From R or RStudio"),
            p("Paste this into the R console. The first time it installs what the app needs (a few minutes); after that it starts in seconds and always runs the latest version."),
            tags$pre(class = "gr-code help-code", APP_LINKS$r_code),
            div(class = "open-actions", copy_btn(APP_LINKS$r_code, "Copy code"))),
          div(class = "open-card",
            div(class = "open-tag", "Works offline"), h3(icon("file-zipper"), " Download a copy"),
            p("Download the folder, unzip it, open ", tags$code("app.R"), " in RStudio and click ", b_("Run App"), ". Background maps and country facts need internet; everything else works offline."),
            div(class = "open-actions",
                tags$a(href = APP_LINKS$zip, class = "copy-btn", icon("download"), " Download (zip)"),
                tags$a(href = APP_LINKS$github, target = "_blank", class = "copy-btn", icon("github"), " Code on GitHub"))))),

      help_section("start", "route", "Three ways to use it",
        "Pick the one that matches what you want",
        div(class = "use-grid",
          div(class = "use-card",
            h3("1. Follow the analysis"),
            p("For a presentation or to understand the argument. Start at ", b_("1 Data"), " and click the ", b_("Next"), " button at the bottom of each page. Read the green answer box first, then the charts."),
            help_go("data", "Start at step 1")),
          div(class = "use-card",
            h3("2. Look up one country"),
            p("Open ", b_("Explore"), " and click a country (or type its name). You get its risk score, what drives it, how it changed since 2017, its neighbours and what helps at national level."),
            help_go("explore", "Open Explore")),
          div(class = "use-card",
            h3("3. Get figures for a report"),
            p("On ", b_("Overview"), ", one button downloads every figure (PNG and PDF) and table in a zip file. Each chart also has its own download icon for the settings you choose."),
            help_go("overview", "Go to Overview")))),

      help_section("pages", "table-list", "The pages",
        "Each tab answers one question. The numbered tabs are the six steps, in order.",
        tags$table(class = "help-table",
          tags$thead(tags$tr(tags$th("Tab"), tags$th("Question"), tags$th("What you will see"), tags$th("What you can do"))),
          tags$tbody(
            tags$tr(tags$td(b_("Overview")), tags$td("All of them, in brief"),
                    tags$td("The six steps, the main findings and the world risk map"),
                    tags$td("Click a step or a finding to open it; click a country to open its profile; download all figures")),
            tags$tr(tags$td(span(class = "pill-n", 1), b_(" Data")), tags$td("What is in the data?"),
                    tags$td("A table of statistics for all 41 variables, their spread, how they relate"),
                    tags$td("Filter by variable group or continent; click a correlation cell; click points to label countries")),
            tags$tr(tags$td(span(class = "pill-n", 2), b_(" Where")), tags$td("Where is risk grouped together?"),
                    tags$td("A map of hot spots and cold spots and a chart of which hazards cluster most"),
                    tags$td("Choose any hazard or weakness with \"Look at\"")),
            tags$tr(tags$td(span(class = "pill-n", 3), b_(" Events")), tags$td("Are the events themselves clustered?"),
                    tags$td("Every earthquake since 1965 as a point pattern: a density map, two tests against randomness, the G and L functions, and the link to country scores"),
                    tags$td("Choose a region and the smallest magnitude")),
            tags$tr(tags$td(span(class = "pill-n", 4), b_(" Why")), tags$td("What explains risk?"),
                    tags$td("Wealth against risk by continent, a map of what wealth does not explain, how the model was chosen"),
                    tags$td("Choose what to explain; click points to label countries")),
            tags$tr(tags$td(span(class = "pill-n", 5), b_(" How sure")), tags$td("Can we trust the results?"),
                    tags$td("Uncertainty ranges for the 30 highest-risk countries, blind spots, checks with four versions of the data"),
                    tags$td("Hover over a bar for the range")),
            tags$tr(tags$td(span(class = "pill-n", 6), b_(" Where to act")), tags$td("Where should action come first?"),
                    tags$td("A map where danger meets vulnerability, the countries carrying both, what helps"),
                    tags$td("Choose a danger (floods, drought, conflict and others)")),
            tags$tr(tags$td(b_("Explore")), tags$td("What about one country?"),
                    tags$td("A world map and a profile of the country you click. Choose Overall risk or any single hazard or weakness: the profile then explains that score, with the facts and recorded events behind it"),
                    tags$td("Click or search for a country; choose what to colour the map by; click a part of a score to see it; jump to neighbours")),
            tags$tr(tags$td(b_("Help")), tags$td("How does it work?"), tags$td("This page"), tags$td("Reopen the tour; copy the links"))))),

      help_section("controls", "hand-pointer", "Buttons and controls",
        "What each symbol does",
        div(class = "ctrl-grid",
          div(h4("On every page"),
            div(class = "ctrl", span(class = "gk", icon("square-caret-down")), div(b_("Menu at the top right"), " changes what the page shows (which variable, hazard or continent).")),
            div(class = "ctrl", span(class = "gk gk-green", icon("check")), div(b_("Green box"), " is the answer to the page's question, worked out from the data. It updates when you change the menu.")),
            div(class = "ctrl", span(class = "gk", icon("filter")), div(b_("Data note"), " under the answer says how many countries the analysis uses. Click it to see which were left out because of missing data.")),
            div(class = "ctrl", span(class = "gk", icon("download")), div(b_("Download"), " saves a chart or map as PNG, PDF or SVG, or its data as CSV.")),
            div(class = "ctrl", span(class = "gk", icon("chevron-down")), div(b_("Fold"), " hides a panel; click the title or the arrow again to show it.")),
            div(class = "ctrl", span(class = "gk", icon("sliders")), div(b_("View"), " (top right) folds or unfolds all panels, or switches to compact spacing.")),
            div(class = "ctrl", span(class = "gk", icon("book-open")), div(b_("Method and interpretation"), " at the bottom of each page explains the statistics in more detail.")),
            div(class = "ctrl", span(class = "gk gk-dark", icon("arrow-right")), div(b_("Next"), " (bottom right) takes you to the next step."))),
          div(h4("On maps"),
            div(class = "ctrl", span(class = "gk", icon("arrow-pointer")), div(b_("Hover"), " over a country to see its name and value. ", b_("Click"), " a country on Overview or Explore to open its profile.")),
            div(class = "ctrl", span(class = "gk", icon("layer-group")), div(b_("Layers"), " switches the background (satellite, light, streets, terrain, dark). Tick ", b_("Place names"), " to show city and country names.")),
            div(class = "ctrl", span(class = "gk", icon("plus")), div(b_("Zoom"), " in and out, or scroll with the mouse and drag to move.")),
            div(class = "ctrl", span(class = "gk", icon("up-right-and-down-left-from-center")), div(b_("Enlarge"), " makes the map fill the page width and most of the screen. Click again to shrink.")),
            div(class = "ctrl", span(class = "gk", icon("expand")), div(b_("Full screen"), " fills the whole screen. Press Esc to leave.")),
            div(class = "ctrl", span(class = "gk", icon("up-down-left-right")), div(b_("Drag the corner"), " (bottom right of a map) to any size. Double-click it to reset.")),
            h4("On charts"),
            div(class = "ctrl", span(class = "gk", icon("arrow-pointer")), div(b_("Hover"), " over a point or bar for its value. On the continent charts, ", b_("click a point"), " to label the country; click again to remove it.")),
            div(class = "ctrl", span(class = "gk", icon("magnifying-glass-plus")), div(b_("Drag a box"), " on a chart to zoom in; double-click to zoom back out."))))),

      help_section("words", "spell-check", "Words used in the dashboard",
        "The statistics in plain words",
        div(class = "glossary",
          lapply(list(
            c("INFORM Risk", "A score from 0 to 10 for how likely a country is to be overwhelmed by a disaster. It combines hazard and exposure, vulnerability and lack of coping capacity."),
            c("Hazard and exposure", "The dangers a country faces (floods, drought, cyclones, earthquakes, epidemics, conflict) and how many people are in their way."),
            c("Vulnerability", "How easily people are harmed: poverty, inequality, poor health, hunger, people forced from their homes."),
            c("Lack of coping capacity", "How poorly a country can prepare and respond: weak government, health care, roads, power and communication."),
            c("Spatial autocorrelation", "Whether neighbouring countries have similar values. If they do, risk is grouped on the map rather than scattered at random."),
            c("Moran's I", "A number that measures spatial autocorrelation. Around 0 means no pattern; closer to 1 means neighbours are very alike."),
            c("Hot spot (High-High)", "A high-risk country surrounded by high-risk neighbours. A cold spot (Low-Low) is the opposite."),
            c("p-value", "How likely a pattern this strong would appear by chance. Below 0.05 is usually taken as a real pattern."),
            c("Point pattern", "Data made of event locations (here, earthquakes), rather than one value per country."),
            c("Complete spatial randomness", "The pattern expected if events fell independently and equally likely anywhere. The Events page tests the earthquakes against it."),
            c("Kernel density", "A smooth map of how many events happen per area, made by spreading each event over its surroundings."),
            c("Quadrat test", "Count events in equal-area cells: if the counts vary much more than their mean (ratio above 1), the events are clustered."),
            c("Nearest-neighbour index", "The average distance to the nearest other event divided by what randomness would give. Below 1 means clustered."),
            c("G and L functions", "Curves that compare distances between events with many random patterns (the grey band). A curve above the band means clustering at that distance."),
            c("Regression", "A method that estimates how much one thing (here, wealth) explains another (risk)."),
            c("What wealth does not explain (residual)", "The difference between a country's actual risk and the risk its wealth predicts."),
            c("Spatial error model", "A regression that also allows neighbouring countries to share causes that are not measured. Used when the leftovers are still grouped on the map."),
            c("Uncertainty range", "How far a score could move if the data behind it contain errors, worked out by recalculating it thousands of times (Monte Carlo simulation)."),
            c("Lack of reliability", "INFORM's own score from 0 to 10 for how weak a country's data are (missing, old or estimated)."),
            c("Blind spot", "A high-risk country whose data are weak, so its score should be checked on the ground."),
            c("Lee's L", "A number that measures whether two things (here, danger and vulnerability) are high in the same places and their neighbours."),
            c("Double burden", "Countries in the top third for both danger and vulnerability."),
            c("Level compared with other countries", "On Explore, a single score is called Very low, Low, Medium, High or Very high by where it falls in the ranking of all countries (each fifth of the ranking is one level). Overall risk uses INFORM's own risk classes."),
            c("Measured facts", "The real-world figures INFORM builds each score from, for example the number of people expected to be exposed to river floods each year, or people affected by disasters in each of the last three years.")),
            function(w) div(class = "gl-item", b_(w[1]), span(w[2]))))),

      help_section("problems", "life-ring", "If something goes wrong",
        "Quick fixes",
        div(class = "glossary",
          div(class = "gl-item", b_("The page says \"Disconnected from the server\" or is grey"),
              span("Reload the page. On the free web version the server sleeps after a while; reloading wakes it.")),
          div(class = "gl-item", b_("The first visit is slow"),
              span("The server is waking up. Wait up to a minute; after that pages open quickly.")),
          div(class = "gl-item", b_("The map background is blank"),
              span("Background maps load from the internet. The coloured countries still work. Try another background with the layers button.")),
          div(class = "gl-item", b_("No flag, capital or summary on Explore"),
              span("These come from free online services. If they are slow or offline, the profile shows only the dashboard's own data.")),
          div(class = "gl-item", b_("Some countries are grey"),
              span("They have missing data for that analysis and were left out. Click the data note under the green box to see which and why.")),
          div(class = "gl-item", b_("The page looks crowded"),
              span("Use View, then Fold all panels, and open only the ones you need. Or use Compact spacing.")))),

      div(class = "help-end", tags$button(type = "button", class = "btn btn-dark", onclick = "window.grOpenGuide && window.grOpenGuide()",
                                          icon("compass"), " Take the one-minute tour"),
          help_go("overview", "Back to the Overview"))
    )
  },
  server = function(id, store, shared) moduleServer(id, function(input, output, session) NULL)
)

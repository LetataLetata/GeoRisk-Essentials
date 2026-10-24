# GEORISK Observatory

A six-step dashboard: spatial statistics of disaster risk for 191 countries (INFORM Risk Index 2026) and of 23,409 recorded earthquakes, built in R Shiny.

Created by Lerata Lerata, Crispen Chipimo, Orifha Makhado and Buhlebethu Masango (University of the Witwatersrand, Johannesburg).

## Open the app

**On the web (no R needed):** https://01a16f44-5481-89af-2d69-51579eddc650.share.connect.posit.cloud

Hosted free on Posit Connect Cloud. If the app has not been used for a while, the first visit takes up to a minute to wake it.

**From R or RStudio (works now).** Paste this into the R console. The first time, it installs the packages the app needs (a few minutes); after that it starts in seconds. It always runs the latest version from this page.

```r
pkgs <- c("shiny", "bslib", "leaflet", "sf", "spdep", "spatialreg", "DT", "ggplot2", "plotly",
          "htmltools", "htmlwidgets", "jsonlite", "patchwork", "ragg", "svglite", "ggrepel", "zip")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)
shiny::runGitHub("GeoRisk-Essentials", "LetataLetata", launch.browser = TRUE)
```

**Download a copy:** [GeoRisk-Essentials-main.zip](https://github.com/LetataLetata/GeoRisk-Essentials/archive/refs/heads/main.zip). Unzip it, open `app.R` in RStudio and click Run App.

**How it is built:** system architecture, app logic and wireframes are in [docs/](docs/README.md).

**New here?** The app opens on a welcome screen (click Continue, or press Enter; click the logo to see it again), then a one-minute tour on the first visit (reopen it with **Quick tour**, top right). The **Help** tab explains how to open the app, what every page and button does, and the statistics in plain words.

A dashboard that follows one chain of analysis about disaster risk in 191 countries, in six steps. Each step answers one question and feeds the next; every step page shows where it sits in the chain and ends with a button to the next step.

| Step | Page | Question | Main analysis | What you see |
|---|---|---|---|---|
| | Overview | All of them | Summary | Workflow diagram (click a step to open it), the main findings, world risk map, download of all figures and tables |
| 1 Describe | Data | What is in the data? | Descriptive statistics, distributions, Spearman correlations | Statistics table for all 41 variables (filter by group and continent), one variable in detail and by continent, all distributions, clickable correlation grid, the countries behind any correlation by continent, variable dictionary |
| 2 Locate | Where | Where is risk concentrated? | Global and local Moran's I | Cluster map and Moran scatterplot for overall risk or any hazard or weakness (floods, drought, cyclones, conflict, food security...), and Moran's I for every INFORM variable |
| 3 Detect | Events | Are the disaster events themselves clustered? | Point pattern analysis against complete spatial randomness: kernel density, quadrat test (variance-to-mean ratio, chi-square), nearest-neighbour index (Clark-Evans), G and L functions with simulation envelopes | Density map of every earthquake of magnitude 5.5 or more (1965-2016) for the world or a continent, both tests, the G and L functions, and the link between events per area and the INFORM earthquake score |
| 4 Explain | Why | What explains risk, and does geography still matter? | Regression on wealth, Lagrange multiplier tests, spatial error model | Wealth and risk by continent (click points to label countries), map of what wealth does not explain, the steps of model choice, model table |
| 5 Test | How sure | Can we trust the results? | Monte Carlo uncertainty, reliability adjustment, robustness checks | 90% ranges for the 30 highest-risk countries, blind spots, clustering under four versions of the data |
| 6 Decide | Where to act | Where should action come first? | Bivariate classes and Lee's L | Map where danger meets vulnerability for any of 12 hazards, countries by region, what helps at national level |
| | Explore | What about one country? | Profile and explanation | Click a country for its profile. "Colour countries by" sets what the profile explains: Overall risk, or any single hazard or weakness. For that score it shows the level compared with other countries, a plain "why this rating", what the score is made of (click a part to drill down), the measured facts behind it (for example people exposed to floods each year), recorded events (people affected by disasters in each of the last three years, drought history, the strongest earthquakes on record, displacement, conflict), and where it fits in the overall score |
| | Help | How does it work? | | How to open the app (web, R, download), three ways to use it, every page and control, the words used, quick fixes |

The six step tabs in the top bar are numbered 1 to 6, and under every page title a "How to use this page" line says what to click. Every page opens with a one-sentence answer computed from the data. Method details are in the collapsed "Method and interpretation" box at the bottom of each page.

## Figures for the report

The Overview page has a button that downloads every figure in publication quality (PNG at 300 dpi and vector PDF), numbered in the order of the analysis with titles, captions and sources, plus three tables (descriptive statistics, robustness, variable dictionary). Each chart and map also has its own download menu (PNG, PDF, SVG, CSV) for the settings chosen on its page; labels added by clicking points are kept in the download.

| Figure | Content |
|---|---|
| 1 | Analytical workflow |
| 2 | INFORM Risk class map |
| 3 | Distributions of risk and its three parts |
| 4 | Distributions of the hazard indicators |
| 5 | INFORM Risk by continent |
| 6 | Correlation matrix of the main variables |
| 7 | Local Moran's I cluster map |
| 8 | Moran scatterplot |
| 9 | Kernel density of earthquakes |
| 10 | G and L functions of earthquakes |
| 11 | Risk against GDP per person, by continent |
| 12 | Regression residual map |
| 13 | Uncertainty ranges, 30 highest-risk countries |
| 14 | Double burden map |

## Layout

Every panel can be folded with the arrow at its top right (or by clicking its title). The View menu (top right) has Fold all panels, Unfold all panels and Compact spacing (remembered in your browser).

## Continents

The data give World Bank regions, which mix continents. Countries are assigned to Africa, Asia, Europe, North America (including Central America and the Caribbean), South America and Oceania following the United Nations geographic scheme (`R/core/variables.R`).

## Missing data

Before each analysis runs, the countries are checked for every variable that analysis needs (`R/analysis/completeness.R`). Only countries with complete data are analysed, and the spatial neighbours are worked out again among them, so a country with missing data never enters a statistic, not even as somebody's neighbour. Excluded countries stay on the maps in grey ("No data: excluded from this analysis") and are listed under the answer on each page.

| Page | Needs | Currently excluded |
|---|---|---|
| Where | The chosen indicator | None |
| Events | Earthquake locations and magnitudes (no gaps); regions with fewer than 20 events are not tested | Africa at magnitude 6.5 and above (13 and 4 events) |
| Data | Nothing: statistics use every available value and report what is missing | None |
| Why | The outcome and GDP per person | 29 countries without GDP (mostly small island states, plus Eritrea, Somalia, South Sudan and Sudan) |
| How sure | INFORM Risk, the three pillars and the Lack of Reliability index | Tajikistan (no reliability score) |
| Where to act | The chosen danger and vulnerability | None |

## Maps

Every map opens on the satellite background (Esri World Imagery). The layer button (top right) switches to Light (CARTO), Streets (OpenStreetMap), Terrain (OpenTopoMap) or Dark (CARTO), and has a "Place names" layer that is off by default: tick it to show names above the coloured countries. None needs a key; they need an internet connection. Offline, the coloured countries still show on a plain background. Zoom buttons are under the layer button, the legend at bottom left and the scale bar at bottom right.

## Presenting

Every map can be made bigger three ways: the Enlarge button (top right, under the zoom buttons) widens it to the full page width and most of the screen height, click again to shrink; drag the handle at the map's bottom right corner to set any height and width (double-click it to reset); or use the full-screen button below Enlarge (press Esc to return). All results are precomputed in `data/results_cache.rds`, so changing a setting only redraws the page. After changing the data or the analysis code, run `Rscript scripts/precompute_results.R` once to refresh them (if you don't, the app notices the change and computes results on first use instead).

## Run it

To run straight from GitHub, use the `shiny::runGitHub()` line at the top of this page. To run a downloaded copy:

1. Open RStudio and set the working directory to this folder (the one containing `app.R`).
2. Install packages once:

```r
install.packages(c("shiny", "bslib", "leaflet", "sf", "spdep", "spatialreg", "DT",
                   "ggplot2", "plotly", "htmltools", "htmlwidgets", "jsonlite", "patchwork", "ragg", "svglite",
                   "ggrepel", "zip"))
```

3. Run:

```r
shiny::runApp(".", launch.browser = TRUE)
```

The processed data is in `data/`, so the app runs immediately. The background maps and the live country facts on the Explore page need internet; everything else works offline.

## Data

| Dataset | Source |
|---|---|
| INFORM Risk Mid-2026 (v0.7.3) and trend 2017-2026 | European Commission Joint Research Centre |
| Indicator data behind each score (people exposed, affected, displaced, and so on) | INFORM Risk Mid-2026, sheet "Indicator Data" (compiled by the EC JRC from GEM, UNDRR, EM-DAT, FAO, WHO, UNHCR, IDMC, UNDP, World Bank and others) |
| Significant earthquakes, magnitude 5.5 and stronger, 1965-2016 | US Geological Survey |
| GDP and energy | Our World in Data |
| Country boundaries 1:50m | Natural Earth |

## Structure

```
app.R             top bar and navigation
R/core/           colours, cards, downloads, maps, data store, page registry
R/analysis/       statistics as plain R functions (no Shiny)
docs/             system architecture, app logic and wireframes
scripts/          prepare_data.R (raw to data/), prepare_indicators.R (indicator facts and earthquakes),
                  precompute_points.R (point pattern results, uses spatstat), precompute_results.R (all other results),
                  make_globe.R (the line globe on the Overview)
R/views/          one file per page: 00_overview, 01_data, 02_where, 02b_events, 03_why, 04_sure, 05_act, 06_explore, 07_help
R/core/variables.R       the variable dictionary and continent lookup
R/analysis/explain.R     why a country gets each score: facts, parts, recorded events
R/analysis/points.R      the point pattern step: reads the stored results and draws its figures
R/analysis/figures.R     publication figures and descriptive statistics (used on screen and for export)
www/              styles, navigation script, logo.svg (the app logo), globe-lines.svg
data/             processed data
```

Each page is one file. If a page file has an error, only that page fails and the rest of the dashboard keeps working. To change a page, edit only its file.

## Limitations

- Countries are coarse and very unequal units; national averages hide differences within countries.
- INFORM is a relative index (0-10), not a probability of disaster, and fills data gaps by imputation. The How sure page shows how much this matters.
- The regression uses one explanation (wealth) to keep the result readable; the unexplained part is mapped and tested.

# Map helpers shared by all views: interactive leaflet choropleths and
# matching static ggplot maps (Equal Earth projection) for publication.

EQUAL_EARTH <- "+proj=eqearth +datum=WGS84 +units=m +no_defs"

# Free background maps (no key needed). The first one (satellite) shows when a
# map opens; the others are in the layer switcher (top right). Place names are
# a separate layer, off by default: tick it in the switcher to show them.
BASEMAPS <- c(
  "Satellite (Esri)" = "Esri.WorldImagery",
  "Light (CARTO)" = "CartoDB.PositronNoLabels",
  "Streets (OpenStreetMap)" = "OpenStreetMap.Mapnik",
  "Terrain (OpenTopoMap)" = "OpenTopoMap",
  "Dark (CARTO)" = "CartoDB.DarkMatterNoLabels"
)
PLACE_NAMES <- "Place names"

leaflet_basemaps <- function(minZoom = 1) {
  m <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = minZoom, worldCopyJump = TRUE, zoomSnap = 0.25,
                                                          zoomControl = FALSE, attributionControl = TRUE))
  for (nm in names(BASEMAPS))
    m <- leaflet::addProviderTiles(m, BASEMAPS[[nm]], group = nm,
                                   options = leaflet::providerTileOptions(maxZoom = if (BASEMAPS[[nm]] == "OpenTopoMap") 17 else 19))
  m <- leaflet::addProviderTiles(m, "CartoDB.VoyagerOnlyLabels", group = PLACE_NAMES,
                                 options = leaflet::providerTileOptions(pane = "shadowPane", opacity = 0.9))
  m <- leaflet::hideGroup(m, PLACE_NAMES)
  m <- leaflet::addLayersControl(m, baseGroups = names(BASEMAPS), overlayGroups = PLACE_NAMES, position = "topright",
                                 options = leaflet::layersControlOptions(collapsed = TRUE))
  # Zoom buttons top right under the layer switcher; the legend gets the bottom left to itself.
  m <- htmlwidgets::onRender(m, "function(el, x) { L.control.zoom({position: 'topright'}).addTo(this); }")
  leaflet::addScaleBar(m, "bottomright", options = leaflet::scaleBarOptions(imperial = FALSE))
}

# Every map in the dashboard starts from the same background maps.
leaflet_base <- function(minZoom = 1) leaflet_basemaps(minZoom)

# values: vector aligned with store$inform_sf rows; kind: "numeric" or a palette name.
leaflet_choropleth <- function(store, values, var, title, label_extra = NULL, view = c(15, 20, 1.6)) {
  sf <- store$inform_sf
  pal <- pal_for(var, values)
  lab <- sprintf("<div class='map-tip'><b>%s</b><br/>%s: <b>%s</b>%s</div>", sf$country, title,
                 if (is.numeric(values)) fmt_num(values, 2) else as.character(values),
                 if (is.null(label_extra)) "" else label_extra)
  # Countries left out of an analysis because of missing data are grey, and say so.
  lab[is.na(values)] <- sprintf("<div class='map-tip'><b>%s</b><br/><i>No data: excluded from this analysis</i></div>", sf$country[is.na(values)])
  m <- leaflet_base() |>
    leaflet::setView(view[1], view[2], view[3]) |>
    leaflet::addPolygons(data = sf, layerId = ~iso3, fillColor = pal(values), fillOpacity = 0.82,
                         color = "#FFFFFF", weight = 0.7, opacity = 0.9, smoothFactor = 0.5,
                         label = lapply(lab, htmltools::HTML),
                         highlightOptions = leaflet::highlightOptions(weight = 2, color = GR$ink, bringToFront = TRUE))
  legend_vals <- if (is.numeric(values)) values else factor(levels(values), levels = levels(values))
  leaflet::addLegend(m, "bottomleft", pal = pal, values = legend_vals, title = title, opacity = 0.95,
                     na.label = "No data", layerId = "legend")
}

# Static map for downloads. fill_type: "inform" (0-10 scale), "continuous",
# "diverging" or "factor" (with a named colour vector in colours).
static_choropleth <- function(store, values, legend_title, fill_type = "continuous", colours = NULL, limits = NULL) {
  geo <- store$geo
  sf <- geo$countries
  d <- data.frame(iso3 = store$inform_sf$iso3, value = values)
  sf <- merge(sf, d, by = "iso3", all.x = TRUE)
  sf <- sf::st_transform(sf, EQUAL_EARTH)
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = geo$outline, fill = "#F3F4F6", colour = NA) +
    ggplot2::geom_sf(data = sf, ggplot2::aes(fill = value), colour = "white", linewidth = 0.08) +
    ggplot2::coord_sf(crs = EQUAL_EARTH, expand = FALSE) + theme_map()
  p <- p + switch(fill_type,
    inform = scale_fill_inform(legend_title, limits = if (is.null(limits)) c(0, 10) else limits),
    continuous = ggplot2::scale_fill_gradientn(colours = GR$seq, name = legend_title, na.value = "#D1D5DB", limits = limits,
                                               guide = ggplot2::guide_colourbar(barwidth = 14, barheight = 0.6, title.position = "top")),
    diverging = {
      m <- max(abs(values), na.rm = TRUE)
      ggplot2::scale_fill_gradientn(colours = GR$div, limits = c(-m, m), name = legend_title, na.value = "#D1D5DB",
                                    guide = ggplot2::guide_colourbar(barwidth = 14, barheight = 0.6, title.position = "top"))
    },
    factor = ggplot2::scale_fill_manual(values = colours, name = legend_title, na.value = "#D1D5DB", drop = FALSE))
  attr(p, "export_data") <- d
  p
}

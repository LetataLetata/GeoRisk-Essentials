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

# Zoom, Enlarge and full-screen buttons, top right under the layer switcher,
# and a drag handle at the bottom right of each map card (see www/app.js).
# Full screen enlarges the whole map card (title, legend and download menu
# included); on the Explore page it enlarges the map together with the
# country panel. Press Esc or the button again to return.
MAP_CONTROLS_JS <- "function(el, x) {
  var map = this;
  L.control.zoom({position: 'topright'}).addTo(map);
  if (window.grMapEnhance) window.grMapEnhance(el, map);   /* Enlarge button and drag handle (www/app.js) */
  var Full = L.Control.extend({ options: { position: 'topright' }, onAdd: function () {
    var box = L.DomUtil.create('div', 'leaflet-bar gr-full');
    var a = L.DomUtil.create('a', '', box); a.href = '#'; a.title = 'Full screen (Esc to leave)'; a.setAttribute('role', 'button');
    a.innerHTML = '<svg viewBox=\"0 0 24 24\" width=\"16\" height=\"16\"><path d=\"M4 9V4h5M20 9V4h-5M4 15v5h5M20 15v5h-5\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2.2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"/></svg>';
    L.DomEvent.disableClickPropagation(box);
    L.DomEvent.on(a, 'click', function (e) {
      L.DomEvent.preventDefault(e);
      var target = el.closest('.explore-grid') || el.closest('.gr-card') || el;
      if (document.fullscreenElement || document.webkitFullscreenElement) { (document.exitFullscreen || document.webkitExitFullscreen).call(document); }
      else { var req = target.requestFullscreen || target.webkitRequestFullscreen; if (req) req.call(target); }
    });
    return box; } });
  new Full().addTo(map);
  /* The frame watcher in www/app.js redraws and refits the map; this is a backup for older browsers. */
  var fix = function () { setTimeout(function () { map.invalidateSize(); }, 250); };
  document.addEventListener('fullscreenchange', fix); document.addEventListener('webkitfullscreenchange', fix);
}"

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
  m <- htmlwidgets::onRender(m, MAP_CONTROLS_JS)
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
                     na.label = "No data", layerId = "legend", bins = 5)
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

# ------------------------------------------------------------ fast maps --
# Colours, hover labels and legend for one choropleth, aligned with
# store$inform_sf. Used to draw a map once and then only recolour it.
choropleth_style <- function(store, values, var, title, label_extra = NULL) {
  sf <- store$inform_sf
  pal <- pal_for(var, values)
  lab <- sprintf("<div class='map-tip'><b>%s</b><br/>%s: <b>%s</b>%s</div>", sf$country, title,
                 if (is.numeric(values)) fmt_num(values, 2) else as.character(values),
                 if (is.null(label_extra)) "" else label_extra)
  lab[is.na(values)] <- sprintf("<div class='map-tip'><b>%s</b><br/><i>No data: excluded from this analysis</i></div>", sf$country[is.na(values)])
  legend_vals <- if (is.numeric(values)) values else factor(levels(values), levels = levels(values))
  list(fill = pal(values), label = lab, legend = list(pal = pal, values = legend_vals, title = title))
}

bivariate_style <- function(store, cls, labels, hz_lab, vu_lab) {
  sf <- store$inform_sf
  fill <- ifelse(is.na(cls), "#D1D5DB", BIVAR_COLS[as.character(cls)])
  tip <- sprintf("<div class='map-tip'><b>%s</b><br/>%s</div>", sf$country, ifelse(is.na(labels), "<i>No data: excluded from this analysis</i>", labels))
  list(fill = unname(fill), label = tip, legend_html = bivariate_legend_html(hz_lab, vu_lab))
}

add_style_legend <- function(m, st) {
  if (!is.null(st$legend_html))
    return(leaflet::addControl(m, htmltools::HTML(st$legend_html), position = "bottomleft", className = "info legend", layerId = "legend"))
  leaflet::addLegend(m, "bottomleft", pal = st$legend$pal, values = st$legend$values, title = st$legend$title,
                     opacity = 0.95, na.label = "No data", layerId = "legend", bins = 5)
}

draw_styled_map <- function(store, st, view = c(15, 20, 1.6)) {
  leaflet_base() |>
    leaflet::setView(view[1], view[2], view[3]) |>
    leaflet::addPolygons(data = store$inform_sf, layerId = ~iso3, fillColor = st$fill, fillOpacity = 0.82,
                         color = "#FFFFFF", weight = 0.7, opacity = 0.9, smoothFactor = 0.5,
                         label = lapply(st$label, htmltools::HTML),
                         highlightOptions = leaflet::highlightOptions(weight = 2, color = GR$ink, bringToFront = TRUE)) |>
    add_style_legend(st)
}

# Send only new colours and labels to a map already on screen.
restyle_map <- function(session, output_id, store, st) {
  session$sendCustomMessage("gr-restyle", list(id = session$ns(output_id), ids = store$inform_sf$iso3, fill = st$fill, labels = st$label))
  leaflet::leafletProxy(output_id, session) |> leaflet::removeControl("legend") |> add_style_legend(st)
}

# GEORISK Observatory: draw the globe used on the Overview banner.
#
# A real globe from the Natural Earth country boundaries the app already uses,
# seen from above Africa (orthographic projection). Countries on the far side
# are cut away before projecting. Writes www/globe.svg (transparent background).
#
# Run from the app folder:  Rscript scripts/make_globe.R

suppressPackageStartupMessages({ library(sf); library(ggplot2) })
sf_use_s2(TRUE)
geo <- readRDS("data/geography.rds")
land <- st_make_valid(st_geometry(geo$countries))
lon0 <- 20; lat0 <- 8
ortho <- sprintf("+proj=ortho +lat_0=%s +lon_0=%s +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs", lat0, lon0)

# Keep only what faces the viewer: a cap of 89 degrees around the centre point.
centre <- st_sfc(st_point(c(lon0, lat0)), crs = 4326)
cap <- st_buffer(centre, units::set_units(89 * 111195, "m"))
vis <- suppressWarnings(st_intersection(land, cap))
vis <- vis[!st_is_empty(vis)]
vis <- st_transform(vis, ortho)
vis <- st_simplify(st_make_valid(vis), dTolerance = 15000)
vis <- vis[!st_is_empty(vis)]

R <- 6371000
disc <- st_buffer(st_sfc(st_point(c(0, 0)), crs = ortho), R, nQuadSegs = 90)
# Lines of latitude and longitude for depth, cut to the visible side.
grat <- st_graticule(lon = seq(-180, 180, 30), lat = seq(-60, 60, 30), ndiscr = 200)
grat <- suppressWarnings(st_intersection(st_geometry(grat), cap))
grat <- st_transform(grat[!st_is_empty(grat)], ortho)

p <- ggplot() +
  geom_sf(data = disc, fill = "#2F80B7", colour = NA) +
  geom_sf(data = grat, colour = "#FFFFFF", alpha = 0.18, linewidth = 0.3) +
  geom_sf(data = vis, fill = "#A7D7A0", colour = "#E8F5E4", linewidth = 0.15) +
  geom_sf(data = disc, fill = NA, colour = "#BBF7D0", linewidth = 0.8, alpha = 0.6) +
  coord_sf(crs = ortho, expand = FALSE, datum = NA) +
  theme_void() + theme(plot.background = element_rect(fill = "transparent", colour = NA),
                       panel.background = element_rect(fill = "transparent", colour = NA))
svglite::svglite("www/globe.svg", width = 4, height = 4, bg = "transparent")
print(p); invisible(dev.off())
cat("Saved www/globe.svg:", round(file.size("www/globe.svg") / 1024), "KB\n")

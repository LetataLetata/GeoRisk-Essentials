# GEORISK Observatory: draw the line globe on the Overview banner.
#
# A one-colour line drawing of the real globe: country outlines from the
# Natural Earth boundaries the app already uses, lines of latitude and
# longitude, and the rim, seen from above Africa (orthographic projection).
# Countries on the far side are cut away before projecting. The banner places
# it half off its right edge. Writes www/globe-lines.svg (transparent).
#
# Run from the app folder:  Rscript scripts/make_globe.R

suppressPackageStartupMessages({ library(sf); library(ggplot2) })
sf_use_s2(TRUE)
geo <- readRDS("data/geography.rds")
land <- st_make_valid(st_geometry(geo$countries))
lon0 <- 15; lat0 <- 10
ortho <- sprintf("+proj=ortho +lat_0=%s +lon_0=%s +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs", lat0, lon0)
ink <- "#FFFFFF"

# Keep only what faces the viewer: a cap of 89 degrees around the centre point.
cap <- st_buffer(st_sfc(st_point(c(lon0, lat0)), crs = 4326), units::set_units(89 * 111195, "m"))
vis <- suppressWarnings(st_intersection(land, cap))
vis <- st_transform(vis[!st_is_empty(vis)], ortho)
vis <- st_simplify(st_make_valid(vis), dTolerance = 12000)
vis <- vis[!st_is_empty(vis)]
grat <- st_graticule(lon = seq(-180, 165, 15), lat = seq(-75, 75, 15), ndiscr = 300)
grat <- suppressWarnings(st_intersection(st_geometry(grat), cap))
grat <- st_transform(grat[!st_is_empty(grat)], ortho)
rim <- st_buffer(st_sfc(st_point(c(0, 0)), crs = ortho), 6371000, nQuadSegs = 120)

p <- ggplot() +
  geom_sf(data = grat, colour = ink, alpha = 0.22, linewidth = 0.25) +
  geom_sf(data = vis, fill = NA, colour = ink, alpha = 0.7, linewidth = 0.3) +
  geom_sf(data = rim, fill = NA, colour = ink, alpha = 0.75, linewidth = 0.6) +
  coord_sf(crs = ortho, expand = FALSE, datum = NA) +
  theme_void() + theme(plot.background = element_rect(fill = "transparent", colour = NA),
                       panel.background = element_rect(fill = "transparent", colour = NA))
svglite::svglite("www/globe-lines.svg", width = 5, height = 5, bg = "transparent")
print(p); invisible(dev.off())
cat("Saved www/globe-lines.svg:", round(file.size("www/globe-lines.svg") / 1024), "KB\n")

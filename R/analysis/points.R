# Step 3, Events: point pattern analysis of earthquakes. The statistics are
# computed once by scripts/precompute_points.R (with spatstat) and stored in
# data/point_pattern.rds; these functions read them and draw the figures.

pp_result <- function(store, region, mag) {
  pp <- store$points; if (is.null(pp)) return(NULL)
  pp$results[[paste(region, mag)]]
}

# Plain-language verdicts for the two tests.
pp_verdict_vmr <- function(q) if (q$p < 0.05 && q$vmr > 1) "clustered" else if (q$p < 0.05 && q$vmr < 1) "regular (evenly spread)" else "not different from random"
pp_verdict_nn <- function(nn) if (nn$p < 0.05 && nn$r < 1) "clustered" else if (nn$p < 0.05 && nn$r > 1) "dispersed" else "not different from random"

# Kernel density map with the events on top, in the region's equal-area projection.
fig_pp_density <- function(store, region, mag) {
  r <- pp_result(store, region, mag); pp <- store$points
  d <- r$density; d <- d[!is.na(d$z), ]
  ol <- pp$outlines[[region]]
  ggplot2::ggplot() +
    ggplot2::geom_tile(data = d, ggplot2::aes(x, y, fill = z)) +
    ggplot2::scale_fill_gradientn("Earthquakes per\n10,000 km2", colours = c("#F7FBF4", "#FDE68A", "#F59E0B", "#DC2626", "#7F1D1D"),
                                  trans = "sqrt", guide = ggplot2::guide_colourbar(barwidth = 0.6, barheight = 8)) +
    ggplot2::geom_sf(data = ol, fill = NA, colour = "#4B5563", linewidth = 0.15) +
    ggplot2::geom_point(data = r$pts, ggplot2::aes(x, y), size = 0.25, alpha = if (r$n > 5000) 0.15 else 0.45, colour = "#111827") +
    ggplot2::coord_sf(crs = pp$crs[[region]], expand = FALSE, datum = NA) +
    ggplot2::labs(x = NULL, y = NULL) + theme_georisk(10) +
    ggplot2::theme(legend.position = "right", panel.grid = ggplot2::element_blank(), axis.text = ggplot2::element_blank())
}

# G function: share of events whose nearest neighbour is within distance d, against complete spatial randomness.
fig_pp_G <- function(store, region, mag) {
  g <- pp_result(store, region, mag)$G
  g$text <- sprintf("Within %.0f km: %.0f%% of earthquakes (random: %.0f%%)", g$r_km, 100 * g$obs, 100 * g$theo)
  ggplot2::ggplot(g, ggplot2::aes(r_km)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), fill = "#9CA3AF", alpha = 0.35) +
    ggplot2::geom_line(ggplot2::aes(y = theo), colour = "#6B7280", linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = obs, text = text, group = 1), colour = "#DC2626", linewidth = 1.1) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, 1)) +
    ggplot2::labs(x = "Distance to the nearest other earthquake (km)", y = "Share of earthquakes, G(d)") + theme_georisk(10)
}

# L function (Ripley's K made linear, minus d): above the band = clustered at that distance.
fig_pp_L <- function(store, region, mag) {
  l <- pp_result(store, region, mag)$L
  l$text <- sprintf("At %.0f km: L(d) - d = %.0f km", l$r_km, l$obs)
  ggplot2::ggplot(l, ggplot2::aes(r_km)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), fill = "#9CA3AF", alpha = 0.35) +
    ggplot2::geom_hline(yintercept = 0, colour = "#6B7280", linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = obs, text = text, group = 1), colour = "#DC2626", linewidth = 1.1) +
    ggplot2::labs(x = "Distance d (km)", y = "L(d) - d (km)") + theme_georisk(10)
}

# Link to the country scores: earthquakes per 10,000 km2 of land against the INFORM earthquake score.
fig_pp_link <- function(store, mag) {
  lk <- store$points$link[[as.character(mag)]]; d <- lk$data[!is.na(lk$data$earthquake), ]
  d$text <- sprintf("%s<br>%d earthquakes, %.2f per 10,000 km2<br>INFORM earthquake score: %.1f", d$country, d$quakes, d$per_10k, d$earthquake)
  ggplot2::ggplot(d, ggplot2::aes(per_10k + 0.01, earthquake, colour = continent, text = text)) +
    ggplot2::geom_point(size = 2, alpha = 0.8) +
    ggplot2::scale_x_log10(labels = function(x) format(x, drop0trailing = TRUE, scientific = FALSE)) +
    ggplot2::scale_colour_manual(values = CONT_COLS, name = NULL) +
    ggplot2::labs(x = "Recorded earthquakes per 10,000 km2 of land, 1965-2016 (log scale; + 0.01 so zero shows)", y = "INFORM earthquake score (0-10)") +
    theme_georisk(10)
}

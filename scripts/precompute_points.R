# GEORISK Observatory: point pattern analysis of earthquakes (step 3, Events).
#
# Tests whether disaster events themselves are clustered, using the methods
# for point pattern data: kernel density (intensity), the quadrat test
# (variance-to-mean ratio and chi-square), the nearest-neighbour index
# (Clark-Evans) with its z-test, and the G and L (Ripley's K) functions with
# simulation envelopes under complete spatial randomness (a homogeneous
# Poisson process). It also links the points back to the country scores:
# earthquakes per 10,000 km2 of land against the INFORM earthquake score.
#
# Data: significant earthquakes, magnitude 5.5 and above, 1965-2016 (USGS),
# with the nearest country within 100 km (data/earthquakes.rds).
# Regions: the world (all events, the whole Earth as the window) and each
# continent (events linked to its countries; window = its land plus 100 km).
# Projections are equal-area, so densities and quadrat counts are fair:
# Mollweide for the world, Lambert azimuthal equal-area centred on each
# continent (where distances are also close to true).
#
# Needs spatstat (only here: the app reads the saved results, so the web
# server does not need it). Run from the app folder; takes a few minutes:
#   Rscript scripts/precompute_points.R

suppressPackageStartupMessages({ library(sf); library(spatstat.geom); library(spatstat.explore); library(spatstat.random) })
sf_use_s2(FALSE)
for (f in c(list.files("R/core", full.names = TRUE), list.files("R/analysis", full.names = TRUE))) source(f, local = FALSE)
store <- load_store("data")
quakes <- readRDS("data/earthquakes.rds")
set.seed(2026)

REGIONS <- c("World", CONTINENT_LEVELS)
MAGS <- c(5.5, 6, 6.5, 7)
NSIM <- 39   # 39 simulations give a pointwise envelope at about the 5% level (1 in 40)
centre <- list(Africa = c(20, 2), Asia = c(95, 30), Europe = c(15, 52), `North America` = c(-95, 35),
               `South America` = c(-60, -18), Oceania = c(150, -15))
land <- st_make_valid(store$geo$countries)
land$continent <- store$inform$continent[match(land$iso3, store$inform$iso3)]

region_setup <- function(region) {
  if (region == "World") {
    crs <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"
    R <- 6371007; a <- 2 * sqrt(2) * R; b <- sqrt(2) * R          # Mollweide ellipse: the whole Earth
    th <- seq(0, 2 * pi, length.out = 721)[-721]
    win <- owin(poly = list(x = a * cos(th), y = b * sin(th)))
    pts <- quakes
    outline <- st_transform(st_geometry(land), crs)
  } else {
    c0 <- centre[[region]]
    crs <- sprintf("+proj=laea +lat_0=%s +lon_0=%s +datum=WGS84 +units=m +no_defs", c0[2], c0[1])
    reg <- land[land$continent %in% region, ]
    shape <- st_union(st_buffer(st_transform(st_geometry(reg), crs), 100000))
    shape <- st_simplify(st_make_valid(shape), dTolerance = 5000)
    win <- as.owin(shape)
    pts <- quakes[quakes$iso3 %in% reg$iso3, ]
    outline <- st_transform(st_geometry(reg), crs)
  }
  xy <- st_coordinates(st_transform(st_as_sf(pts, coords = c("lon", "lat"), crs = 4326), crs))
  list(crs = crs, win = win, pts = pts, xy = xy, outline = outline)
}

analyse <- function(rs, mag) {
  keep <- rs$pts$magnitude >= mag
  X <- ppp(rs$xy[keep, 1], rs$xy[keep, 2], window = rs$win, check = FALSE)
  X <- X[inside.owin(X$x, X$y, rs$win)]
  X <- unique(X)
  n <- npoints(X); A <- area.owin(rs$win); lam <- n / A
  if (n < 20) return(list(n = n, too_few = TRUE))
  # Quadrat test: equal-area cells over the window (cells cut by the edge are weighted by area).
  nx <- 12; ny <- if (diff(rs$win$yrange) / diff(rs$win$xrange) < 0.7) 6 else 10
  qc <- quadratcount(X, nx = nx, ny = ny)
  tess <- as.tess(qc); cell_area <- tile.areas(tess)
  ok <- cell_area > 0.5 * max(cell_area)                       # whole or nearly whole cells only
  cnt <- vapply(tiles(tess), function(t) npoints(X[t]), numeric(1))   # same order as tile.areas
  m <- sum(ok); xbar <- mean(cnt[ok]); s2 <- stats::var(cnt[ok]); vmr <- s2 / xbar
  chi <- (m - 1) * vmr; p_q <- stats::pchisq(chi, m - 1, lower.tail = FALSE)
  # Nearest-neighbour index (Clark-Evans), as in the notes: r = observed / expected mean distance.
  d <- nndist(X); dobs <- mean(d); dexp <- 0.5 / sqrt(lam); se <- 0.26136 / sqrt(n * lam)
  r <- dobs / dexp; z <- (dobs - dexp) / se; p_nn <- 2 * stats::pnorm(-abs(z))
  # G and L functions with envelopes from 39 homogeneous Poisson simulations.
  rmaxG <- stats::quantile(d, 0.98)
  Genv <- envelope(X, Gest, nsim = NSIM, r = seq(0, rmaxG, length.out = 120), verbose = FALSE, savefuns = FALSE)
  sub <- if (n > 3000) X[sample.int(n, 3000)] else X          # L function on a random 3,000 for speed
  rmaxL <- min(diff(rs$win$xrange), diff(rs$win$yrange)) / 8
  Lenv <- envelope(sub, Lest, nsim = NSIM, r = seq(0, rmaxL, length.out = 120), correction = "border", verbose = FALSE, savefuns = FALSE)
  # Kernel density (intensity), events per 10,000 km2. Gaussian kernel; bandwidth a
  # third of Scott's rule, because the full rule smooths the plate boundaries away.
  bw <- bw.scott(X) / 3
  den <- density(X, sigma = mean(bw), dimyx = c(140, 200), edge = TRUE)
  dd <- as.data.frame(den); names(dd) <- c("x", "y", "z"); dd$z <- dd$z * 1e10
  gdf <- data.frame(r_km = Genv$r / 1000, obs = Genv$obs, theo = Genv$theo, lo = Genv$lo, hi = Genv$hi)
  ldf <- data.frame(r_km = Lenv$r / 1000, obs = (Lenv$obs - Lenv$r) / 1000, theo = 0, lo = (Lenv$lo - Lenv$r) / 1000, hi = (Lenv$hi - Lenv$r) / 1000)
  above <- ldf$r_km[ldf$obs > ldf$hi]
  list(n = n, area_km2 = A / 1e6, lambda_10k = lam * 1e10, too_few = FALSE,
       quadrat = list(m = m, mean = xbar, var = s2, vmr = vmr, chi = chi, df = m - 1, p = p_q, nx = nx, ny = ny),
       nn = list(dobs_km = dobs / 1000, dexp_km = dexp / 1000, r = r, z = z, p = p_nn),
       G = gdf, L = ldf, cluster_upto_km = if (length(above)) max(above) else NA, L_sample = npoints(sub),
       density = dd, bandwidth_km = mean(bw) / 1000, pts = data.frame(x = X$x, y = X$y))
}

# Link to the areal data: earthquakes per 10,000 km2 of land per country against the INFORM earthquake score.
link_scores <- function(mag) {
  a <- as.numeric(st_area(st_transform(land, "+proj=moll +datum=WGS84 +units=m"))) / 1e6
  q <- quakes[!is.na(quakes$iso3) & quakes$magnitude >= mag, ]
  cnt <- table(factor(q$iso3, levels = land$iso3))
  d <- data.frame(iso3 = land$iso3, country = land$country, quakes = as.numeric(cnt), area_km2 = a)
  d$per_10k <- 1e4 * d$quakes / d$area_km2
  d$earthquake <- store$inform$earthquake[match(d$iso3, store$inform$iso3)]
  d$continent <- store$inform$continent[match(d$iso3, store$inform$iso3)]
  ct <- suppressWarnings(stats::cor.test(d$per_10k, d$earthquake, method = "spearman", exact = FALSE))
  list(data = d, rho = unname(ct$estimate), p = ct$p.value, n = sum(!is.na(d$earthquake)))
}

t0 <- Sys.time(); out <- list(regions = REGIONS, mags = MAGS, results = list(), link = list(), outlines = list(), crs = list())
for (rg in REGIONS) {
  rs <- region_setup(rg)
  out$outlines[[rg]] <- st_simplify(rs$outline, dTolerance = if (rg == "World") 20000 else 8000)
  out$crs[[rg]] <- rs$crs
  # (the spatstat window is not saved, so the app does not need spatstat)
  for (mg in MAGS) {
    key <- paste(rg, mg)
    out$results[[key]] <- tryCatch(analyse(rs, mg), error = function(e) list(error = conditionMessage(e)))
    r <- out$results[[key]]
    cat(sprintf("[%4.0fs] %-14s M%.1f  n=%5s  VMR=%s  R=%s\n", as.numeric(Sys.time() - t0, units = "secs"), rg, mg,
                r$n %||% "-", if (!is.null(r$quadrat)) round(r$quadrat$vmr, 1) else "-", if (!is.null(r$nn)) round(r$nn$r, 2) else r$error %||% "-"))
  }
}
for (mg in MAGS) out$link[[as.character(mg)]] <- link_scores(mg)
out$note <- "Significant earthquakes M5.5+, 1965-2016 (USGS). Equal-area projections; 39 simulations of complete spatial randomness."
saveRDS(out, "data/point_pattern.rds", compress = "xz")
cat("Saved data/point_pattern.rds (", round(file.size("data/point_pattern.rds") / 1024), "KB )\n")

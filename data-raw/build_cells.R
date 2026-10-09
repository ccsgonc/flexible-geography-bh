# Builds the public, disclosure-controlled grid used by the app.
#
# Input (NOT part of this repository): establishment-level value added of
# tourist accommodation in Belo Horizonte, 2015, allocated to RAIS local units
# (Goncalves 2020). Confidential: never commit it.
#
# Output: app/cells.csv, one row per published grid cell.
#
# Method: adaptive (quadtree) square grid in SIRGAS 2000 / UTM 23S.
#   Level 1 = 500 m squares; each level doubles the side (1, 2, 4, 8 km).
#   Bottom-up, an establishment is published in the smallest square that,
#   counting only establishments not yet published at a finer level, passes:
#     - threshold rule: at least MIN_N establishments;
#     - dominance rule: the largest establishment holds at most MAX_SHARE of the
#       cell's value added.
#   Published cells are disjoint (a parent is drawn minus its published
#   children), so no establishment can be recovered by differencing cells.
#   What is left after the coarsest level is reported only as a city total.
#
# Usage: Rscript data-raw/build_cells.R <path/to/novosdados.xlsx> [app/cells.csv]

suppressMessages({
  library(readxl)
  library(sf)
  library(dplyr)
})

MIN_N     <- 3
MAX_SHARE <- 0.85
SIDES     <- c(500, 1000, 2000, 4000, 8000)

args <- commandArgs(TRUE)
input  <- args[1]
output <- if (length(args) >= 2) args[2] else "app/cells.csv"

raw <- read_excel(input, sheet = "arquivo (3)", col_types = "text")
est <- data.frame(
  va    = as.numeric(raw$income),
  jobs  = as.numeric(raw$adultpop),
  hotel = raw$superzip == "1",
  lat   = as.numeric(raw$latitude),
  lon   = as.numeric(raw$longitude)
)
stopifnot(!anyNA(est))

# Belo Horizonte municipal bounding box; points outside are geocoding failures
# and are kept only in the city total.
in_bh <- est$lat <= -19.78 & est$lat >= -20.06 & est$lon >= -44.07 & est$lon <= -43.85

xy <- st_coordinates(st_transform(st_as_sf(est, coords = c("lon", "lat"), crs = 4326), 31983))
x0 <- floor(min(xy[in_bh, 1]) / max(SIDES)) * max(SIDES)
y0 <- floor(min(xy[in_bh, 2]) / max(SIDES)) * max(SIDES)

passes <- function(va, n) n >= MIN_N && max(va) / sum(va) <= MAX_SHARE

pending <- in_bh
cells <- list()
for (side in SIDES) {
  key <- paste(floor((xy[, 1] - x0) / side), floor((xy[, 2] - y0) / side))
  for (k in unique(key[pending])) {
    idx <- which(pending & key == k)
    if (passes(est$va[idx], length(idx))) {
      ij <- as.numeric(strsplit(k, " ")[[1]])
      cells[[length(cells) + 1]] <- data.frame(
        side = side,
        xmin = x0 + ij[1] * side, ymin = y0 + ij[2] * side,
        n = length(idx), n_hotels = sum(est$hotel[idx]),
        va = sum(est$va[idx]), jobs = sum(est$jobs[idx])
      )
      pending[idx] <- FALSE
    }
  }
}
cells <- bind_rows(cells)

# Geometry: each square minus the published squares nested inside it.
sq <- lapply(seq_len(nrow(cells)), function(i) with(cells[i, ],
  st_polygon(list(rbind(c(xmin, ymin), c(xmin + side, ymin), c(xmin + side, ymin + side),
                        c(xmin, ymin + side), c(xmin, ymin))))))
geom <- st_sfc(sq, crs = 31983)
for (i in seq_along(geom)) {
  inner <- which(cells$side < cells$side[i] &
                 cells$xmin >= cells$xmin[i] & cells$xmin < cells$xmin[i] + cells$side[i] &
                 cells$ymin >= cells$ymin[i] & cells$ymin < cells$ymin[i] + cells$side[i])
  if (length(inner)) geom[i] <- st_difference(geom[i], st_union(st_sfc(sq[inner], crs = 31983)))
}
area_km2 <- as.numeric(st_area(geom)) / 1e6
anchor <- st_coordinates(st_transform(st_point_on_surface(geom), 4326))
geom <- st_transform(geom, 4326)

# Geometry serialised for a plain CSV that the browser-only (webR) app can read
# without sf: "lng lat,lng lat,..." per ring, rings of a polygon joined by ";"
# (first = outer), polygons of a multipolygon joined by "|". A cell minus its
# nested cells can be L-shaped, have holes or fall apart in pieces.
rings <- vapply(geom, function(g) {
  polys <- if (inherits(g, "MULTIPOLYGON")) unclass(g) else list(unclass(g))
  paste(vapply(polys, function(p) paste(vapply(p, function(r)
    paste(sprintf("%.6f %.6f", r[, 1], r[, 2]), collapse = ","), ""), collapse = ";"), ""), collapse = "|")
}, "")

out <- data.frame(
  cell = sprintf("C%03d", seq_len(nrow(cells))),
  side_m = cells$side,
  area_km2 = round(area_km2, 4),
  establishments = cells$n,
  hotels = cells$n_hotels,
  value_added = round(cells$va),
  jobs = cells$jobs,
  lat = round(anchor[, 2], 6),
  lng = round(anchor[, 1], 6),
  rings = rings
)
out <- out[order(-out$value_added), ]
out$cell <- sprintf("C%03d", seq_len(nrow(out)))

# Not-published remainder goes in as a single row without geometry, so the city
# total in the app matches the source.
rest <- !in_bh | pending
out <- rbind(out, data.frame(
  cell = "UNMAPPED", side_m = NA, area_km2 = NA, establishments = sum(rest), hotels = sum(est$hotel[rest]),
  value_added = round(sum(est$va[rest])), jobs = sum(est$jobs[rest]),
  lat = NA, lng = NA, rings = ""
))

stopifnot(sum(out$establishments) == nrow(est),
          abs(sum(out$value_added) - sum(est$va)) < nrow(out))

dir.create(dirname(output), showWarnings = FALSE, recursive = TRUE)
tmp <- paste0(output, ".tmp")
write.csv(out, tmp, row.names = FALSE)
if (file.exists(output)) unlink(output)
stopifnot(file.rename(tmp, output))

message(sprintf("%d cells published (%s); %d establishments (%.1f%% of value added) unmapped",
  nrow(out) - 1, paste(names(table(cells$side)), table(cells$side), sep = " m: ", collapse = ", "),
  sum(rest), 100 * sum(est$va[rest]) / sum(est$va)))

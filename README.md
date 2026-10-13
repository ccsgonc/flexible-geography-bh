# Tourist accommodation in Belo Horizonte, 2015: a flexible-geography explorer

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23328479.svg)](https://doi.org/10.5281/zenodo.23328479)

Interactive companion to

> Gonçalves, C. C. S. (2020). Production of economic statistics for tourist accommodation services of Belo Horizonte under the flexible geography approach. *Revista de Turismo Contemporâneo*, 8(2), 198–221. <https://doi.org/10.21680/2357-8211.2020v8n2ID19375>

**Open the app:** <https://ccsgonc.github.io/flexible-geography-bh/>

Official statistics are released for fixed administrative units. Under the flexible-geography approach the user defines the region. In this app that region is whatever is in view on the map: zoom and pan, and the value added, establishments and jobs in view are added up.

The app is a [Shinylive](https://posit-dev.github.io/r-shinylive/) build: R runs in the browser through WebAssembly, so no server is involved. The first visit downloads the R runtime and packages and takes a little while. Later visits load from the browser cache.

## Data

The value added of tourist accommodation services in Belo Horizonte in 2015 was derived from IBGE's national and regional accounts. It was allocated to the accommodation establishments in RAIS 2015 in proportion to their employment, and the establishments were geolocated from their addresses (see the article for the full method).

Establishment-level data are confidential and are **not** in this repository. The app shows an adaptive grid built by [`data-raw/build_cells.R`](data-raw/build_cells.R):

- the grid starts with 500 m squares (SIRGAS 2000 / UTM 23S), which merge into 1, 2, 4 or 8 km squares where establishments are sparse;
- a cell is published only if it holds **at least 3 establishments** and **no establishment accounts for more than 85%** of its value added;
- cells do not overlap: a coarser cell excludes the finer cells published inside it, so no establishment can be recovered by differencing;
- what cannot be placed in any cell passing both rules, plus two establishments geolocated outside the municipality, is reported only in the city total (13 of 260 establishments, 7.8% of value added).

[`app/cells.csv`](app/cells.csv) holds the published cells: one row per cell plus an `UNMAPPED` row with the remainder.

## Repository layout

| Path | What it is |
|---|---|
| `app/app.R`, `app/cells.csv` | The Shiny app and its data |
| `data-raw/build_cells.R` | Builds `cells.csv` from the confidential source (not included) |
| `docs/` | Shinylive export served by GitHub Pages |

To rebuild after changing the app:

```r
shinylive::export("app", "docs")
```

To run it locally as a regular Shiny app: `shiny::runApp("app")`.

## Citing the app

> Gonçalves, C. C. S. (2026). *Tourist accommodation in Belo Horizonte, 2015: a flexible-geography explorer* (v1.0.0) [Software]. Zenodo. <https://doi.org/10.5281/zenodo.23328480>

The DOI above is for v1.0.0. To cite whatever version is current, use the concept DOI in the badge, [10.5281/zenodo.23328479](https://doi.org/10.5281/zenodo.23328479).

## Licence

Code: MIT. The original interface was adapted from RStudio's SuperZIP example (MIT).

library(shiny)
library(bslib)
library(leaflet)
library(DT)

# Data ------------------------------------------------------------------------

cells <- read.csv("cells.csv", stringsAsFactors = FALSE)
unmapped <- cells[cells$cell == "UNMAPPED", ]
cells <- cells[cells$cell != "UNMAPPED", ]

city_total <- sum(cells$value_added) + unmapped$value_added
city_estab <- sum(cells$establishments) + unmapped$establishments
city_jobs  <- sum(cells$jobs) + unmapped$jobs

# Each cell is its square minus the finer published cells nested in it, so it
# can be L-shaped, have holes or come in pieces. Geometry comes as
# "lng lat,lng lat,..." per ring, ";" between rings of a polygon (first = outer)
# and "|" between polygons. Drawn as GeoJSON MultiPolygons, without needing sf.
parse_ring <- function(r)
  unname(do.call(rbind, lapply(strsplit(strsplit(r, ",", fixed = TRUE)[[1]], " ", fixed = TRUE), as.numeric)))
cell_polys <- lapply(strsplit(cells$rings, "|", fixed = TRUE), function(polys)
  lapply(strsplit(polys, ";", fixed = TRUE), function(rings) lapply(rings, parse_ring)))
cells$rings <- NULL
cell_bbox <- lapply(cell_polys, function(p) {
  m <- do.call(rbind, unlist(p, recursive = FALSE))
  c(min(m[, 1]), min(m[, 2]), max(m[, 1]), max(m[, 2]))
})
bbox <- do.call(rbind, cell_bbox)
bbox <- c(min(bbox[, 1]), min(bbox[, 2]), max(bbox[, 3]), max(bbox[, 4]))

# area_km2 is the cell's area net of the finer cells nested in it
cells$density <- cells$value_added / cells$area_km2
cells$hotel_share <- 100 * cells$hotels / cells$establishments

brl <- function(x) paste0("R$ ", formatC(round(x), format = "d", big.mark = ","))
brl_short <- function(x) ifelse(x >= 1e6, sprintf("R$ %.2f million", x / 1e6),
                         ifelse(x >= 1e3, sprintf("R$ %s thousand", formatC(x / 1e3, format = "f", digits = 0, big.mark = ",")),
                                sprintf("R$ %.0f", x)))

colour_vars <- c(
  "Value added per km²" = "density",
  "Value added" = "value_added",
  "Jobs" = "jobs",
  "Hotels and apart-hotels (% of establishments)" = "hotel_share"
)

article_doi <- "https://doi.org/10.21680/2357-8211.2020v8n2ID19375"

# UI --------------------------------------------------------------------------

ui <- page_navbar(
  id = "nav",
  title = "Tourist accommodation in Belo Horizonte, 2015",
  theme = bs_theme(version = 5, preset = "flatly", base_font = font_collection("system-ui", "sans-serif")),
  fillable = TRUE,

  nav_panel(
    "Map",
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        p(class = "small text-muted mb-1",
          "Zoom and pan the map: the figures below add up the cells in view, ",
          "a flexible geography defined by you."),
        value_box(title = "Value added in view", value = textOutput("vb_va"),
                  textOutput("vb_share"), theme = "primary", fill = FALSE),
        div(class = "small", textOutput("vb_counts")),
        selectInput("colour", "Colour cells by", colour_vars),
        plotOutput("bars", height = "220px")
      ),
      card(full_screen = TRUE, card_body(padding = 0, leafletOutput("map", height = "100%")))
    )
  ),

  nav_panel(
    "Data",
    card(
      card_header("Published grid cells — select a row to see it on the map"),
      DTOutput("table")
    )
  ),

  nav_panel(
    "About",
    card(
      card_body(
        class = "mx-auto", style = "max-width: 760px;",
        h4("What this is"),
        p("An interactive companion to the article ",
          tags$em("Production of economic statistics for tourist accommodation services of Belo Horizonte under the flexible geography approach"),
          " (Revista de Turismo Contemporâneo, 8(2), 198–221, 2020). ",
          "Statistics are usually released for fixed administrative units. Under the flexible-geography approach the user ",
          "draws the region: here, whatever is in view on the map."),
        h4("Data and method"),
        p("The value added of tourist accommodation services for Belo Horizonte in 2015 was derived from IBGE's national ",
          "and regional accounts and allocated to the accommodation establishments recorded in RAIS 2015 in proportion ",
          "to their employment. The establishments were then geolocated from their addresses."),
        h4("Confidentiality"),
        p("Establishment-level data cannot be published. The map shows an adaptive grid instead: squares of 500 m ",
          "that merge into 1, 2, 4 or 8 km squares where establishments are sparse. A cell is published only when it ",
          "holds at least 3 establishments and no single establishment accounts for more than 85% of its value added. ",
          "Cells do not overlap, so no establishment can be recovered by subtracting one cell from another."),
        p(textOutput("unmapped_note", inline = TRUE)),
        h4("Citation"),
        p("Gonçalves, C. C. S. (2020). Production of economic statistics for tourist accommodation services of ",
          "Belo Horizonte under the flexible geography approach. ", tags$em("Revista de Turismo Contemporâneo"),
          ", 8(2), 198–221. ", tags$a(href = article_doi, target = "_blank", article_doi)),
        p(class = "small text-muted", "Base map © OpenStreetMap contributors.")
      )
    )
  )
)

# Server ----------------------------------------------------------------------

server <- function(input, output, session) {

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 9)) |>
      addTiles() |>
      fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) |>
      addScaleBar(position = "bottomleft")
  })

  in_view <- reactive({
    b <- input$map_bounds
    if (is.null(b)) return(cells)
    cells[cells$lat >= b$south & cells$lat <= b$north &
          cells$lng >= b$west & cells$lng <= b$east, ]
  })

  observe({
    var <- input$colour
    values <- cells[[var]]
    # Values span orders of magnitude (8 km cells on the outskirts vs 500 m
    # cells downtown), so the classes are log-spaced; the share is linear.
    if (var == "hotel_share") {
      breaks <- c(0, 20, 40, 60, 80, 100)
    } else {
      lo <- min(values[values > 0])
      breaks <- signif(10^seq(log10(lo), log10(max(values)), length.out = 7), 2)
      breaks[1] <- 0
      breaks[length(breaks)] <- max(values) * 1.001
    }
    pal <- colorBin("YlOrRd", values, bins = breaks, right = TRUE)
    label <- names(colour_vars)[colour_vars == var]
    fmt <- switch(var,
      density = function(x) brl_short(x),
      value_added = function(x) brl_short(x),
      jobs = function(x) formatC(x, format = "d", big.mark = ","),
      hotel_share = function(x) sprintf("%.0f%%", x))

    popups <- sprintf(
      "<b>Cell %s</b> (%s m grid)<br>Value added: %s<br>Jobs: %s<br>Establishments: %d (%d hotels or apart-hotels)",
      cells$cell, formatC(cells$side_m, format = "d", big.mark = ","),
      brl(cells$value_added), formatC(cells$jobs, format = "d", big.mark = ","),
      cells$establishments, cells$hotels)
    features <- lapply(seq_len(nrow(cells)), function(i) list(
      type = "Feature",
      properties = list(popup = popups[i], style = list(fillColor = pal(values[i]))),
      geometry = list(type = "MultiPolygon", coordinates = cell_polys[[i]])
    ))

    leafletProxy("map") |>
      clearGeoJSON() |>
      clearControls() |>
      addGeoJSON(list(type = "FeatureCollection", features = features), layerId = "cells",
                 fillOpacity = 0.6, color = "#7a2e0e", weight = 0.6, opacity = 0.7) |>
      addLegend("bottomright", pal = pal, values = values, title = label,
                labFormat = function(type, cuts, p) {
                  n <- length(cuts)
                  paste(fmt(cuts[-n]), "–", fmt(cuts[-1]))
                }, opacity = 0.8)
  })

  output$vb_va <- renderText(brl_short(sum(in_view()$value_added)))
  output$vb_share <- renderText(sprintf("%.1f%% of the city total (%s)",
                                        100 * sum(in_view()$value_added) / city_total, brl_short(city_total)))
  output$vb_counts <- renderText(sprintf("%s establishments · %s jobs · %d of %d cells",
    formatC(sum(in_view()$establishments), format = "d", big.mark = ","),
    formatC(sum(in_view()$jobs), format = "d", big.mark = ","), nrow(in_view()), nrow(cells)))

  output$bars <- renderPlot({
    v <- in_view()
    if (nrow(v) == 0) return(NULL)
    v <- head(v[order(-v$value_added), ], 8)
    op <- par(mar = c(3, 4.2, 2, 1), cex = 0.85)
    on.exit(par(op))
    barplot(rev(v$value_added) / 1e6, names.arg = rev(v$cell), horiz = TRUE, las = 1,
            col = "#2c3e50", border = NA, xlab = "", main = "Largest cells in view (R$ million)",
            cex.main = 0.95, font.main = 1)
  }, bg = "transparent")

  output$table <- renderDT({
    datatable(
      data.frame(
        Cell = cells$cell,
        `Grid (m)` = cells$side_m,
        Establishments = cells$establishments,
        `Hotels/apart-hotels` = cells$hotels,
        `Value added (R$)` = cells$value_added,
        Jobs = cells$jobs,
        `Share of BH total (%)` = round(100 * cells$value_added / city_total, 2),
        check.names = FALSE
      ),
      rownames = FALSE, selection = "single",
      options = list(pageLength = 15, dom = "ftip")
    ) |> formatCurrency("Value added (R$)", currency = "", digits = 0)
  })

  # Zoom only once the Map tab is visible: fitBounds on a hidden map computes
  # the zoom for a zero-sized container.
  pending_zoom <- reactiveVal(NULL)

  observeEvent(input$table_rows_selected, {
    pending_zoom(cell_bbox[[input$table_rows_selected]])
    nav_select("nav", "Map")
  })

  observeEvent(input$nav, {
    b <- pending_zoom()
    if (input$nav == "Map" && !is.null(b)) {
      leafletProxy("map") |> fitBounds(b[1], b[2], b[3], b[4])
      pending_zoom(NULL)
    }
  })

  output$unmapped_note <- renderText(sprintf(
    "%d of the %d establishments (%s, %.1f%% of the value added) are not on the map: they could not be placed in any cell that passes both rules, or their address was geolocated outside the municipality. They are included in the Belo Horizonte total.",
    unmapped$establishments, city_estab, brl(unmapped$value_added), 100 * unmapped$value_added / city_total))
}

shinyApp(ui, server)

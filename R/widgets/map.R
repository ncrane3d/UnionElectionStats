stateBoundaries <- readLines("./inst/app/www/states.json") %>%
    paste(collapse = "\n")
countyBoundaries <- readLines("./inst/app/www/counties.json") %>%
    paste(collapse = "\n")

map <- function() {
    renderLeaflet({
        leaflet() |>
            addTiles() |>
            addGeoJSON(
                geojson = stateBoundaries,
                weight = 1,
                color = "rgb(205,196,203)",
                fill = FALSE,
            ) |>
            addGeoJSON(
                geojson = countyBoundaries,
                weight = 1,
                color = "rgb(205,196,203)",
                fill = FALSE,
                group = "counties"
            ) |>
            groupOptions("counties", zoomLevels = 6:20) |>
            setView(0.249818018854, 0.57650864633, zoom = 3)
    })
}

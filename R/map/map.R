stateBoundaries <- readLines("./inst/app/www/states.json") %>%
    paste(collapse = "\n")
countyBoundaries <- readLines("./inst/app/www/counties.json") %>%
    paste(collapse = "\n")

map <- function(input, output) {
    return(renderLeaflet({
        leaflet(options = leafletOptions(minZoom = 3)) |>
            addTiles() |>
            addGeoJSON(
                geojson = stateBoundaries,
                weight = 1,
                color = "rgb(143,137,141)",
            ) |>
            addGeoJSON(
                geojson = countyBoundaries,
                weight = 1,
                color = "rgb(143,137,141)",
                fill = FALSE,
                group = "counties"
            ) |>
            groupOptions("counties", zoomLevels = 6:20) |>
            setMaxBounds(
                lat1 = 5.499550,
                lng1 = -167.276413,
                lat2 = 83.162102,
                lng2 = -52.233040
            ) |>
            setView(
                lat = 39.82,
                lng = -98.58,
                zoom = 3,
            )
    }))
}

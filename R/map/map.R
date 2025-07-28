stateBoundaries <- readLines("./inst/app/www/states.json") %>%
    paste(collapse = "\n")
countyBoundaries <- readLines("./inst/app/www/counties.json") %>%
    paste(collapse = "\n")


# pal <- colorQuantile(c("red", "blue"), query$state_count, n = 7)

linecolor <- "rgb(143,137,141)"

map <- function(input, output, pool) {
    highlightFunction <- function(fips) {
        if (fips == input$state) {
            return("white")
        } else {
            return(linecolor)
        }
    }
    return(renderLeaflet({
        leaflet(options = leafletOptions(minZoom = 3)) |>
            addTiles() |>
            #State border layer
            addGeoJSON(
                geojson = stateBoundaries,
                weight = 1,
                color = "rgb(143,137,141)",
            ) |>
            #County border layer
            addGeoJSON(
                geojson = countyBoundaries,
                weight = 1,
                color = "rgb(143,137,141)",
                fill = FALSE,
                group = "counties"
            ) |>
            #Conditional rendering for county layer
            groupOptions("counties", zoomLevels = 6:20) |>
            #Map panning bounds
            setMaxBounds(
                lat1 = 5.499550,
                lng1 = -167.276413,
                lat2 = 83.162102,
                lng2 = -52.233040
            ) |>
            #Initial map location and zoom
            setView(
                lat = 39.82,
                lng = -98.58,
                zoom = 3,
            )
    }))
}

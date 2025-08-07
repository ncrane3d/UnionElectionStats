stateBoundaries <- sf::read_sf("./inst/app/www/states.json")
countyBoundaries <- sf::read_sf("./inst/app/www/counties.json")

source('./R/map/map_data_initialization.R', local = TRUE)

getPalette <- function(column) {
    colorNumeric(c("red", "blue"), column)
}
map <- function(input, output, pool, current_data_slice) {
    boundaries <- getBoundaries(pool)
    statePalette <- getPalette(boundaries[1]$state_count)
    countyPalette <- getPalette(boundaries[2]$normalized_vote)
    mapHighlight <- highlightOptions(
        color = "white",
        weight = 2,
        opacity = 1,
        bringToFront = TRUE
    )
    return(renderLeaflet({
        leaflet(options = leafletOptions(minZoom = 3)) |>
            addTiles() |>
            #State border layer
            addPolygons(
                data = boundaries[[1]],
                weight = 1,
                color = ~ statePalette(state_count),
                group = "states",
                highlightOptions = mapHighlight
            ) |>
            #County border layer
            addPolygons(
                data = boundaries[[2]],
                weight = 1,
                color = ~ countyPalette(normalized_vote),
                group = "counties",
            ) |>
            #Individual election markers
            addCircleMarkers(
                data = current_data_slice(),
                color = "red",
                stroke = FALSE,
                fillOpacity = 0.75,
                group = "counties",
                clusterOptions = markerClusterOptions(
                    #Expands overlapping markers out into a starburst on max zoom
                    spiderfyOnMaxZoom = TRUE,
                    showCoverageOnHover = FALSE
                ),
                #Popup on click of individual elections that displays basic info
                label = ~ sprintf(
                    "Employer: %s<br/>Year closed: %s<br/>Pro-union vote share: %s",
                    employer,
                    yrclosed,
                    round(
                        ((votes_for /
                            (votes_for + votes_against)) *
                            100),
                        digits = 2
                    )
                ) %>%
                    lapply(htmltools::HTML)
            ) |>
            #Zoom based conditional rendering for layers
            groupOptions("counties", zoomLevels = 5:20) |>
            groupOptions("states", zoomLevels = 0:4) |>
            #Map panning bounds
            setMaxBounds(
                lat1 = 72.89817,
                lng1 = -179.912096,
                lat2 = 1,
                lng2 = -54.892994
            ) |>
            #Initial map location and zoom
            setView(
                lat = 39.82,
                lng = -98.58,
                zoom = 3,
            ) %>%
            #On render, applies click event to visible polygons (using leaflet javascript library)
            onRender(
                'function(el, x){
                var map = this;
                map.eachLayer(function(layer){
                    if(layer instanceof L.Polygon){
                        layer.on("click", function(e){
                            map.fitBounds(layer.getBounds());
                        })
                    }
            });
            }'
            )
    }))
}

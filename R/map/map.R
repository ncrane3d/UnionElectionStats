stateBoundaries <- sf::read_sf("./inst/app/www/states.json")
countyBoundaries <- sf::read_sf("./inst/app/www/counties.json")

source('./R/map/map_data_initialization.R', local = TRUE)

getPalette <- function(column) {
    colorNumeric(c("red", "blue"), column)
}
map <- function(input, output, pool, current_data_slice, current_query) {
    return(renderLeaflet({
        leaflet(options = leafletOptions(minZoom = 3)) |>
            addTiles() |>
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

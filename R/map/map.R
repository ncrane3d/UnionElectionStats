stateBoundaries <- sf::read_sf("./inst/app/www/states.json")
countyBoundaries <- sf::read_sf("./inst/app/www/counties.json")

source('./R/map/map_data_initialization.R', local = TRUE)

getPalette <- function(column) {
    colorNumeric(c("red", "blue"), column)
}

map <- function(input, output, pool, current_data_slice, current_query) {
    territoryOpacity <- 0.5
    boundaries <- getBoundaries(pool, current_query, current_data_slice)
    mapHighlight <- highlightOptions(
        color = "white",
        weight = 2,
        opacity = 1,
        bringToFront = FALSE
    )
        #Error handling for when there are no points to render on map
    getCircleMarkerData <- function(){
        if (nrow(current_data_slice()) > 1){
            return(current_data_slice())
        } else {
            #Returns dataframe containing 1 point in Bangladesh, out of constrained view of user
            return(data.frame(latitude=c(23.6850), longitude=c(90.3563), yrclosed=c(1), employer=c("none"), votes_for= c(1), votes_against=c(1)))
        }
    }
    #Zoom on click of territory shape
    observeEvent( input$map_shape_click, {
        click <- input$map_shape_click
        if(!is.null(click)){
            if(nchar(click$id) < 3) { #Clicked shape is a state
                #Get index of row of clicked shape in geojson
                index <- which(boundaries()[[1]]$state == click$id)
                #Get geometry of clicked shape
                shape <- boundaries()[[1]]$geometry[index]
            } else { #Clicked shape is a county
                index <- which(boundaries()[[2]]$FIPS == click$id)
                shape <- boundaries()[[2]]()$geometry[index]
            }
            #Get bounds of clicked shape
            bounds = st_bbox(shape)
            #Fit bounds of map to clicked shape
            leafletProxy("map") %>% fitBounds(lat2 = as.numeric(bounds$ymin), lng2= as.numeric(bounds$xmin), lat1=as.numeric(bounds$ymax), lng1=as.numeric(bounds$xmax))
        }
    })
  observe({
    statePalette <- getPalette(boundaries()[1]$state_count)
    countyPalette <- getPalette(boundaries()[2]$normalized_vote)
    req(boundaries)
    leafletProxy("map") %>%
    clearMarkerClusters() %>%
    addPolygons(
                data = boundaries()[[1]],
                weight = 1,
                fillOpacity = territoryOpacity,
                color = ~ statePalette(state_count),
                group = "states",
                layerId=~boundaries()[[1]]$state,
                highlightOptions = mapHighlight,
                options= leafletOptions(pane="shapes"),
            ) |>
            #County border layer
            addPolygons(
                data = boundaries()[[2]],
                weight = 1,
                fillOpacity = territoryOpacity,
                color = ~ countyPalette(normalized_vote),
                group = "counties",
                layerId=~boundaries()[[2]]$FIPS,
                highlightOptions = mapHighlight,
                options= leafletOptions(pane="shapes"),
            ) |>
            #Individual election markers
            addCircleMarkers(
                data = getCircleMarkerData(),
                color = "white",
                stroke = FALSE,
                fillOpacity = 0.75,
                group = "counties",
                clusterOptions = markerClusterOptions(
                    #Expands overlapping markers out into a starburst on max zoom
                    spiderfyOnMaxZoom = TRUE,
                    showCoverageOnHover = FALSE
                ),
                options= leafletOptions(pane="markers"),
                #Popup on click of individual elections that displays basic info
                label = ~ sprintf(
                    "Employer: %s<br/>Year closed: %s<br/>Pro-union vote share: %s",
                    employer,
                    year_closed,
                    round(
                        ((votes_for /
                            (votes_total)) *
                            100),
                        digits = 2
                    )
                ) %>%
                    lapply(htmltools::HTML)
            )
  })
  #Basemap
    return(renderLeaflet({
        leaflet(options = leafletOptions(minZoom = 3)) |>
            addTiles("https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png") |>
            addMapPane(name="shapes", zIndex=410) %>%
            addMapPane(name="labels", zIndex=415) %>%
            addMapPane(name="markers", zIndex=420) %>%
            addTiles("https://{s}.basemaps.cartocdn.com/light_only_labels/{z}/{x}/{y}.png", options= leafletOptions(pane = "labels")) |>
            #Zoom based conditional rendering for layers
            groupOptions("counties", zoomLevels = 5:20) |>
            groupOptions("states", zoomLevels = 0:4) |>
            #Map panning bounds
            setMaxBounds(
                lat1 = 72.89817,
                lng1 = -179.912096,
                lat2 = 1,
                lng2 = -54.892994
            )
    }))
}
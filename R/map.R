mapModule <- function(id, current_data_slice) {  
  moduleServer(
    id,
    function(input, output, session) { 
        output$map <- renderLeaflet({
            leaflet(options = leafletOptions(minZoom = 3)) |>
            addTiles("https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png") |>
            addMapPane(name="shapes", zIndex=410) %>%
            addMapPane(name="labels", zIndex=415) %>%
            addMapPane(name="markers", zIndex=420) %>%
            addTiles("https://{s}.basemaps.cartocdn.com/light_only_labels/{z}/{x}/{y}.png", options= leafletOptions(pane = "labels")) |>
            #Zoom based conditional rendering for layers
            groupOptions("points", zoomLevels = 7:20) |>
            groupOptions("counties", zoomLevels = 5:20) |>
            groupOptions("states", zoomLevels = 0:4) |>
            #Map panning bounds
            setMaxBounds(
                lat1 = 72.89817,
                lng1 = -179.912096,
                lat2 = 1,
                lng2 = -54.892994
            )
        })

        boundaryCalculator <- function(current_data_slice) {
            stateBoundaries <- sf::read_sf("./inst/app/www/states.json")
            countyBoundaries <- sf::read_sf("./inst/app/www/counties.json")
            currentdata <- current_data_slice()
            state_fips <- with(currentdata, substr(FIPS, 1, nchar(FIPS) - 3))
            state_freq <- data.frame(table(state_fips)) %>% rename(state_count = Freq)
            county_freq <- data.frame(table(currentdata$FIPS)) %>% rename( FIPS = Var1, county_count = Freq)
            countyBoundaries <- full_join(
                countyBoundaries,
                county_freq,
                by = c("FIPS" = "FIPS")
            )
            countyBoundaries <- full_join(
                countyBoundaries,
                state_freq,
                by = c("STATE" = "state_fips")
            )
            stateBoundaries <- full_join(
                stateBoundaries,
                state_freq,
                by = c("state" = "state_fips")
            )
            countyBoundaries$normalized_vote <- with(
                countyBoundaries,
                (county_count / state_count)
            )
            return(list(stateBoundaries, countyBoundaries))
        }

        getBoundaries <- function(pool, current_query, current_data_slice) {
            return(reactive({boundaryCalculator(current_data_slice)}))
        }

        getPalette <- function(column) {
            #colorNumeric(viridis(10), column, reverse = TRUE)
            colorQuantile(viridis(10), domain = column, n = 10, reverse = TRUE)
        }

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
                #unsure if this has performance implications
                coord_filtered_slice <- current_data_slice()[!is.na(current_data_slice()$longitude) & !is.na(current_data_slice()$latitude), ]
                return(st_as_sf(coord_filtered_slice, coords = c("longitude", "latitude"), crs = 4326))
                #return(st_as_sf(current_data_slice(), coords = c("jittered_lon", "jittered_lat"), crs = 4326))
            } else {
                #Returns dataframe containing 1 point in Bangladesh, out of constrained view of user
                return(st_as_sf((data.frame(latitude=c(23.6850), longitude=c(90.3563), yrclosed=c(1), employer=c("none"), votes_for= c(1), votes_against=c(1))), coords = c("lon", "lat"), crs = 4326))
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
                    #Get bounds of clicked shape
                    bounds = st_bbox(shape)
                    #Fit bounds of map to clicked shape
                    leafletProxy("map") %>% fitBounds(lat2 = as.numeric(bounds$ymin), lng2= as.numeric(bounds$xmin), lat1=as.numeric(bounds$ymax), lng1=as.numeric(bounds$xmax))
                } 
            }
        })


        #State Layer
        observe({
            req(boundaries)
            statePalette <- getPalette(boundaries()[1]$state_count)
            leafletProxy("map") %>%
            addPolygons(
                data = boundaries()[[1]],
                weight = 1,
                fillOpacity = territoryOpacity,
                color = ~ statePalette(state_count),
                group = "states",
                layerId=~boundaries()[[1]]$state,
                highlightOptions = mapHighlight,
                options= leafletOptions(pane="shapes"),
            )
        })

        #County Layer
        observe({
            req(boundaries)
            countyPalette <- getPalette(boundaries()[2]$county_count)
            leafletProxy("map") %>%
            addPolygons(
                data = boundaries()[[2]],
                weight = 1,
                fillOpacity = .75,
                color = ~ countyPalette(normalized_vote),
                group = "counties",
                layerId=~boundaries()[[2]]$FIPS,
                options = pathOptions(pane = "shapes"),
                popup = ~sprintf(
                    "Name: %s (%s)",
                    NAME,
                    FIPS
                ),
                highlightOptions = mapHighlight
            )
        })

        #Election Points
        observe({
            #Initial overflowing fix attempt
            #req(input$map_zoom >= 7)
            leafletProxy("map") %>%
            removeGlPoints("electionPopup") %>%
            addGlPoints(
                data = getCircleMarkerData(),
                group = "points",
                pane = "markers",
                layerId = "electionPopup",
                fillColor = "#440154",
                #fillColor = ~ifelse(jittered, "red", "blue"),
                radius = 7,
                opacity =.65,
                popup = ~sprintf(
                    "Case Number: %s<br/>Employer: %s<br/>Year closed: %s<br/>Pro-union vote share: %s<br/>County: %s (%s)",
                    case_number,
                    employer,
                    year_closed,
                    round((votes_for / votes_total) * 100, 2),
                    county,
                    FIPS
                )
            )
        })

        observe({
            req(input$map_zoom)
            if (input$map_zoom < 5) {
                breaks <- quantile(boundaries()[[1]]$state_count, probs = seq(0, 1, .1), na.rm = TRUE)
                labels <- character(10)  
                for (i in 1:10) {
                    labels[i] <- paste0(breaks[i], " – ", breaks[i + 1])
                }

                leafletProxy("map") %>%
                clearControls() %>%
                addLegend("bottomleft", 
                    colors = rev(viridis(10)), 
                    labels = labels,
                    title = "Election Frequency",
                    opacity = 1
                )   
            } else {
                breaks <- quantile(boundaries()[[2]]$county_count, probs = seq(0, 1, .1), na.rm = TRUE)
                labels <- character(10) 
                for (i in 1:10) {
                    labels[i] <- paste0(breaks[i], " – ", breaks[i + 1])
                }

                leafletProxy("map") %>%
                clearControls() %>%
                addLegend("bottomleft", 
                    colors = rev(viridis(10)), 
                    labels = labels,
                    title = "Election Frequency",
                    opacity = 1
                )
            }
        })
    }
  )
}  

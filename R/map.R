mapModule <- function(id, current_data_slice, slice_ignoring_regional_filtering) {  
  moduleServer(
    id,
    function(input, output, session) { 
        #Generates the decile palette the map uses
        getPalette <- function(column, n_bins = 10) {
            probs <- seq(0, 1, length.out = n_bins + 1)
            breaks <- quantile(column, probs = probs, na.rm = TRUE)

            breaks <- unique(breaks)
            if (length(breaks) < 2) {
                #Case for no breaks
                breaks <- c(min(column, na.rm = TRUE), max(column, na.rm = TRUE))
            }
            colorBin(palette = viridis(length(breaks) - 1),
                    domain = column,
                    bins = breaks,
                    reverse = TRUE)
        }
    

        output$map <- renderLeaflet({
            leaflet(options = leafletOptions(minZoom = 3)) |>
            addTiles("https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png") |>
            addMapPane(name="shapes", zIndex=410) %>%
            addMapPane(name="labels", zIndex=415) %>%
            addMapPane(name="markers", zIndex=420) %>%
            addTiles("https://{s}.basemaps.cartocdn.com/light_only_labels/{z}/{x}/{y}.png", 
                    options= leafletOptions(pane = "labels")) |>
            #Instead of readding and deleting shapes every filter, they are added as a transparent layer at the beginninng
            addPolygons(
                data = state_shapes,
                group = "states",
                layerId = ~state,
                weight = 1,
                color = "transparent", 
                fillColor = "transparent",
                options = pathOptions(pane="shapes")
            ) %>%
                addPolygons(
                data = county_shapes,
                group = "counties",
                layerId = ~FIPS,
                weight = 1,
                color = "transparent",
                fillColor = "transparent",
                options = pathOptions(pane="shapes")
            ) %>%
            groupOptions("points", zoomLevels = 7:20) |>
            groupOptions("counties", zoomLevels = 5:20) |>
            groupOptions("states", zoomLevels = 0:4) |>
            setMaxBounds(
                    lat1 = 72.89817,
                    lng1 = -179.912096,
                    lat2 = 1,
                    lng2 = -54.892994
                )
        })
    
        #Adds points to the map, this is an optional layer currently due to computational intensity
        observe({
            leafletProxy("map") %>% clearGroup("points")
            data_points <- current_data_slice() #Need to call reactive to retrieve values

            if (nrow(data_points) > 0) {
                #Filter out bad coordinates
                valid_pts <- data_points[!is.na(data_points$longitude) & !is.na(data_points$latitude), ]
                
                if (nrow(valid_pts) > 0) {
                pts_sf <- st_as_sf(valid_pts, coords = c("longitude", "latitude"), crs = 4326)
                leafletProxy("map") %>%
                    addGlPoints(
                    data = pts_sf,
                    group = "points",
                    pane = "markers",
                    layerId = pts_sf$case_number, 
                    fillColor = "#440154",
                    radius = 7,
                    opacity = 0.65,
                    popup = ~sprintf(
                        "Case Number: %s<br/>Employer: %s<br/>Year closed: %s<br/>Pro-union vote share: %s<br/>County: %s (%s)",
                        case_number, employer, year_closed, 
                        round((votes_for / votes_total) * 100, 2), 
                        county, FIPS
                    )
                    )
                }
            }
        })
        
        #This function makes users zoom upon clicking a state
        #It's meant to help users who may not understand there's a county layer below
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

        #This uses our palette to color the map and legend
        observe({
            req(current_data_slice(), slice_ignoring_regional_filtering())
            
            #Data Aggregation
            curr_data <- as.data.table(current_data_slice())
            incl_data <- as.data.table(slice_ignoring_regional_filtering())
            
            # Calculate counts for coloring (current view)
            curr_state_counts <- curr_data[, .(count = .N), by = state_fips]
            curr_county_counts <- curr_data[, .(count = .N), by = FIPS]
            
            # Calculate counts for Legend/Palette (inclusive view - determines the scale)
            incl_state_counts <- incl_data[, .(count = .N), by = state_fips]
            incl_county_counts <- incl_data[, .(count = .N), by = FIPS]
            
            #Create Palettes
            statePalette <- getPalette(incl_state_counts$count, 10)
            countyPalette <- getPalette(incl_county_counts$count, 10)
            
            # Map the counts to the shapefile rows
            m_states <- match(state_shapes$state, curr_state_counts$state_fips)
            state_vals <- curr_state_counts$count[m_states]
            state_colors <- statePalette(state_vals)
            
            m_counties <- match(county_shapes$FIPS, curr_county_counts$FIPS)
            county_vals <- curr_county_counts$count[m_counties]
            county_colors <- countyPalette(county_vals)
            
            proxy <- leafletProxy("map")
            proxy %>%
                addPolygons(
                data = state_shapes,
                group = "states",
                layerId = ~state,
                weight = 1,
                fillOpacity = 0.5,
                color = state_colors, 
                highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = FALSE),
                options = pathOptions(pane="shapes")
                )
            
            county_popups <- sprintf(
                ifelse(as.numeric(county_shapes$STATE) < 10, "Name: %s (0%s)", "Name: %s (%s)"),
                county_shapes$NAME, county_shapes$FIPS
            )
            
            proxy %>%
                addPolygons(
                data = county_shapes,
                group = "counties",
                layerId = ~FIPS,
                weight = 1,
                fillOpacity = 0.75,
                color = county_colors,
                popup = county_popups,
                highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = FALSE),
                options = pathOptions(pane="shapes")
                )
            
            zoom <- input$map_zoom
            if (is.null(zoom)) zoom <- 3
            
            proxy %>% clearControls()
            
            if (zoom < 5) {
                proxy %>% addLegend("bottomleft", pal = statePalette, values = incl_state_counts$count, 
                                    title = "Election Frequency", opacity = 1)
            } else {
                proxy %>% addLegend("bottomleft", pal = countyPalette, values = incl_county_counts$count, 
                                    title = "Election Frequency", opacity = 1)
            }
        })
    }
  )
}
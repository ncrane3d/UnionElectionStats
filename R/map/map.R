source('./R/map/map_data_initialization.R', local = TRUE)

getPalette <- function(column) {
    colorNumeric(viridis(10), column, reverse = TRUE)
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
        return(st_as_sf(current_data_slice(), coords = c("longitude", "latitude"), crs = 4326))
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
        } #else { #Clicked shape is a county
        #     index <- which(boundaries()[[2]]$FIPS == click$id)
        #     shape <- boundaries()[[2]]$geometry[index]
        # }
    }
})

observeEvent( input$map_glify_mouseover, {

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
    countyPalette <- getPalette(boundaries()[2]$normalized_vote)
    leafletProxy("map") %>%
    addPolygons(
        data = boundaries()[[2]],
        weight = 1,
        fillOpacity = .75,
        #fillColor = "white",
        color = ~ countyPalette(normalized_vote),
        group = "counties",
        layerId=~boundaries()[[2]]$FIPS,
        options= leafletOptions(pane="shapes"),
        #popup = ~paste("Name:", NAME, " (", FIPS, ")"), 
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
    leafletProxy("map") %>%
    #Possible layer overflow fixes
    #groupOptions("counties", zoomLevels = 5:20) %>%
    #req(input$map_zoom >= 5)
    removeGlPoints("electionPopup") %>%
    addGlPoints(
        data = getCircleMarkerData(),
        group = "counties",
        pane = "markers",
        layerId = "electionPopup",
        #color = "#440154",
        fillColor = "#440154",
        radius = 5,
        opacity =.5,
        popup = ~sprintf(
            "Case Number: %s<br/>Employer: %s<br/>Year closed: %s<br/>Pro-union vote share: %s",
            case_number,
            employer,
            year_closed,
            round((votes_for / votes_total) * 100, 2)
        )
    )
})

observe({
    leafletProxy("map") %>%
    addLegend("bottomleft", 
    pal = viridis(10), 
    values = boundaries()[1]$state_count,
    title = "Election Density by Deciles",
    #labFormat = labelFormat(prefix = "$"),
    opacity = 1
  )
})
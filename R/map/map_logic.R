map_logic <- function(input, output) {
    observeEvent(input$leafmap_geojson_click, {
        evt <- input$leafmap_geojson_click
        cat("click registered\n")
        if (is.null(evt)) {
            return()
        } else {
            cat("click registered for real\n")
            leafletProxy("map") %>%
                setView(lng = evt$lng, lat = evt$lat, zoom = 5)
        }
    })
}

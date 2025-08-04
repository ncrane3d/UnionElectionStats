map_logic <- function(input, output, pool) {
    observeEvent(input$map_shape_click, {
        evt <- input$map_shape_click
        cat("click registered\n")
        if (is.null(evt)) {
            return()
        } else {
            cat("click registered for real\n")
            leafletProxy("map") %>%
                setView(10, 10, 10)
        }
    })
}

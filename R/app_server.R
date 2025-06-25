#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  output$map <- renderLeaflet({leaflet() |>
    addTiles() |>
    setView(0.12490888888, 0.24981777777, zoom = 3)})
}

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import DBI
#' @import RPostgres
#' @noRd
#'

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("UE_IP"),
  dbname = "unionelectiondb",
  user = "ueuser",
  password = Sys.getenv("UE_DB_PASS"),
  port = 21701
)

app_server <- function(input, output, session) {
  # Your application server logic
  stateBoundaries <- readLines("./inst/app/www/states.json") %>%
    paste(collapse = "\n")
  countyBoundaries <- readLines("./inst/app/www/counties.json") %>%
    paste(collapse = "\n")
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      addGeoJSON(
        geojson = stateBoundaries
      ) |>
      addGeoJSON(
        geojson = countyBoundaries,
        group = "counties"
      ) |>
      groupOptions("counties", zoomLevels = 6:20) |>
      setView(0.249818018854, 0.57650864633, zoom = 3)
  })
  #About Me Images TODO: Replace with actual images
  output$pfp_left <- renderImage(
    {
      list(
        src = "./resources/images/pfp_empty.png",
        height = "auto",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )

  output$pfp_middle <- renderImage(
    {
      list(
        src = "./resources/images/pfp_empty.png",
        height = "auto",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )

  output$pfp_right <- renderImage(
    {
      list(
        src = "./resources/images/pfp_empty.png",
        height = "auto",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )
  #Contact Us Background
  output$contact_bg <- renderImage(
    {
      list(
        src = "./resources/images/rally.jpg",
        height = "auto",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )

  output$pfp_developer_left <- renderImage(
    {
      list(
        src = "./resources/images/pfp_empty.png",
        height = "auto",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )

  output$pfp_developer_right <- renderImage(
    {
      list(
        src = "./resources/images/pfp_empty.png",
        height = "auto",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )
}

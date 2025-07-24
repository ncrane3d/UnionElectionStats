#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import DBI
#' @import RPostgres
#' @noRd

source('./R/widgets/map.R', local = TRUE)

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
  cat("serving")
  output$leafmap <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3)) |>
      addTiles() |>
      addGeoJSON(
        geojson = stateBoundaries,
        weight = 1,
        color = "rgb(143,137,141)",
        options = pathOptions(interactive = TRUE)
      ) |>
      addGeoJSON(
        geojson = countyBoundaries,
        weight = 1,
        color = "rgb(143,137,141)",
        fill = FALSE,
        group = "counties"
      ) |>
      groupOptions("counties", zoomLevels = 6:20) |>
      setMaxBounds(
        lat1 = 5.499550,
        lng1 = -167.276413,
        lat2 = 83.162102,
        lng2 = -52.233040
      ) |>
      setView(
        lat = 39.82,
        lng = -98.58,
        zoom = 3,
      )
  })

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

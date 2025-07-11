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
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(0.249818018854, 0.57650864633, zoom = 3)
  })
  #About Me Images TODO: Replace with actual images
  output$pfp_left <- renderImage(
    {
      list(
        src = "./inst/app/www/pfp_empty.png",
        height = "auto",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )

  output$pfp_middle <- renderImage(
    {
      list(
        src = "./inst/app/www/pfp_empty.png",
        height = "auto",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )

  output$pfp_right <- renderImage(
    {
      list(
        src = "./inst/app/www/pfp_empty.png",
        height = "auto",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )

  output$pfp_developer_left <- renderImage(
    {
      list(
        src = "./inst/app/www/pfp_empty.png",
        height = "auto",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )

  output$pfp_developer_right <- renderImage(
    {
      list(
        src = "./inst/app/www/pfp_empty.png",
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
        src = "./inst/app/www/rally1.jpg",
        height = "auto",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )

  output$gompers <- renderImage(
    {
      list(
        src = "./inst/app/www/gompers3.png",
        height = "auto",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )
  output$contact_us_text <- renderImage(
    {
      list(
        src = "./inst/app/www/contactus.svg",
        height = "auto",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )
}

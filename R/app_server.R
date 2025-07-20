#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import DBI
#' @import RPostgres
#' @import pool
#' @import ggplot2
#' @import ggthemes
#' @noRd
#'



app_server <- function(input, output, session) {
  # Your application server logic
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(0.249818018854, 0.57650864633, zoom = 3)
  })

  #Move the pool over?
  pool <- dbPool(
  Postgres(),
  host = Sys.getenv("UE_IP"),
  dbname = "unionelectiondb",
  user = "ueuser",
  password = Sys.getenv("UE_DB_PASS"),
  port = 21701
)

  #My first attempts to make the graph reactive, not working so far
  query <- reactive({
    sql <- "
    SELECT *
    FROM unionelections 
    WHERE yrclosed >= ?lowerBound AND yrclosed <= ?upperBound;
    "
    query <- sqlInterpolate(pool, sql, lowerBound = input$timeframe[1], upperBound = input$timeframe[2])
    result <- dbGetQuery(pool, query)
  })

  output$testPlot <- renderPlot({
    ggplot(data = query(), aes(x = yrclosed, y = eligible)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Eligible Voters Per Year", x = "Year Closed", y = "Eligible Voters") +
    theme_fivethirtyeight()
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

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

get_static_sql <- function() {
  staticSQL <- "
  SELECT *, (CAST(votes_for AS float) / (votes_for + votes_against)) * 100  AS votePercentage
  FROM unionelections 
  WHERE yrclosed >= ?lowerBoundYear 
  AND yrclosed <= ?upperBoundYear 
  AND (CAST(votes_for AS float) / (votes_for + votes_against)) * 100 >= ?lowerBoundFavor 
  AND (CAST(votes_for AS float) / (votes_for + votes_against)) * 100 <= ?upperBoundFavor "
}

get_dynamic_sql <- function() {
  #Changes the part of the query that grabs the petition columns
  if (length(input$electionType) == 3) {
    dynamicSQL <- ""
  } else if (length(input$electionType) == 2) {
    dynamicSQL <- paste0("AND (petition = '", input$electionType[1], "' OR petition = '", input$electionType[2], "') ")
  } else if (length(input$electionType) == 1) {
    dynamicSQL <- paste0("AND petition = '", input$electionType[1], "' ")
  } else {
    dynamicSQL <- ""
  }
}

current_data_slice <- reactive({

  sql <- paste0(get_static_sql(), get_dynamic_sql(), ";")
  query <- sqlInterpolate(
    pool,
    sql,
    lowerBoundYear = input$timeframe[1],
    upperBoundYear = input$timeframe[2],
    lowerBoundFavor = input$percentageFavor[1],
    upperBoundFavor = input$percentageFavor[2]
  )
  result <- dbGetQuery(pool, query)
})

#change the label
#scatter / bar / histogram?
  output$test <- renderTable({
    current_data_slice()
  })

  output$testPlot <- renderPlot({
    ggplot(data = current_data_slice(), aes(x = yrclosed, y = eligible)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Title", x = "Year Closed", y = "Eligible Voters") +
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

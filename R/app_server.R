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

  pool <- dbPool(
    Postgres(),
    host = Sys.getenv("UE_IP"),
    dbname = "unionelectiondb",
    user = "ueuser",
    password = Sys.getenv("UE_DB_PASS"),
    port = 21701
 )

  get_slider_sql <- function() {
    sliderSQL <- "
    SELECT unionelections.*, populationdata.rural, (CAST(votes_for AS float) / (votes_for + votes_against)) * 100  AS votePercentage
    FROM unionelections 
    LEFT JOIN populationdata ON unionelections.fips = populationdata.fips 
    WHERE yrclosed >= ?lowerBoundYear 
    AND yrclosed <= ?upperBoundYear 
    AND (CAST(votes_for AS float) / (votes_for + votes_against)) * 100 >= ?lowerBoundFavor 
    AND (CAST(votes_for AS float) / (votes_for + votes_against)) * 100 <= ?upperBoundFavor "
  }

  get_petition_sql <- function() {
    if (length(input$electionType) == 3) {
      petitionSQL <- ""
    } else if (length(input$electionType) == 2) {
      petitionSQL <- paste0("AND (petition = '", input$electionType[1], "' OR petition = '", input$electionType[2], "') ")
    } else if (length(input$electionType) == 1) {
      petitionSQL <- paste0("AND petition = '", input$electionType[1], "' ")
    } else {
      petitionSQL <- "AND TRUE = FALSE "
    }
  }

  get_industry_sql <- function() {
    if (input$industry == "All") {
      industrySQL <- ""
    } else if (input$industry == "Agriculture, Forestry and Fishing") {
      industrySQL <- paste0("AND sic2 IN (1, 2, 7, 8, 9) ")
    } else if (input$industry == "Mining") {
      industrySQL <- paste0("AND sic2 IN (10, 12, 13, 14) ")
    } else if (input$industry == "Construction") {
      industrySQL <- paste0("AND sic2 IN (15, 16, 17) ")
    } else if (input$industry == "Manufacturing") {
      industrySQL <- paste0("AND sic2 IN (20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39) ")
    } else if (input$industry == "Transportation and Utilities") {
      industrySQL <- paste0("AND sic2 IN (40, 41, 42, 43, 44, 45, 46, 47, 48, 49) ")
    } else if (input$industry == "Wholesale") {
      industrySQL <- paste0("AND sic2 IN (50, 51) ")
    } else if (input$industry == "Retail") {
      industrySQL <- paste0("AND sic2 IN (52, 53, 54, 55, 56, 57, 58, 59) ")
    } else if (input$industry == "FIRE") {
      industrySQL <- paste0("AND sic2 IN (60, 61, 62, 63, 64, 65, 67) ")
    } else if (input$industry == "Services") {
      industrySQL <- paste0("AND sic2 IN (70, 72, 73, 75, 76, 78, 79, 80, 81, 82, 83, 84, 86, 87, 88, 89) ")
    } else if (input$industry == "Public Administration") {
      industrySQL <- paste0("AND sic2 IN (91, 92, 93, 94, 95, 96, 97, 99) ")
    } else {
      industrySQL <- ""
    }
  }

  get_county_sql <- function() {
    if (input$county == "No State Selected" | input$county == "All") {
      countySQL <- ""
    } else if (input$county == "All Rural Counties") {
      countySQL <- "AND rural = TRUE "
    } else if (input$county == "All Urban Counties") {
      countySQL <- "AND rural = FALSE "
    } else {
      countySQL <- paste0("AND unionelections.fips = '", input$county, "' ")
    }
  }

  get_state_sql <- function() {
    if (input$state == "All") {
      stateSQL <- ""
    } else {
      stateSQL <- paste0("AND unionelections.state = '", input$state, "' ")
    }
  }

  current_data_slice <- reactive({
    sql <- paste0(get_slider_sql(), get_petition_sql(), get_industry_sql(), get_state_sql(), get_county_sql(), ";")
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

  current_county_selection <- reactive({
    sql <- "
      SELECT County, FIPS
      FROM populationdata 
      WHERE State = ?selectedState;"

    query <- sqlInterpolate(
      pool,
      sql,
      selectedState = state_choices[input$state]
    )
    stateCounties <- dbGetQuery(pool, query) 
  })

  observeEvent(input$state, {
    if (input$state == 0) {
      countyDataframeToText <- c("All", "All Rural Counties", "All Urban Counties")
    } else {
      countyDataframeToText <-  c("All", "All Rural Counties", "All Urban Counties", setNames(current_county_selection()$fips, current_county_selection()$county))
    }
    updateSelectInput(inputId = "county", choices = countyDataframeToText)
  })

  observeEvent(input$winnersChecked, {
    if (input$winnersChecked == TRUE) {
      updateSliderInput(inputId = "percentageFavor", step = .01, value = c(50.01, 100))
    } else {
      updateSliderInput(inputId = "percentageFavor", step = 1, value = c(0, 100))
    }
  })

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

  state_choices <- c(
    "AL" = "Alabama",
    "AK" = "Alaska",
    "AZ" = "Arizona",
    "AR" = "Arkansas",
    "CA" = "California",
    "CO" = "Colorado",
    "CT" = "Connecticut",
    "DE" = "Delaware",
    "DC" = "District of Columbia",
    "FL" = "Florida",
    "GA" = "Georgia",
    "HI" = "Hawaii",
    "ID" = "Idaho",
    "IL" = "Illinois",
    "IN" = "Indiana",
    "IA" = "Iowa",
    "KS" = "Kansas",
    "KY" = "Kentucky",
    "LA" = "Louisiana",
    "ME" = "Maine",
    "MT" = "Montana",
    "NE" = "Nebraska",
    "NV" = "Nevada",
    "NH" = "New Hampshire",
    "NJ" = "New Jersey",
    "NM" = "New Mexico",
    "NY" = "New York",
    "NC" = "North Carolina",
    "ND" = "North Dakota",
    "OH" = "Ohio",
    "OK" = "Oklahoma",
    "OR" = "Oregon",
    "MD" = "Maryland",
    "MA" = "Massachusetts",
    "MI" = "Michigan",
    "MN" = "Minnesota",
    "MS" = "Mississippi",
    "MO" = "Missouri",
    "PA" = "Pennsylvania",
    "RI" = "Rhode Island",
    "SC" = "South Carolina",
    "SD" = "South Dakota",
    "TN" = "Tennessee",
    "TX" = "Texas",
    "UT" = "Utah",
    "VT" = "Vermont",
    "VA" = "Virginia",
    "WA" = "Washington",
    "WV" = "West Virginia",
    "WI" = "Wisconsin",
    "WY" = "Wyoming"
  )
}

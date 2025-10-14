observe({

  get_industry_sic_range <- function() {
    if (input$industry == "All") {
      industryRange <- c(1:99)
    } else if (input$industry == "Agriculture, Forestry and Fishing") {
      industryRange <- c(1, 2, 7, 8, 9)
    } else if (input$industry == "Mining") {
      industryRange <- c(10, 12, 13, 14)
    } else if (input$industry == "Construction") {
      industryRange <- c(15, 16, 17)
    } else if (input$industry == "Manufacturing") {
      industryRange <- c(20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39)
    } else if (input$industry == "Transportation and Utilities") {
      industryRange <- c(40, 41, 42, 43, 44, 45, 46, 47, 48, 49)
    } else if (input$industry == "Wholesale") {
      industryRange <- c(50, 51)
    } else if (input$industry == "Retail") {
      industryRange <- c(52, 53, 54, 55, 56, 57, 58, 59)
    } else if (input$industry == "FIRE") {
      industryRange <- c(60, 61, 62, 63, 64, 65, 67)
    } else if (input$industry == "Services") {
      industryRange <- c(70, 72, 73, 75, 76, 78, 79, 80, 81, 82, 83, 84, 86, 87, 88, 89)
    } else if (input$industry == "Public Administration") {
      industryRange <- c(91, 92, 93, 94, 95, 96, 97, 99)
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
      countySQL <- paste0("AND unionelections.FIPS = '", input$county, "' ")
    }
  }

  get_state_sql <- function() {
    if (input$state == "All") {
      stateSQL <- ""
    } else {
      #stateSQL <- glue("AND unionelections.state = '{state}' ", state = input$state)
      stateSQL <- paste0("AND unionelections.state = '", input$state, "' ")
    }
  }

    electionData <- fread("resources/Data/Elections_Data_Cleaned_V0.csv")
    populationData <- fread("resources/Data/Population_Data_2020.csv")
    electionData[populationData, on = 'FIPS', Rural := i.Rural][]
    electionData[, vote_percentage := (votes_for / votes_total) * 100]
    electionDataSubset <- electionData[
        petition %in% input$electionType & 
        year_closed <= input$timeframe[2] & 
        year_closed >= input$timeframe[1] &
        vote_percentage <= input$percentageFavor[2] &
        vote_percentage >= input$percentageFavor[1] &
        sic %in% get_industry_sic_range() &

    ]
    print(electionDataSubset)  
  })
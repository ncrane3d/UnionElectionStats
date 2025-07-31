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
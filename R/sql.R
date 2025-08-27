get_slider_sql <- function() {
    sliderSQL <- "
    SELECT unionelections.petition, unionelections.election_type, unionelections.eligible, unionelections.votes_for, unionelections.votes_against, unionelections.votes_total, unionelections.filed_date, unionelections.election_date, unionelections.closed_date, unionelections.year_closed, unionelections.city, unionelections.state, unionelections.county, unionelections.FIPS, unionelections.longitude, unionelections.latitude, unionelections.employer, unionelections.SIC, unionelections.unit_type, populationdata.rural, (CAST(votes_for AS float) / NULLIF(votes_total, 0)) * 100  AS votePercentage
    FROM unionelections 
    LEFT JOIN populationdata ON CAST(unionelections.fips AS int) = populationdata.fips 
    WHERE year_closed >= ?lowerBoundYear 
    AND year_closed <= ?upperBoundYear 
    AND (CAST(votes_for AS float) / NULLIF(votes_total, 0)) * 100 >= ?lowerBoundFavor 
    AND (CAST(votes_for AS float) / NULLIF(votes_total, 0)) * 100 <= ?upperBoundFavor "
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
      industrySQL <- paste0("AND CAST(sic AS int) IN (1, 2, 7, 8, 9) ")
    } else if (input$industry == "Mining") {
      industrySQL <- paste0("AND CAST(sic AS int) IN (10, 12, 13, 14) ")
    } else if (input$industry == "Construction") {
      industrySQL <- paste0("AND CAST(sic AS int) IN (15, 16, 17) ")
    } else if (input$industry == "Manufacturing") {
      industrySQL <- paste0("AND CAST(sic AS int) IN (20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39) ")
    } else if (input$industry == "Transportation and Utilities") {
      industrySQL <- paste0("AND CAST(sic AS int) IN (40, 41, 42, 43, 44, 45, 46, 47, 48, 49) ")
    } else if (input$industry == "Wholesale") {
      industrySQL <- paste0("AND CAST(sic AS int) IN (50, 51) ")
    } else if (input$industry == "Retail") {
      industrySQL <- paste0("AND CAST(sic AS int) IN (52, 53, 54, 55, 56, 57, 58, 59) ")
    } else if (input$industry == "FIRE") {
      industrySQL <- paste0("AND CAST(sic AS int) IN (60, 61, 62, 63, 64, 65, 67) ")
    } else if (input$industry == "Services") {
      industrySQL <- paste0("AND CAST(sic AS int) IN (70, 72, 73, 75, 76, 78, 79, 80, 81, 82, 83, 84, 86, 87, 88, 89) ")
    } else if (input$industry == "Public Administration") {
      industrySQL <- paste0("AND CAST(sic AS int) IN (91, 92, 93, 94, 95, 96, 97, 99) ")
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

  getCurrentData <- function() {
    sql <- paste0(
      get_slider_sql(),
      get_petition_sql(),
      get_industry_sql(),
      get_state_sql(),
      get_county_sql(),
      "LIMIT 500 "
    )
    return(sqlInterpolate(
      pool,
      sql,
      lowerBoundYear = input$timeframe[1],
      upperBoundYear = input$timeframe[2],
      lowerBoundFavor = input$percentageFavor[1],
      upperBoundFavor = input$percentageFavor[2]
    ))
  }
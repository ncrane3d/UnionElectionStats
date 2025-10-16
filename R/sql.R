sqlModule <- function(id, electionTypeInput, industryTypeInput, selectedCountyInput, selectedStateInput, timeframeLowerBoundInput, timeframeUpperBoundInput, percentageFavorLowerBoundInput, percentageFavorUpperBoundInput) {  
  moduleServer(
    id,
    function(input, output, session) {  
      get_slider_sql <- function() {
        sliderSQL <- "SELECT unionelections.case_number, unionelections.petition, unionelections.election_type, unionelections.eligible, unionelections.votes_for, unionelections.votes_against, unionelections.votes_total, unionelections.filed_date, unionelections.year_filed, unionelections.month_filed, unionelections.day_filed, unionelections.election_date, unionelections.year_election, unionelections.month_election, unionelections.day_election, unionelections.closed_date, unionelections.year_closed, unionelections.month_closed, unionelections.day_closed, unionelections.city, unionelections.state, unionelections.county, unionelections.FIPS, unionelections.longitude, unionelections.latitude, unionelections.employer, unionelections.SIC, unionelections.unit_type, populationdata.rural 
          FROM read_csv('resources/Data/Elections_Data_Cleaned_V0.csv', types = {{'year_closed':'INTEGER','votes_for':'DOUBLE'}}, ignore_errors=true) AS unionelections 
          LEFT JOIN read_csv_auto('resources/Data/Population_Data_2020.csv', ignore_errors=true) AS populationdata ON TRY_CAST(unionelections.FIPS AS int) = populationdata.FIPS 
          WHERE unionelections.year_closed >= {lowerBoundYear} 
          AND unionelections.year_closed <= {upperBoundYear} 
          AND unionelections.votes_for / NULLIF(unionelections.votes_total, 0) * 100 >= {lowerBoundFavor} 
          AND unionelections.votes_for / NULLIF(unionelections.votes_total, 0) * 100 <= {upperBoundFavor} "
      }

      get_petition_sql <- function() {
        electionType <- electionTypeInput()
        if (length(electionType) == 3) {
          petitionSQL <- ""
        } else if (length(electionType) == 2) {
          petitionSQL <- paste0("AND (petition = '", electionType()[1], "' OR petition = '", electionType()[2], "') ")
        } else if (length(electionType) == 1) {
          petitionSQL <- paste0("AND petition = '", electionType()[1], "' ")
        } else {
          petitionSQL <- "AND TRUE = FALSE "
        }
      }

      get_industry_sql <- function() {
        industryType <- industryTypeInput()
        if (industryType == "All") {
          industrySQL <- ""
        } else if (industryType == "Agriculture, Forestry and Fishing") {
          industrySQL <- paste0("AND TRY_CAST(sic as INTEGER) IN (1, 2, 7, 8, 9) ")
        } else if (industryType == "Mining") {
          industrySQL <- paste0("AND TRY_CAST(sic as INTEGER) IN (10, 12, 13, 14) ")
        } else if (industryType == "Construction") {
          industrySQL <- paste0("AND TRY_CAST(sic as INTEGER) IN (15, 16, 17) ")
        } else if (industryType == "Manufacturing") {
          industrySQL <- paste0("AND TRY_CAST(sic as INTEGER) IN (20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39) ")
        } else if (industryType == "Transportation and Utilities") {
          industrySQL <- paste0("AND TRY_CAST(sic as INTEGER) IN (40, 41, 42, 43, 44, 45, 46, 47, 48, 49) ")
        } else if (industryType == "Wholesale") {
          industrySQL <- paste0("AND TRY_CAST(sic as INTEGER) IN (50, 51) ")
        } else if (industryType == "Retail") {
          industrySQL <- paste0("AND TRY_CAST(sic as INTEGER) IN (52, 53, 54, 55, 56, 57, 58, 59) ")
        } else if (industryType == "FIRE") {
          industrySQL <- paste0("AND TRY_CAST(sic as INTEGER) IN (60, 61, 62, 63, 64, 65, 67) ")
        } else if (industryType == "Services") {
          industrySQL <- paste0("AND TRY_CAST(sic as INTEGER) IN (70, 72, 73, 75, 76, 78, 79, 80, 81, 82, 83, 84, 86, 87, 88, 89) ")
        } else if (industryType == "Public Administration") {
          industrySQL <- paste0("AND TRY_CAST(sic as INTEGER) IN (91, 92, 93, 94, 95, 96, 97, 99) ")
        } else {
          industrySQL <- ""
        }
      }

      get_county_sql <- function() {
        selectedCounty <- selectedCountyInput()
        if (selectedCounty == "No State Selected" | selectedCounty == "All") {
          countySQL <- ""
        } else if (selectedCounty == "All Rural Counties") {
          countySQL <- "AND rural = TRUE "
        } else if (selectedCounty == "All Urban Counties") {
          countySQL <- "AND rural = FALSE "
        } else {
          countySQL <- paste0("AND unionelections.FIPS = '", selectedCounty, "' ")
        }
      }

      get_state_sql <- function() {
        selectedState <- selectedStateInput()
        if (selectedState == "All") {
          stateSQL <- ""
        } else {
          stateSQL <- paste0("AND unionelections.state = '", selectedState, "' ")
        }
      }

      return(reactive({
        timeframeLowerBound <- timeframeLowerBoundInput()
        timeframeUpperBound <- timeframeUpperBoundInput()
        percentageFavorLowerBound <- percentageFavorLowerBoundInput()
        percentageFavorUpperBound <- percentageFavorUpperBoundInput()
        sql <- paste0(
          get_slider_sql(),
          get_petition_sql(),
          get_industry_sql(),
          get_state_sql(),
          get_county_sql()
        )
        glue(
          sql,
          lowerBoundYear = timeframeLowerBound,
          upperBoundYear = timeframeUpperBound,
          lowerBoundFavor = percentageFavorLowerBound,
          upperBoundFavor = percentageFavorUpperBound
        )
      }))
    }
  )
}  
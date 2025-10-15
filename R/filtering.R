filteringModule <- function(id, electionTypeInput, industryTypeInput, selectedCountyInput, selectedStateInput, timeframeLowerBoundInput, timeframeUpperBoundInput, percentageFavorLowerBoundInput, percentageFavorUpperBoundInput) {  
  moduleServer(
    id,
    function(input, output, session) {  
      get_industry_sic_range <- function() {
        industryType <- industryTypeInput()
        if (industryType == "All") {
          industrySQL <- c(1:99)
        } else if (industryType == "Agriculture, Forestry and Fishing") {
          industrySQL <- c(1, 2, 7, 8, 9)
        } else if (industryType == "Mining") {
          industrySQL <- c(10, 12, 13, 14)
        } else if (industryType == "Construction") {
          industrySQL <- c(15, 16, 17)
        } else if (industryType == "Manufacturing") {
          industrySQL <- c(20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39)
        } else if (industryType == "Transportation and Utilities") {
          industrySQL <- c(40, 41, 42, 43, 44, 45, 46, 47, 48, 49)
        } else if (industryType == "Wholesale") {
          industrySQL <- c(50, 51)
        } else if (industryType == "Retail") {
          industrySQL <- c(52, 53, 54, 55, 56, 57, 58, 59)
        } else if (industryType == "FIRE") {
          industrySQL <- c(60, 61, 62, 63, 64, 65, 67)
        } else if (industryType == "Services") {
          industrySQL <- c(70, 72, 73, 75, 76, 78, 79, 80, 81, 82, 83, 84, 86, 87, 88, 89)
        } else if (industryType == "Public Administration") {
          industrySQL <- c(91, 92, 93, 94, 95, 96, 97, 99)
        } 
      }

      return(reactive({
          #Reactive handling
          electionType <- electionTypeInput()    
          selectedCounty <- selectedCountyInput()
          selectedState <- selectedStateInput()
          timeframeLowerBound <- timeframeLowerBoundInput()
          timeframeUpperBound <- timeframeUpperBoundInput()
          percentageFavorLowerBound <- percentageFavorLowerBoundInput()
          percentageFavorUpperBound <- percentageFavorUpperBoundInput()

          #Datatable preparation
          electionData <- fread("resources/Data/Elections_Data_Cleaned_V0.csv")
          populationData <- fread("resources/Data/Population_Data_2020.csv")
          electionData[populationData, on = 'FIPS', Rural := i.Rural][]
          electionData[, vote_percentage := (votes_for / votes_total) * 100]

          #Datatable filtering
          if (selectedState != "All") {
            if (selectedCounty == "All") {
              electionDataSubset <- electionData[
                petition %in% electionType & 
                year_closed >= timeframeLowerBound & 
                year_closed <= timeframeUpperBound &
                vote_percentage >= percentageFavorLowerBound &
                vote_percentage <= percentageFavorUpperBound &
                SIC %in% get_industry_sic_range() &
                state == selectedState
              ]
            } else if (selectedCounty == "All Rural Counties") {
              electionDataSubset <- electionData[
                petition %in% electionType & 
                year_closed >= timeframeLowerBound & 
                year_closed <= timeframeUpperBound &
                vote_percentage >= percentageFavorLowerBound &
                vote_percentage <= percentageFavorUpperBound &
                SIC %in% get_industry_sic_range() &
                state == selectedState &
                Rural == 1
              ]
            } else if (selectedCounty == "All Urban Counties") {
              electionDataSubset <- electionData[
                petition %in% electionType & 
                year_closed >= timeframeLowerBound & 
                year_closed <= timeframeUpperBound &
                vote_percentage >= percentageFavorLowerBound &
                vote_percentage <= percentageFavorUpperBound &
                SIC %in% get_industry_sic_range() &
                state == selectedState &
                Rural == 0
              ]
            } else {
              electionDataSubset <- electionData[
                petition %in% electionType & 
                year_closed >= timeframeLowerBound & 
                year_closed <= timeframeUpperBound &
                vote_percentage >= percentageFavorLowerBound &
                vote_percentage <= percentageFavorUpperBound &
                SIC %in% get_industry_sic_range() &
                state == selectedState &
                FIPS == selectedCounty
              ]
            }
          } else {
            electionDataSubset <- electionData[
              petition %in% electionType & 
              year_closed >= timeframeLowerBound & 
              year_closed <= timeframeUpperBound &
              vote_percentage >= percentageFavorLowerBound &
              vote_percentage <= percentageFavorUpperBound &
              SIC %in% get_industry_sic_range() 
            ]
          }
          setDF(electionDataSubset)
      }))
    }
  )
}  
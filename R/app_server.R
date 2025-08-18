#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import DBI
#' @import RPostgres
#' @import pool
#' @import sf
#' @import dplyr
#' @import htmltools
#' @import htmlwidgets
#' @noRd

source('./R/map/map.R', local = TRUE)
#'

app_server <- function(input, output, session) {
  # Your application server logic
  source('./R/sql.R', local = TRUE)
  pool <- dbPool(
    Postgres(),
    host = Sys.getenv("UE_IP"),
    dbname = "unionelectiondb",
    user = "ueuser",
    password = Sys.getenv("UE_DB_PASS"),
    port = 21701
  )
  current_query <- reactive({getCurrentData()})
  output$map <- map(input, output, pool, current_data_slice, current_query)
  boundaries <- getBoundaries(pool, current_query)
  statePalette <- getPalette(boundaries[1]$state_count)
  countyPalette <- getPalette(boundaries[2]$normalized_vote)
    mapHighlight <- highlightOptions(
        color = "white",
        weight = 2,
        opacity = 1,
        bringToFront = TRUE
    )
    boundaries <- getBoundaries(pool, current_query)
    statePalette <- getPalette(boundaries[1]$state_count)
    countyPalette <- getPalette(boundaries[2]$normalized_vote)
    mapHighlight <- highlightOptions(
        color = "white",
        weight = 2,
        opacity = 1,
        bringToFront = TRUE
    )
    #Error handling for when there are no points to render on map
    getCircleMarkerData <- function(){
        if (nrow(current_data_slice()) > 1){
            return(current_data_slice())
        } else {
            #Returns dataframe containing 1 point in Bangladesh, out of constrained view of user
            return(data.frame(latitude=c(23.6850), longitude=c(90.3563), yrclosed=c(1), employer=c("none"), votes_for= c(1), votes_against=c(1)))
        }
    }
  observe({
    leafletProxy("map") %>%
    clearShapes() %>%
    clearMarkerClusters %>%
    addPolygons(
                data = boundaries[[1]](),
                weight = 1,
                color = ~ statePalette(state_count),
                group = "states",
                highlightOptions = mapHighlight
            ) |>
            #County border layer
            addPolygons(
                data = boundaries[[2]](),
                weight = 1,
                color = ~ countyPalette(normalized_vote),
                group = "counties",
            ) |>
            #Individual election markers
            addCircleMarkers(
                data = getCircleMarkerData(),
                color = "red",
                stroke = FALSE,
                fillOpacity = 0.75,
                group = "counties",
                clusterOptions = markerClusterOptions(
                    #Expands overlapping markers out into a starburst on max zoom
                    spiderfyOnMaxZoom = TRUE,
                    showCoverageOnHover = FALSE
                ),
                #Popup on click of individual elections that displays basic info
                label = ~ sprintf(
                    "Employer: %s<br/>Year closed: %s<br/>Pro-union vote share: %s",
                    employer,
                    yrclosed,
                    round(
                        ((votes_for /
                            (votes_for + votes_against)) *
                            100),
                        digits = 2
                    )
                ) %>%
                    lapply(htmltools::HTML)
            )
  })
  
  current_data_slice <- reactive({dbGetQuery(pool, paste0(current_query(), ";"))
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
      countyDataframeToText <- c(
        "All",
        "All Rural Counties",
        "All Urban Counties"
      )
    } else {
      countyDataframeToText <- c(
        "All",
        "All Rural Counties",
        "All Urban Counties",
        setNames(
          current_county_selection()$fips,
          current_county_selection()$county
        )
      )
    }
    updateSelectInput(inputId = "county", choices = countyDataframeToText)
  })

  observeEvent(input$percentageFavor, {
    if ((input$winnersChecked == TRUE) & (input$percentageFavor[1] <= 50.01)) {
      updateSliderInput(
        inputId = "percentageFavor",
        step = .01,
        value = c(50.01, input$percentageFavor[2])
      )
    }
  })

  observeEvent(input$winnersChecked, {
    if (input$winnersChecked == TRUE) {
      updateSliderInput(
        inputId = "percentageFavor",
        step = .01,
        value = c(50.01, 100)
      )
    } else {
      updateSliderInput(
        inputId = "percentageFavor",
        step = 1,
        value = c(0, 100)
      )
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

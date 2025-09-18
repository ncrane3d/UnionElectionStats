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
#' @import lubridate
#' @import duckdb
#' @import glue
#' @import leafgl
#' @import viridis
#' @import rmapshaper
#' @noRd
#'

app_server <- function(input, output, session) {
  # Your application server logic
  source('./R/sql.R', local = TRUE)
  source('./R/map/map.R', local = TRUE)

  pool = dbConnect(duckdb())
  DBI::dbExecute(pool, "INSTALL httpfs; LOAD httpfs;")

  current_query <- reactive({getCurrentData()})
  current_data_slice <- reactive({
    dbGetQuery(pool, current_query()) #%>%
    # mutate(
    #   # Create grouping key for nearby points
    #   lon_group = round(longitude, 5),
    #   lat_group = round(latitude, 5)
    # ) %>%
    # group_by(lon_group, lat_group) %>%
    # mutate(
    #   n = n(),
    #   jittered = n > 1,
    #   jittered_lon = if (n() > 1) jitter(longitude, amount = 1e-5) else longitude,
    #   jittered_lat = if (n() > 1) jitter(latitude, amount = 1e-5) else latitude
    # ) %>%
    # ungroup() %>%
    # select(-n, -lon_group, -lat_group)
  })

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3)) |>
    addTiles("https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png") |>
    addMapPane(name="shapes", zIndex=410) %>%
    addMapPane(name="labels", zIndex=415) %>%
    addMapPane(name="markers", zIndex=420) %>%
    addTiles("https://{s}.basemaps.cartocdn.com/light_only_labels/{z}/{x}/{y}.png", options= leafletOptions(pane = "labels")) |>
    #Zoom based conditional rendering for layers
    groupOptions("points", zoomLevels = 7:20) |>
    groupOptions("counties", zoomLevels = 5:20) |>
    groupOptions("states", zoomLevels = 0:4) |>
    #Map panning bounds
    setMaxBounds(
        lat1 = 72.89817,
        lng1 = -179.912096,
        lat2 = 1,
        lng2 = -54.892994
    )
  })

  current_county_selection <- reactive({
    req(state_choices[input$state])
    sql <- "
      SELECT County, FIPS
      FROM read_csv_auto('resources/Data/Population_Data_2020.csv', ignore_errors=true) 
      WHERE State = {selectedState}"

    query <- glue(
      sql,
      selectedState = sQuote(state_choices[input$state])
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
          current_county_selection()$FIPS,
          current_county_selection()$County
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

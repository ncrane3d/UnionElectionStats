#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import DBI
#' @import RPostgres
#' @import pool
#' @import tidyverse
#' @import ggplot2
#' @import dplyr
#' @import gcookbook
#' @import hrbrthemes
#' @noRd
#'

app_server <- function(input, output, session) {
  # Your application server logic
  source('./R/sql.R', local = TRUE)

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

  observeEvent(input$percentageFavor, {
    if ((input$winnersChecked == TRUE) & (input$percentageFavor[1] <= 50.01) ) {
      updateSliderInput(inputId = "percentageFavor", step = .01, value = c(50.01, input$percentageFavor[2]))
    }
  })

  observeEvent(input$winnersChecked, {
    if (input$winnersChecked == TRUE) {
      updateSliderInput(inputId = "percentageFavor", step = .01, value = c(50.01, 100))
    } else {
      updateSliderInput(inputId = "percentageFavor", step = 1, value = c(0, 100))
    }
  })

  output$customVisualization <- renderPlot ({
    if (input$customGraphType == "LINE") {
      req(customLineGraphVariableHandler())
      customLineGraphVariableHandler() + labs(x = "Year Election Closed", y = input$customAxes) + plotTheme()
    } else if (input$customGraphType == "HIST") {
      req(customHistogramVariableHandler())
      customHistogramVariableHandler() + labs(x = input$customAxes, y = "Frequency") + plotTheme()
    }
  })

  totalVotes <- function(){
    return(with(current_data_slice(), votes_for + votes_against))
  }
  unionVoteShare <- function() {
    return(with(current_data_slice(), (100 * votes_for/(votes_for + votes_against))))
  }
  participationRate <- function() {
    return(with(current_data_slice(), (100 * (votes_for + votes_against)/eligible)))
  }
  plot <- function() {

  }
  statLine <- function(func, color, alpha) {
    if (missing(func)){
      func = "mean"
    }
    if (missing(color)){
      color = "black"
    }
    if (missing(alpha)) {
      alpha = 1
    }
    return(stat_summary(fun.y = func, geom="line", color = color, alpha = alpha))
  }
  plotTheme <- function() {
    return(theme_ipsum_rc() + theme(plot.background = element_rect(fill="#FCF9F6", color = "#FCF9F6")))
  }
  limitToMaxEligible <- function(){
    return(ylim(c(0, max(current_data_slice()$eligible))))
  }

  customLineGraphVariableHandler <- function() {
    if (input$customAxes == "Elections") {
      return(ggplot(current_data_slice(), aes(x = yrclosed)) +
    geom_line(aes(fill=..count..), stat="bin", binwidth=1))
    } else if (input$customAxes == "Eligible Employees") {
      return(ggplot(current_data_slice(), aes(x= yrclosed, y = current_data_slice()$eligible)) + geom_line() + limitToMaxEligible())
    } else if (input$customAxes == "Total Votes") {
      return(ggplot(current_data_slice(), aes(x= yrclosed, y = totalVotes())) + geom_line() + limitToMaxEligible())
    } else if (input$customAxes == "Eligible per Election") {
      return(ggplot(current_data_slice(), aes(x = yrclosed, y = current_data_slice()$eligible)) + statLine(alpha=0.5) + statLine(func="median", color="red", alpha=0.5) + limitToMaxEligible()+ scale_color_manual(values = c("Mean" = "black", "Median" = "red"), labels = c("Mean Eligible", "Median Eligible")))
    } else if (input$customAxes == "Avg. Votes per Election") {
      return(ggplot(current_data_slice(), aes(x = yrclosed, y = totalVotes())) + statLine() + limitToMaxEligible())
    } else if (input$customAxes == "Avg. Votes For Union") {
      return(ggplot(current_data_slice(), aes(x = yrclosed, y = current_data_slice()$votes_for)) + statLine() + limitToMaxEligible())
    } else if (input$customAxes == "Avg. Votes Against Union") {
      return(ggplot(current_data_slice(), aes(x = yrclosed, y = current_data_slice()$votes_against)) + statLine() + limitToMaxEligible())
    } else if (input$customAxes == "Avg. Union Vote Share") {
      return(ggplot(current_data_slice(), aes(x = yrclosed, y = unionVoteShare())) + statLine())
    } else if (input$customAxes == "Avg. Participation Rate") {
      return(ggplot(current_data_slice(), aes(x = yrclosed, y = participationRate())) + statLine())
    } else {
      return()
    }
    return(ggplot(current_data_slice(), aes(x = yrclosed, y = yAxis)) +
    geom_line())
  }

  customHistogramVariableHandler <- function() {
    ggplot(current_data_slice(), aes(x = customHistogramVariableHandler()))
    if (input$customAxes == "Petition Type") {
      return(ggplot(data.frame(current_data_slice()$petition), aes(x=current_data_slice()$petition)) + geom_bar())
    } else if (input$customAxes == "Election Type") {
      return(ggplot(data.frame(current_data_slice()$elec_type), aes(x=current_data_slice()$elec_type)) + geom_bar())
    } else if (input$customAxes == "Votes For/Against Union") {
      return(ggplot(current_data_slice()) + geom_histogram(aes(x = current_data_slice()$votes_for), alpha = 0.5, binwidth = 5, fill = "green") + geom_histogram(aes(x = current_data_slice()$votes_against), alpha = 0.5, binwidth = 5, fill = "red"))
    } else if (input$customAxes == "Total Votes") {
      xAxis <- totalVotes()
    } else if (input$customAxes == "Union Vote Share") {
      xAxis <- unionVoteShare()
    } else if (input$customAxes == "Participation Rate") {
      xAxis <- participationRate()
    } else {
      return()
    }
    return(ggplot(current_data_slice(), aes(x = xAxis)) + geom_histogram(binwidth = 1.53))
  }
  
observeEvent(input$customGraphType, {
    if (input$customGraphType == "LINE") {
      lineGraphChoices <- c(
        "Elections",
        "Eligible Employees",
        "Total Votes",
        "Eligible per Election",
        "Avg. Votes per Election",
        "Avg. Votes For Union",
        "Avg. Votes Against Union",
        "Avg. Union Vote Share",
        "Avg. Participation Rate"
      )
      axisLabel <- "Select Y Axis"
    } else if (input$customGraphType == "HIST") {
      lineGraphChoices <- c(
        "Petition Type",
        "Election Type",
        "Votes For/Against Union", 
        "Total Votes",
        "Union Vote Share",
        "Participation Rate"
      )
      axisLabel <- "Select X Axis"
    }
    updateSelectInput(inputId = "customAxes", label = axisLabel, choices = lineGraphChoices)
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

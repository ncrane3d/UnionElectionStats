#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import DBI
#' @import RPostgres
#' @import pool
#' @import sendmailR
#' @import shinyFeedback
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
#' @import tidyverse
#' @import ggplot2
#' @import dplyr
#' @import gcookbook
#' @import ggiraph
#' @import sf
#' @import dplyr
#' @import htmltools
#' @import htmlwidgets
#' @import plotly
#' @import data.table
#' @noRd

#'

app_server <- function(input, output, session) {
  # Your application server logic
  source('./R/sql.R', local = TRUE)
  #source('./R/map/map.R', local = TRUE)
  source('./R/custom_graphs.R', local = TRUE)
  source('./R/preset_graphs.R', local = TRUE)

  currentDataSelection <- filteringModule("filter", reactive(input$electionType), reactive(input$industry), reactive(input$county), reactive(input$state), reactive(input$timeframe[1]), reactive(input$timeframe[2]), reactive(input$percentageFavor[1]), reactive(input$percentageFavor[2]))
  observe({
    print(head(currentDataSelection()))
  })

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

  # output$map <- renderLeaflet({
  #   leaflet(options = leafletOptions(minZoom = 3)) |>
  #   addTiles("https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png") |>
  #   addMapPane(name="shapes", zIndex=410) %>%
  #   addMapPane(name="labels", zIndex=415) %>%
  #   addMapPane(name="markers", zIndex=420) %>%
  #   addTiles("https://{s}.basemaps.cartocdn.com/light_only_labels/{z}/{x}/{y}.png", options= leafletOptions(pane = "labels")) |>
  #   #Zoom based conditional rendering for layers
  #   groupOptions("points", zoomLevels = 7:20) |>
  #   groupOptions("counties", zoomLevels = 5:20) |>
  #   groupOptions("states", zoomLevels = 0:4) |>
  #   #Map panning bounds
  #   setMaxBounds(
  #       lat1 = 72.89817,
  #       lng1 = -179.912096,
  #       lat2 = 1,
  #       lng2 = -54.892994
  #   )
  # })

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

  output$customVisualization <- renderPlot ({
    if (input$customGraphType == "LINE") {
      req(customLineGraphVariableHandler())
      customLineGraphVariableHandler() + labs(x = "Year Election Closed", y = input$customAxes) + plotTheme()
    } else if (input$customGraphType == "HIST") {
      req(customHistogramVariableHandler())
      customHistogramVariableHandler() + labs(x = input$customAxes, y = "Frequency") + plotTheme()
    }
  })

output$unitTypePreset <- renderPlot ({
  getUnitTypeGraph()
})

output$elecTypePreset <- renderPlot({
  getElectionTypeGraph()
})

output$regionalPreset <- renderPlot({
  getRegionalBreakdown()
})

output$industryPreset <- renderPlot({
  getIndustryBreakdown()
})

output$elecTypePreset <- renderPlot({
  getElectionTypeGraph()
})

output$linePreset <- renderPlot({
  getLineGraph()
})

output$heatmapPreset <- renderPlot({
  getHeatmap()
})

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

  observe({
    req("./resources/csv/featured-analysis.csv")
    faCSV <- read.csv("./resources/csv/featured-analysis.csv")
    faCSV <- data.frame(id = 1:nrow(faCSV), faCSV)
    for (i in 1:nrow(faCSV)) {
      local({
        ii <- i #Without this line only the last loop will be kept.
        output[[paste0("figure", ii)]] <- 
          renderImage({
            list(src = faCSV$imagePath[ii], width = "80%", height ="auto")
          }, deleteFile = FALSE)
      })
    }
  })  

  output$insertFeaturedAnalysisFromCSV <- renderUI ({
    req("./resources/csv/featured-analysis.csv")
    faCSV <- read.csv("./resources/csv/featured-analysis.csv")
    faCSV <- data.frame(id = 1:nrow(faCSV), faCSV)
    formattedPapers <- tagList()
    for (i in 1:nrow(faCSV)) {
      if (faCSV$imagePath[i] != "") {
        formattedPapers <- tagAppendChildren(formattedPapers, createFeaturedAnalysisAccordionWithImage(faCSV, i))
      } else {
        formattedPapers <- tagAppendChildren(formattedPapers, createFeaturedAnalysisAccordionNoImage(faCSV, i))
      }    
    }
    return(formattedPapers)
  })

  createFeaturedAnalysisAccordionWithImage <- function(faCSV, i) {
    accordion_panel(
      title = faCSV$title[i],
      div(
        align = "left",
        div(strong("Author(s): "), p(paste(faCSV$author[i], collapse = ", "))),
        div(strong("Abstract: "), p(faCSV$abstract[i])),
        div(strong("Featured Figure: "), div(imageOutput(paste0("figure", i)) %>% tagAppendAttributes(class = "accordion-figure"), align = "center")),
        div(strong("Link: "), p(tags$a(href=faCSV$link[i], faCSV$title[i])))
      ) %>%
      tagAppendAttributes(id = "accordion-analysis"),
    )
  }

  createFeaturedAnalysisAccordionNoImage <- function(faCSV, i) {
    accordion_panel(
      title = faCSV$title[i],
      div(
        align = "left",
        div(strong("Author(s): "), p(paste(faCSV$author[i], collapse = ", "))),
        div(strong("Abstract: "), p(faCSV$abstract[i])),
        div(strong("Link: "), p(tags$a(href=faCSV$link[i], faCSV$title[i])))
      ) %>%
      tagAppendAttributes(id = "accordion-analysis"),
    )
  }

  observeEvent(input$citationPopup, {
      showModal(modalDialog(
        title = "Suggested Citation Format",
        "When citing this resource, plese use the following suggested citation style:",
        p(tags$br(), "Schaller, Z., Young S., & Kamphorst, J. (2025). \"Union Election Stats.\" ", tags$i("Self Published."), 
        "Retrieved from ", tags$a(href='http://unionelectionstats.com', 'unionelectionstats.com.')),
        easyClose = TRUE
      ))
    })

    observeEvent(input$submitButton, {
      if(input$name == "") {
        feedbackWarning("name", (input$name == ""), "Please fill out the name field before pressing submit.")
      }  else {
        hideFeedback("name")
      }

      if(input$email == "") {
        feedbackWarning("email", (input$email == ""), "Please fill out the email field before pressing submit.")
      } else {
        hideFeedback("email")
      }

      if(input$subject == "") {
        feedbackWarning("subject", (input$subject == ""), "Please fill out the subject field before pressing submit.")
      } else {
        hideFeedback("subject")
      }

      if(input$message == "") {
        feedbackWarning("message", (input$message == ""), "Please fill out the message field before pressing submit.")
      } else {
        hideFeedback("message")
      }

      #TODO: Add in email functionality (will have to open email socket on computre)
      if(!(input$name == "" || input$email == "" || input$subject == "" || input$message == "")) {
        print(paste(input$name, input$email, input$subject, input$message, "\n"))
        # from <- isolate(input$email)
        # to <- isolate("ncrane3d@gmail.com")
        # subject <- isolate(input$subject)
        # msg <- isolate(input$message)
        # sendmail(from, to, subject, msg)

        showModal(modalDialog(
        title = "Your Feedback Has Been Received",
        "Thank you for submission! We will be in touch with you soon. In the meantime feel free to keep exploring the visualizations
         on the home page, or take a dive into some further reading on the Featured Analysis Page.",
        easyClose = TRUE
      ))

        #Clean up form
        updateTextInput(inputId = "name", value = "")
        updateTextInput(inputId = "email", value = "")
        updateTextInput(inputId = "subject", value = "")
        updateTextAreaInput(inputId = "message", value = "")
      }
  })
  
  output$jonne <- renderImage(
    {
      list(
        src = "./resources/images/jonne_kamphorst.jpg",
        height = "auto",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )

  output$zachary <- renderImage(
    {
      list(
        src = "./resources/images/zachary_schaller.png",
        height = "auto",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )

  output$sam <- renderImage(
    {
      list(
        src = "./resources/images/samuel_young.jpg",
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

  output$lucy <- renderImage(
    {
      list(
        src = "./resources/images/lucy_lewark.png",
        height = "auto",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )

  output$nathan <- renderImage(
    {
      list(
        src = "./resources/images/nathan_crane.jpg",
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

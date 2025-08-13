#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import DBI
#' @import RPostgres
#' @import pool
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

  # output$word <- renderImage({
  #   list(src = "./resources/images/featuredAnalysis/Where-Unions-Fell.png")
  #   }, deleteFile = FALSE
  # )

  # output$homie <- renderImage({
  #   list(src = "./resources/images/featuredAnalysis/Where-Unions-Fell.png")
  #   }, deleteFile = FALSE
  # )


  output$insertFeaturedAnalysisFromCSV <- renderUI ({
    req("./resources/csv/featured-analysis.csv")
    faCSV <- read.csv("./resources/csv/featured-analysis.csv")
    faCSV <- data.frame(id = 1:nrow(faCSV), faCSV)
    formattedPapers <- tagList()
    for (i in 1:nrow(faCSV)) {
      formattedPapers <- tagAppendChildren(formattedPapers, createFeaturedAnalysisAccordionWithImage(faCSV, i))
      # if (faCSV$imagePath[i] == "" | faCSV$imagePath[i] == NULL) {
      #   formattedPapers <- tagAppendChildren(formattedPapers, createFeaturedAnalysisAccordionWithImage(faCSV, i))
      # } else {
      #   formattedPapers <- tagAppendChildren(formattedPapers, createFeaturedAnalysisAccordionNoImage(faCSV, i))
      # }    
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
        src = "./resources/images/pfp_empty.png",
        height = "auto",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )

  output$nathan <- renderImage(
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

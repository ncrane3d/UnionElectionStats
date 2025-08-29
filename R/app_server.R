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

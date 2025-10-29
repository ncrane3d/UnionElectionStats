#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import sendmailR
#' @import shinyFeedback
#' @import sf
#' @import dplyr
#' @import htmltools
#' @import htmlwidgets
#' @import lubridate
#' @import glue
#' @import leafgl
#' @import viridis
#' @import rmapshaper
#' @import tidyverse
#' @import ggplot2
#' @import dplyr
#' @import gcookbook
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

  #Datatable preparation
  electionData <- fread("resources/Data/Elections_Data_Cleaned_V0.csv")
  populationData <- fread("resources/Data/Population_Data_2020.csv")
  electionData[populationData, on = 'FIPS', Rural := i.Rural][]
  electionData[, vote_percentage := (votes_for / votes_total) * 100]

  #Conducts initial filter, without state/county
  electionDataSubset <- filteringModule("filtering", reactive(input$electionType), reactive(input$industry), reactive(input$county), reactive(input$state), reactive(input$timeframe[1]), reactive(input$timeframe[2]), reactive(input$percentageFavor[1]), reactive(input$percentageFavor[2]), populationData, electionData)
  slice_ignoring_regional_filtering <- reactive({setDF(electionDataSubset())})

  #Conduct regional filtering and take appropiate slice
  current_data_slice <- reactive({
    if (input$state != "All") {
      if (input$county == "All" | input$county == "No State Selected") {
        electionDataRegional <- electionDataSubset()[
          state == input$state
        ]
      } else {
        electionDataRegional <- electionDataSubset()[
          state == input$state &
          FIPS == input$county
        ]
      }
    } else {
      electionDataRegional <- electionDataSubset()
    }
    setDF(electionDataRegional)
  })
  mapModule("mapBuilder", current_data_slice, slice_ignoring_regional_filtering)
  customGraphModule("customGraphBuilder", current_data_slice, reactive(input$customGraphType), reactive(input$customAxes), plotTheme(), plotMargin(), limitToMaxEligible(), totalVotes(), unionVotes(), unionVoteShare(), participationRate(), statLine())
  # Uncomment this block to enable the preset graph module
  # presetGraphModule("presetGraphBuilder", current_data_slice, reactive(input$customAxes), plotTheme(), plotMargin(), limitToMaxEligible(), totalVotes(), unionVotes(), unionVoteShare(), participationRate(), statLine())

  current_county_selection <- reactive({
    req(state_choices[input$state])
    stateCounties <- unique(current_data_slice()[current_data_slice()$state == input$state & !is.na(current_data_slice()$FIPS), c("county", "FIPS")])
    stateCounties <- stateCounties[order(stateCounties$county), ]
  })
  
  observeEvent(input$state, {
    if (input$state == 0 | input$state == "All") {
      default <- "No State Selected"
      countyDataframeToText <- c(
        "No State Selected",
        "All Rural Counties",
        "All Urban Counties"
      )
    } else {
      default <- "All"
      countyDataframeToText <- c(
        "All",
        "All Rural Counties",
        "All Urban Counties",
        setNames(
          current_county_selection()$FIPS,
          current_county_selection()$county
        )
      )
    }
    updateSelectInput(inputId = "county", choices = countyDataframeToText, selected = default)
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

  plotMargin <- function() {
    #return(theme(plot.background = element_rect(fill="#FCF9F6", color = "#FCF9F6"), plot.margin = unit(c(0.5,0,0,0), "cm")))
    return(theme(plot.background = element_rect(fill = "#FCF9F6", color = "#FCF9F6")))
  }
  
  plotTheme <- function() {
    #return(theme_ipsum_rc() + plotMargin())
    return(theme_minimal(base_family = "roboto_condensed") + plotMargin())
  }

  limitToMaxEligible <- function(){
    return(ylim(c(0, max(current_data_slice()$eligible))))
  }

  totalVotes <- function(){
    return(with(current_data_slice(), votes_for + votes_against))
  }

  unionVoteShare <- function() {
    return(with(current_data_slice(), (100 * votes_for/(votes_for + votes_against))))
  }

  participationRate <- function() {
    return(with(current_data_slice(), (100 * (votes_for + votes_against)/eligible)))
  }

  statLine <- function(func, color, alpha, show_guide) {
    if (missing(func)){
      func = "mean"
    }
    if (missing(color)){
      color = "black"
    }
    if (missing(alpha)) {
      alpha = 1
    }
    if (missing(show_guide)){
      show.legend = FALSE
    }
    return(stat_summary(fun.y = func, geom="line", color = color, alpha = alpha))
  }

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

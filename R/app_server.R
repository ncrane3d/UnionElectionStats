#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import mailR
#' @import shinyFeedback
#' @import sf
#' @import scales
#' @import htmltools
#' @import htmlwidgets
#' @import lubridate
#' @import glue
#' @import leafgl
#' @import viridis
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
#' @import fst
#' @import tidyr
#' @import forcats
#' @import ggoutlier
#' @import ggthemes
#' @import shinyalert
#' @noRd

#'

app_server <- function(input, output, session) {
  #popup at website start
  observeEvent(session, {
    showModal(modalDialog(
      title = "Welcome to Union Election Stats",
      p("This website is a user-friendly hub for data on National Labor Relations Board (NLRB) representation elections.") %>%
        p("Use the filter panel on the left to explore the history of union organizing through interactive visualizations.
          Use the Downloads tab to access the database containing the known universe of elections from 1962 to 2024.") %>%
        p("Read more about the project on the About page. Use of data requires citation."),
      easyClose = TRUE
    ))
  })


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
      } else if (input$county == "All Urban Counties" | input$county == "All Rural Counties") {
        electionDataRegional <- electionDataSubset()[
          state == input$state
        ]
      }else {
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

  #conducts calculations and data processing for graphing module
  current_data_slice_processed <- reactive({
    if(input$customGraphType == "LINE"){
      current_data_slice_processed <- current_data_slice() %>%
        group_by(year_closed) %>%
        filter(is.na(year_closed)==F)
      if(input$customAxes == "Eligible Employees" ){
        return(current_data_slice_processed %>% mutate(sum_eligible = sum(eligible, na.rm = T))
               %>% ungroup())
      }
      else if(input$customAxes == "Total Votes"){
        return(current_data_slice_processed %>% mutate(sum_votes = sum(votes_total, na.rm = T))
               %>% ungroup())
      }
      else if (input$customAxes == "Eligible per Election"){
        return(current_data_slice_processed %>%
                 filter(is.na(year_closed)==F) %>%
                 group_by(year_closed) %>%
                 summarize(avg_eligible = mean(eligible, na.rm=T),
                           med_eligible = median(eligible),
                           .groups = "drop") %>%
                 tidyr::pivot_longer(cols = c(avg_eligible,med_eligible),
                              names_to = "variable",
                              values_to = "stats"))
      }
      else if (input$customAxes == "Avg. Votes per Election"){
        return(current_data_slice_processed %>%
                 filter(is.na(year_closed)==F) %>%
                 group_by(year_closed) %>%
                 summarize(avgVotesAgainst = mean(votes_against,na.rm = T),
                           avgVotesFor = mean(votes_for,na.rm = T),
                           .groups = "drop") %>%
                 tidyr::pivot_longer(cols = c(avgVotesAgainst,avgVotesFor),
                              names_to = "variable",
                              values_to = "stats"))
      }
      else if (input$customAxes == "Avg. Union Vote Share"){
        return(current_data_slice_processed %>%
                 mutate(unionVoteShare =
                          (100 * sum(votes_for)/(sum(votes_for) + sum(votes_against)))) %>%
                 ungroup())
      }
      else if (input$customAxes =="Avg. Participation Rate") {
        return(current_data_slice_processed %>%
                 mutate(participationRate =
                          (100 * (sum(votes_for) + sum(votes_against))/sum(eligible))) %>%
                 ungroup())
      }
      else if (input$customAxes == "Union Win Rate"){
        return(current_data_slice_processed %>%
                 mutate(winRate = ifelse((votes_for/votes_total) > .5,1,0),
                        winRateCalc = (100*(sum(winRate, na.rm = T) / length(winRate)))))
      }
      else{
        return(current_data_slice_processed)
      }
    }
    else if (input$customGraphType == "HIST"){
      current_data_slice_processed <- current_data_slice()
      if (input$customAxes == "Petition Type"){
        return(current_data_slice_processed %>%
                 filter(is.na(petition)==F))
      }
      else if (input$customAxes == "Election Type"){
        return(current_data_slice_processed %>%
                 filter(is.na(election_type)==F)%>%
                 mutate(election_type = fct_relevel(election_type,"S", "R", "C", "B", "E")))
      }
      else if (input$customAxes == "Union Support"){
        return(current_data_slice_processed %>%
                 mutate(winRate = ifelse((votes_for/votes_total) > .5,1,0)) %>%
                 summarise(winRate = as.numeric(100*(sum(winRate,na.rm = T)/length(winRate))),
                           votesFor = 100*(sum(votes_for,na.rm = T)/sum(votes_total))) %>%
                 tidyr::pivot_longer(cols = c(winRate,votesFor),
                              names_to = "variable",
                              values_to = "stats"))
      }
      else if(input$customAxes == "Total Votes"){
        return(current_data_slice_processed %>%
                 group_by(case_number,unitID) %>%
                 filter(is.na(case_number)==F) %>%
                 filter(is.na(unitID)==F) %>%
                 mutate(sum_votes = sum(votes_total, na.rm = T)) %>%
                 ungroup()%>%
                 mutate(upperbound = as.numeric(summary(sum_votes)[5] + (1.5*IQR(sum_votes)))))
        #sum_votes = ifelse(sum_votes > upperbound,upperbound,sum_votes)))
      }
      else if(input$customAxes == "Union Vote Share"){
        current_data_slice_processed <- current_data_slice() %>%
          group_by(case_number,unitID) %>%
          filter(is.na(case_number)==F)%>%
          filter(is.na(unitID)==F)
        return(current_data_slice_processed %>%
                 mutate(unionVoteShare =
                          (100 * sum(votes_for)/(sum(votes_for) + sum(votes_against)))) %>%
                 ungroup())
      }
      else if(input$customAxes == "Participation Rate"){
        current_data_slice_processed <- current_data_slice() %>%
          group_by(case_number,unitID) %>%
          filter(is.na(case_number)==F)%>%
          filter(is.na(unitID)==F)
        return(current_data_slice_processed %>%
                 mutate(participationRate =
                          (100 * (sum(votes_for) + sum(votes_against))/sum(eligible))) %>%
                 ungroup())
      }
      else{
        return(current_data_slice_processed)
      }
    }
  })

  #helper method for labels
  dataLabel <- reactive({
    if (input$customAxes == "Election Type"){
      current_data_slice <- current_data_slice()
      return(current_data_slice %>%
               filter(is.na(election_type)==F) %>%
               summarise(electionTypeShareS = 100*(sum((election_type == "S"))/ length(election_type)),
                         electionTypeShareR = 100*(sum((election_type == "R"))/ length(election_type)),
                         electionTypeShareC = 100*(sum((election_type == "C"))/ length(election_type)),
                         electionTypeShareB = 100*(sum((election_type == "B"))/ length(election_type)),
                         electionTypeShareE = 100*(sum((election_type == "E"))/ length(election_type))) %>%
               tidyr::pivot_longer(cols = c(electionTypeShareS, electionTypeShareR, electionTypeShareC,electionTypeShareB,electionTypeShareE),
                            names_to = "variable",
                            values_to = "stats"))
    }
  })

  labelsDF <- reactive({
    yPosAdd <- sum(current_data_slice()$election_type == "S",na.rm = T) * .03
    return(data.frame(
      label_text = round(dataLabel()[,2],1),
      y_pos = c(sum(current_data_slice()$election_type == "S",na.rm = T)+yPosAdd,
                sum(current_data_slice()$election_type == "R",na.rm = T)+yPosAdd,
                sum(current_data_slice()$election_type == "C", na.rm = T)+yPosAdd,
                sum(current_data_slice()$election_type == "B",na.rm = T)+yPosAdd,
                sum(current_data_slice()$election_type == "E",na.rm = T)+yPosAdd),
      x_pos = c(1,2,3,4,5),
      election_type = c("S", "R", "C", "B", "E")))
  })

  mapModule("mapBuilder", current_data_slice, slice_ignoring_regional_filtering,
            reactive(input$showElections))

  customGraphModule("customGraphBuilder", current_data_slice_processed, labelsDF,
                    reactive(input$customGraphType),
                    reactive(input$customAxes), plotTheme(), plotMargin(),
                    PlotElementsSize(), statLine())

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
        "Avg. Union Vote Share",
        "Avg. Participation Rate",
        "Union Win Rate"
      )
      axisLabel <- "Select Y Axis"
    } else if (input$customGraphType == "HIST") {
      lineGraphChoices <- c(
        "Petition Type",
        "Election Type",
        "Total Votes",
        "Union Vote Share",
        "Union Support",
        "Participation Rate"
      )
      axisLabel <- "Select X Axis"
    }
    updateSelectInput(inputId = "customAxes", label = axisLabel, choices = lineGraphChoices)
  })

  observeEvent(input$industry, {
    if(input$industry == "All"){
      updateSliderInput(session, inputId = "timeframe", value = input$timeframe,
                        min = 1962, max = 2025)
    }
    if(input$industry != "All"){
      if(input$timeframe[2] <= 2010){
        updateSliderInput(session, inputId = "timeframe", value = input$timeframe,
                          min = 1962, max = 2010)
      }
      else if(input$timeframe[2] > 2010){
        updateSliderInput(session, inputId = "timeframe", value = c(1962,2010),
                          min = 1962, max = 2010)
      }
    }
  })

  plotMargin <- function(){
    return(ggthemes::theme_calc() + theme(plot.background = element_rect(fill = "#F2F4FA", color = "#F2F4FA"),
                                panel.background = element_rect(fill = 'white'),
                                panel.grid.major.y = element_line("gray")))
  }



  plotTheme <- function() {
    return(theme_minimal(base_family = "roboto_condensed") + plotMargin())
  }

  #changes the size of tick marks and x/y titles
  PlotElementsSize <- function() {
    return(theme(axis.text.x = element_text(size = 12),
                 axis.text.y = element_text(size = 12),
                 axis.title.x = element_text(size = 15),
                 axis.title.y = element_text(size = 15),
                 plot.title = element_text(size = 18),
                 plot.subtitle = element_text(size = 11)))
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
    req("./inst/app/www/resources/csv/featured-analysis.csv")
    faCSV <- read.csv("./inst/app/www/resources/csv/featured-analysis.csv")
    faCSV <- data.frame(id = 1:nrow(faCSV), faCSV)
    for (i in 1:nrow(faCSV)) {
      local({
        ii <- i #Without this line only the last loop will be kept.
        output[[paste0("figure", ii)]] <-
          renderImage({
            list(src = faCSV$imagePath[ii], width = "100%", height ="auto")
          }, deleteFile = FALSE)
      })
    }
  })

  output$insertFeaturedAnalysisFromCSV <- renderUI ({
    req("./inst/app/www/resources/csv/featured-analysis.csv")
    faCSV <- read.csv("./inst/app/www/resources/csv/featured-analysis.csv")
    faCSV <- data.frame(id = 1:nrow(faCSV), faCSV)
    panels <- lapply(1:nrow(faCSV), function(i) {
      if (faCSV$imagePath[i] != "") {
        createFeaturedAnalysisAccordionWithImage(faCSV, i)
      } else {
        createFeaturedAnalysisAccordionNoImage(faCSV, i)
      }
    })

    accordion(
      !!!panels,
      open = 1
    )
  })

  createFeaturedAnalysisAccordionWithImage <- function(faCSV, i) {
    accordion_panel(
      title = faCSV$title[i],
      collapsed = FALSE,
      div(
        align = "left",
        div(strong("Author(s): "), p(paste(faCSV$author[i], collapse = ", "))),
        div(class="abstract-row",
            div(class="abstract-text", strong("Abstract: "), p(faCSV$abstract[i])),
            div(class="abstract-figure", strong("Featured Figure: "), div(imageOutput(paste0("figure", i)) %>% tagAppendAttributes(class = "accordion-figure"), align = "center")),
        ),
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

    if(!(input$name == "" || input$email == "" || input$subject == "" || input$message == "")) {
      gmail_pass <- Sys.getenv("GMAIL_PASS")
      send.mail(
        from = "unionelectionstats@gmail.com",
        to = "unionelectionstats@gmail.com",
        subject = isolate(input$subject),
        body = glue(isolate(input$message), "\n\nEmail sent by: ", isolate(input$name), "\n\nEmail address provided: ", isolate(input$email)),
        smtp = list(
          host.name = "smtp.gmail.com",
          port = 587,
          user.name="unionelectionstats@gmail.com",
          passwd = gmail_pass,
          tls=TRUE
        ),
        authenticate = TRUE,
        send = TRUE)

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

  session$onSessionEnded(function() {
    # Remove leaflet layers explicitly
    try(leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% clearControls(), silent = TRUE)

    # Clear stored reactive data
    slice_ignoring_regional_filtering <- NULL
    current_data_slice <- NULL

    # Run garbage collection
    gc(full = TRUE)
  })
}

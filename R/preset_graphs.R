customGraphModule <- function(id, current_data_slice, customAxesInput) {  
  moduleServer(
    id,
    function(input, output, session) {
      far_west = c("WA", "OR", "NV", "CA", "HI", "AK")
      rocky_mountain = c("MT", "ID", "WY", "UT", "CO")
      southwest = c("AZ", "NM", "TX", "OK")
      plains = c("ND", "SD", "MN", "NE", "IA", "KS", "MO")
      great_lakes = c("WI", "MI", "IL", "IN", "OH")
      mideast = c("NY", "PA", "NJ", "DE", "MD")
      new_england = c("ME", "NH", "VT", "MA", "RI", "CT")
      southeast = c("KY", "AR", "IN", "WV", "TN", "VA", "DC", "NC", "SC", "GA", "AL", "MS", "LA", "FL", "GU")

      getIndustryData <- function(){
          industry_data = current_data_slice()
          industry_data$SIC[current_data_slice()$SIC < 10] <- "Agriculture, Forestry, & Fishing"
          industry_data$SIC[current_data_slice()$SIC >= 10 & current_data_slice()$SIC < 15] <- "Mining"
          industry_data$SIC[current_data_slice()$SIC >= 15 & current_data_slice()$SIC < 20] <- "Construction" 
          industry_data$SIC[current_data_slice()$SIC >= 20 & current_data_slice()$SIC < 40] <- "Manufacturing"
          industry_data$SIC[current_data_slice()$SIC >= 40 & current_data_slice()$SIC < 50] <- "Communications,\nElectric, Gas,\nand Sanitary\nServices"
          industry_data$SIC[current_data_slice()$SIC >= 50 & current_data_slice()$SIC < 52] <- "Wholesale Trade"
          industry_data$SIC[current_data_slice()$SIC >= 52 & current_data_slice()$SIC < 60] <- "Retail Trade"
          industry_data$SIC[current_data_slice()$SIC >= 60 & current_data_slice()$SIC < 68] <- "Finance, Insurance,\nand Real Estate"
          industry_data$SIC[current_data_slice()$SIC >= 68 & current_data_slice()$SIC < 90] <- "Services"
          industry_data$SIC[current_data_slice()$SIC >= 90 & current_data_slice()$SIC < 100] <- "Public\nAdministration"
          industry_data$ac[current_data_slice()$SIC < 10] <- "AFF"
          industry_data$ac[current_data_slice()$SIC >= 10 & current_data_slice()$SIC < 15] <- "Mining"
          industry_data$ac[current_data_slice()$SIC >= 15 & current_data_slice()$SIC < 20] <- "Construction" 
          industry_data$ac[current_data_slice()$SIC >= 20 & current_data_slice()$SIC < 40] <- "Manufacturing"
          industry_data$ac[current_data_slice()$SIC >= 40 & current_data_slice()$SIC < 50] <- "CEGSS"
          industry_data$ac[current_data_slice()$SIC >= 50 & current_data_slice()$SIC < 52] <- "Wholesale Trade"
          industry_data$ac[current_data_slice()$SIC >= 52 & current_data_slice()$SIC < 60] <- "Retail Trade"
          industry_data$ac[current_data_slice()$SIC >= 60 & current_data_slice()$SIC < 68] <- "FIRE"
          industry_data$ac[current_data_slice()$SIC >= 68 & current_data_slice()$SIC < 90] <- "Services"
          industry_data$ac[current_data_slice()$SIC >= 90 & current_data_slice()$SIC < 100] <- "PA"
          return(industry_data)
      }

      getIndustryBreakdown <- function(){
          industry_data = getIndustryData()
          #gg <- ggplot(data.frame(industry_data$ac), aes(x=industry_data$ac, fill = industry_data$ac)) + geom_bar_interactive(aes(tooltip= industry_data$SIC, data_id=industry_data$SIC), hover_nearest = TRUE) + scale_fill_viridis_d_interactive( guide= "none") + labs(x = "Industry", y = "Frequency") + plotMargin()
          #return(girafe_options(girafe(ggobj = gg), opts_sizing(rescale = TRUE, width = 1), opts_toolbar(position="top", saveaspng = FALSE, hidden=c("selection", "zoom", "misc"))))
          return (ggplot(data.frame(industry_data$ac), aes(x=industry_data$ac, fill = industry_data$ac)) + geom_bar() + scale_fill_viridis_d() + labs(x = "Industry", y = "Frequency") + plotTheme() + plotMargin())
      }

      getRegionalBreakdown <- function(){
          getRegion <- function(x){
              if (x == "1") {
                  return("TEST")
              } else {
                  return(x)
              }
          }
          regional_data = current_data_slice()
          regional_data$state[regional_data$state %in% far_west] <- "Far West"
          regional_data$state[regional_data$state %in% rocky_mountain] <- "Rocky Mountain"
          regional_data$state[regional_data$state %in% southwest] <- "Southwest"
          regional_data$state[regional_data$state %in% plains] <- "Plains"
          regional_data$state[regional_data$state %in% great_lakes] <- "Great Lakes"
          regional_data$state[regional_data$state %in% mideast] <- "Mideast"
          regional_data$state[regional_data$state %in% new_england] <- "New England"
          regional_data$state[regional_data$state %in% southeast] <- "Southeast"
          #gg <- ggplot(data.frame(regional_data$state), aes(x=regional_data$state, fill = regional_data$state)) + geom_bar_interactive(aes(tooltip= regional_data$state, data_id=regional_data$state), hover_nearest = TRUE) + scale_fill_viridis_d_interactive(guide= "none") + labs(x = "Region", y = "Frequency") + plotMargin()
          #return(girafe_options(girafe(ggobj = gg), opts_sizing(rescale = TRUE, width = 1), sizingPolicy(defaultWidth = "100%", defaultHeight = "100%"), opts_toolbar(position="top", saveaspng = FALSE, hidden=c("selection", "zoom", "misc"))))
          return (ggplot(data.frame(regional_data$state), aes(x=regional_data$state, fill = regional_data$state)) + geom_bar() + scale_fill_viridis_d() + labs(x = "Region", y = "Frequency") + plotTheme() + plotMargin())
      }
      getUnitTypeGraph <- function(){
          return(ggplot(data.frame(current_data_slice()$unit), aes(x=current_data_slice()$unit, fill= current_data_slice()$unit)) + geom_bar() + scale_fill_viridis_d() + labs(x = "Unit Type", y = "Frequency") + plotTheme() + theme(legend.position="none"))
      }

      getElectionTypeGraph <- function(){
          election_type_data = current_data_slice()
          election_type_data$election_type[election_type_data$election_type == "C"] <- "Consent"
          election_type_data$election_type[election_type_data$election_type == "S"] <- "Stipulated"
          election_type_data$election_type[election_type_data$election_type == "R"] <- "Regional Director\nOrdered"
          election_type_data$election_type[election_type_data$election_type == "E"] <- "Expedited"
          election_type_data$election_type[election_type_data$election_type == "B"] <- "Board Ordered"
          return(ggplot(election_type_data, aes(x = year_closed, color = election_type, group = election_type)) + geom_freqpoly_interactive(aes(tooltip="test", data_id="test")) + scale_colour_viridis_d() + labs(x = "Year Closed", y = "Frequency") + plotTheme())
      }

      getHeatmap <- function(){
        heatmap_data <- current_data_slice()

        # Replace empty strings with NA and parse dates
        for(col in c("filed_date", "election_date", "closed_date")) {
          heatmap_data[[col]][heatmap_data[[col]] == ""] <- NA
          heatmap_data[[col]] <- suppressWarnings(ymd(heatmap_data[[col]]))
        }

        heatmap_data$filing_to_elec <- with(heatmap_data,
          ifelse(!is.na(filed_date) & !is.na(election_date),
                as.numeric(difftime(election_date, filed_date, units = "days")) / 30, NA_real_))
        heatmap_data$elec_to_close <- with(heatmap_data,
          ifelse(!is.na(election_date) & !is.na(closed_date),
                as.numeric(difftime(closed_date, election_date, units = "days")) / 30, NA_real_))

        return(
        ggplot(na.omit(heatmap_data[, c("filing_to_elec", "elec_to_close")]),
              aes(x = filing_to_elec, y = elec_to_close)) +
          geom_bin2d() +
          xlim(0, max(heatmap_data$filing_to_elec, na.rm = TRUE)) +
          ylim(0, max(heatmap_data$elec_to_close, na.rm = TRUE)) +
          labs(x = "Filing to Election (Months)", y = "Election to Close (Months)") +
          scale_fill_viridis_b(direction = -1) +
          plotTheme()
        )
      }

      getLineGraph <- function(){
          industry_data = getIndustryData()
          if (input$customAxes == "Elections") {
            return(ggplot(current_data_slice(), aes(x = year_closed, color = election_type, group = election_type)) +
          geom_line(stat="bin", binwidth=1)+ scale_colour_viridis_d() + plotTheme())
          } else if (input$customAxes == "Eligible Employees") {
            return(ggplot(current_data_slice(), aes(x= year_closed, y = current_data_slice()$eligible, color = election_type, group = election_type)) + geom_line_interactive() + scale_colour_viridis_d_interactive(guide = "none") + limitToMaxEligible() + plotTheme())
          } else if (input$customAxes == "Total Votes") {
            return(ggplot(current_data_slice(), aes(x= year_closed, y = totalVotes(), color = election_type, group = election_type)) + geom_line_interactive() + limitToMaxEligible() + plotTheme())
          } else if (input$customAxes == "Eligible per Election") {
            return(ggplot(current_data_slice(), aes(x = year_closed, y = current_data_slice()$eligible, color = election_type, group = election_type)) + stat_summary(fun.y="mean", geom="line", alpha=0.5, show.legend=TRUE) + stat_summary(fun.y="median", geom="line", alpha=0.5, show.legend=TRUE) + limitToMaxEligible() + plotTheme())
          } else if (input$customAxes == "Avg. Votes per Election") {
            return(ggplot(current_data_slice(), aes(x = year_closed, y = totalVotes(), color = election_type, group = election_type)) + statLine() + limitToMaxEligible() + plotTheme())
          } else if (input$customAxes == "Avg. Votes For Union") {
            return(ggplot(current_data_slice(), aes(x = year_closed, y = current_data_slice()$votes_for, color = election_type, group = election_type)) + statLine() + limitToMaxEligible() + plotTheme())
          } else if (input$customAxes == "Avg. Votes Against Union") {
            return(ggplot(current_data_slice(), aes(x = year_closed, y = current_data_slice()$votes_against, color = election_type, group = election_type)) + statLine() + limitToMaxEligible() + plotTheme())
          } else if (input$customAxes == "Avg. Union Vote Share") {
            return(ggplot(current_data_slice(), aes(x = year_closed, y = unionVoteShare(), color = election_type, group = election_type)) + statLine() + plotTheme())
          } else if (input$customAxes == "Avg. Participation Rate") {
            return(ggplot(current_data_slice(), aes(x = year_closed, y = participationRate(), color = election_type, group = election_type)) + statLine() + plotTheme())
          } else {
            return()
          }
          return(ggplot(current_data_slice(), aes(x = year_closed, y = yAxis, color = election_type, group = election_type)) +
          geom_line() + plotTheme())
      }

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
    }
  )
}  


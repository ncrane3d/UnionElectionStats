customGraphModule <- function(id, current_data_slice, graphTypeInput, customAxesInput, plotTheme, plotMargin, limitToMaxEligible, totalVotes, unionVotes, unionVoteShare, participationRate, statLine) {  
  moduleServer(
    id,
    function(input, output, session) {
      customLineGraphVariableHandler <- function() {
        customAxes <- customAxesInput()
        if (customAxes == "Elections") {
          return(ggplot(current_data_slice(), aes(x = year_closed)) +
        geom_line(aes(fill=..count..), stat="bin", binwidth=1))
        } else if (customAxes == "Eligible Employees") {
          return(ggplot(current_data_slice(), aes(x= year_closed, y = current_data_slice()$eligible)) + geom_line() + limitToMaxEligible)
        } else if (customAxes == "Total Votes") {
          return(ggplot(current_data_slice(), aes(x= year_closed, y = totalVotes)) + geom_line() + limitToMaxEligible)
        } else if (customAxes == "Eligible per Election") {
          return(ggplot(current_data_slice(), aes(x = year_closed, y = current_data_slice()$eligible)) + stat_summary(aes(color="Mean"), fun.y="mean", geom="line", alpha=0.5, show.legend=TRUE) + stat_summary(aes(color="Median"), fun.y="median", geom="line", alpha=0.5, show.legend=TRUE) + limitToMaxEligible + scale_color_manual(values = c("Mean" = "black", "Median" = "purple"), labels = c("Mean Eligible", "Median Eligible")))
        } else if (customAxes == "Avg. Votes per Election") {
          return(ggplot(current_data_slice(), aes(x = year_closed, y = totalVotes)) + statLine + limitToMaxEligible)
        } else if (customAxes == "Avg. Votes For Union") {
          return(ggplot(current_data_slice(), aes(x = year_closed, y = current_data_slice()$votes_for)) + statLine + limitToMaxEligible)
        } else if (customAxes == "Avg. Votes Against Union") {
          return(ggplot(current_data_slice(), aes(x = year_closed, y = current_data_slice()$votes_against)) + statLine + limitToMaxEligible)
        } else if (customAxes == "Avg. Union Vote Share") {
          return(ggplot(current_data_slice(), aes(x = year_closed, y = unionVoteShare)) + statLine)
        } else if (customAxes == "Avg. Participation Rate") {
          return(ggplot(current_data_slice(), aes(x = year_closed, y = participationRate)) + statLine)
        } else {
          return()
        }
        return(ggplot(current_data_slice(), aes(x = year_closed, y = yAxis)) +
        geom_line())
      }

      customHistogramVariableHandler <- function() {
        customAxes <- customAxesInput()
        if (customAxes == "Petition Type") {
          return(ggplot(data.frame(current_data_slice()$petition), aes(x=current_data_slice()$petition, fill = current_data_slice()$petition)) + geom_bar() + scale_fill_viridis_d() + theme(legend.position="none"))
        } else if (customAxes == "Election Type") {
          return(ggplot(data.frame(current_data_slice()$election_type), aes(x=current_data_slice()$election_type, fill = current_data_slice()$election_type)) + geom_bar() + scale_fill_viridis_d() + theme(legend.position="none"))
        } else if (customAxes == "Votes For/Against Union") {
          return(ggplot(current_data_slice()) + geom_histogram(aes(x = current_data_slice()$votes_for, color = "For"), alpha = 0.5, bins = 30, show.legend=TRUE) + geom_histogram(aes(x = current_data_slice()$votes_against, color ="Against"), alpha = 0.5, bins = 30, show.legend=TRUE) + scale_colour_manual("", breaks= c("For", "Against"), values = c("For"="purple", "Against"="black")) + xlab(" "))
        } else if (customAxes == "Total Votes") {
          xAxis <- totalVotes
        } else if (customAxes == "Union Vote Share") {
          xAxis <- unionVoteShare
        } else if (customAxes == "Participation Rate") {
          xAxis <- participationRate
        } else {
          return()
        }
        return(ggplot(current_data_slice(), aes(x = xAxis)) + geom_histogram(bins = 30))
      }

      output$customVisualization <- renderPlot ({
        graphType <- graphTypeInput()
        customAxes <- customAxesInput()
        if (graphType == "LINE") {
          req(customLineGraphVariableHandler())
          customLineGraphVariableHandler() + labs(x = "Year Election Closed", y = customAxes) + plotTheme
        } else if (graphType == "HIST") {
          req(customHistogramVariableHandler())
          customHistogramVariableHandler() + labs(x = customAxes, y = "Frequency") + plotTheme
        }
      })
    }
  )
}  

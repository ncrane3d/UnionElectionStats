customGraphModule <- function(id, current_data_slice_processed,
                              labelsDFElection, labelsDFPetition,
                              graphTypeInput, customAxesInput,
                              plotTheme, plotMargin, PlotElementsSize,statLine) {
  moduleServer(
    id,
    function(input, output, session) {
      customLineGraphVariableHandler <- function() {
        customAxes <- customAxesInput()
        if (customAxes == "Elections") {
          return(ggplot(current_data_slice_processed(), aes(x = year_closed)) +
                   geom_line(aes(y = after_stat(count)), stat = "count")+
                labs(y = "Cases Closed",
                     title = customAxes))
        } else if (customAxes == "Eligible Employees") {
          return(ggplot(current_data_slice_processed(),aes(x=year_closed,y=sum_eligible))+
                   geom_line()+
                   scale_y_continuous(limits = c(0,max(current_data_slice_processed()$sum_eligible,na.rm = TRUE)))+
                   labs(y = "Employees",
                        title = "Sum of Eligible Employees Across Elections")
          )
        } else if (customAxes == "Total Votes") {
          return(ggplot(current_data_slice_processed(), aes(x = year_closed,y=sum_votes))+
                   geom_line()+
                   scale_y_continuous(limits = c(0,max(current_data_slice_processed()$sum_votes,na.rm = TRUE)))+
                   labs(y = customAxes,
                        title = "Sum of Votes Across Elections")
          )
        } else if (customAxes == "Eligible per Election") {
          return(ggplot(current_data_slice_processed(), aes(x=year_closed, y=stats, colour=variable)) +
                   geom_line()+
                   scale_y_continuous(limits = c(0,max(current_data_slice_processed()$stats,na.rm = TRUE)))+
                   scale_color_manual(values = c("#471164FF","#22A884FF"), labels = (labels = c("Mean", "Median")))+
                   labs(y = "Employees",
                        title = "Eligible Employees per Election"))
        } else if (customAxes == "Avg. Votes per Election") {
          return(ggplot(current_data_slice_processed(), aes(x=year_closed, y= stats, colour = variable))+
                   geom_line()+
                   scale_y_continuous(limits = c(0,max(current_data_slice_processed()$stats,na.rm = TRUE)))+
                   scale_color_manual(values = c("#471164FF","#22A884FF"), labels = (labels = c("Against Union", "For Union")))+
                   labs(y = customAxes,
                        title = "Breakdown of Votes"))
        } else if (customAxes == "Avg. Union Vote Share") {
          return(ggplot(current_data_slice_processed(), aes(x=year_closed,y= unionVoteShare))+
                   geom_line()+
                   scale_y_continuous(limits = c(0,100))+
                   labs(y = "(%) of Votes for a Union",
                        title = customAxes))
        } else if (customAxes == "Avg. Participation Rate") {
          return(ggplot(current_data_slice_processed(), aes(x=year_closed,y= participationRate))+
                   geom_line()+
                   labs(y = "(%) of Eligible Who Voted",
                        title = customAxes))
        } else if (customAxes == "Union Win Rate") {
          return(ggplot(current_data_slice_processed(), aes(x = year_closed, y = winRateCalc))+
                   geom_line()+
                   scale_y_continuous(limits = c(0,100))+
                   labs(y = "(%) Won By Unions",
                        title = customAxes))
        }
        else{
          return()
        }
        return(ggplot(current_data_slice_processed(), aes(x = year_closed, y = yAxis)) +
                 geom_line())
      }

      customHistogramVariableHandler <- function() {
        customAxes <- customAxesInput()
        if (customAxes == "Petition Type") {
          return(ggplot(data.frame(current_data_slice_processed()$petition),
                        aes(x=current_data_slice_processed()$petition,
                            fill = current_data_slice_processed()$petition)) +
                   geom_bar(color = "black") +
                   geom_text(data = labelsDFPetition(), aes(x = x_pos, y = y_pos, label = paste(stats,"%"), fill = petition_type),
                             position = position_dodge(width = 0.9), vjust = -.5, size = 11 / .pt)+
                   scale_fill_manual(values = c( "#A5DB36FF", "#31688EFF","#481A6CFF"))+
                   labs(title = customAxes,
                        x = NULL,
                        y = "Frequency"))
        } else if (customAxes == "Election Type") {
          return(ggplot(data.frame(current_data_slice_processed()$election_type),
                        aes(x=current_data_slice_processed()$election_type,
                            fill = current_data_slice_processed()$election_type)) +
                   geom_bar(position = position_dodge(width = 0.9), color = "black") +
                   geom_text(data = labelsDFElection(), aes(x = x_pos, y = y_pos, label = paste(stats,"%"), fill = election_type),
                             position = position_dodge(width = 0.9), vjust = -.5, size = 11 / .pt)+
                   scale_fill_manual(values = c("#A5DB36FF","#22A884FF", "#31688EFF","#414487FF","#481A6CFF")) +
                   scale_x_discrete(labels = c("Stipulation", "Regional Director", "Consent",
                                               "Board ordered", "Expedited "))+
                   labs(title = customAxes,
                        x = NULL,
                        y = "Frequency",
                        subtitle = "With Shares")
          )
        } else if (customAxes == "Union Support") {
          return(ggplot(current_data_slice_processed(), aes(x=variable, y = stats, fill = variable))+
                   geom_bar(stat = "identity",color = "black")+
                   theme(legend.position="none")+
                   scale_y_continuous(labels = scales::comma)+
                   scale_fill_manual(values = c("#414487FF","#22A884FF"))+
                   scale_x_discrete(labels = c("Votes For", "Win Rate"))+
                   labs(x = NULL, y = "Percent", title = "Union Support"))
        }
        else if (customAxes == "Total Votes") {
          return(ggoutlier::ggoutlier_hist(current_data_slice_processed(), "sum_votes",
                                cut_off_ceiling = current_data_slice_processed()$upperbound[1],
                                fill = "#39568CFF", fill_outlier_bins = "#A5DB36FF", binwidth = 10)+
                   labs(x = "Total Votes per Election", title = "Distribution of Total Votes per Election",
                        y = "Frequency", subtitle = "With Outlier Bin"))
        } else if (customAxes == "Union Vote Share") {
          return(ggplot(current_data_slice_processed(), aes(x = unionVoteShare))+
                   geom_histogram(bins = 15,fill = "#39568CFF", color = "black")+
                   labs(title = customAxes,
                        x = customAxes,
                        y = "Frequency"))
        } else if (customAxes == "Participation Rate") {
          return(ggplot(current_data_slice_processed(), aes(x = participationRate))+
                   geom_histogram(bins = 15,fill = "#39568CFF", color = "black")+
                   labs(title = customAxes,
                        x = "(%) of Eligible Who Voted",
                        y = "Frequency"))
        } else {
          return()
        }
        return(ggplot(current_data_slice_processed(), aes(x = xAxis)) + geom_histogram(bins = 15))
      }

      output$customVisualization <- renderPlot ({
        graphType <- graphTypeInput()
        customAxes <- customAxesInput()
        if (graphType == "LINE") {
          req(customLineGraphVariableHandler())
          customLineGraphVariableHandler() + labs(x = "Year",
                                                  caption = "Source: unionelectionstats.com")+
            scale_y_continuous(labels = label_comma())+
            plotTheme +
            PlotElementsSize +
            theme(legend.position = "bottom", legend.title = element_blank(),
                  legend.direction = "horizontal",  panel.grid.major.x = element_line(color = "gray"))

        } else if (graphType == "HIST") {
          req(customHistogramVariableHandler())
          customHistogramVariableHandler() + labs(caption = "Source: unionelectionstats.com") +
            scale_y_continuous(labels = label_comma())+
            plotTheme + PlotElementsSize + theme(legend.position = "none")
        }
      })
    }
  )
}

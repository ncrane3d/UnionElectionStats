customGraphModule <- function(id, current_data_slice_processed, labelsDF,
                              graphTypeInput,
                              customAxesInput, plotTheme, plotMargin, PlotElementsSize,
                              statLine) {
  moduleServer(
    id,
    function(input, output, session) {
      customLineGraphVariableHandler <- function() {
        customAxes <- customAxesInput()
        if (customAxes == "Elections") {
          return(ggplot(current_data_slice_processed(), aes(x = year_closed)) +
                   geom_line(aes(y = after_stat(count)), stat = "count"))
        } else if (customAxes == "Eligible Employees") {
          return(ggplot(current_data_slice_processed(),aes(x=year_closed,y=sum_eligible))+
                   geom_line()+
                   scale_y_continuous(labels = scales::comma,
                                      limits = c(0,max(current_data_slice_processed()$sum_eligible,na.rm = TRUE)))
          )
        } else if (customAxes == "Total Votes") {
          return(ggplot(current_data_slice_processed(), aes(x = year_closed,y=sum_votes))+
                   geom_line()+
                   scale_y_continuous(labels = scales::comma,
                                      limits = c(0,max(current_data_slice_processed()$sum_votes,na.rm = TRUE)))
          )
        } else if (customAxes == "Eligible per Election") {
          return(ggplot(current_data_slice_processed(), aes(x=year_closed, y=stats, colour=variable)) +
                   geom_line()+
                   scale_y_continuous(labels = scales::comma,
                                      limits = c(0,max(current_data_slice_processed()$stats,na.rm = TRUE)))+
                   scale_color_manual(values = c("#471164FF","#22A884FF"), labels = (labels = c("Mean", "Median"))))
        } else if (customAxes == "Avg. Votes per Election") {
          return(ggplot(current_data_slice_processed(), aes(x=year_closed, y= stats, colour = variable))+
                   geom_line()+
                   scale_y_continuous(labels = scales::comma,
                                      limits = c(0,max(current_data_slice_processed()$stats,na.rm = TRUE)))+
                   scale_color_manual(values = c("#471164FF","#22A884FF"), labels = (labels = c("Against", "For"))))
        } else if (customAxes == "Avg. Union Vote Share") {
          return(ggplot(current_data_slice_processed(), aes(x=year_closed,y= unionVoteShare))+
                   geom_line()+
                   scale_y_continuous(labels = scales::comma,
                                      limits = c(0,max(current_data_slice_processed()$unionVoteShare,na.rm = TRUE)))
          )
        } else if (customAxes == "Avg. Participation Rate") {
          return(ggplot(current_data_slice_processed(), aes(x=year_closed,y= participationRate))+
                   geom_line())
        } else if (customAxes == "Avg. Win Rate") {
          return(ggplot(current_data_slice_processed(), aes(x = year_closed, y = winRateCalc))+
                   geom_line()+
                   scale_y_continuous(labels = scales::comma,
                                      limits = c(0,max(current_data_slice_processed()$winRateCalc,na.rm = TRUE))))
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
                   geom_bar() +
                   scale_fill_manual(values = c("#481A6CFF", "#31688EFF", "#A5DB36FF"))+
                   labs(title = customAxes,
                        x = NULL,
                        y = "Frequency"))
        } else if (customAxes == "Election Type") {
          return(ggplot(data.frame(current_data_slice_processed()$election_type),
                        aes(x= forcats::fct_infreq(current_data_slice_processed()$election_type),
                            fill = current_data_slice_processed()$election_type)) +
                   geom_bar(position = position_dodge(width = 0.9)) +
                   geom_text(data = labelsDF(), aes(x = x_pos, y = y_pos, label = stats, fill = election_type),
                             position = position_dodge(width = 0.9), vjust = -.5)+
                   scale_fill_manual(values = c("#481A6CFF","#414487FF", "#31688EFF","#22A884FF","#A5DB36FF")) +
                   scale_x_discrete(labels = c("Stipulation", "Regional Director", "Consent",
                                               "Board ordered", "Expedited "))+
                   labs(title = customAxes,
                        x = NULL,
                        y = "Frequency")
          )
        } else if (customAxes == "Union Support") {
          return(ggplot(current_data_slice_processed(), aes(x=variable, y = stats, fill = variable))+
                   geom_bar(stat = "identity")+
                   theme(legend.position="none")+
                   scale_y_continuous(labels = scales::comma)+
                   scale_fill_manual(values = c("#414487FF","#22A884FF"))+
                   scale_x_discrete(labels = c("Votes For", "Win Rate"))+
                   labs(x = NULL, y = "Percent", title = "Union Support"))
        }
        else if (customAxes == "Total Votes") {
          return(ggoutlier_hist(current_data_slice_processed(), "sum_votes",
                                cut_off_ceiling = current_data_slice_processed()$upperbound[1],
                                fill = "#39568CFF", fill_outlier_bins = "#A5DB36FF", binwidth = 10)+
                   labs(x = "Total Votes per Election", title = "Distribution of Total Votes per Election",
                        y = "Frequency", subtitle = "With Outlier Bin"))
        } else if (customAxes == "Union Vote Share") {
          return(ggplot(current_data_slice_processed(), aes(x = unionVoteShare))+
                   geom_histogram(bins = 15,fill = "#39568CFF")+
                   labs(title = customAxes,
                        x = customAxes,
                        y = "Frequency"))
        } else if (customAxes == "Participation Rate") {
          return(ggplot(current_data_slice_processed(), aes(x = participationRate))+
                   geom_histogram(bins = 15,fill = "#39568CFF")+
                   labs(title = customAxes,
                        x = customAxes,
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
          customLineGraphVariableHandler() + labs(title = customAxes,
                                                  x = "Year Election Closed",
                                                  y = customAxes,
                                                  caption = "Source: unionelectionstats.com")+
            plotTheme +
            PlotElementsSize +
            theme(legend.position = "bottom", legend.title = element_blank(),
                  legend.direction = "horizontal",  panel.grid.major.x = element_line(color = "gray"))

        } else if (graphType == "HIST") {
          req(customHistogramVariableHandler())
          customHistogramVariableHandler() + labs(caption = "Source: unionelectionstats.com") +
            plotTheme + PlotElementsSize + theme(legend.position = "none")
        }
      })
    }
  )
}

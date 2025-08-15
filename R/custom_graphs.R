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
      show_guide = FALSE
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
      return(ggplot(current_data_slice(), aes(x = yrclosed, y = current_data_slice()$eligible)) + stat_summary(aes(color="Mean"), fun.y="mean", geom="line", alpha=0.5, show_guide=TRUE) + stat_summary(aes(color="Median"), fun.y="median", geom="line", alpha=0.5, show_guide=TRUE) + limitToMaxEligible() + scale_color_manual(values = c("Mean" = "black", "Median" = "purple"), labels = c("Mean Eligible", "Median Eligible")))
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
      return(ggplot(current_data_slice()) + geom_histogram(aes(x = current_data_slice()$votes_for, color = "For"), alpha = 0.5, bins = 30, show_guide=TRUE) + geom_histogram(aes(x = current_data_slice()$votes_against, color ="Against"), alpha = 0.5, bins = 30, show_guide=TRUE) + scale_colour_manual("", breaks= c("For", "Against"), values = c("For"="purple", "Against"="black")) + xlab(" "))
    } else if (input$customAxes == "Total Votes") {
      xAxis <- totalVotes()
    } else if (input$customAxes == "Union Vote Share") {
      xAxis <- unionVoteShare()
    } else if (input$customAxes == "Participation Rate") {
      xAxis <- participationRate()
    } else {
      return()
    }
    return(ggplot(current_data_slice(), aes(x = xAxis)) + geom_histogram(bins = 30))
  }
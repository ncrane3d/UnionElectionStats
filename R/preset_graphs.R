far_west = c("WA", "OR", "NV", "CA", "HI", "AK")
rocky_mountain = c("MT", "ID", "WY", "UT", "CO")
southwest = c("AZ", "NM", "TX", "OK")
plains = c("ND", "SD", "MN", "NE", "IA", "KS", "MO")
great_lakes = c("WI", "MI", "IL", "IN", "OH")
mideast = c("NY", "PA", "NJ", "DE", "MD")
new_england = c("ME", "NH", "VT", "MA", "RI", "CT")
southeast = c("KY", "AR", "IN", "WV", "TN", "VA", "DC", "NC", "SC", "GA", "AL", "MS", "LA", "FL")
getIndustryData <- function(){
    industry_data = current_data_slice()
    industry_data$sic2[current_data_slice()$sic2 < 10] <- "Agriculture, Forestry, & Fishing"
    industry_data$sic2[current_data_slice()$sic2 >= 10 & current_data_slice()$sic2 < 15] <- "Mining"
    industry_data$sic2[current_data_slice()$sic2 >= 15 & current_data_slice()$sic2 < 20] <- "Construction" 
    industry_data$sic2[current_data_slice()$sic2 >= 20 & current_data_slice()$sic2 < 40] <- "Manufacturing"
    industry_data$sic2[current_data_slice()$sic2 >= 40 & current_data_slice()$sic2 < 50] <- "Communications, Electric, Gas, and Sanitary Services"
    industry_data$sic2[current_data_slice()$sic2 >= 50 & current_data_slice()$sic2 < 52] <- "Wholesale Trade"
    industry_data$sic2[current_data_slice()$sic2 >= 52 & current_data_slice()$sic2 < 60] <- "Retail Trade"
    industry_data$sic2[current_data_slice()$sic2 >= 60 & current_data_slice()$sic2 < 68] <- "FIRE"
    industry_data$sic2[current_data_slice()$sic2 >= 68 & current_data_slice()$sic2 < 90] <- "Services"
    industry_data$sic2[current_data_slice()$sic2 >= 90 & current_data_slice()$sic2 < 100] <- "Public Administration"
    return(industry_data)
}
getIndustryBreakdown <- function(){
    industry_data = getIndustryData()
    return(ggplot(data.frame(industry_data$sic2), aes(x=industry_data$sic2)) + geom_bar() + labs(x = "Industry", y = "Frequency"))

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
    return(ggplot(data.frame(regional_data$state), aes(x=regional_data$state)) + geom_bar() + labs(x = "Region", y = "Frequency"))
}
getUnitTypeGraph <- function(){
    return(ggplot(data.frame(current_data_slice()$unit_type), aes(x=current_data_slice()$unit_type)) + geom_bar() + labs(x = "Unit Type", y = "Frequency"))
}

getElectionTypeGraph <- function(){
    elec_type_data = current_data_slice()
    elec_type_data$elec_type[elec_type_data$elec_type == "C"] <- "Consent"
    elec_type_data$elec_type[elec_type_data$elec_type == "S"] <- "Stipulated"
    elec_type_data$elec_type[elec_type_data$elec_type == "R"] <- "Regional Director Ordered"
    elec_type_data$elec_type[elec_type_data$elec_type == "E"] <- "Expedited"
    elec_type_data$elec_type[elec_type_data$elec_type == "B"] <- "Board Ordered"
    return(ggplot(elec_type_data, aes(x = yrclosed, color = elec_type, group = elec_type)) + geom_freqpoly_interactive(aes(tooltip=elec_type, data_id=elec_type)) + scale_color_viridis_d())
}

getHeatmap <- function(){
    heatmap_data = current_data_slice()
    heatmap_data$filing_to_elec <- with(heatmap_data, election_date - filed_date)
    heatmap_data$elec_to_close <- with(heatmap_data, closed_date - election_date)
    print(head(heatmap_data))
    return(ggplot(heatmap_data, aes(x=filing_to_elec, y = elec_to_close)) + geom_tile())
}

getLineGraph <- function(){
    industry_data = getIndustryData()
    if (input$customAxes == "Elections") {
      return(ggplot(current_data_slice(), aes(x = yrclosed, color = elec_type, group = elec_type)) +
    geom_line(aes(fill=..count..), stat="bin", binwidth=1))
    } else if (input$customAxes == "Eligible Employees") {
      return(ggplot(current_data_slice(), aes(x= yrclosed, y = current_data_slice()$eligible, color = elec_type, group = elec_type)) + geom_line_interactive() + limitToMaxEligible())
    } else if (input$customAxes == "Total Votes") {
      return(ggplot(current_data_slice(), aes(x= yrclosed, y = totalVotes(), color = elec_type, group = elec_type)) + geom_line_interactive() + limitToMaxEligible())
    } else if (input$customAxes == "Eligible per Election") {
      return(ggplot(current_data_slice(), aes(x = yrclosed, y = current_data_slice()$eligible, color = elec_type, group = elec_type)) + stat_summary(fun.y="mean", geom="line", alpha=0.5, show_guide=TRUE) + stat_summary_interactive(fun.y="median", geom="line", alpha=0.5, show_guide=TRUE) + limitToMaxEligible())
    } else if (input$customAxes == "Avg. Votes per Election") {
      return(ggplot(current_data_slice(), aes(x = yrclosed, y = totalVotes(), color = elec_type, group = elec_type)) + statLine() + limitToMaxEligible())
    } else if (input$customAxes == "Avg. Votes For Union") {
      return(ggplot(current_data_slice(), aes(x = yrclosed, y = current_data_slice()$votes_for, color = elec_type, group = elec_type)) + statLine() + limitToMaxEligible())
    } else if (input$customAxes == "Avg. Votes Against Union") {
      return(ggplot(current_data_slice(), aes(x = yrclosed, y = current_data_slice()$votes_against, color = elec_type, group = elec_type)) + statLine() + limitToMaxEligible())
    } else if (input$customAxes == "Avg. Union Vote Share") {
      return(ggplot(current_data_slice(), aes(x = yrclosed, y = unionVoteShare(), color = elec_type, group = elec_type)) + statLine())
    } else if (input$customAxes == "Avg. Participation Rate") {
      return(ggplot(current_data_slice(), aes(x = yrclosed, y = participationRate(), color = elec_type, group = elec_type)) + statLine())
    } else {
      return()
    }
    return(ggplot(current_data_slice(), aes(x = yrclosed, y = yAxis, color = elec_type, group = elec_type)) +
    geom_line())
}


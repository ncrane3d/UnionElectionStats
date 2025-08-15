getUnitTypeGraph <- function(){
    return(ggplot(data.frame(current_data_slice()$unit_type), aes(x=current_data_slice()$elec_type)) + geom_bar() + labs(x = "Unit Type", y = "Frequency"))
}

getElectionTypeGraph <- function(){
    return()
}
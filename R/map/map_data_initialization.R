# stateBoundariesInitial <- sf::read_sf("./inst/app/www/states.json")
# stateBoundaries <- ms_simplify(stateBoundariesInitial) 
# countyBoundariesInitial <- sf::read_sf("./inst/app/www/counties.json")
# countyBoundaries <- ms_simplify(countyBoundariesInitial)

stateBoundaries <- sf::read_sf("./inst/app/www/states.json")
countyBoundaries <- sf::read_sf("./inst/app/www/counties.json")

testfunc <- function(current_data_slice) {
        stateBoundaries <- sf::read_sf("./inst/app/www/states.json")
        countyBoundaries <- sf::read_sf("./inst/app/www/counties.json")
        currentdata <- current_data_slice()
        state_fips <- with(currentdata, substr(FIPS, 1, nchar(FIPS) - 3))
        state_freq <- data.frame(table(state_fips)) %>% rename(state_count = Freq)
        county_freq <- data.frame(table(currentdata$FIPS)) %>% rename( FIPS = Var1, county_count = Freq)
        countyBoundaries <- full_join(
        countyBoundaries,
        county_freq,
        by = c("FIPS" = "FIPS")
        )
        countyBoundaries <- full_join(
            countyBoundaries,
            state_freq,
            by = c("STATE" = "state_fips")
        )
        stateBoundaries <- full_join(
            stateBoundaries,
            state_freq,
            by = c("state" = "state_fips")
        )
        countyBoundaries$normalized_vote <- with(
        countyBoundaries,
        (county_count / state_count)
        )
        return(list(stateBoundaries, countyBoundaries))
    }

getBoundaries <- function(pool, current_query, current_data_slice) {
    return(reactive({testfunc(current_data_slice)}))
}

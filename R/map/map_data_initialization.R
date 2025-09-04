stateBoundaries <- sf::read_sf("./inst/app/www/states.json")
countyBoundaries <- sf::read_sf("./inst/app/www/counties.json")

getstate_count <- function(pool, current_query) {
    sql <- 'SELECT SUBSTRING(FIPS,1,LENGTH(FIPS) - 3), COUNT(*) AS state_count
FROM (?userquery)
GROUP BY SUBSTRING(FIPS,1,LENGTH(FIPS) - 3);'
    query <- sqlInterpolate(
        pool,
        sql,
        userquery = current_query()
    )
    return(dbGetQuery(pool, query))
}
getcounty_count <- function(pool, current_query) {
    sql <- 'SELECT FIPS, COUNT(*) AS county_count
FROM (?userquery)
GROUP BY FIPS;'
    query <- sqlInterpolate(
        pool,
        sql,
        userquery = current_query()
    )
    return(dbGetQuery(pool, query))
}

getStateBoundaries <- function(pool, state_countdf) {
    stateBoundaries <- full_join(
        stateBoundaries,
        state_countdf(),
        by = c("state" = "substring")
    )
    return(stateBoundaries)
}

getCountyBoundaries <- function(pool, state_countdf, current_query) {
    county_countdf <- getcounty_count(pool, current_query)
    countyBoundaries <- full_join(
        countyBoundaries,
        county_countdf,
        by = c("FIPS" = "fips")
    )
    countyBoundaries <- full_join(
        countyBoundaries,
        state_countdf(),
        by = c("STATE" = "substring")
    )
    countyBoundaries$normalized_vote <- with(
        countyBoundaries,
        (county_count / state_count)
    )
    return(countyBoundaries)
}
testfunc <- function(current_data_slice) {
        stateBoundaries <- sf::read_sf("./inst/app/www/states.json")
        countyBoundaries <- sf::read_sf("./inst/app/www/counties.json")
        currentdata <- current_data_slice()
        print("CURRENT DATA")
        print(head(currentdata))
        state_fips <- with(currentdata, substr(fips, 1, nchar(fips) - 3))
        print("STATEFIPS")
        print(head(state_fips))
        state_freq <- data.frame(table(state_fips)) %>% rename(state_count = Freq)
        county_freq <- data.frame(table(currentdata$fips)) %>% rename( fips = Var1, county_count = Freq)
        print("TRANSPOSE")
        print(head(county_freq))
        print("TRANSPOSE STATE")
        print(head(state_freq))
        print(head(countyBoundaries))
        countyBoundaries <- full_join(
        countyBoundaries,
        county_freq,
        by = c("FIPS" = "fips")
        )
        print("COUNTY COUNT SUCCESS")
        countyBoundaries <- full_join(
            countyBoundaries,
            state_freq,
            by = c("STATE" = "state_fips")
        )
        print("COUNTY STATE SUCCESS")
        stateBoundaries <- full_join(
            stateBoundaries,
            state_freq,
            by = c("state" = "state_fips")
        )
        countyBoundaries$normalized_vote <- with(
        countyBoundaries,
        (county_count / state_count)
    )
        print("COUNTY BOUNDS")
        print(head(countyBoundaries))
        return(list(stateBoundaries, countyBoundaries))
    }

getBoundaries <- function(pool, current_query, current_data_slice) {
    return(reactive({testfunc(current_data_slice)}))
}

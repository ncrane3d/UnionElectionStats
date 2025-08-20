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

getBoundaries <- function(pool, current_query) {
    state_countdf <- reactive({getstate_count(pool, current_query)})
    return(list(
        stateBoundaries <- reactive({getStateBoundaries(pool, state_countdf)}),
        countyBoundaries <- reactive({getCountyBoundaries(pool, state_countdf, current_query)})
    ))
}

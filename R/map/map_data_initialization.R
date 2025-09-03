stateBoundaries <- sf::read_sf("./inst/app/www/states.json")
countyBoundaries <- sf::read_sf("./inst/app/www/counties.json")

getstate_count <- function(pool, current_query) {
    sql <- glue("SELECT SUBSTRING(sub.FIPS,1,LENGTH(sub.FIPS) - 3), COUNT(*) AS state_count, SUBSTRING(sub.FIPS, 1, LENGTH(sub.FIPS) - 3) AS substring
        FROM ({userQuery}) AS sub
        GROUP BY substring",
        userQuery = current_query()
    )
    return(dbGetQuery(pool, sql))
}
getcounty_count <- function(pool, current_query) {
    sql <- glue("SELECT sub.FIPS, COUNT(*) AS county_count
        FROM ({userQuery}) AS sub
        GROUP BY sub.FIPS",
        userQuery = current_query()
    )
    return(dbGetQuery(pool, sql))
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
        by = c("FIPS" = "FIPS")
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

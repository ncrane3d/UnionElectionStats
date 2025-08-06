stateBoundaries <- sf::read_sf("./inst/app/www/states.json")
countyBoundaries <- sf::read_sf("./inst/app/www/counties.json")

getstate_count <- function(pool) {
    sql <- 'SELECT SUBSTRING(cast (FIPS as varchar),1,LENGTH(cast (FIPS as varchar)) - 3), COUNT(*) AS state_count
FROM unionelections
GROUP BY SUBSTRING(cast (FIPS as varchar),1,LENGTH(cast (FIPS as varchar)) - 3);'
    query <- sqlInterpolate(
        pool,
        sql,
    )
    return(dbGetQuery(pool, query))
}
getcounty_count <- function(pool) {
    sql <- 'SELECT cast (FIPS as varchar), COUNT(*) AS county_count
FROM unionelections
GROUP BY cast (FIPS as varchar);'
    query <- sqlInterpolate(
        pool,
        sql,
    )
    return(dbGetQuery(pool, query))
}

getStateBoundaries <- function(pool, state_countdf) {
    stateBoundaries <- full_join(
        stateBoundaries,
        state_countdf,
        by = c("state" = "substring")
    )
    return(stateBoundaries)
}

getCountyBoundaries <- function(pool, state_countdf) {
    county_countdf <- getcounty_count(pool)
    countyBoundaries <- full_join(
        countyBoundaries,
        county_countdf,
        by = c("FIPS" = "fips")
    )
    countyBoundaries <- full_join(
        countyBoundaries,
        state_countdf,
        by = c("STATE" = "substring")
    )
    countyBoundaries$normalized_vote <- with(
        countyBoundaries,
        (county_count / state_count)
    )
    return(countyBoundaries)
}

getBoundaries <- function(pool) {
    state_countdf <- getstate_count(pool)
    return(list(
        stateBoundaries = getStateBoundaries(pool, state_countdf),
        countyBoundaries = getCountyBoundaries(pool, state_countdf)
    ))
}

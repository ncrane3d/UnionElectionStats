stateBoundaries <- sf::read_sf("./inst/app/www/states.json")
countyBoundaries <- sf::read_sf("./inst/app/www/counties.json")
getpalette <- function(column) {
    colorNumeric(c("red", "blue"), column)
}

linecolor <- "rgb(143,137,141)"
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
map <- function(input, output, pool, current_data_slice) {
    highlightFunction <- function(fips) {
        if (fips == input$state) {
            return("white")
        } else {
            return(linecolor)
        }
    }
    state_countdf <- getstate_count(pool)
    county_countdf <- getcounty_count(pool)
    stateBoundaries <- full_join(
        stateBoundaries,
        state_countdf,
        by = c("STATE" = "substring")
    )
    countyBoundaries <- full_join(
        countyBoundaries,
        county_countdf,
        by = c("FIPS" = "fips")
    )
    pal <- getpalette(stateBoundaries$state_count)
    pal2 <- getpalette(countyBoundaries$county_count)
    return(renderLeaflet({
        leaflet(options = leafletOptions(minZoom = 3)) |>
            addTiles() |>
            #State border layer
            addPolygons(
                data = stateBoundaries,
                weight = 1,
                color = ~ pal(state_count),
                group = "states"
            ) |>
            #County border layer
            addPolygons(
                data = countyBoundaries,
                weight = 1,
                color = ~ pal2(county_count),
                group = "counties"
            ) |>
            #Individual election markers
            addCircleMarkers(
                data = current_data_slice(),
                group = "counties",
                color = "red",
                opacity = 0.75,
                fillOpacity = 0.75,
                clusterOptions = markerClusterOptions(
                    #Expands overlapping markers out into a starburst on max zoom
                    spiderfyOnMaxZoom = TRUE,
                    showCoverageOnHover = TRUE
                ),
                #Popup on click of individual elections that displays basic info
                popup = ~ paste(
                    paste("Employer: ", htmlEscape(employer)),
                    paste(
                        paste("<br>Year closed: ", htmlEscape(yrclosed)),
                        paste(
                            "<br>Pro-Union vote share: ",
                            paste(
                                htmlEscape(
                                    round(
                                        ((votes_for /
                                            (votes_for + votes_against)) *
                                            100),
                                        digits = 2
                                    ),
                                )
                            ),
                            "%"
                        )
                    )
                )
            ) |>
            #Zoom based conditional rendering for layers
            groupOptions("counties", zoomLevels = 6:20) |>
            groupOptions("states", zoomLevels = 0:5) |>
            #Map panning bounds
            setMaxBounds(
                lat1 = 5.499550,
                lng1 = -167.276413,
                lat2 = 83.162102,
                lng2 = -52.233040
            ) |>
            #Initial map location and zoom
            setView(
                lat = 39.82,
                lng = -98.58,
                zoom = 3,
            )
    }))
}

dashboard <- function() {
    nav_panel(
        title = "Home",
        align = "left",
    absolutePanel(
        width = "99vw",
        height= "85vh",
        left="0px",
        right="10vw",
        card_header("Lorem ipsum donor ad somet"),
        layout_sidebar( sidebar = sidebar(selectInput("aggregate", "Aggregate", c("State", "County", "Industry")), selectInput("State", "State", c("All", "Colorado")), selectInput("county", "County", c("All", "Larimer")), checkboxGroupInput("electionType", "Election Type", c("Certification (RC)", "Decertification (RD)", "Emp. Petition (RM)")), sliderInput("timeframe", "Timeframe", min = 1960, max = 2025, value = c(1960, 2025)), sliderInput("percentageFavor", "Percentage in favor", min = 0, max = 100, value = c(0,100))),
        height="85vh",
        layout_columns(
            card(leafletOutput("map")), 
            card(card_header("Custom Visualization"), card("jfaksjdlsj"), selectInput("select", "Graph type:", list("Pi Chart" = "PI", "Line Graph" = "LINE", "Histogram" = "HIST")))
            ),
        layout_columns(
            card(card_header("test"), fill = FALSE), 
            card("card 2", fill = TRUE),
            card("card 3", fill = TRUE)
            )        )
    ))

}
dashboard <- function() {
    nav_panel(
        title = "Home",
        align = "left",
        absolutePanel(
            width = "100vw",
            height = "85vh",
            left = "0px",
            right = "10vw",
            top = '57px',
            card_header(
                layout_columns(
                    "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum",
                    "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum"
                ),
                class = "dashboardheader",
                height = "10vh"
            ),
            layout_sidebar(
                sidebar = sidebar(
                    selectInput(
                        "aggregate",
                        "Aggregate",
                        c("State", "County", "Industry")
                    ),
                    selectInput("State", "State", c("All", "Colorado")),
                    selectInput("county", "County", c("All", "Larimer")),
                    checkboxGroupInput(
                        "electionType",
                        "Election Type",
                        c(
                            "Certification (RC)",
                            "Decertification (RD)",
                            "Emp. Petition (RM)"
                        )
                    ),
                    sliderInput(
                        "timeframe",
                        "Timeframe",
                        sep = "",
                        min = 1960,
                        max = 2025,
                        value = c(1960, 2025)
                    ),
                    sliderInput(
                        "percentageFavor",
                        "Percentage in favor",
                        min = 0,
                        max = 100,
                        value = c(0, 100)
                    )
                ),
                height = "85vh",
                layout_columns(
                    card(leafletOutput("map")),
                    card(
                        card_header("Custom Visualization"),
                        card(),
                        selectInput(
                            "select",
                            "Graph type:",
                            list(
                                "Pi Chart" = "PI",
                                "Line Graph" = "LINE",
                                "Histogram" = "HIST"
                            )
                        )
                    )
                ),
                layout_columns(
                    card(card_header("test"), fill = FALSE),
                    card("card 2", fill = TRUE),
                    card("card 3", fill = TRUE)
                )
            )
        )
    )
}

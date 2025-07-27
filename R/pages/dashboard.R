dashboard <- function() {
    nav_panel(
        title = "Home",
        align = "left",
        div(
            width = "100vw",
            height = "85vh",
            left = "0px",
            right = "10vw",
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
                        "state", 
                        "State", 
                        c(
                            "All", 
                            "Colorado"
                        )
                    ),
                    selectInput(
                        "county",
                        "County", 
                        c(
                            "All", 
                            "Larimer"
                        )
                    ),
                    selectInput(
                        "industry",
                        "Industry",
                        c(
                            "All",
                            "Agriculture, Forestry and Fishing",
                            "Mining",
                            "Construction",
                            "Manufacturing",
                            "Transportation and Utilities",
                            "Wholesale",
                            "Retail",
                            "FIRE",
                            "Services",
                            "Public Administration"
                        )
                    ),
                    checkboxGroupInput(
                        "regionType",
                        "Region Type",
                        c(
                            "Urban" = 0,
                            "Rural" = 1
                        ),
                        selected = list(0, 1)
                    ),
                    checkboxGroupInput(
                        "electionType",
                        "Election Type",
                        c(
                            "Certification (RC)" = "RC",
                            "Decertification (RD)" = "RD",
                            "Emp. Petition (RM)" = "RM"
                        ),
                        selected = list("RC", "RD", "RM")
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
                        "Votes For Petition",
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
                    card(card_header("Preset Graph 1"), fill = TRUE, plotOutput("testPlot")),
                    card(card_header("Preset Graph 2"), fill = TRUE, tableOutput("test")),
                    card(card_header("Preset Graph 3"), fill = TRUE)
                )
            )
        )
    )
}

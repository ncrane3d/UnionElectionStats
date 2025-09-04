dashboard <- function() {
    nav_panel(
        title = "Home",
        align = "left",
        div(
            width = "100vw",
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
                            "Alabama" = "AL",
                            "Alaska" = "AK",
                            "Arizona" = "AZ",
                            "Arkansas" = "AR",
                            "California" = "CA",
                            "Colorado" = "CO",
                            "Connecticut" = "CT",
                            "Delaware" = "DE",
                            "District of Columbia" = "DC",
                            "Florida" = "FL",
                            "Georgia" = "GA",
                            "Hawaii" = "HI",
                            "Idaho" = "ID",
                            "Illinois" = "IL",
                            "Indiana" = "IN",
                            "Iowa" = "IA",
                            "Kansas" = "KS",
                            "Kentucky" = "KY",
                            "Louisiana" = "LA",
                            "Maine" = "ME",
                            "Montana" = "MT",
                            "Nebraska" = "NE",
                            "Nevada" = "NV",
                            "New Hampshire" = "NH",
                            "New Jersey" = "NJ",
                            "New Mexico" = "NM",
                            "New York" = "NY",
                            "North Carolina" = "NC",
                            "North Dakota" = "ND",
                            "Ohio" = "OH",
                            "Oklahoma" = "OK",
                            "Oregon" = "OR",
                            "Maryland" = "MD",
                            "Massachusetts" = "MA",
                            "Michigan" = "MI",
                            "Minnesota" = "MN",
                            "Mississippi" = "MS",
                            "Missouri" = "MO",
                            "Pennsylvania" = "PA",
                            "Rhode Island" = "RI",
                            "South Carolina" = "SC",
                            "South Dakota" = "SD",
                            "Tennessee" = "TN",
                            "Texas" = "TX",
                            "Utah" = "UT",
                            "Vermont" = "VT",
                            "Virginia" = "VA",
                            "Washington" = "WA",
                            "West Virginia" = "WV",
                            "Wisconsin" = "WI",
                            "Wyoming" = "WY"
                        )
                    ) %>%
                        tagAppendAttributes(class = "artificial-gap"),
                    tooltip(
                        selectInput(
                            "county",
                            "County",
                            c(
                                "No State Selected"
                            )
                        ) %>%
                            tagAppendAttributes(class = "tooltip-adjustment"),
                        "By 2020 FIPS Codes.",
                        placement = "right"
                    ),
                    tooltip(
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
                        ) %>%
                            tagAppendAttributes(class = "tooltip-adjustment"),
                        "By sic Code. No data after 2010.",
                        placement = "right"
                    ),
                    checkboxGroupInput(
                        "electionType",
                        "Petition Type",
                        c(
                            "Certification (RC)" = "RC",
                            "Decertification (RD)" = "RD",
                            "Emp. Petition (RM)" = "RM"
                        ),
                        selected = list("RC", "RD", "RM")
                    ) %>%
                        tagAppendAttributes(class = "artificial-gap"),
                    sliderInput(
                        "timeframe",
                        "Timeframe",
                        sep = "",
                        min = 1960,
                        max = 2025,
                        value = c(1960, 2025)
                    ) %>%
                        tagAppendAttributes(class = "artificial-gap"),
                    sliderInput(
                        "percentageFavor",
                        "Pro-union Vote Share",
                        min = 0,
                        max = 100,
                        value = c(0, 100)
                    ),
                    tooltip(
                        checkboxInput(
                            "winnersChecked",
                            "Winning Elections Only",
                            value = FALSE
                        ),
                        "Sets slider to hundredths place.",
                        placement = "right"
                    )
                ),
                layout_columns(
                    card(leafletOutput("map"), height = "75vh"),
                    card(
                        card_header("Custom Visualization"),
                        layout_columns(
                            width = 1/2,
                            selectInput(
                                "customGraphType",
                                "Graph type:",
                                list(
                                    "Line Graph" = "LINE",
                                    "Histogram" = "HIST"
                                )
                            ),
                            selectInput(
                                "customAxes",
                                "Select Axes:",
                                c(
                                    "Elections",
                                    "Eligible Employees",
                                    "Total Votes",
                                    "Eligible per Election",
                                    "Avg. Votes per Election",
                                    "Avg. Votes For Union",
                                    "Avg. Votes Against Union",
                                    "Avg. Union Vote Share",
                                    "Avg. Participation Rate"
                                )
                            )
                        ) %>% 
                        tagAppendAttributes(class = "custom-visualization-margin"),
                        card(plotOutput("customVisualization"))
                    )
                ),
                accordion(
                    accordion_panel(
                        title="test",
                        layout_column_wrap(
                            width = 1/2,
                            card(card_header("Industries"), height="30vh", fill = TRUE, girafeOutput("industryPreset")),
                            card(card_header("Unit Types"), height="30vh", fill = TRUE, plotOutput("unitTypePreset")),
                            card(card_header("Regions"), height="30vh", fill = TRUE, girafeOutput("regionalPreset")),
                            card(card_header("Election Types"), height="30vh", fill = TRUE, plotOutput("elecTypePreset")),
                            card(card_header("Heatmap"), height="30vh", fill = TRUE, plotOutput("heatmapPreset")),
                            card(card_header("Lines"), height="30vh", fill = TRUE, plotOutput("linePreset"))
                        )
                    )
                )
            )
        )
    )
}

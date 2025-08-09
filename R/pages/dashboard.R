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
                    "Thank you for visiting Union Election Stats! Here you can explore the history of union elections '
                    through various visualizations and filters. It is important to note that this is not union membership 
                    data. The value in union elections is that they provide deeper levels of disaggregation than would  
                    otherwise be possible with just membership information. It allows us to analyze the decline of U.S. 
                    unions through geography, industry, time, and more. The data presented here is a cleaned version of 
                    what is publicly released by the National Labor Relations Board (NLRB). This can be accessed in its 
                    raw or cleaned forms in the Downloads tab. This website has been developed in relation to my ongoing 
                    research, it can be found in the Featured Analysis tab, as well as other related works. Lastly, be 
                    sure to submit a form in the Contact Us tab with any inquiries or bug reports.", 
                    "It was noted that this website does not represent union membership data, however if you would like 
                    access to that information please visit unionstats.com. In contrast, if you would like even more 
                    visualizations of union election data, unionelections.org is another great resource. It includes 
                    post 2000 information, as well as open union election cases.  
                    Finally, I would like to thank the Russell Sage Foundation for their support in this project, it 
                    has enabled my cleaning of this dataset, the creation of this website, and every other aspect of 
                    this project. Real people are affected by the decline of unions in the United States, through 
                    disaggregation we can figure out who they are and what those outcomes have been. It is both relevant 
                    and important work that wouldn't be possible without the Russell Sage Foundation."
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
                        "By SIC Code. No data after 2010.",
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
                    card(card_header("Preset Graph 1"), fill = TRUE),
                    card(card_header("Preset Graph 2"), fill = TRUE),
                    card(card_header("Preset Graph 3"), fill = TRUE)
                )
            )
        )
    )
}

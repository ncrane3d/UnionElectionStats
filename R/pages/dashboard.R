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
                    p("Welcome to Union Election Stats, a user-friendly hub for data on National Labor Relations Board (NLRB) 
                    representation elections.  Here you can explore the history of union organizing through interactive 
                    visualizations, learn about recent trends in the labor movement, and download data for your own bespoke 
                    analysis.  The database was constructed by a team of academic economists/political scientists, Zachary 
                    Schaller, Sammy Young, and Jonne Kamphorst, and is based on NLRB records.  It contains the known universe 
                    of representation elections from 1962 to 2024.  Soon it will also track the most recent elections as cases 
                    close.  Use the Downloads tab to access the database, check out the Featured Analysis tab to dive deeper 
                    into union scholarship, and use the Contact Us tab with any inquiries or bug reports.", actionLink("citationPopup","Use of data requires 
                    citation")) %>% 
                        tagAppendAttributes(class = "paragraphIndent"),
                    div(
                        p("This website is about union election data (the flow of organizing activity), it is not about union 
                            membership data (the stock of unionized workers). For membership data, please visit ", 
                            tags$a(href="http://unionstats.com/", "unionstats.com."),  
                            "For info on strikes and work stoppages, use the labor action tracker at" , 
                            tags$a(href="http://striketracker.ilr.cornell.edu", "striketracker.ilr.cornell.edu."),    
                            "And for additional election analysis (for years after 2000) and some excellent real-time case tracking, 
                            check out ", 
                            tags$a(href="http://unionelections.org", "unionelections.org."))%>% 
                                tagAppendAttributes(class = "paragraphIndent"),
                        p("We stand on the shoulders of giants to bring you this resource.  Many thanks to Henry Farber, Bruce Western, 
                            J.P. Ferguson, Thomas Holmes, David Lee, Alexandre Mas, and Jack Fiorito for their seminal data collection 
                            and research on union elections.  We also thank the Russell Sage Foundation for their support (grant # 2307-44744).") %>% 
                                tagAppendAttributes(class = "paragraphIndent")
                    ),  
                    
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
                            "Maryland" = "MD",
                            "Massachusetts" = "MA",
                            "Michigan" = "MI",
                            "Minnesota" = "MN",
                            "Mississippi" = "MS",
                            "Missouri" = "MO",
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
                ),
                card(
                    card_header("Responsible Data Use"),
                    "As the saying goes, “figures never lie, but liars sure figure.”  Please use these 
                    data responsibly and authentically.  Be transparent about your methods and avoid 
                    temptations to graph hack and p-hack.  Additionally, real people are behind the data 
                    generating process, so please be respectful of the blood, sweat, and tears that have 
                    been shed on all sides of industrial relations."
                )
            )
        )
    )
}

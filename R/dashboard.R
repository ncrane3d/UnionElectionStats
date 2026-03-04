dashboard <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "Home",
    align = "left",
    div(
      width = "100vw",
      left = "0px",
      right = "10vw",
      card_header(
        layout_columns(
          p("The database was constructed by a team of academic economists/political scientists and is based on NLRB records.
          Please cite if you use the data or maps/graphs. Use the Contact Us tab with any inquiries or bug reports.
          This website is about union election data (the flow of organizing activity), it is not about union
            membership data (the stock of unionized workers). For membership data, please visit",
            tags$a(href="http://unionstats.com/", "unionstats.com.", target="_blank"),"For info on strikes and work stoppages, use the labor action tracker at" ,
            tags$a(href="http://striketracker.ilr.cornell.edu", "striketracker.ilr.cornell.edu.", target="_blank"),
            "And for additional election analysis (for years after 2000) and some excellent real-time case tracking,
                            check out ",
            tags$a(href="http://unionelections.org", "unionelections.org.", target="_blank"),
            actionLink("citationPopup","Please cite if you use the data or maps/graphs")) %>%
            tagAppendAttributes(class = "paragraphMarginFix"),
          div(
            p("We stand on the shoulders of giants to bring you this resource.  Many thanks to Henry Farber, Bruce Western,
                            J.P. Ferguson, Thomas Holmes, David Lee, Alexandre Mas, and Jack Fiorito for their seminal data collection
                            and research on union elections.  We also thank the Russell Sage Foundation for their support (grant # 2307-44744).") %>%
              tagAppendAttributes(class = "paragraphMarginFix")
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
            min = 1962,
            max = 2025,
            value = c(1962, 2025)
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
          ),
          tooltip(
            checkboxInput(
              "showElections",
              "Show Individual Elections",
              value = FALSE
            ),
            "Will also zoom map to the zoom level of points.",
            placement = "right"
          ),
          p(style="color:red;", "Warning: Loading all election points is resource intensive, filtering will be slower.")
        ),
        layout_columns(
          card(leafletOutput("mapBuilder-map"), height = "75vh"),
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
                  "Avg. Union Vote Share",
                  "Avg. Participation Rate",
                  "Avg. Win Rate"
                )
              )
            ) %>%
              tagAppendAttributes(class = "custom-visualization-margin"),
            card(plotOutput("customGraphBuilder-customVisualization"))  %>%
              tagAppendAttributes(class = "card-border-remover")
          )
        ),
        # Uncomment this block of code to reintroduce preset ui position
        # card(
        #     card_header("Presets"),
        #     layout_column_wrap(
        #         width = 1/2,
        #         card(card_header("Industries"), height="30vh", fill = TRUE, plotOutput("presetGraphBuilder-industryPreset")),
        #         card(card_header("Unit Types"), height="30vh", fill = TRUE, plotOutput("presetGraphBuilder-unitTypePreset")),
        #         card(card_header("Regions"), height="30vh", fill = TRUE, plotOutput("presetGraphBuilder-regionalPreset")),
        #         card(card_header("Election Types"), height="30vh", fill = TRUE, plotOutput("presetGraphBuilder-elecTypePreset")),
        #         card(card_header("Heatmap"), height="30vh", fill = TRUE, plotOutput("presetGraphBuilder-heatmapPreset")),
        #         card(card_header("Lines"), height="30vh", fill = TRUE, plotOutput("presetGraphBuilder-linePreset"))
        #     )
        # ),
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

downloads <- function() {
    height <- "87vh"
    nav_panel(
        title = "Downloads",
        align = "left",
        div(
            layout_column_wrap(
                width = 1 / 3,
                card(
                    card_header("Clean dataset"),
                    height = height,
                    p("Below are the links to download the cleaned NLRB data. The full dataset is at the election level 
                    (unique case_number, unitID combinations).  For convenience, we also offer aggregated versions at the 
                    state, county, annual, and industry levels") %>% 
                        tagAppendAttributes(class = "paragraphIndent"),
                    div(
                        h6("Full Dataset:"),
                        tags$a(href = "", "Union-Elections-Full.csv"),
                        tags$br(),
                        tags$a(href = "", "Union-Elections-Full.dta"),
                        tags$br(),
                        tags$a(href = "", "Union-Elections-Full.fst"),
                        h6("State:"),
                        tags$a(href = "", "Union-Elections-State.csv"),
                        h6("County:"),
                        tags$a(href = "", "Union-Elections-County.csv"),
                        h6("Annual:"),
                        tags$a(href = "", "Union-Elections-Annual.csv"),
                        h6("Industry by SIC Code:"),
                        tags$a(href = "", "Union-Elections-SIC.csv"),
                        h6("Industry by NAICS Code:"),
                        tags$a(href = "", "Union-Elections-NAICS.csv"),
                    )
                ),
                card(
                    card_header("Raw dataset"),
                    height = height,
                    p("FOR ADVANCED RESEARCHERS ONLY.  In merging the fragments of data from many sources, we had to make decisions 
                    about how to reconcile differences and prevent duplication.  We did our best, but we’re not offended if you want 
                    to do it differently. This raw dataset is simply the fragments we have stacked on top of each other.  Variable 
                    definitions were harmonized, but the fragments were not deduplicated/reconciled.  Only use this raw version if 
                    you are A) interested in a particular election and want to see the full set of info available on it, even if 
                    some of it is wrong, messy, or unintelligible; or B) Very comfortable with data wrangling and well-versed in 
                    the NLRB election process/procedures.") %>% 
                        tagAppendAttributes(class = "paragraphIndent"),
                    div(
                        h6("Raw File Types:"),
                        tags$a(href = "", "Union-Elections-Raw.csv"),
                        tags$br(),
                        tags$a(href = "", "Union-Elections-Raw.dta"),
                        tags$br(),
                        tags$a(href = "", "Union-Elections-Raw.fst")
                    )
                ),
                card(
                    card_header("Documentation"),
                    height = height,
                    p("Below are the variable definitions for both the cleaned and raw dataset, as well as a history of version changes. 
                    It is automated through the site’s github which can be found ", tags$a(href="https://github.com/ncrane3d/UnionElectionStats", "here.")) %>% 
                        tagAppendAttributes(class = "paragraphIndent"),
                    tags$a(href = "", "Documentation Spreadsheet"),
                    accordion_panel(
                        title = "Version History",
                        p("Data is here")
                    ) %>%
                        tagAppendAttributes(id = "accordion-download")
                )
            )
        )  %>%
            tagAppendAttributes(class = "centered-card"),
    )
}

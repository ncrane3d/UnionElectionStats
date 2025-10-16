downloads <- function(id) {
    ns <- NS(id)
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
                        #downloadLink("cleanCSV", label = "Elections_Data_Cleaned_V0.csv"),
                        tags$a(href = "https://unionelectionstats.s3.us-east-2.amazonaws.com/Elections_Data_Cleaned_V0.csv", "Elections_Data_Cleaned_V0.csv"),
                        tags$br(),
                        #downloadLink("cleanDTA", label = "Elections_Data_Cleaned_V0.dta"),
                        tags$a(href = "https://unionelectionstats.s3.us-east-2.amazonaws.com/Elections_Data_Cleaned_V0.dta", "Elections_Data_Cleaned_V0.dta"),
                        tags$br(),
                        #downloadLink("cleanFST", label = "Elections_Data_Cleaned_V0.fst"),
                        tags$a(href = "https://unionelectionstats.s3.us-east-2.amazonaws.com/Elections_Data_Cleaned_V0.fst", "Elections_Data_Cleaned_V0.fst"),
                        h6("State:"),
                        tags$a(href = "", "Coming Soon!"),
                        h6("County:"),
                        tags$a(href = "", "Coming Soon!"),
                        h6("Annual:"),
                        tags$a(href = "", "Coming Soon!"),
                        h6("Industry by SIC Code:"),
                        tags$a(href = "", "Coming Soon!"),
                        h6("Industry by NAICS Code:"),
                        tags$a(href = "", "Coming Soon!"),
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
                        #downloadLink("rawCSV", label = "Elections_Data_Raw_V0.csv"),
                        tags$a(href = "https://unionelectionstats.s3.us-east-2.amazonaws.com/Elections_Data_Raw_V0.csv", "Elections_Data_Raw_V0.csv"),
                        tags$br(),
                        #downloadLink("rawDTA", label = "Elections_Data_Raw_V0.dta"),
                        tags$a(href = "https://unionelectionstats.s3.us-east-2.amazonaws.com/Elections_Data_Raw_V0.dta", "Elections_Data_Raw_V0.dta"),
                        tags$br(),
                        #downloadLink("rawFST", label = "Elections_Data_Raw_V0.fst")
                        tags$a(href = "https://unionelectionstats.s3.us-east-2.amazonaws.com/Elections_Data_Raw_V0.fst", "Elections_Data_Raw_V0.fst"),
                    )
                ),
                card(
                    card_header("Documentation"),
                    height = height,
                    p("Below are the variable definitions for both the cleaned and raw dataset, as well as a history of version changes. 
                    It is automated through the site’s github which can be found ", tags$a(href="https://github.com/ncrane3d/UnionElectionStats", "here.")) %>% 
                        tagAppendAttributes(class = "paragraphIndent"),
                    div(
                        #downloadLink("varDocClean", label = "Variable Documentation Elections Data Cleaned.xlsx"),
                        tags$a(href = "https://unionelectionstats.s3.us-east-2.amazonaws.com/Variable+Documentation+Elections+Data+Cleaned.xlsx", "Variable Documentation Elections Data Cleaned.xlsx"),
                        tags$br(),
                        #downloadLink("varDocRaw", label = "Variable Documentation Elections Data Raw.xlsx"),
                        tags$a(href = "https://unionelectionstats.s3.us-east-2.amazonaws.com/Variable+Documentation+Elections+Data+Raw.xlsx", "Variable Documentation Elections Data Raw.xlsx"),
                    ),
                    div(
                        h6("Version History"),
                        tags$a(href = "https://github.com/ncrane3d/UnionElectionStats", "V0.1.0 Alpha"),
                    )

                )
            )
        )  %>%
            tagAppendAttributes(class = "centered-card"),
    )
}

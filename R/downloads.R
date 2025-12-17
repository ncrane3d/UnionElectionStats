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
                        tagAppendAttributes(class = "paragraphMarginFix"),
                    div(
                        h6("Full Dataset:"),
                        tags$a(href = "http://drive.google.com/uc?id=1A8inZAf2TMBRWedoq_33_KAjSg2R0of1&export=download", "Elections_Data_Cleaned_V0.csv", target="_blank"),
                        tags$br(),
                        tags$a(href = "http://drive.google.com/uc?id=1T4QovyyRTZDbnf2Ev6TS61exEJuYAtEk&export=download", "Elections_Data_Cleaned_V0.dta", target="_blank"),
                        tags$br(),
                        tags$a(href = "http://drive.google.com/uc?id=1M5JZ6aNJqYlEHtEGvOW54Qqe5UVMQr02&export=download", "Elections_Data_Cleaned_V0.fst", target="_blank"),
                        tags$br(),
                        tags$br(),
                        h6("State:"),
                        tags$a(href = "http://drive.google.com/uc?id=1cd21L32dPGaFI_LEBSHYBaLyXcjzJtnT&export=download", "State_Cross_Section.csv", target="_blank"),
                        tags$br(),
                        tags$a(href = "http://drive.google.com/uc?id=1BBedxQPKZIPJWE_G5RJBOhwLg8iXJZOS&export=download", "State_Panel.csv", target="_blank"),
                        h6("County:"),
                        tags$a(href = "http://drive.google.com/uc?id=12hotDZtBSOEfdXn0Et1VbYW7uYZ1lUM6&export=download", "County_Cross_Section.csv", target="_blank"),
                        tags$br(),
                        tags$a(href = "http://drive.google.com/uc?id=1TP9CxFnLClK1uxlpRRbFO0BSimYvWHJv&export=download", "County_Panel.csv", target="_blank"),
                        h6("Annual Time Series:"),
                        tags$a(href = "http://drive.google.com/uc?id=134PRcJEboo5KksJzRnmtQPYo78SqU-pU&export=download", "Annual_Time_Series.csv", target="_blank"),
                        h6("Industry by SIC Code:"),
                        tags$a(href = "http://drive.google.com/uc?id=1Rm6SkwldDrwppqk8X3kXavLlvYIhjfK0&export=download", "SIC_Cross_Section.csv", target="_blank"),
                        tags$br(),
                        tags$a(href = "http://drive.google.com/uc?id=1fdo4KejX-94vbCPujDYN2dU29mgMEBVq&export=download", "SIC_Panel.csv", target="_blank"),
                        h6("Industry by NAICS Code:"),
                        tags$a(href = "http://drive.google.com/uc?id=1rVo2x9QDabj8CwRhr23jJLNST3aWnPAM&export=download", "NAICS_Cross_Section.csv", target="_blank"),
                        tags$br(),
                        tags$a(href = "http://drive.google.com/uc?id=11EB10TfQXIdyhwU9IX4QQOSHLioTMUA8&export=download", "NAICS_Panel.csv", target="_blank")
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
                        tagAppendAttributes(class = "paragraphMarginFix"),
                    div(
                        h6("Raw File Types:"),
                        tags$a(href = "http://drive.google.com/uc?id=1kh_EncPPZK83PU54M_ryy4ixfomO5QuQ&export=download", "Elections_Data_Raw_V0.csv", target="_blank"),
                        tags$br(),
                        tags$a(href = "http://drive.google.com/uc?id=1ArLQgYco1KsnCPSRIlVU-tTZFg_Jbqc-&export=download", "Elections_Data_Raw_V0.dta", target="_blank"),
                        tags$br(),
                        tags$a(href = "http://drive.google.com/uc?id=1RUS5BS-eJAtuxGzUEOUu4wTiEX3CmEac&export=download", "Elections_Data_Raw_V0.fst", target="_blank"),
                    )
                ),
                card(
                    card_header("Documentation"),
                    height = height,
                    p("Below are the variable definitions for both the cleaned and raw dataset, as well as a history of version changes. 
                    It is automated through the site’s github which can be found ", tags$a(href="https://github.com/ncrane3d/UnionElectionStats", "here.", target="_blank")) %>% 
                        tagAppendAttributes(class = "paragraphMarginFix"),
                    div(
                        #downloadLink("varDocClean", label = "Variable Documentation Elections Data Cleaned.xlsx"),
                        tags$a(href = "http://drive.google.com/uc?id=15bkPANK1FwyiaxZx8HAZeoPR6tpoeEBq&export=download", "Variable Documentation Elections Data Cleaned.xlsx", target="_blank"),
                        tags$br(),
                        #downloadLink("varDocRaw", label = "Variable Documentation Elections Data Raw.xlsx"),
                        tags$a(href = "http://drive.google.com/uc?id=1XogQh-legyHCfCRcwKUd_1i1C7un__nL&export=download", "Variable Documentation Elections Data Raw.xlsx", target="_blank"),
                    ),
                    div(
                        h6("Version History"),
                        tags$a(href = "https://github.com/ncrane3d/UnionElectionStats/releases/latest", "v1.0.0", target="_blank"),
                    )

                )
            )
        )  %>%
            tagAppendAttributes(class = "centered-card"),
    )
}

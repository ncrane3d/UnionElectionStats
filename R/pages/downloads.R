downloads <- function() {
    height <- "87vh"
    nav_panel(
        title = "Downloads",
        align = "left",
        absolutePanel(
            right = "10vw",
            left = "10vw",
            height = height,
            layout_column_wrap(
                width = 1 / 3,
                card(
                    card_header("Clean dataset"),
                    height = height,
                    "Description of format",
                    random_text(nwords = 50),
                    accordion(
                        accordion_panel(
                            title = "Geographic Aggregates",
                            accordion(
                                accordion_panel(
                                    title = "State",
                                    tags$a(href = "", "Link_To_State_Data")
                                ),
                                accordion_panel(
                                    title = "County",
                                    tags$a(href = "", "Link_To_County_Data")
                                ),
                            ),
                            open = FALSE,
                            multiple = FALSE
                        ),
                        accordion_panel(
                            title = "Time Aggregates",
                            p("Data is here")
                        ),
                        accordion_panel(
                            title = "Industry Aggregates",
                            p("Data is here")
                        ),            
                        open = FALSE
                    )
                ),
                card(
                    card_header("Raw dataset"),
                    height = height,
                    "Description of format",
                    random_text(nwords = 50),
                    tags$a(href = "", "File Types V")
                ),
                card(
                    card_header("Documentation"),
                    height = height,
                    "Overall description of dataset",
                    random_text(nwords = 50),
                    tags$a(href = "", "Documentation Spreadsheet"),
                    tags$a(href = "google.com", "Version History V")
                )
            )
        )
    )
}

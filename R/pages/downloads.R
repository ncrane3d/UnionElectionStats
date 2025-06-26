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
                    p("Description of format"),
                    random_text(nwords = 50),
                    tags$a(href = "", "Geographic Aggregates V"),
                    tags$a(href = "", "Time Aggregates V"),
                    tags$a(href = "", "Industry Aggregates V")
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

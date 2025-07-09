downloads <- function() {
    height <- "87vh"
    nav_panel(
        title = "Downloads",
        align = "left",
        div(
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
                    div(
                        accordion_panel(
                            title = "Geographic Aggregates",
                            div(
                                accordion_panel(
                                    title = "State",
                                    tags$a(href = "", "Link_To_State_Data")
                                ) %>%
                                    tagAppendAttributes(
                                        id = "accordion-download"
                                    ),
                                accordion_panel(
                                    title = "County",
                                    tags$a(href = "", "Link_To_County_Data")
                                ) %>%
                                    tagAppendAttributes(
                                        id = "accordion-download"
                                    ),
                            ),
                            open = FALSE,
                            multiple = FALSE
                        ) %>%
                            tagAppendAttributes(id = "accordion-download"),
                        accordion_panel(
                            title = "Time Aggregates",
                            p("Data is here")
                        ) %>%
                            tagAppendAttributes(id = "accordion-download"),
                        accordion_panel(
                            title = "Industry Aggregates",
                            p("Data is here")
                        ) %>%
                            tagAppendAttributes(id = "accordion-download"),
                        open = FALSE
                    )
                ),
                card(
                    card_header("Raw dataset"),
                    height = height,
                    "Description of format",
                    random_text(nwords = 50),
                    accordion_panel(
                        title = "File Types",
                        p("Data is here")
                    ) %>%
                        tagAppendAttributes(id = "accordion-download")
                ),
                card(
                    card_header("Documentation"),
                    height = height,
                    "Overall description of dataset",
                    random_text(nwords = 50),
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

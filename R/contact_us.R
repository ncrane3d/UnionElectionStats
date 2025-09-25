contact <- function(id) {
    ns <- NS(id)
    nav_panel(
        title = "Contact Us",
        align = "left",
        div(
            useShinyFeedback(),
            card(
                id = "picketsign",
                card_header("Contact Us", align = "center"),
                layout_columns(
                    col_widths = 12,
                    id = "picketInput",
                    layout_column_wrap(
                        width = 1 / 2,
                        textInput("name", "Name"),
                        textInput("email", "Email"),
                    ),
                    textInput("subject", "Subject", width = "100%"),
                    tags$div(
                        id = "picketInput",
                        textAreaInput(
                            "message",
                            "Message",
                            width = "100%",
                        )
                    ),
                    tags$div(
                        align = "center",
                        actionButton(
                            inputId = "submitButton",
                            label = "Submit"
                        )
                    )
                )
            )
        ) %>% tagAppendAttributes(class="contact_us_container"),
        style = css(
            background_image = "url(www/picket_line.png)",
            background_repeat = "no-repeat",
            background_size = "cover",
            background_position = "center bottom",
            min_height = "100vh",
        )
    )
}

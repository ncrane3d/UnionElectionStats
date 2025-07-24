contact <- function() {
    nav_panel(
        title = "Contact Us",
        align = "left",
        div(
            card(
                class = "contactcard",
                card_header("Contact us"),
                fill = FALSE,
                layout_column_wrap(
                    width = 1 / 2,
                    height = "5vh",
                    fill = FALSE,
                    textInput("name", "Name"),
                    textInput("email", "Email"),
                ),
                layout_column_wrap(
                    width = 1,
                    textInput("subject", "Subject"),
                ),
                textAreaInput(
                    "message",
                    "Message",
                    resize = "none",
                    value = "Your message here...",
                    width = "100%",
                    height = "30vh"
                ),
                tags$div(
                    align = "center",
                    actionButton(
                        inputId = "submitButton",
                        label = "Submit"
                    )
                )
            )
        ) %>%
            tagAppendAttributes(class = "contact_us_container"),
        style = css(
            background_image = "url(https://upload.wikimedia.org/wikipedia/commons/b/b4/Minnesota_rally_in_solidarity_with_Wisconsin_union_protesters_%285479652239%29.jpg)",
            background_repeat = "no-repeat",
            background_size = "cover",
            background_position = "center bottom",
        )
    )
}

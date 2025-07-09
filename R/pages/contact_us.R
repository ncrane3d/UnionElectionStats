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
                tags$div(align = "center", submitButton(text = "Submit"))
            )
        ) %>%
            tagAppendAttributes(class = "contact_us_container"),
        style = css(
            background_image = "url(www/rally1.jpg)",
            background_repeat = "no-repeat",
            background_size = "cover",
            background_position = "center bottom",
        )
    )
}

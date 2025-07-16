contact <- function() {
    nav_panel(
        title = "Contact Us",
        align = "left",
        div(
            card(
                height = "40%",
                id = "picketpost",
                fill = TRUE
            ),
            card(
                id = "picketsign",
                card_header(
                    p(" "),
                    imageOutput(
                        "contact_us_text",
                        width = "100%",
                        height = "auto"
                    )
                ),
                fill = FALSE,
                width = "15vw",
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
                    width = "100%",
                    height = "10vh"
                ),
                tags$div(align = "center", submitButton(text = "Submit"))
            ),
            tags$div(
                position = "fixed",
                bottom = 0,
                left = 0,
                right = 0,
                marginLeft = "auto",
                marginRight = "auto",
                marginBottom = "0px",
                id = "gompers",
                imageOutput("gompers", width = "320px", height = "auto")
            ),
        ) %>%
            tagAppendAttributes(class = "contact_us_container"),
        style = css(
            background_image = "url(www/picket_line.jpg)",
            background_repeat = "repeat",
            background_size = "cover",
            background_position = "center",
            overflow = "hidden"
        )
    )
}

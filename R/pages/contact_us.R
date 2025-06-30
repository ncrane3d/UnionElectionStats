contact <- function() {
    nav_panel(
        title = "Contact Us",
        align = "left",
        fillPage(
            tags$div(
                position = "absolute",
                class = "contactbg",
                tags$img(
                    src = "https://upload.wikimedia.org/wikipedia/commons/b/b4/Minnesota_rally_in_solidarity_with_Wisconsin_union_protesters_%285479652239%29.jpg",
                    height = "100vh"
                )
            ),
            #In the example above, we used CSS to add a scaling background image to the page. You can also use img() to create this effect:
            #Pass img() to any Shiny UI page method (e.g., page_fillable()). img() creates an image.
            #Pass the path or URL of your desired image to img()’s src parameter. Set additional parameters to control the appearance of the image (e.g., width and height).
            absolutePanel(
                right = "20vw",
                left = "20vw",
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
                    submitButton(text = "Submit")
                )
            )
        )
    )
}

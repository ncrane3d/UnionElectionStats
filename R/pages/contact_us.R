contact <- function() {
    nav_panel(
        title = "Contact Us",
        align = "center",
        #In the example above, we used CSS to add a scaling background image to the page. You can also use img() to create this effect:
        #Pass img() to any Shiny UI page method (e.g., page_fillable()). img() creates an image.
        #Pass the path or URL of your desired image to img()’s src parameter. Set additional parameters to control the appearance of the image (e.g., width and height).
        card(
            card_header("Contact us"),
            layout_column_wrap(
                width = 1 / 3,
                height = "5vh",
                fill = TRUE,
                textInput("name", "Name"),
                textInput("email", "Email"),
                textInput("subject", "Subject")
            ),
            textAreaInput(
                "message",
                "Message",
                resize = "none",
                value = "Your message here...",
                width = "100%",
            ),
            submitButton(text = "Submit")
        )
    )
}

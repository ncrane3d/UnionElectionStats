about <- function() {
    loremIpsum <- "Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat."
    nav_panel(
        title = "About",
        align = "center",
        absolutePanel(
            align = "center",
            right = "10vw",
            left = "10vw",
            card(
                card_header("Research Purpose", align = "center"),
                p(
                    "   Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat. In id cursus mi pretium tellus duis convallis. Tempus leo eu aenean sed diam urna tempor. Pulvinar vivamus fringilla lacus nec metus bibendum egestas. Iaculis massa nisl malesuada lacinia integer nunc posuere. Ut hendrerit semper vel class aptent taciti sociosqu.
                Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat. In id cursus mi pretium tellus duis convallis. Tempus leo eu aenean sed diam urna tempor. Pulvinar vivamus fringilla lacus nec metus bibendum egestas. Iaculis massa nisl malesuada lacinia integer nunc posuere. Ut hendrerit semper vel class aptent taciti sociosqu. Ad litora torquent per conubia nostra inceptos himenaeos.
                Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat. In id cursus mi pretium tellus duis convallis. Tempus leo eu aenean sed diam urna tempor. Pulvinar vivamus fringilla lacus nec metus bibendum egestas. Iaculis massa nisl malesuada lacinia integer nunc posuere. Ut hendrerit semper vel class aptent taciti sociosqu. Ad litora torquent per conubia nostra inceptos himenaeos.",
                    align = "left"
                )
            ),
            card(
                height = "50vh",
                card_header("Meet the Team", align = "center"),
                div(
                    align = "center"
                ),
                layout_column_wrap(
                    width = 1 / 3,
                    bioPanel(
                        "First Last",
                        "pfp_left",
                        loremIpsum
                    ),
                    bioPanel(
                        "First Last",
                        "pfp_middle",
                        loremIpsum
                    ),
                    bioPanel(
                        "First Last",
                        "pfp_right",
                        loremIpsum
                    ),
                ),
            )
            #page_fillable()
        )
    )
}

bioPanel <- function(firstlast, img, description) {
    absolutePanel(
        div(),
        align = "center",
        width = "25vw",
        height = "25vw",
        imageOutput(img, width = "10vw", height = "10vw"),
        h4(firstlast),
        p(description)
    )
}

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
                card_header("Priniple Investigators", align = "center"),
                div(
                    align = "center"
                ),
                layout_column_wrap(
                    width = 1 / 3,
                    min_width = 300,
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
            ),
            card(
                card_header("Site Developers", align = "center"),
                div(
                    align = "center"
                ),
                layout_columns(
                    col_widths = c(-1, 4, -2, 4, -1),
                    min_width = 300,
                    bioPanel(
                        "First Last",
                        "pfp_developer_left",
                        loremIpsum
                    ),
                    bioPanel(
                        "First Last",
                        "pfp_developer_right",
                        loremIpsum
                    )
                ),
            )
        )
    )
}

bioPanel <- function(firstlast, img, description) {
    div(
        align = "center",
        imageOutput(img, width = "150", height = "auto"),
        h4(firstlast),
        p(description)
    )
}

about <- function() {
    loremIpsum <- "Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat."
    nav_panel(
        title = "About",
        align = "center",
        div(
            card(
                card_header("Research Purpose", align = "center"),
                p(
                    "   Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat. In id cursus mi pretium tellus duis convallis. Tempus leo eu aenean sed diam urna tempor. Pulvinar vivamus fringilla lacus nec metus bibendum egestas. Iaculis massa nisl malesuada lacinia integer nunc posuere. Ut hendrerit semper vel class aptent taciti sociosqu.
                Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat. In id cursus mi pretium tellus duis convallis. Tempus leo eu aenean sed diam urna tempor. Pulvinar vivamus fringilla lacus nec metus bibendum egestas. Iaculis massa nisl malesuada lacinia integer nunc posuere. Ut hendrerit semper vel class aptent taciti sociosqu. Ad litora torquent per conubia nostra inceptos himenaeos.
                Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat. In id cursus mi pretium tellus duis convallis. Tempus leo eu aenean sed diam urna tempor. Pulvinar vivamus fringilla lacus nec metus bibendum egestas. Iaculis massa nisl malesuada lacinia integer nunc posuere. Ut hendrerit semper vel class aptent taciti sociosqu. Ad litora torquent per conubia nostra inceptos himenaeos.",
                    align = "left"
                )
            )%>%
            tagAppendAttributes(class = "scrollable-panel"),
            card(
                card_header("Principle Investigators", align = "center"),
                div(
                    align = "center"
                ),
                layout_column_wrap(
                    width = 1 / 3,
                    min_width = 300,
                    bioPanel(
                        "Jonne Kamphorst",
                        "jonne",
                        "I am a Postdoctoral Scholar at Stanford University’s Politics and Social Change Lab and the Human-centered Artificial Intelligence institute. 
                        I received my Ph.D. in Political Science from the European University Institute (EUI) in 2023. 
                        I will start as an Assistant Professor in Political Science and Quantitative Social Science Methods at Sciences Po in Paris in January 2026. "
                    ),
                    bioPanel(
                        "Zachary Schaller",
                        "zachary",
                        "I am an applied microeconomist specializing in industrial relations, regional economic development, construction IO, and economic history. 
                        I am particularly interested in labor market institutions, with my current research focused on unions and how deunionization in the US has affected local labor markets."
                    ),
                    bioPanel(
                        "Samuel Young",
                        "sam",
                        "I am currently an assistant professor of economics at Arizona State University. 
                        Previously, I was a postdoctoral fellow at the U.S. Census Bureau. 
                        I received my Ph.D. in economics from MIT in 2022."
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
        ) %>%
            tagAppendAttributes(class = "centered-card"),
    )
}

bioPanel <- function(firstlast, img, description) {
    div(
        div(
            align = "center",
            imageOutput(img, width = "150", height = "auto"),
            h4(firstlast)
        )
        div(
            p(description)
        )
    ),
}

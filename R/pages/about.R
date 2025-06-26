about <- function() {
    nav_panel(
    title = "About",
    align = "right",
    layout_columns(
        card(
            card_header("Research Purpose", align = "center"),
            p("   Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat. In id cursus mi pretium tellus duis convallis. Tempus leo eu aenean sed diam urna tempor. Pulvinar vivamus fringilla lacus nec metus bibendum egestas. Iaculis massa nisl malesuada lacinia integer nunc posuere. Ut hendrerit semper vel class aptent taciti sociosqu.
                Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat. In id cursus mi pretium tellus duis convallis. Tempus leo eu aenean sed diam urna tempor. Pulvinar vivamus fringilla lacus nec metus bibendum egestas. Iaculis massa nisl malesuada lacinia integer nunc posuere. Ut hendrerit semper vel class aptent taciti sociosqu. Ad litora torquent per conubia nostra inceptos himenaeos.
                Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat. In id cursus mi pretium tellus duis convallis. Tempus leo eu aenean sed diam urna tempor. Pulvinar vivamus fringilla lacus nec metus bibendum egestas. Iaculis massa nisl malesuada lacinia integer nunc posuere. Ut hendrerit semper vel class aptent taciti sociosqu. Ad litora torquent per conubia nostra inceptos himenaeos.",
                align = "left"
            )
        ),
        card(
            card_header("Meet the Team", align = "center"),
            layout_columns(
                imageOutput("pfp_left"),
                imageOutput("pfp_middle"),
                imageOutput("pfp_right"),
                h2("First Last", align = "left"),
                h2("First Last", align = "left"),
                h2("First Last", align = "left"),
                p("Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat.", align = "left"),
                p("Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat.", align = "left"),
                p("Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat.", align = "left"),
                col_widths = c(-1, 2, -2, 2, -2, 2, -1, -1, 2, -2, 2, -2, 2, -1, -1, 2, -2, 2, -2, 2, -1),
                #row_heights = c(2, 1, 1)
            )
        ),
        col_widths = c(-1, 10, -1, -1, 10),
        page_fillable()
        )
    )
}

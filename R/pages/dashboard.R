dashboard <- function() {
    nav_panel(
        title = "Home",
    align = "right",
    card(
        full_screen = TRUE,
        card_header("Lorem ipsum donor ad somet"),
        layout_sidebar(sidebarPanel(width = 2),
    mainPanel(width = 12, 
        layout_column_wrap(width = "2000px", 
            card(width = 12,solidHeader = TRUE, right = "0px", top = "0px", "TEST!")),
        layout_columns(
            card(card_header("test"), fill = FALSE), 
            card(card_header("Custom Visualization"), card("jfaksjdlsj"), selectInput("select", "Graph type:", list("Pi Chart" = "PI", "Line Graph" = "LINE", "Histogram" = "HIST")))
            ),
        layout_columns(
            card(card_header("test"), fill = FALSE), 
            card("card 2", fill = TRUE),
            card("card 3", fill = TRUE)
            )        )
    )))

}
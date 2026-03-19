about <- function(id) {
    ns <- NS(id)
    loremIpsum <- "Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat."
    nav_panel(
        title = "About",
        align = "center",
        div(
            card(
                card_header("Research Purpose", align = "center"),
                div(
                    p("In a time of high income/wealth inequality and widespread wage theft, the labor movement is enjoying renewed interest.
                      But we must learn from history if we are to ensure that modern policy fulfills the needs of both capital and labor.
                      To that end, quality data is essential.
                      This team brings over 15 years of combined expertise on union organizing records to provide the richest and most complete dataset on NLRB elections ever compiled.
                      We do not say this to brag, but to highlight that this project is the outcome of a multi-year effort to gather, harmonize, and merge many fragments of data collected from previous scholars and various FOIA requests.
                      We also painstakingly geocoded the data whenever possible, chasing down location info by hand when necessary.
                      The database is thus ambitious in its scope and novel in its comprehensiveness and accessibility.
                      It contains the known universe of representation elections from 1962 to 2024.
                      Soon it will also track the most recent elections as cases close.", align = "left") %>%
                        tagAppendAttributes(class = "paragraphMarginFix"),
                    tags$br(),
                    p("Election data are especially useful since they are generated from high-frequency events with rich geographic and industry detail.
                      Whereas membership data come primarily from annual surveys reported at coarse levels of aggregation,
                      election records are administrative micro-data, allowing for much richer analysis.
                      Our hope is that by making the data more accessible, other folks can make bold new contributions to knowledge and policy.
                      Happy researching!", align = "left") %>%
                        tagAppendAttributes(class = "paragraphMarginFix"),
                )
            )%>%
            tagAppendAttributes(class = "scrollable-panel"),
            card(
                card_header("Principal Investigators", align = "center"),
                layout_column_wrap(
                    width = 1 / 3,
                    min_width = 300,
                    bioPanel(
                        "Jonne Kamphorst",
                        "www/resources/images/jonne_kamphorst.jpg",
                        "I am an Assistant Professor in Political Science at Sciences Po in Paris. Previously, I was a postdoctoral scholar at Stanford University's
                        Sociology and Computer Science departments. I received my PhD from the European University Institute in 2023."
                    ),
                    bioPanel(
                        "Zachary Schaller",
                        "www/resources/images/zachary_schaller.png",
                        "I am an applied microeconomist specializing in industrial relations, regional economic development, construction IO, and economic history.
                        I am particularly interested in labor market institutions, with my current research focused on unions and how deunionization in the US has affected local labor markets."
                    ),
                    bioPanel(
                        "Samuel Young",
                        "www/resources/images/samuel_young.jpg",
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
                layout_column_wrap(
                    width = 1/3,
                    min_width = 300,
                    bioPanel(
                        "Lucy Lewark",
                        "www/resources/images/lucy_lewark.png",
                        "I am an undergraduate student in Art and Computer Science at Colorado State University.
                        I am particularly interested in topics at the intersection of Computer Science and visual
                        design, whether that's game design or general optimization for human-computer interaction."
                    ),
                    bioPanel(
                        "Nathan Crane",
                        "www/resources/images/nathan_crane.jpg",
                        "I am a recent CSU graduate working as a software engineer in the Colorado area. Feel free to reach out!"
                    ),
                    bioPanel(
                      "Alison Podgorski",
                      "www/resources/images/Alison_Podgorski.jpg",
                      "I am an undergraduate student in Statistics and Economics at Colorado State University.
                      I am currently focused on studying quantitative analysis in microeconomics."
                    )
                ),
            )
        ) %>%
            tagAppendAttributes(class = "centered-card"),
    )
}

bioPanel <- function(firstlast, imgPath, description) {
    div(
        align = "center",
        tags$figure(
            tags$img(src=imgPath, alt="coolest", width = "60%", height = "auto")
        ),
        h4(firstlast),
        p(description)
    )%>%
    tagAppendAttributes(class = "bio-picture")
}

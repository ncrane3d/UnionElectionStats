featuredAnalysis <- function() {
    loremIpsum <- "Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat. In id cursus mi pretium tellus duis convallis. Tempus leo eu aenean sed diam urna tempor. Pulvinar vivamus fringilla lacus nec metus bibendum egestas. Iaculis massa nisl malesuada lacinia integer nunc posuere. Ut hendrerit semper vel class aptent taciti sociosqu. Ad litora torquent per conubia nostra inceptos himenaeos.
                    Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat. In id cursus mi pretium tellus duis convallis. Tempus leo eu aenean sed diam urna tempor. Pulvinar vivamus fringilla lacus nec metus bibendum egestas. Iaculis massa nisl malesuada lacinia integer nunc posuere. Ut hendrerit semper vel class aptent taciti sociosqu. Ad litora torquent per conubia nostra inceptos himenaeos.
                    Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat. In id cursus mi pretium tellus duis convallis. Tempus leo eu aenean sed diam urna tempor. Pulvinar vivamus fringilla lacus nec metus bibendum egestas. Iaculis massa nisl malesuada lacinia integer nunc posuere. Ut hendrerit semper vel class aptent taciti sociosqu. Ad litora torquent per conubia nostra inceptos himenaeos.
                    Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat. In id cursus mi pretium tellus duis convallis. Tempus leo eu aenean sed diam urna tempor. Pulvinar vivamus fringilla lacus nec metus bibendum egestas. Iaculis massa nisl malesuada lacinia integer nunc posuere. Ut hendrerit semper vel class aptent taciti sociosqu. Ad litora torquent per conubia nostra inceptos himenaeos.
                    Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat. In id cursus mi pretium tellus duis convallis. Tempus leo eu aenean sed diam urna tempor. Pulvinar vivamus fringilla lacus nec metus bibendum egestas. Iaculis massa nisl malesuada lacinia integer nunc posuere. Ut hendrerit semper vel class aptent taciti sociosqu. Ad litora torquent per conubia nostra inceptos himenaeos."
    authors <- c("Jane Doe", "John Doe", "Terry Mason")
    nav_panel(
        title = "Featured Analysis",
        align = "right",
        accordion(
            paperPanel(
                "Decomposing the Decline of Unions: Revisiting Sectoral and Regional Shifts",
                "Schaller, Zachary",
                "This study uses newly disaggregated National Labor Relations Board (NLRB) election data to revisit the theory 
                that sectoral and regional shifts in economic activity contributed substantially to private-sector union decline 
                in the United States. Unlike most studies, which focus on differential employment growth among union and non-union 
                establishments, this article focuses on how such shifts may have affected organizational rates themselves. Improved 
                data permit a shift-share decomposition that indicates that approximately 40% of the decline in union elections is in 
                response to sectoral shifts, the majority attributable to changes within each sector. Moreover, in an update to Dickens 
                and Leonard’s 1985 study, the author shows that declining organization rates since 1980 are responsible for a decline in 
                union density of 5.4 percentage points.",
                "https://colostate.primo.exlibrisgroup.com/permalink/01COLSU_INST/51l05n/cdi_proquest_journals_2774436954"
            ),
            paperPanel(
                "The Decline of U.S. Labor Unions: Import Competition and NLRB Elections",
                "Schaller, Zachary",
                "Why has private sector union participation fallen away so much in the United States since the late 1950s? Featuring an 
                improved dataset on National Labor Relations Board (NLRB) representation elections, I present evidence that import penetration 
                accounts for approximately 40 percent of the decline in union formation for U.S. manufacturing. This estimate translates 
                to 4.6 percent of the decline in private sector union density. The effect is driven by trade with low-income countries and, 
                to some extent, other high-income countries. China is not a factor early on, but their strong import growth since 2000 can 
                account for about 12 percentage points of the total decline.",
                "https://colostate.primo.exlibrisgroup.com/permalink/01COLSU_INST/51l05n/cdi_proquest_journals_2771563299"

            ),
            paperPanel(
                "A Game Theory Perspective on Delivery Methods in Construction",
                "Schaller, Zachary ; Killingsworth, John",
                "Why is Integrated Project Delivery (IPD) a relatively underutilized procurement method in construction? Purpose: Expose 
                and explain a few market failures that owners/developers might be ignoring by choosing traditional methods over IPD. Research 
                Method: Game theoretic modeling and application of microeconomic principles. Informed by interviews with IPD participants, we model 
                the important strategic and social advantages of IPD that complement more well-known efficiency advantages. Findings: Our primary 
                insight is that traditional design-bid-build projects encounter pervasive moral hazard problems and externalities that reduce the 
                efficiency of construction and create conflict between participants. At a basic human behavior level, IPD eliminates or mitigates 
                these issues. Limitations: The interviews we conducted provide insight, not empirical inference. Therefore, this paper stands on 
                its theoretical contribution and makes no boast of providing representative data or causal analysis. Implications: Owners/developers 
                would do well to embrace IPD given its social and strategic contributions to Lean Construction. Additional efficiencies we highlight 
                complement the more well-known advantages, possibly tipping the scales toward IPD for a greater number of construction projects. Value 
                for practitioners: This paper will explain how non-integrated methods such as designbid-build create greater cost and conflict than 
                previously realized. It suggests a path forward through (scalable) IPD that mitigates these costs.",
                "https://colostate.primo.exlibrisgroup.com/permalink/01COLSU_INST/51l05n/cdi_proquest_journals_3076055158"
            )
        )
    )
}

paperPanel <- function(paperName, authors, abstract, link) {
    accordion_panel(
        title = paperName,
        div(
            align = "left",
            div(strong("Author(s): "), p(paste(authors, collapse = ", "))),
            div(strong("Abstract: "), p(abstract)),
            div(strong("Read: "), p(tags$a(href=link, paperName)))
        ) %>%
            tagAppendAttributes(id = "accordion-analysis")
    )
}

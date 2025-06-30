#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import magrittr
#' @import leaflet
#' @import shinipsum
#'
#' @noRd
app_ui <- function(request) {
  #Importing local files
  source('./R/pages/dashboard.R', local = TRUE)
  source('./R/pages/about.R', local = TRUE)
  source('./R/pages/downloads.R', local = TRUE)
  source('./R/pages/featured_analysis.R', local = TRUE)
  source('./R/pages/contact_us.R', local = TRUE)

  #Set up site theme
  theme <- bs_theme(
    bg = "#93AFC2", #Light blue background
    fg = "#283044", #Dark blue of navbar
    primary = "#93AFC2", #Light blue background
    secondary = "#131620", #Black/blue for text
    base_font = "Jost"
  ) %>%
    bs_add_rules(
      list(
        #Shiny adds in classes to most elements it generates, if you'd like to add more rules just inspect the webapge to find what the classes are.
        ".nav-link, .navbar-brand { color: #FDF9F6; }",
        ".navbar-brand:hover { color: #FDF9F6; }",
        ".nav-link, .nav-link-active { font-size: medium; }",
        ".card, .sidebar { background-color:#FDF9F6 !important; }", #Using important here will overwrite everything, could cause errors.
        ".dashboardheader, { background-color: #FDF9F6 !important; padding: 25px; margin-top : -25px !important;}",
        ".sidebar {position: sticky; overflow: hidden !important;}",
        ".contactcard {margin-top: 15vh;}",
        ".contactbg {max-width: 100%; height: auto; margin : -25px !important; position: absolute;}"
      )
    )

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_navbar(
      theme = theme,
      navbar_options = list(bg = "#283044"),
      title = "Union Election Stats",
      nav_spacer(),
      dashboard(),
      about(),
      downloads(),
      featuredAnalysis(),
      contact(),
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "UnionElectionStats"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

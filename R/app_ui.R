#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  #Importing local files
  source('./R/pages/dashboard.R', local = TRUE)
  source('./R/pages/about.R', local = TRUE)
  source('./R/pages/downloads.R', local = TRUE)
  source('./R/pages/featured_analysis.R', local = TRUE)
  source('./R/pages/contact_us.R', local = TRUE)

  #Set up site theme
  theme <- bs_theme (
    bg = "#93AFC2", #Light blue background
    fg = "#283044", #Dark blue of navbar
    primary = "#131620", #Black/blue for text
    secondary = "#FDF9F6", #Off-white
    base_font = "Jost"
  )

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      theme = theme,
      
      page_navbar(
        sidebar = sidebar(),
        title = "Union Election Stats",
        nav_spacer(),
        dashboard(),
        about(),
        downloads(),
        featuredAnalysis(),
        contact(),
      )
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

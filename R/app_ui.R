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
        #Shiny adds in classes to most elements it generates, if you'd like to add more rules just inspect the webpage to find what the classes are.
        ".nav-link, .navbar-brand { color: #FDF9F6; }",
        ".navbar-brand:hover { color: #FDF9F6; }",
        ".nav-link, .nav-link-active { font-size: medium; }",
        ".card, .sidebar { background-color:#FDF9F6 !important; border-radius: 0px;}", #Using important here will overwrite everything, could cause errors.
        ".centered-card {  width: 80%; margin: auto; overflow: hidden; }",
        ".dashboardheader, { background-color: #FDF9F6 !important; padding: 25px; margin-top : -25px !important;}",
        ".scrollable-panel {overflow: hidden !important; }",
        ".sidebar {position: sticky; }",
        ".contactcard {margin-top: 8vh;}",
        ".contactbg {max-width: 100%; height: auto; margin : -25px !important; position: absolute;}",
        ".contact_us_container {width:60%; margin: auto; }",
        ".tab-pane {padding:25px 0px 0px 0px !important; }",
        ".navbar div .collapsing { max-height: 200px; }",
        ".bslib-sidebar-layout {margin-bottom: 0px; border: 0 !important;}",
        ".navbar {position: sticky; top: 0; z-index:1; }",
        ".tooltip-adjustment, .artificial-gap {margin-bottom: 16px !important; }",
        ".sidebar-content {row-gap: 8px !important; }", 
        ".custom-visualization-margin { margin-bottom: 10px !important; }",
        # Below is styling for all accordions, as seen on featured analysis page
        ".accordion { width: 80%; margin-left: auto; margin-right: auto; --bs-accordion-btn-focus-box-shadow: 0 0 0 .25rem rgba(147, 175, 194, 0) !important;}",
        ".accordion-item, .accordion-header, .accordion-button{  background-color: #FDF9F6;}",
        ".accordion-title {font-weight: bold;}",
        ".accordion-item { margin-bottom: 18px; margin-top: 2px; border-top-width: 1px !important; }",
        ".accordion-body p { margin-bottom: 0px; text-align:left; padding: 16px; font-weight: normal;}",
        ".accordion-button:hover {background-color:rgb(189, 185, 188); }",
        #Below is styling for text accordions, as seen on the download page
        "#accordion-download { margin: 0px; }",
        "#accordion-download .accordion-title { font-weight: normal;}",
        "#accordion-download .accordion-header { display: inline-block; font-weight: normal !important;}",
        "#accordion-download .accordion-button.collapsed:after { background-image: url('https://www.svgrepo.com/show/470572/caret-down.svg') ; background-size: 20px; color: #000000;  margin-left: 5px; width: 20px; height: 20px; } ",
        "#accordion-download .accordion-button:not(.collapsed)::after { background-image: url('https://www.svgrepo.com/show/470572/caret-down.svg') ; background-size: 20px; color: #000000;  margin-left: 5px; width: 20px; height: 20px; transform: rotate(180deg);  }"
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

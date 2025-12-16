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
        ".card, .sidebar { background-color:#FDF9F6 !important; border-radius: 0px;}",
        ".centered-card {  width: 80%; margin: auto; overflow: hidden; }",
        ".dashboardheader, { background-color: #FDF9F6 !important; padding: 25px; margin-top : -25px !important;}",
        ".dashboardheader bslib-layout-columns { margin-bottom: 0 !important; }",
        ".scrollable-panel {overflow: hidden !important; }",
        ".sidebar {position: sticky; top: 10vh;}",
        ".leaflet-left {z-index:500 !important; }",
        ".contact_us_container {width:100%; margin: auto; height: auto; }",
        ".contact_us_container #picketsign {width:60%; margin: auto; }",
        ".contact_us_container div { margin-bottom: 0px!important; }",
        ".tab-pane {padding:25px 0px 0px 0px !important; }",
        ".navbar div .collapsing { max-height: 200px; }",
        ".bslib-sidebar-layout {margin-bottom: 0px; border: 0 !important;}",
        ".navbar {position: sticky; top: 0; z-index:1000; }",
        ".tooltip-adjustment, .artificial-gap {margin-bottom: 16px !important; }",
        ".sidebar-content {row-gap: 8px !important; }", 
        ".custom-visualization-margin { margin-bottom: 10px !important; }",
        "paragraphMarginFix { margin-bottom: 0px !important;}",
        ".modal-body a { color: #283044 !important; }",
        ".modal-title { margin: auto; font-size: 32px !important; }",
        "div h6 {margin-bottom: 0px; margin-top: 8px; }",
        ".modal-content {background-color: #FDF9F6; }",
        # Below is styling for all accordions, as seen on featured analysis page
        ".accordion { width: 80%; margin-left: auto; margin-right: auto; --bs-accordion-btn-focus-box-shadow: 0 0 0 .25rem rgba(147, 175, 194, 0) !important;}",
        ".accordion-item, .accordion-header, .accordion-button{  background-color: #FDF9F6;}",
        ".accordion-title {font-weight: bold;}",
        ".accordion-item { margin-bottom: 18px; margin-top: 2px; border-top-width: 1px !important; }",
        ".accordion-body p { margin-bottom: 0px; text-align:left; padding: 16px; font-weight: normal;}",
        ".accordion-button:hover {background-color:rgb(189, 185, 188); }",
        ".accordion-figure { height: auto !important; }",
        ".abstract-row { display: flex; flex-wrap: wrap; gap: 1rem;}",
        ".abstract-figure img { width: 100%; height: auto; display: block; }",
        ".abstract-figure { flex: 0 1 320px; align-self: flex-start; height: auto !important;}",
        ".abstract-text { flex: 1 1 320px; }",
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
      dashboard("dashboard"),
      about("about"),
      downloads("downloads"),
      featuredAnalysis("featuredAnalysis"),
      contact("contact"),
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

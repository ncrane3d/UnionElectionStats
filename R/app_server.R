#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  #About Me Images TODO: Replace with actual images
  output$pfp_left<- renderImage( 
    { 
      list(src = "./resources/images/pfp_empty.png", height= "auto", width= "100%") 
    }, 
    deleteFile = FALSE
  ) 

  output$pfp_middle<- renderImage( 
    { 
      list(src = "./resources/images/pfp_empty.png", height= "auto", width= "100%") 
    }, 
    deleteFile = FALSE
  ) 

  output$pfp_right<- renderImage( 
    { 
      list(src = "./resources/images/pfp_empty.png", height= "auto", width= "100%") 
    }, 
    deleteFile = FALSE
  ) 
}

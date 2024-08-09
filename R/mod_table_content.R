#' table_content UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_table_content_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' table_content Server Functions
#'
#' @noRd 
mod_table_content_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_table_content_ui("table_content_1")
    
## To be copied in the server
# mod_table_content_server("table_content_1")

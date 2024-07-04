#' generic_charts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_generic_charts_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' generic_charts Server Functions
#'
#' @noRd 
mod_generic_charts_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_generic_charts_ui("generic_charts_1")
    
## To be copied in the server
# mod_generic_charts_server("generic_charts_1")

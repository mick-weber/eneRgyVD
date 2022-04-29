#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # MODULE
  mod_inputs_server("inputs_1")
  # MODULE
  mod_map_selector_server("map_selector_1")


}


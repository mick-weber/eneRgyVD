#' unit_converter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shinyWidgets prettyRadioButtons


mod_unit_converter_ui <- function(id){
  ns <- NS(id)
  tagList(tags$li(
      shinyWidgets::prettyRadioButtons(inputId = ns("in_unit"), label = NULL,
                                       choices = c("kWh", "MWh", "GWh", "TJ"),
                                       selected = "kWh",
                                       inline = TRUE,
                                       status =  "default",
                                       icon = icon("check"),
                                       animation = "jelly",),class = "dropdown")
    )
}

#' unit_converter Server Functions
#'
#' @noRd
mod_unit_converter_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
})
}

  # testing module
  # nameApp <- function() {
  #     ui <- fluidPage(
  #       mod_unit_converter_ui("id")
  #     )
  #     server <- function(input, output, session) {
  #       mod_unit_converter_server("id")
  #     }
  #     shinyApp(ui, server)
  # }
  # nameApp()

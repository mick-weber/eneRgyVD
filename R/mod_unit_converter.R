#' unit_converter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd



mod_unit_converter_ui <- function(id){
  ns <- NS(id)
  #tagList(
    tags$li(class = "dropdown", # dropdown class required
      shinyWidgets::prettyRadioButtons(inputId = ns("selected_unit"), label = NULL,
                                       choices = c("kWh", "MWh", "GWh", "TJ"),
                                       selected = "MWh",
                                       inline = TRUE,
                                       status =  "default",
                                       icon = icon("check"),
                                       animation = "jelly")
      )# End tags$li
}

#' unit_converter Server Functions
#'
#' @noRd
mod_unit_converter_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    selectedUnit <- reactiveValues()

    observe({
      selectedUnit$unit_to <- input$selected_unit
    })

    return(selectedUnit) # never forget this one

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

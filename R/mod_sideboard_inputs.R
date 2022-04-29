#' sideboard_inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sideboard_inputs_ui <- function(id){
  ns <- NS(id)
  tagList(

    selectizeInput(inputId = "selected_communes",
                   label = "Commune(s)",
                   choices = communes_names,
                   selected = NULL,
                   multiple = TRUE,
                   options = list(placeholder = "Sélectionner une ou plusieurs communes")),
    # Select input for zooming on the districts (WIP feature)
    selectizeInput(inputId = "district",
                   label = "Zoom par district",
                   choices = districts_names,
                   selected = 0,
                   multiple = FALSE)

  )
}

#' sideboard_inputs Server Functions
#'
#' @noRd
mod_sideboard_inputs_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # do something

  })
}

## To be copied in the UI
# mod_sideboard_inputs_ui("sideboard_inputs_1")

## To be copied in the server
# mod_sideboard_inputs_server("sideboard_inputs_1")



  # testing module
  nameApp <- function() {
      ui <- fluidPage(
        mod_sideboard_inputs_ui("id")
      )
      server <- function(input, output, session) {
        mod_sideboard_inputs_server("id")
      }
      shinyApp(ui, server)
  }
  nameApp()




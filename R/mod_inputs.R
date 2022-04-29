#' sideboard_inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_inputs_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
    # Select input for municipalities
    shiny::selectizeInput(inputId = ns("selected_communes"),
                   label = "Commune(s)",
                   choices = communes_names,
                   selected = NULL,
                   multiple = TRUE),

    # options = list(placeholder = "Sélectionner une ou plusieurs communes")

    # Select input for zooming on the districts (WIP feature)
    shiny::selectizeInput(inputId = ns("selected_district"),
                   label = "Zoom par district",
                   choices = districts_names,
                   selected = 0,
                   multiple = FALSE)

    # Other select inputs for datasets ?

  )
}

#' sideboard_inputs Server Functions
#'
#' @noRd
mod_inputs_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    inputVals <- reactiveValues()


    observe({

      inputVals$selectedCommunes <- input$selected_communes
      inputVals$selectedDistrict <- input$selected_district

    })

    # Our returning values when the module is called

    return(inputVals)

  })
}

## To be copied in the UI
# mod_sideboard_inputs_ui("inputs_1")

## To be copied in the server
# mod_sideboard_inputs_server("inputs_1")



  # testing module
  # nameApp <- function() {
  #     ui <- fluidPage(
  #       mod_inputs_ui("id")
  #     )
  #     server <- function(input, output, session) {
  #       mod_inputs_server("id")
  #     }
  #     shinyApp(ui, server)
  # }
  # nameApp()




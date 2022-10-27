#' collapse_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_collapse_box_ui <- function(id){
  ns <- NS(id)
  tagList(

    uiOutput(ns("vd_box"))

  )
}

#' collapse_box Server Functions
#'
#' @noRd
mod_collapse_box_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$vd_box <- renderUI({

      bs4Dash::box(
        solidHeader = FALSE,
        collapsible = TRUE,
        title = "Le canton en quelques chiffres",
        background = NULL,
        width = 12,
        status = "success",
        footer = fluidRow(
          column(
            width = 6,
            bs4Dash::descriptionBlock(
              # numberColor = "green",
              marginBottom = TRUE, # for testing
              number = "2020",
              header = "2'000 GWh",
              text = "Production d'électricité"
            )
          ),
          column(
            width = 6,
            bs4Dash::descriptionBlock(
              # numberColor = "green",
              number = "2020",
              header = "5'000 GWh",
              text = "Consommation d'électricité"
            )
          )
        )
      )

    })
  })
}

## To be copied in the UI
# mod_collapse_box_ui("collapse_box_1")

## To be copied in the server
# mod_collapse_box_server("collapse_box_1")

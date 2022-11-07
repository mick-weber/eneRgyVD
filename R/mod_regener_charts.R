#' regener_charts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_regener_charts_ui <- function(id){
  ns <- NS(id)
  tagList(

    # ggalluvial plot
    shiny::plotOutput(ns("chart_alluvial"))

  )
}

#' regener_charts Server Functions
#'
#' @noRd
mod_regener_charts_server <- function(id, data, var_commune){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$chart_alluvial <- shiny::renderPlot({

      data %>%
        create_alluvial_chart(var_commune = var_commune) # fct_helpers.R


    })


  })
}

## To be copied in the UI
# mod_regener_charts_ui("regener_charts_1")

## To be copied in the server
# mod_regener_charts_server("regener_charts_1")

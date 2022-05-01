#' elec_prod_filter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_elec_prod_filter_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' elec_prod_filter Server Functions
#'
#' @noRd
mod_elec_prod_filter_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    # for tabProd-related selectInputs we retrieve available years and techs from reactive dataset

    ## reactive dataset filtered by selected communes
    subset_elec_prod <- reactive({

      elec_prod_communes %>%
        filter(commune %in% input$selected_communes)

    })

    ## min/max available years


  })
}

## To be copied in the UI
# mod_elec_prod_filter_ui("elec_prod_filter_1")

## To be copied in the server
# mod_elec_prod_filter_server("elec_prod_filter_1")

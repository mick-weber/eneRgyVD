#' vd_collapse_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_vd_collapse_box_ui <- function(id){
  ns <- NS(id)
  tagList(

    uiOutput(ns("vd_box"))

  )
}

#' vd_collapse_box Server Functions
#'
#' @noRd
mod_vd_collapse_box_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$vd_box <- renderUI({

      shinydashboardPlus::box(
        solidHeader = FALSE,
        collapsible = TRUE,
        title = "Le canton en quelques chiffres",
        background = NULL,
        width = 12,
        status = "success",
        footer = fluidRow(
          fluidRow(
          column(
            width = 6,
            shinydashboardPlus::descriptionBlock(
              numberColor = "green", marginBottom = TRUE, # for testing
              number = vd_last_year, # utils_helpers.R
              header = paste0(format(prod_elec_vd_last_year/1e6, big.mark = "'", digits = 1), " GWh"),
              text = "Production d'électricité"
            )# End descriptionBlock
          ),# End column
          column(
            width = 6,
            shinydashboardPlus::descriptionBlock(
              numberColor = "green",
              number = vd_last_year, # utils_helpers.R
              header = paste0(format(cons_elec_vd_last_year/1e6, big.mark = "'", digits = 1), " GWh"),
              text = "Consommation d'électricité"
            )# End descriptionBlock
          )# End column
        ),# End 1st fluidrow
      column(
        width = 12,
        shinydashboardPlus::descriptionBlock(
          numberColor = "green",
          number = vd_last_year, # utils_helpers.R
          header = scales::label_percent(accuracy = .1)(coverage_elec_vd_last_year),
          text = "Taux de couverture annuel"
        )
      )

        )# # End 2nd fluidRow
      )# End box
    })

  })
}

## To be copied in the UI
# mod_vd_collapse_box_ui("vd_collapse_box_1")

## To be copied in the server
# mod_vd_collapse_box_server("vd_collapse_box_1")

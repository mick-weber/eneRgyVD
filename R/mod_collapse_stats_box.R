#' collapse_stats_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_collapse_stats_box_ui <- function(id){
  ns <- NS(id)
  tagList(

    uiOutput(ns("vd_box"))

  )
}

#' collapse_stats_box Server Functions
#'
#' @noRd
mod_collapse_stats_box_server <- function(id,
                                          title,
                                          production_value,
                                          consumption_value,
                                          year){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$vd_box <- renderUI({

      shinydashboardPlus::box(
        solidHeader = FALSE,
        collapsible = TRUE,
        title = title,
        background = NULL,
        width = 12,
        status = "success",
        footer = fluidRow(
          fluidRow(
          column(
            width = 6,
            shinydashboardPlus::descriptionBlock(
              numberColor = "green", marginBottom = TRUE, # for testing
              number = year,
              header = paste0(format(production_value/1e6, big.mark = "'", digits = 1), " GWh"),
              text = htmltools::HTML(paste0("Production", tags$br(), "d'électricité")),
              rightBorder = TRUE
            )# End descriptionBlock
          ),# End column
          column(
            width = 6,
            shinydashboardPlus::descriptionBlock(
              numberColor = "green",
              number = year,
              header = paste0(format(consumption_value/1e6, big.mark = "'", digits = 1), " GWh"),
              text = htmltools::HTML(paste0("Consommation", tags$br(), "d'électricité")),
              rightBorder = FALSE
            )# End descriptionBlock
          )# End column
        ),# End 1st fluidrow
      column(
        width = 12,
        shinydashboardPlus::descriptionBlock(
          numberColor = "green",
          number = year,
          header = scales::label_percent(accuracy = .1)(production_value/consumption_value),
          text = htmltools::HTML(paste0("Taux de couverture", tags$br(), "électrique annuel")),
          rightBorder = FALSE
        )
      )

        )# # End 2nd fluidRow
      )# End box
    })

  })
}


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
                                          selectedUnit,
                                          production_value,
                                          consumption_value,
                                          year){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$vd_box <- renderUI({

      bs4Dash::box(
        width = 12, # = 100% of the width = 4 allowed for the module
        solidHeader = FALSE,
        collapsible = TRUE,
        title = title,
        background = NULL,
        status = "success",
        fluidRow(
          column(
            width = 6,
            bs4Dash::descriptionBlock(
              marginBottom = TRUE,
              number = year,
              # header: we paste the value in kwh and pass it to convert_units(), and we format it
              header = paste(format(production_value %>% convert_units(unit_to = selectedUnit$unit_to), big.mark = "'", digits = 1, scientific = FALSE), selectedUnit$unit_to),
              text = htmltools::HTML(paste0("Production", tags$br(), "d'électricité")),
              rightBorder = TRUE
            )# End descriptionBlock
          ),# End column
          column(
            width = 6,
            bs4Dash::descriptionBlock(
              number = year,
              # header: we paste the value in kwh and pass it to convert_units(), and we format it
              header = paste(format(consumption_value%>% convert_units(unit_to = selectedUnit$unit_to), big.mark = "'", digits = 1, scientific = FALSE), selectedUnit$unit_to),
              text = htmltools::HTML(paste0("Consommation", tags$br(), "d'électricité")),
              rightBorder = FALSE
            )# End descriptionBlock
          )# End column
        ),# End 1st fluidrow

        fluidRow(
      column(
        width = 12,
        bs4Dash::descriptionBlock(
          number = year,
          # header: no need for unit conversion since it's percents
          header = scales::label_percent(accuracy = .1)(production_value/consumption_value),
          text = htmltools::HTML(paste0("Taux de couverture", tags$br(), "electrique annuel")),
          rightBorder = FALSE
        )# End descriptionBlock
      )# End column
      )# End 2nd fluidRow
      )# End box
    })# End renderUI
  })# End moduleServer
}


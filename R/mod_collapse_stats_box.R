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
                                          prod_elec_value,
                                          cons_elec_value,
                                          cons_rg_value,
                                          year){
  moduleServer(id, function(input, output, session){
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
            width = 12, # before: 6 !!CONS_ELEC removed!!
            bs4Dash::descriptionBlock(
              marginBottom = TRUE,
              number = year,
              header = paste(format(prod_elec_value,
                                    big.mark = "'", digits = 1, scientific = FALSE), selectedUnit$unit_to),
              text = tags$p("Production", icon("bolt", class = "iconColor")),
              rightBorder = FALSE # set to TRUE when !! CONS_ELEC removed !! comes
            )# End descriptionBlock 1/3
          ),# End column

          # !!CONS_ELEC removed!! # column(
          # !!CONS_ELEC removed!! #   width = 6,
          # !!CONS_ELEC removed!! #   bs4Dash::descriptionBlock(
          # !!CONS_ELEC removed!! #     number = year,
          # !!CONS_ELEC removed!! #     # header: we paste the value in kwh and pass it to convert_units(), and we format it + add unit
          # !!CONS_ELEC removed!! #     header = paste(format(cons_elec_value%>% convert_units(unit_to = selectedUnit$unit_to),
          # !!CONS_ELEC removed!! #                           big.mark = "'", digits = 1, scientific = FALSE),
          # !!CONS_ELEC removed!! #                    selectedUnit$unit_to),
          # !!CONS_ELEC removed!! #     text = tags$p("Consommation", icon("bolt", class = "iconColor")),
          # !!CONS_ELEC removed!! #     rightBorder = FALSE
          # !!CONS_ELEC removed!! #   )# End descriptionBlock 2/3
          # !!CONS_ELEC removed!! # )# End column

        ),# End fluidRow1

         fluidRow(
        # !!CONS_ELEC removed!! #   column(
        # !!CONS_ELEC removed!! #     width = 6,
        # !!CONS_ELEC removed!! #     bs4Dash::descriptionBlock(
        # !!CONS_ELEC removed!! #       number = year,
        # !!CONS_ELEC removed!! #       # header: no need for unit conversion since it's percents
        # !!CONS_ELEC removed!! #       header = scales::label_percent(accuracy = .1)(prod_elec_value/cons_elec_value),
        # !!CONS_ELEC removed!! #       text = tags$p("Part production", icon("bolt", class = "iconColor")),
        # !!CONS_ELEC removed!! #       rightBorder = TRUE
        # !!CONS_ELEC removed!! #     )# End descriptionBlock 3/3
        # !!CONS_ELEC removed!! #   ),# End column

        column(
          width = 12, # before: 6 !!CONS_ELEC removed!!
          bs4Dash::descriptionBlock(
            number = regener_current_year, # !! utils_helpers.R
            # header: we paste the value in kwh and pass it to convert_units(), and we format it + add unit
            header = paste(format(cons_rg_value,
                                  big.mark = "'", digits = 1, scientific = FALSE),
                           selectedUnit$unit_to),
            text = tags$p("Consommation", icon("fire", class = "iconColor")),
            rightBorder = FALSE
          )# End descriptionBlock
        )# End column
      )# End fluidRow2
      )# End box
    })# End renderUI
  })# End moduleServer
}


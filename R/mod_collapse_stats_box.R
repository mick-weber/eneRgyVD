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
    bslib::as_fill_carrier(uiOutput(ns("statbox"))) # https://rstudio.github.io/bslib/reference/as_fill_carrier.html
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
                                          subsidies_value,
                                          year_elec,
                                          year_rgr,
                                          year_subsidies){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    output$statbox <- renderUI({

      bslib::as_fill_carrier()

        bslib::card(fill = TRUE,

                    bslib::card_header(title,
                                       class = "bg-secondary"),

                    bslib::layout_column_wrap(width = "140px",
                                              fixed_width = TRUE,
                                              heights_equal = "all",
                                              fill = FALSE,
                                              fillable = TRUE,
                                              class = "justify-content-center",


                                              # fct_helpers.R

           make_statbox_item(icon_name = "lightning-charge-fill",icon_class = "text-warning",
                             title = "Production<br>électrique", value = prod_elec_value, unit = selectedUnit, year = year_elec),

           make_statbox_item(icon_name = "fire", icon_class = "text-danger",
                             title = "Consommation<br>bâtiments", value = cons_rg_value, unit = selectedUnit, year = year_rgr),

           make_statbox_item(icon_name = "house-check-fill",icon_class = "text-success",
                             title = "Subventions<br>rénovation M01", value = subsidies_value, unit = "dossiers", year = year_subsidies),

           make_statbox_item(icon_name = "plugin", icon_class = "text-warning",
                             title = "Consommation<br>électrique", value = subsidies_value, unit = selectedUnit, year = year_elec)


                    )# End layout_column_wrap
        )# End card
    })# End RenderUI
  })# End moduleServer
}




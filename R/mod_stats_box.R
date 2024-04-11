#' collapse_stats_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_stats_box_ui <- function(id){

  ns <- NS(id)

  tagList(

    bslib::as_fill_carrier(uiOutput(ns("statbox"))) # REQUIRED : https://rstudio.github.io/bslib/reference/as_fill_carrier.html
  )

}

#' collapse_stats_box Server Functions
#'
#' @noRd
mod_stats_box_server <- function(id,
                                 title,
                                 selectedUnit,
                                 elec_prod_value,
                                 elec_cons_value,
                                 cons_rg_value,
                                 subsidies_value,
                                 year_elec_cons,
                                 year_elec_prod,
                                 year_rgr,
                                 year_subsidies){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    output$statbox <- renderUI({

      bslib::as_fill_carrier() # REQUIRED : https://rstudio.github.io/bslib/reference/as_fill_carrier.html

        bslib::card(fill = TRUE,
                    #max_height = "50vh", # Limit the extension when VD box is displayed alone

                    bslib::card_header(title, bslib::tooltip(
                      id = ns("tooltip_card"),
                      placement = "right",
                      options = list(customClass = "customTooltips"), # custom.scss
                      trigger = bsicons::bs_icon("info-circle"),
                      "Les années de référence correspondent aux dernières années pour lesquelles des données sont disponibles"
                    ),
                                       class = "bg-secondary"),

                    bslib::layout_column_wrap(width = "140px",
                                              fixed_width = TRUE,
                                              heights_equal = "all",
                                              fill = FALSE,
                                              fillable = TRUE,
                                              class = "justify-content-center",

                                              # fct_helpers.R

           # 1. Cons elec
           make_statbox_item(iconBgClass = "iconBgElec",
                            title = "Distribution<br>électricité", value = elec_cons_value, unit = selectedUnit, year = year_elec_cons),

           # 2. RG cons
           make_statbox_item(iconBgClass = "iconBgRgr",
                             title = "Consommation<br>chaleur bâtiment", value = cons_rg_value, unit = selectedUnit, year = year_rgr),

           # 3. Subsidies M01
           make_statbox_item(iconBgClass = "iconBgSubs",
                             title = "Subventions<br>rénovation M01", value = subsidies_value, unit = "dossiers", year = year_subsidies),

           # 4. Prod elec
           make_statbox_item(iconBgClass = "iconBgElec",
                             title = "Production<br>électricité", value = elec_prod_value, unit = selectedUnit, year = year_elec_prod)



                    )# End layout_column_wrap
        )# End card
    })# End RenderUI
  })# End moduleServer
}




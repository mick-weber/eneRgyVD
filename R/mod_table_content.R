#' table_content UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_table_content_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::card(
      bslib::card_header(strong("Données disponibles"), class = "bg-secondary"),
      bslib::card_body(

        # Section Energie ----
        actionButton(ns("test"), label = "Energie", class = "disabledTocButtons"),

          actionLink(ns("data_1"), class = "mx-3", "Distribution d'électricité", icon = icon("bolt")),
          actionLink(ns("data_2"), class = "mx-3", "Production d'électricité",  icon = icon("bolt")),
          actionLink(ns("data_3"), class = "mx-3", "Besoins théoriques des bâtiments", icon = icon("fire")),
          actionLink(ns("data_4"), class = "mx-3", "Consommations théoriques des bâtiments", icon = icon("fire")),
          actionLink(ns("data_5"), class = "mx-3", "Informations parc des bâtiments", icon = icon("fire")),
          actionLink(ns("data_6"), class = "mx-3", "Subventions (vue par bâtiments)", icon = icon("house")),
          actionLink(ns("data_7"), class = "mx-3", "Subventions (vue par mesures)", icon = icon("house")),

        # Section Climat ----
        actionButton(ns("test"), "Climat", class = "disabledTocButtons"),

          actionLink(ns("data_8"), class = "mx-3", "Climat (exemple générique)", icon = icon("earth")),

        # Section Mobilité ----
        actionButton(ns("test"), "Mobilité", class = "disabledTocButtons"),
          actionLink(ns("data_9"), class = "mx-3", "Mobilité (exemple générique)", icon = icon("car")),

      )
    )
  )
}

#' table_content Server Functions
#'
#' @noRd
mod_table_content_server <- function(id, parent){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    purrr::pwalk(subset(subpanels_tribble, select = c("data_id", "nav_panel","navset_name","nav_name")), # selecting all cols result in unnused arguments
                 .f = \(data_id, nav_panel, navset_name, nav_name) shiny::observeEvent(

                   input[[data_id]], {

                     # 1. Redirect to navbar's nav_panel()
                     bslib::nav_select(id = "nav",
                                       selected = nav_panel, session = parent)

                     # 1. Redirect to nested navset's nav_panel()
                     bslib::nav_select(id = navset_name,
                                       selected = nav_name, session = parent)
                   }
                 )) #End pwalk

  })
}

## To be copied in the UI
# mod_table_content_ui("table_content_1")

## To be copied in the server
# mod_table_content_server("table_content_1")

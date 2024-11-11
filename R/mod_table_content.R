#' table_content UI Function
#'
#' @description A shiny Module used to display the table of contents in the home page with redirection links
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
      bslib::card_header(strong("Données disponibles",
                                bslib::tooltip(
                                  id = "tooltip_toc_card",
                                  placement = "right",
                                  options = list(customClass = "customTooltips"), # custom.scss
                                  trigger = bsicons::bs_icon("info-circle"),

                                  "L'accès aux données peut se faire via la barre de navigation ou directement en cliquant sur les liens ci-dessous")),

                         class = "bg-secondary"),
      bslib::card_body(

        # Add spacing between underline and text in actionLink elements
        style = "text-underline-offset:5px;",

        # Section Energie ----
        actionButton(ns("header_energie"), label = "Energie", class = "disabledTocButtons"),

        actionLink(ns("data_1"), class = "mx-3", "Electricité (distribution)", icon = icon("bolt")),
        actionLink(ns("data_2"), class = "mx-3", "Electricité (production)",  icon = icon("bolt")),
        actionLink(ns("data_8"), class = "mx-3", "Distribution de gaz naturel", icon = icon("fire-flame-simple")),
        actionLink(ns("data_3"), class = "mx-3", "Chaleur des bâtiments (besoins théoriques)", icon = icon("fire")),
        actionLink(ns("data_4"), class = "mx-3", "Chaleur des bâtiments (consommations théoriques)", icon = icon("fire")),
        actionLink(ns("data_5"), class = "mx-3", "Chaleur des bâtiments (informations bâtiments)", icon = icon("fire")),
        actionLink(ns("data_6"), class = "mx-3", "Subventions bâtiments (vue par bâtiments)", icon = icon("house")),
        actionLink(ns("data_7"), class = "mx-3", "Subventions bâtiments (vue par mesures)", icon = icon("house")),

        # Section Adaptation ----
        actionButton(ns("header_adapt"), "Adaptation", class = "disabledTocButtons"),

        actionLink(ns("data_10"), class = "mx-3", "Adaptation (exemple générique)", icon = icon("earth")),

        # Section Mobilité ----
        actionButton(ns("header_mob"), "Mobilité", class = "disabledTocButtons"),

        actionLink(ns("data_11"), class = "mx-3", "Mobilité (exemple générique)", icon = icon("car")),

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

    # This function allows to flexibly redirect TOC items clicks to corresponding nav_panels in utils_helpers.R

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

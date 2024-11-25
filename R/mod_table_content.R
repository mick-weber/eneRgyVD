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

                                  "L'accès aux données peut se faire via la barre de navigation ou directement en cliquant sur les liens ci-dessous"))
                         ),
      bslib::card_body(

        tags$div(id = "introjs_toc_accordion",
                 bslib::accordion(
                   class = "TOCaccordionPanelBold", # custom.scss : affects only this TOC module !
                   open = FALSE,
                   multiple = TRUE,
                   bslib::accordion_panel(title = "Électricité", value = "elec", icon = icon("bolt", class = "text-primary"),
                                          tags$div(class = "row", actionLink(ns("data_1"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             "Distribution", icon = NULL, tags$span(bsicons::bs_icon("box-arrow-up-right")))),
                                          tags$div(class = "row", actionLink(ns("data_2"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             "Production",  icon = NULL, tags$span(bsicons::bs_icon("box-arrow-up-right"))))
                   ),

                   bslib::accordion_panel(title = "Gaz naturel", value = "ng", icon = icon("fire", class = "text-primary"),
                                          tags$div(class = "row", actionLink(ns("data_8"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             "Distribution", icon = NULL, tags$span(bsicons::bs_icon("box-arrow-up-right"))))
                   ),
                   bslib::accordion_panel(title = "Chaleur bâtiments", value = "regener", icon = icon("city", class = "text-primary"),
                                          tags$div(class = "row", actionLink(ns("data_3"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             "Besoins théoriques", icon = NULL, tags$span(bsicons::bs_icon("box-arrow-up-right")))),
                                          tags$div(class = "row", actionLink(ns("data_4"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             "Consommations théoriques", icon = NULL, tags$span(bsicons::bs_icon("box-arrow-up-right")))),
                                          tags$div(class = "row", actionLink(ns("data_5"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             "Informations bâtiments", icon = NULL, tags$span(bsicons::bs_icon("box-arrow-up-right"))))
                   ),
                   bslib::accordion_panel(title = "Subventions bâtiments", value = "subsidies", icon = icon("house-circle-check", class = "text-primary"),
                                          tags$div(class = "row", actionLink(ns("data_6"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             "Vue par bâtiments subventionnés", icon = NULL, tags$span(bsicons::bs_icon("box-arrow-up-right")))),
                                          tags$div(class = "row", actionLink(ns("data_7"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             "Vue par travaux subventionnés", icon = NULL, tags$span(bsicons::bs_icon("box-arrow-up-right"))))

                   ),
                   bslib::accordion_panel(title = "Adaptation climat", value = "adaptation", icon = icon("temperature-half", class = "text-primary"),
                                          tags$div(class = "row", actionLink(ns("data_20"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             "Surface de canopée", icon = NULL, tags$span(bsicons::bs_icon("box-arrow-up-right"))))

                   ),

                   bslib::accordion_panel(title = "Mobilité", value = "mobility", icon = icon("car", class = "text-primary"),
                                          tags$div(class = "row", actionLink(ns("data_30"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             "Qualité de desserte des transports publics", icon = NULL, tags$span(bsicons::bs_icon("box-arrow-up-right"))))
                   )
                 )#End accordion
        )#End introjs div
      )# End card_body
    ),#End card

    # Section Energie ----
    #actionButton(ns("header_energie"), label = "Energie", class = "disabledTocButtons"),

    # actionLink(ns("data_1"), class = "border p-3 customTOC", "Electricité (distribution)", icon = icon("bolt")),
    # actionLink(ns("data_2"), class = "border p-3 customTOC", "Electricité (production)",  icon = icon("bolt")),
    # actionLink(ns("data_8"), class = "border p-3 customTOC", "Distribution de gaz naturel", icon = icon("fire-flame-simple")),
    # actionLink(ns("data_3"), class = "border p-3 customTOC", "Chaleur des bâtiments (besoins théoriques)", icon = icon("fire")),
    # actionLink(ns("data_4"), class = "border p-3 customTOC", "Chaleur des bâtiments (consommations théoriques)", icon = icon("fire")),
    # actionLink(ns("data_5"), class = "border p-3 customTOC", "Chaleur des bâtiments (informations bâtiments)", icon = icon("fire")),
    # actionLink(ns("data_6"), class = "border p-3 customTOC", "Subventions bâtiments (vue par bâtiments)", icon = icon("house")),
    # actionLink(ns("data_7"), class = "border p-3 customTOC", "Subventions bâtiments (vue par mesures)", icon = icon("house")),

    # Section Adaptation ----
    #actionButton(ns("header_adapt"), "Adaptation", class = "disabledTocButtons"),

        #actionLink(ns("data_10"), class = "border p-3 customTOC", "Adaptation (exemple générique)", icon = icon("earth")),

        # Section Mobilité ----
        #actionButton(ns("header_mob"), "Mobilité", class = "disabledTocButtons"),

        #actionLink(ns("data_11"), class = "border p-3 customTOC", "Mobilité (exemple générique)", icon = icon("car")),

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

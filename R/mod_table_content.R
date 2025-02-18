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
                                  trigger = phosphoricons::ph(title = NULL, "info"),

                                  "L'accès aux données peut se faire via la barre de navigation ou directement en cliquant sur les liens ci-dessous"))
                         ),
      bslib::card_body(

        tags$div(id = "introjs_toc_accordion",
                 bslib::accordion(
                   class = "TOCaccordionPanelBold", # custom.scss : affects only this TOC module !
                   open = FALSE,
                   multiple = TRUE,
                   bslib::accordion_panel(title = "Électricité", value = "elec", icon = phosphoricons::ph(title = NULL, "lightning", weight = "fill", fill = "gold"),
                                          tags$div(class = "row", actionLink(ns("data_1"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             tags$span(style = "margin-left:15px;", "Distribution"), icon = NULL, tags$span(phosphoricons::ph(title = NULL, "arrow-square-out")))),
                                          tags$div(class = "row", actionLink(ns("data_2"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             tags$span(style = "margin-left:15px;", "Production"),  icon = NULL, tags$span(phosphoricons::ph(title = NULL, "arrow-square-out"))))
                   ),

                   bslib::accordion_panel(title = "Gaz naturel", value = "ng", icon = phosphoricons::ph(title = NULL, "flame", weight = "fill", fill = "#4A708B"),
                                          tags$div(class = "row", actionLink(ns("data_8"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             tags$span(style = "margin-left:15px;", "Distribution"), icon = NULL, tags$span(phosphoricons::ph(title = NULL, "arrow-square-out"))))
                   ),
                   bslib::accordion_panel(title = "Chaleur bâtiments", value = "regener", icon = phosphoricons::ph(title = NULL, "building-apartment", weight = "fill", fill = "black"),
                                          tags$div(class = "row", actionLink(ns("data_3"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             tags$span(style = "margin-left:15px;", "Besoins théoriques"), icon = NULL, tags$span(phosphoricons::ph(title = NULL, "arrow-square-out")))),
                                          tags$div(class = "row", actionLink(ns("data_4"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             tags$span(style = "margin-left:15px;", "Consommations théoriques"), icon = NULL, tags$span(phosphoricons::ph(title = NULL, "arrow-square-out")))),
                                          tags$div(class = "row", actionLink(ns("data_5"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             tags$span(style = "margin-left:15px;", "Informations bâtiments"), icon = NULL, tags$span(phosphoricons::ph(title = NULL, "arrow-square-out"))))
                   ),
                   bslib::accordion_panel(title = "Subventions bâtiments", value = "subsidies", icon = phosphoricons::ph(title = NULL, "files"),
                                          tags$div(class = "row", actionLink(ns("data_7"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             tags$span(style = "margin-left:15px;", "Vue par travaux subventionnés"), icon = NULL, tags$span(phosphoricons::ph(title = NULL, "arrow-square-out")))),
                                          tags$div(class = "row", actionLink(ns("data_6"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             tags$span(style = "margin-left:15px;", "Vue cumulée du parc subventionné"), icon = NULL, tags$span(phosphoricons::ph(title = NULL, "arrow-square-out"))))

                   ),
                   bslib::accordion_panel(title = "Mobilité", value = "mobility", icon = phosphoricons::ph(title = NULL, "road-horizon", weight = "bold", fill = "grey20"),
                                          tags$div(class = "row", actionLink(ns("data_30"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             tags$span(style = "margin-left:15px;", "Part des voitures électriques"), icon = NULL, tags$span(phosphoricons::ph(title = NULL, "arrow-square-out")))),
                                          tags$div(class = "row", actionLink(ns("data_31"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             tags$span(style = "margin-left:15px;", "Taux de motorisation"), icon = NULL, tags$span(phosphoricons::ph(title = NULL, "arrow-square-out")))),
                                          tags$div(class = "row", actionLink(ns("data_32"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             tags$span(style = "margin-left:15px;", "Qualité de desserte des transports publics"), icon = NULL, tags$span(phosphoricons::ph(title = NULL, "arrow-square-out"))))
                   ),
                   bslib::accordion_panel(title = "Adaptation climat", value = "adaptation", icon = phosphoricons::ph(title = NULL, "thermometer-simple", weight = "fill", fill = "#EE7600"),
                                          tags$div(class = "row", actionLink(ns("data_20"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             tags$span(style = "margin-left:15px;", "Surface de canopée urbaine"), icon = NULL, tags$span(phosphoricons::ph(title = NULL, "arrow-square-out")))),
                                          tags$div(class = "row", actionLink(ns("data_21"), class = "customTOC border p-2 rounded d-flex justify-content-between align-items-center",
                                                                             tags$span(style = "margin-left:15px;", "Exposition aux dangers naturels"), icon = NULL, tags$span(phosphoricons::ph(title = NULL, "arrow-square-out"))))

                   )
                 )#End accordion
        )#End introjs div
      )# End card_body
    ),#End card
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

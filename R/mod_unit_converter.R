#' unit_converter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_unit_converter_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$p("Conversion d'unités",
           style = "font-weight:500;margin-bottom:0.5rem",
           bslib::tooltip(
             id = "tooltip_convert_unit",
             placement = "right",
             options = list(customClass = "customTooltips"), # custom.scss
             trigger = phosphoricons::ph(title = NULL, "info"),
             "Modifier l'affichage et l'export des unités de certaines données"
           )
    ),

    bslib::accordion(open = FALSE,
                     class = "fs-sidebar-header rotatedSVG",
                     bslib::accordion_panel(title = "Afficher les unités",
                                            icon = phosphoricons::ph(title = NULL, "calculator"),

                                            bslib::navset_tab(
                                              bslib::nav_panel(title = tags$div(style = "font-size:1rem;", "Energie"),

                                                               div(style = "font-size:1rem;padding-left:2vh;padding-top:1vh;",
                                                                   shinyWidgets::prettyRadioButtons(inputId = ns("energy_unit"),
                                                                                                    label = NULL,
                                                                                                    choices = energy_units_table$unit,
                                                                                                    selected = "MWh", # default
                                                                                                    inline = FALSE,
                                                                                                    status =  "default",
                                                                                                    icon = icon("check"),
                                                                                                    animation = "jelly")
                                                               )# End div

                                              ),# End nav_panel
                                              bslib::nav_panel(title = tags$div(style = "font-size:1rem;", "CO2-eq"),

                                                               div(style = "font-size:1rem;padding-left:2vh;padding-top:1vh;",
                                                                   shinyWidgets::prettyRadioButtons(inputId = ns("co2_unit"),
                                                                                                    label = NULL,
                                                                                                    choices = co2_units_table$unit,
                                                                                                    selected = "tCO2-eq", # default
                                                                                                    inline = FALSE,
                                                                                                    status =  "default",
                                                                                                    icon = icon("check"),
                                                                                                    animation = "jelly")
                                                               )# End div
                                              )# End nav_panel
                                            ),# End navset_tab
                                            tags$div(style = "font-size:0.9rem;font-style:italic;text-align:center;",
                                                     "Note : ces conversions ne s'appliquent qu'à certaines statistiques."
                                            ),

                     ))# End accordion
  )
}

#' unit_converter Server Functions
#'
#' @noRd
mod_unit_converter_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Initiate units holder ----
    selectedUnits <- reactiveValues()

    # Populate with available units ----
    # (independently = efficient)
    observe({
      selectedUnits$energy_unit <- input$energy_unit
    })

    observe({
      selectedUnits$co2_unit <- input$co2_unit
    })

    # Return units ----

    # !search 'convert_units(' to look where conversion occur
    energy_transform_notifs <- c("électricité", "gaz naturel", "chaleur des bâtiments") # add other data as needed
    ges_transform_notifs <- c("consommation des bâtiments") # add other data as needed

    # Notify notifications when energy unit changes
    observeEvent(selectedUnits$energy_unit, ignoreInit = TRUE, {

      purrr::walk(energy_transform_notifs, function(unit) {
        shiny::showNotification(glue::glue("Conversion d'unité appliquée pour les données : {unit} ({selectedUnits$energy_unit})"), type = "message")
      })
    })

    # Notify when energy unit changes
    observeEvent(selectedUnits$co2_unit, ignoreInit = TRUE, {

      purrr::walk(ges_transform_notifs, function(unit) {
        shiny::showNotification(glue::glue("Conversion d'unité appliquée pour les données : {unit} ({selectedUnits$co2_unit})"), type = "message")
      })
    })





    return(selectedUnits)

  })
}

## To be copied in the UI
# mod_unit_converter_ui("unit_converter_1")

## To be copied in the server
# mod_unit_converter_server("unit_converter_1")

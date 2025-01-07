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
             trigger = phosphoricons::ph("info"),
             "Modifier l'affichage et l'export des unités de certaines données"
           )
    ),

    bslib::accordion(open = FALSE,
                     class = "fs-sidebar-header rotatedSVG",
                     bslib::accordion_panel(title = "Afficher les unités",
                                            icon = phosphoricons::ph("calculator"),

                                            bslib::navset_tab(
                                              bslib::nav_panel(title = tags$div(style = "font-size:1rem;", "Energie"),

                                                               div(style = "font-size:1rem;padding-left:2vh;padding-top:1vh;",
                                                                   shinyWidgets::prettyRadioButtons(inputId = ns("energy_unit"),
                                                                                                    label = NULL,
                                                                                                    choices = c("kWh", "MWh", "GWh", "TJ"),
                                                                                                    selected = "MWh",
                                                                                                    inline = FALSE,
                                                                                                    status =  "default",
                                                                                                    icon = icon("check"),
                                                                                                    animation = "jelly")
                                                               )# End div

                                              ),# End nav_panel
                                              bslib::nav_panel(title = tags$div(style = "font-size:1rem;", "CO2"),

                                                               div(style = "font-size:1rem;padding-left:2vh;padding-top:1vh;",
                                                                   shinyWidgets::prettyRadioButtons(inputId = ns("co2_unit"),
                                                                                                    label = NULL,
                                                                                                    choices = c("kgCO2", "tCO2", "ktCO2"),
                                                                                                    selected = "tCO2",
                                                                                                    inline = FALSE,
                                                                                                    status =  "default",
                                                                                                    icon = icon("check"),
                                                                                                    animation = "jelly")
                                                               )# End div
                                              )# End nav_panel
                                            )# End navset_tab
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

    return(selectedUnits)

  })
}

## To be copied in the UI
# mod_unit_converter_ui("unit_converter_1")

## To be copied in the server
# mod_unit_converter_server("unit_converter_1")

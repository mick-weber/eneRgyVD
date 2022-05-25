#' communes_boxes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_communes_boxes_ui <- function(id){
  ns <- NS(id)
  tagList(

    h4("Votre sélection en quelques chiffres"),

    shinydashboard::valueBoxOutput(ns("prod_elec_commune")),
    shinydashboard::valueBoxOutput(ns("conso_elec_commune")),
    shinydashboard::valueBoxOutput(ns("coverage_elec_commune"))
  )
}

#' communes_boxes Server Functions
#'
#' @noRd
mod_communes_boxes_server <- function(id, inputVals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

      # Initialising value holder for boxes

    boxValues <- reactiveValues()

      # Computing the statistics in a reactive context

    observe({

      # Requires the subset dataset to exist
      # note: inputVals$prod_dataset is only filtered with communes (doesn't require any year selected)

      req(inputVals$prod_dataset)

      boxValues$elec_last_year <- 2020 # must be updated when conso dataset arrives to find the common latest year

      boxValues$prod_elec <- inputVals$prod_dataset %>%
        dplyr::filter(annee == 2020) %>% # year hard coded temporarily
        dplyr::summarise(production_totale = sum(production_totale/1e3, na.rm = TRUE)) %>% # en MWh
        dplyr::pull() %>%
        round(digits = 0)

      boxValues$conso_elec <- 150 # link to dataset when available


    })

      # Rendering the valueBoxes

    # Electricity production valueBox
    output$prod_elec_commune <- shinydashboard::renderValueBox({

      req(boxValues$prod_elec)

      shinydashboard::valueBox(value = format(boxValues$prod_elec, big.mark = "'"),
               subtitle = paste("MWh produits en", boxValues$elec_last_year ),
               icon = icon("flash", lib = "glyphicon"),
               color = "light-blue")
    })
    # Electricity consumption valueBox
    output$conso_elec_commune <- shinydashboard::renderValueBox({

      req(boxValues$conso_elec)

      shinydashboard::valueBox(value = "200",
               subtitle = paste("MWh consommés en", boxValues$elec_last_year ),
               icon = icon("flash", lib = "glyphicon"),
               color = "light-blue")
    })
    # Coverage valueBox
    output$coverage_elec_commune <- shinydashboard::renderValueBox({

      req(boxValues$prod_elec, boxValues$conso_elec)

      shinydashboard::valueBox(value = "XX%",
               subtitle = paste("Couverture annuelle électrique en", boxValues$elec_last_year ),
               icon = icon("scale", lib = "glyphicon"),
               color = "light-blue")
    })
  })
}

## To be copied in the UI
# mod_communes_boxes_ui("communes_boxes_1")

## To be copied in the server
# mod_communes_boxes_server("communes_boxes_1")

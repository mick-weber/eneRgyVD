#' map_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

library(tidyverse)

# HELP COULD BE USEFUL HERE : https://community.rstudio.com/t/update-input-that-is-outside-of-shiny-module-namespace/111138/6
# Because we need to update an input based on the selected polygon(s), and that input is across another module !

mod_map_selector_ui <- function(id){
  ns <- NS(id)
  tagList(

    # our map with a shinycssloader (trying)
    #leaflet::leafletOutput("map", height = "500px", width = "700px") # %>%
      # shinycssloaders::withSpinner(color="#3A862D")

    textOutput(ns("testing"))

  )
}

#' map_selector Server Functions
#'
#' @noRd
mod_map_selector_server <- function(id, inputVals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$testing <- renderText({inputVals$selectedCommunes})


  #   selected_ids <- reactiveValues(ids = vector())
  #
  #   output$map <- leaflet::renderLeaflet({
  #
  #     leaflet::leaflet(options = leafletOptions(
  #       # set initial zoom (found empirically)
  #       minZoom = 9,
  #       # remove leaflet url bottom of the map
  #       attributionControl = F)) %>%
  #       # Couche de base des districts si un district est sélectionné
  #       leaflet::addPolygons(data = sf_districts,
  #                            fillColor = NULL,
  #                            fillOpacity = 0,
  #                            color = "black",
  #                            weight = 2,
  #                            # group is then used in leafem::addHomeButton()
  #                            group = "Vue cantonale") %>%
  #       # Couche des lacs
  #       leaflet::addPolygons(data = sf_lacs,
  #                            fillColor = "lightblue",
  #                            color = "grey",
  #                            weight = 1) %>%
  #       # Première couche des communes (en blanc, état non-sélectionné)
  #       leaflet::addPolygons(data = sf_communes,
  #                            fillColor = "white",
  #                            color = "grey",
  #                            weight = 1,
  #                            # layerID is needed to throw back the right item when clicked
  #                            layerId = ~NOM_MIN,
  #                            label = ~NOM_MIN,
  #                            group = "regions",
  #                            highlightOptions = highlightOptions(
  #                              weight = 5,
  #                              fillColor = NULL,
  #                              fillOpacity = NULL,
  #                              color = "#FFA500",
  #                              bringToFront = FALSE)) %>%
  #       # Seconde couche des communes (en rouge, état sélectionné)
  #       leaflet::addPolygons(data = sf_communes,
  #                            fillColor = "red",
  #                            fillOpacity = 0.5,
  #                            weight = 1,
  #                            color = "black",
  #                            stroke = TRUE,
  #                            layerId = ~NO_COM_FED, # any random layerId other than NOM_MIN (required)
  #                            label = ~NOM_MIN,
  #                            group = ~NOM_MIN,
  #                            highlightOptions = highlightOptions(
  #                              weight = 5,
  #                              fillColor = NULL,
  #                              fillOpacity = NULL,
  #                              color = "#CFCFCF",
  #                              bringToFront = FALSE)) %>%
  #       # This will be switched on/off through the code below using click events
  #       leaflet::hideGroup(group = sf_communes$NOM_MIN)   %>%
  #       #  !! DOESNT WORKAdapt original view by capping the max bounds
  #       # leaflet::setMaxBounds(lng1 = 6.047974, lat1 = 46.126556, lng2 = 7.135620, lat2 = 46.949325) %>%
  #       # Set the background to white
  #       leaflet.extras::setMapWidgetStyle(list(background= "white")) %>%
  #       # Add home button to zoom back to original view
  #       leaflet.extras::addResetMapButton() %>%
  #       leaflet.extras::addFullscreenControl(position = "topleft",
  #                                            pseudoFullscreen = FALSE) %>%
  #       # Set max limits where the users cannot pan further (approximate VD coords with padding)
  #       leaflet::setMaxBounds(lng1 = 5.9, lat1 = 46.1, lng2 = 7.3, lat2 = 47.1)
  #   })
  #
  #   # END RENDER LEAFLET
  #
  #   # Define leaflet proxy to draw the base map once only and build on its proxy
  #   proxy <- leafletProxy("map")
  #
  #   # Create empty vector to hold all click ids
  #   selected <- reactiveValues(groups = vector())
  #
  #   # Handles the highlight/removal of a polygon on CLICK EVENTS
  #   observeEvent(input$map_shape_click, {
  #     if(input$map_shape_click$group == "regions"){
  #       selected$groups <- c(selected$groups, input$map_shape_click$id)
  #       proxy %>% showGroup(group = input$map_shape_click$id)
  #     } else {
  #       selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
  #       proxy %>% hideGroup(group = input$map_shape_click$group)
  #     }
  #     # Update selectInput for bilateral communication map/widget
  #     # inputId got customized with the ns of the respective input module and a '-' for the link
  #     updateSelectizeInput(session = session,
  #                          inputId = "inputs_1-selected_communes",
  #                          choices = communes_names,
  #                          selected = selected$groups)
  #   })
  #
  #   # Handles the highlight/removal of a polygon on SELECT INPUT EVENTS
  #   observeEvent(inputVals$selectedCommunes, {
  #     removed_via_selectInput <- setdiff(selected$groups, inputVals$selectedCommunes)
  #     added_via_selectInput <- setdiff(inputVals$selectedCommunes, selected$groups)
  #
  #     if(length(removed_via_selectInput) > 0){
  #       selected$groups <- inputVals$selectedCommunes
  #       proxy %>% hideGroup(group = removed_via_selectInput)
  #     }
  #
  #     if(length(added_via_selectInput) > 0){
  #       selected$groups <- inputVals$selectedCommunes
  #       proxy %>% showGroup(group = added_via_selectInput)
  #     }
  #   }, ignoreNULL = FALSE)
  #
  #   # NEW FEATURE : change fitBounds based on input$district
  #
  #   observeEvent(inputVals$selectedDistrict, {
  #
  #     proxy %>%
  #       fitBounds(lng1= bboxes %>% pluck(inputVals$selectedDistrict) %>% pluck("xmin"),
  #                 lng2= bboxes %>% pluck(inputVals$selectedDistrict) %>% pluck("xmax"),
  #                 lat1= bboxes %>% pluck(inputVals$selectedDistrict) %>% pluck("ymin"),
  #                 lat2= bboxes %>% pluck(inputVals$selectedDistrict) %>% pluck("ymax"))
  #   })
  #
  #
  })
}

## To be copied in the UI
# mod_map_selector_ui("map_selector_1")

## To be copied in the server
# mod_map_selector_server("map_selector_1")



  # # testing module
  # mod_map_selector_App <- function() {
  #     ui <- fluidPage(
  #         mod_map_selector_ui("id")
  #     )
  #     server <- function(input, output, session) {
  #         mod_map_selector_server("id")
  #     }
  #     shinyApp(ui, server)
  # }
  # mod_map_selector_App()





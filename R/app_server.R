#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  ## Inputs module ----
    # This retrieves the inputs saved in mod_inputs.R

   inputVals <- mod_inputs_server("inputs_1")


   # TEST AREA


   # /TEST AREA

   # Leaflet select map ----

   #   Unfortunately this couldn't be modularized because for some reason the communication between
   #   the map and the mod_inputs.R module couldn't be established bilaterally (only unilaterally, either way)

   selected_ids <- reactiveValues(ids = vector())

   output$map <- leaflet::renderLeaflet({
   # Fct in fct_helpers.R that generates the base, non-reactive map
   # Note : we should add an argument sf_districts_labels if needed later to plot labels

     create_select_leaflet(sf_districts = sf_districts,
                           sf_lacs = sf_lacs,
                           sf_communes = sf_communes)

   })

   # END RENDER LEAFLET ; START PROXYLEAFLET WORK


   # Define leaflet proxy to draw the base map once only and build on its proxy
   proxy <- leaflet::leafletProxy("map")

   # Create empty vector to hold all click ids
   selected <- reactiveValues(groups = vector())

   # Handles the highlight/removal of a polygon on CLICK EVENTS
   observeEvent(input$map_shape_click, {
     if(input$map_shape_click$group == "regions"){
       selected$groups <- c(selected$groups, input$map_shape_click$id)
       proxy %>% leaflet::showGroup(group = input$map_shape_click$id)
     } else {
       selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
       proxy %>% leaflet::hideGroup(group = input$map_shape_click$group)
     }
     # Update selectInput for bilateral communication map/widget
     # inputId got customized with the NS of the respective input module (inputs_1-)
     # this allows to update a widget from another module

     updateSelectizeInput(session = session,
                          inputId = "inputs_1-selected_communes",
                          choices = communes_names,
                          selected = selected$groups)
   })

   # Handles the highlight/removal of a polygon on SELECT INPUT EVENTS
   observeEvent(inputVals$selectedCommunes, {
     removed_via_selectInput <- setdiff(selected$groups, inputVals$selectedCommunes)
     added_via_selectInput <- setdiff(inputVals$selectedCommunes, selected$groups)

     if(length(removed_via_selectInput) > 0){
       selected$groups <- inputVals$selectedCommunes
       proxy %>% leaflet::hideGroup(group = removed_via_selectInput)
     }

     if(length(added_via_selectInput) > 0){
       selected$groups <- inputVals$selectedCommunes
       proxy %>% leaflet::showGroup(group = added_via_selectInput)
     }
   }, ignoreNULL = FALSE)

   # NEW FEATURE : change fitBounds based on input$district through inputVals$selectedDistrict

   observeEvent(inputVals$selectedDistrict, {

     proxy %>%
       leaflet::fitBounds(
         lng1= bboxes %>% purrr::pluck(inputVals$selectedDistrict) %>% purrr::pluck("xmin"),
         lng2= bboxes %>% pluck(inputVals$selectedDistrict) %>% pluck("xmax"),
         lat1= bboxes %>% pluck(inputVals$selectedDistrict) %>% pluck("ymin"),
         lat2= bboxes %>% pluck(inputVals$selectedDistrict) %>% pluck("ymax"))

  # NEW FEATURE TO ADD : add a popup menu when clicking on the municipalities to get basic indicators (1-2 max.)

     # Requires to prepare data and then add it in the popup arg inside leaflet::addPolygons()
   })



  # END PROXY LEAFLET WORK
  # END MAP SELECTOR




   # MODULES ----
   ## tabProd: call the chart
   mod_prod_charts_server("prod_chart1", inputVals = inputVals)
}


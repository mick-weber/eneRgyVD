#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_ui <- function(id){
  ns <- NS(id)
  tagList(

    leaflet::leafletOutput(ns("map")) |>
      shinycssloaders::withSpinner(type = 6,
                                   color = main_color)

  )
}

#' map Server Functions
#'
#' @noRd
mod_map_server <- function(id, inputVals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    selected_ids <- reactiveValues(ids = vector())

    output$map <- leaflet::renderLeaflet({
      # Fct in fct_helpers.R that generates the base, non-reactive map
      # Note : we should add an argument sf_districts_labels if needed later to plot labels

      create_select_leaflet(sf_districts = sf_districts,
                            sf_lacs = sf_lacs,
                            sf_communes = sf_communes)

    })

    # END RENDER LEAFLET BASE; START PROXYLEAFLET WORK


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

      # updateSelectizeInput(session = session,
      #                      inputId = "inputs_1-selected_communes",
      #                      choices = choices_canton_communes,
      #                      selected = selected$groups)
    })

    # Handles the highlight/removal of a polygon and borders from inputVals$selectedCommunes

    ## VD border highlighting (Canton selected only) ----
    observeEvent(inputVals$selectedCommunes, {

      if("Canton de Vaud" %in% inputVals$selectedCommunes){

        proxy |>
          # Contour for Canton VD, orange + light orange fill
          leaflet::addPolylines(data = sf_canton, # utils_helpers.R ; static
                                layerId = "highlight_canton", # layer id to remove if unselected
                                color = "#FFB90F",
                                fill = T, fillColor = "#FFE3A0",
                                # Disable clikable events otherwise it will crash (!)
                                options = leaflet::pathOptions(interactive = FALSE)
          )
      }else {
        # If Canton de Vaud is not in the selection, we remove the canton highlight Polyline
        proxy |>
          leaflet::removeShape(layerId = "highlight_canton")

      }

      ## Polygons highlighting (Communes selected only) ----
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
    }, ignoreNULL = FALSE) # Don't trigger when input is NULL

    # NEW FEATURE : change fitBounds based on input$district through inputVals$selectedDistrict

    observeEvent(inputVals$selectedDistrict, {

      proxy %>%
        leaflet::fitBounds(
          lng1= bboxes %>% purrr::pluck(inputVals$selectedDistrict) %>% purrr::pluck("xmin"),
          lng2= bboxes %>% purrr::pluck(inputVals$selectedDistrict) %>% purrr::pluck("xmax"),
          lat1= bboxes %>% purrr::pluck(inputVals$selectedDistrict) %>% purrr::pluck("ymin"),
          lat2= bboxes %>% purrr::pluck(inputVals$selectedDistrict) %>% purrr::pluck("ymax"))
    })

    # Return all active selected communes from the map
    # --> fed into app_server's updateSelectizeInput() generated in mod_inputs.R

    return(reactive(selected$groups))


  })
}

## To be copied in the UI
# mod_map_ui("map_1")

## To be copied in the server
# mod_map_server("map_1")

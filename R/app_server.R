#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Module for dropdown block unit converter
  mod_unit_converter_server("unit_converter")


  ## Inputs module ----
    # This retrieves the inputs saved in mod_inputs.R

   inputVals <- mod_inputs_server("inputs_1")

   ## Subset data for consumption data (fed into mod_elec_charts_server("consumption_charts", ...))

   subset_cons_data <- reactive({

     # explicitely require communes to be selected
     validate(
       need(inputVals$selectedCommunes, "Sélectionner au moins une commune pour générer un résultat.")
     )

     # waiting on these to get initialized (renderUIs)
     req(inputVals$min_selected_cons,
         inputVals$max_selected_cons,
         inputVals$cons_dataset)

     # further filter cons_dataset with selected min/max values

     inputVals$cons_dataset %>%
       dplyr::filter(annee >= inputVals$min_selected_cons,
                     annee <= inputVals$max_selected_cons)

   })




   ## Subset data for production data (fed into mod_elec_charts_server("production_charts", ...))
   subset_prod_data <- reactive({

     # explicitely require communes to be selected
     validate(
       need(inputVals$selectedCommunes, "Sélectionner au moins une commune pour générer un résultat.")
     )
     # waiting on these to get initialized (renderUIs)
     req(inputVals$min_selected,
         inputVals$max_selected,
         inputVals$techs_selected,
         inputVals$prod_dataset)

     # prod by commune filtered with commune pickerInput(), years from sliderInput(), techs from pickerInput()
     # TESTING, WE DONT NEED THIS IF WE TAKE DIRECTLY INPUTVALS$PROD_DATASET
     # elec_prod_communes %>%
     # dplyr::filter(commune %in% inputVals$selectedCommunes)  %>%

     inputVals$prod_dataset %>%
       dplyr::filter(annee >= inputVals$min_selected,
                     annee <= inputVals$max_selected) %>%
       dplyr::filter(categorie_diren %in% inputVals$techs_selected)

   }) # End reactive()


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




   # Output modules ----

   ## tabCons: call the chart server logic ----
   mod_elec_charts_server("consumption_charts",
                          inputVals = inputVals,
                          subsetData = subset_cons_data,
                          # args for create_bar_plotly() & create_sunburst_plotly()
                          year = inputVals$max_selected_cons,
                          var_year = "annee",
                          var_commune = "commune",
                          var_rank_2 = "secteur",
                          var_values = "consommation_kwh",
                          color_palette = colors_sectors,
                          third_rank = FALSE,
                          var_rank_3_1 = NULL, var_rank_3_2 = NULL,
                          fct_table_dt_type = create_cons_table_dt)

   ## tabProd: call the chart server logic ----
   mod_elec_charts_server("production_charts",
                          inputVals = inputVals,
                          subsetData = subset_prod_data,
                          # args for create_bar_plotly() & create_sunburst_plotly()
                          year = inputVals$max_selected,
                          var_year = "annee",
                          var_commune = "commune",
                          var_rank_2 = "categorie_diren",
                          var_values = "production_totale",
                          color_palette = colors_categories,
                          third_rank = TRUE,
                          var_rank_3_1 = "injection_totale", var_rank_3_2 = "autoconso_totale",
                          # name of fct to create dt table
                          fct_table_dt_type = create_prod_table_dt)
   ## tabMap: boxes for statistics ----
   # Module for rendering the vd collapse box
   mod_vd_collapse_box_server("vd_box")
   # Module for rendering the commune boxes
   mod_communes_boxes_server("communes_box", inputVals = inputVals)
   ## tabReport ----
   # Module for producing rmd report based on downloadable_report.Rmd
   mod_download_rmd_server("rmd", inputVals = inputVals)
   ## tabInfo ----
   # Module for producing the text about the app
   mod_about_the_app_server("about")

}


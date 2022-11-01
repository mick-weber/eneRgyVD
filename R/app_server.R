#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny sf
#' @noRd
app_server <- function(input, output, session) {

   info_dev_message() # defined in fct_helpers.R. Warns that this is a development version

  ## Inputs module ----

    # This retrieves the reactiveVal() selected unit to convert the dataframes from
   selectedUnit <- mod_unit_converter_server("unit_converter")

    # This retrieves the inputs saved in mod_inputs.R
   inputVals <- mod_inputs_server("inputs_1")

  # subset_cons_data ----
   ## Subset data for consumption data (fed into mod_elec_charts_server("consumption_charts", ...))

   subset_cons_data <- reactive({

     # explicitely require communes to be selected
     validate(
       need(inputVals$selectedCommunes, "Sélectionner au moins une commune pour générer un résultat.")
     )

     # waiting on these to get initialized (renderUIs)
     req(inputVals$min_selected_cons,
         inputVals$max_selected_cons,
         inputVals$cons_dataset,
         selectedUnit$unit_to)

     # further filter cons_dataset with selected min/max values and convert to selectedUnit()
      # CONVERSION TEST IN PROGRESS
     inputVals$cons_dataset %>%
       dplyr::filter(annee >= inputVals$min_selected_cons,
                     annee <= inputVals$max_selected_cons)  %>%
         convert_units(colnames = "consommation",
                       unit_from = "kWh",
                       unit_to = selectedUnit$unit_to)

   })

   # Subset data prod & cons ----
   ## Subset data for production data (fed into mod_elec_charts_server("production_charts", ...))
   subset_prod_data <- reactive({

     # explicitely require communes to be selected
     validate(
       need(inputVals$selectedCommunes, "Sélectionner au moins une commune pour générer un résultat.")
     )
     # waiting on these to get initialized (renderUIs)
     req(inputVals$min_selected_prod,
         inputVals$max_selected_prod,
         inputVals$techs_selected,
         inputVals$prod_dataset)

     # prod by commune filtered with commune pickerInput(), years from sliderInput(), techs from pickerInput()

     inputVals$prod_dataset %>%
       dplyr::filter(annee >= inputVals$min_selected_prod,
                     annee <= inputVals$max_selected_prod) %>%
       dplyr::filter(categorie_diren %in% inputVals$techs_selected) %>%
        convert_units(colnames = contains(c("Injection", "Production", "Autoconso", "Puissance")),
                      unit_from = "kWh",
                      unit_to = selectedUnit$unit_to)

   }) # End reactive()


  # Sunburst data prod/cons ----

  subset_sunburst_prod_data <- reactive({

    req(subset_prod_data())

    subset_prod_data() %>%
      filter(annee == inputVals$max_selected_prod)

  })

  subset_sunburst_cons_data <- reactive({

    req(subset_cons_data())

    subset_cons_data() %>%
      filter(annee == inputVals$max_selected_cons)

  })

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
         lng2= bboxes %>% purrr::pluck(inputVals$selectedDistrict) %>% purrr::pluck("xmax"),
         lat1= bboxes %>% purrr::pluck(inputVals$selectedDistrict) %>% purrr::pluck("ymin"),
         lat2= bboxes %>% purrr::pluck(inputVals$selectedDistrict) %>% purrr::pluck("ymax"))
   })



  # END PROXY LEAFLET WORK
  # END MAP SELECTOR




   # Output modules ----

   ## tabCons: call the chart server logic ----
   mod_elec_charts_server("consumption_charts",
                          inputVals = inputVals,
                          subsetData = subset_cons_data,
                          selectedUnit = selectedUnit,
                          # args for create_bar_plotly() & create_sunburst_plotly()
                          sunburstData = subset_sunburst_cons_data,
                          legend_title = "Secteur",
                          var_year = "annee",
                          var_commune = "commune",
                          var_rank_2 = "secteur",
                          var_values = "consommation",
                          color_palette = colors_sectors,
                          third_rank = FALSE,
                          var_rank_3_1 = NULL, var_rank_3_2 = NULL,
                          # name of fct to create dt table
                          fct_table_dt_type = create_cons_table_dt,
                          # name of dl prefix to supply to download module
                          dl_prefix = "cons_elec_",
                          # documentation file from utils_helpers.R
                          doc_vars = elec_cons_doc)

   ## tabProd: call the chart server logic ----
   mod_elec_charts_server("production_charts",
                          inputVals = inputVals,
                          subsetData = subset_prod_data,
                          selectedUnit = selectedUnit,
                          # args for create_bar_plotly() & create_sunburst_plotly()
                          sunburstData = subset_sunburst_prod_data,
                          legend_title = "Technologies",
                          var_year = "annee",
                          var_commune = "commune",
                          var_rank_2 = "categorie_diren",
                          var_values = "production_totale",
                          color_palette = colors_categories,
                          third_rank = TRUE,
                          var_rank_3_1 = "injection_totale", var_rank_3_2 = "autoconso_totale",
                          # name of fct to create dt table
                          fct_table_dt_type = create_prod_table_dt,
                          # name of dl prefix to supply to download module
                          dl_prefix = "prod_elec_",
                          # documentation file from utils_helpers.R
                          doc_vars = elec_prod_doc)

   ## tabMap: boxes for statistics ----
   # Module for rendering the vd collapse box
   mod_collapse_stats_box_server("vd_box",
                                 title = "Canton de Vaud",
                                 production_value = prod_elec_vd_last_year, # utils_helpers.R
                                 consumption_value = cons_elec_vd_last_year, # utils_helpers.R
                                 year = last_common_elec_year) # utils_helpers.R

  # Dynamic module for rendering the communes collapse box
   output$communes_box <- renderUI({

     req(inputVals$selectedCommunes)

     mod_collapse_stats_box_server("communes_box",
                                   title = "Commune(s) sélectionnée(s)",
                                   production_value = inputVals$common_year_elec_prod, # mod_inputs.R
                                   consumption_value = inputVals$common_year_elec_cons, # mod_inputs.R
                                   year = last_common_elec_year) # utils_helpers.R
   })


   ## tabReport ----
   # Module for producing rmd report based on downloadable_report.Rmd
   mod_download_rmd_server("rmd", inputVals = inputVals)
   ## tabInfo ----
   # Module for producing the text about the app
   mod_about_the_app_server("about")

}


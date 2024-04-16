#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny sf
#' @importFrom purrr pwalk pluck
#' @noRd
app_server <- function(input, output, session) {

  # Print test area if needed ----

  # observe({
  #   print(inputVals$elec_cons_dataset)
  # })

   # Record logs ----
   ## We record session and errors (no inputs/outputs)

  # make sure the logs subfolder exists to store logs below
  # note to myself : if logs don't work this is probably due to lacking write authorisations !

  # RUN <sudo chmod -R 757 /srv/shiny-server/> in terminal if needed

  if(!dir.exists("./logs")){
    dir.create("./logs")
  }

   shinylogs::track_usage(
      what = c("session","errors"),
      storage_mode = shinylogs::store_json(path = "logs/") # store in the /logs subfolder
   )

   # Dev message ----

       info_dev_message() # defined in fct_helpers.R

   # Bookmarking feature ----

   # List of authorized inputs for bookmarking
   bookmarkingWhitelist <- c(
      "inputs_1-selected_communes",  # which communes are selected
      "unit_converter-selected_unit" # which unit is selected
   )

   # Trigger bookmarking only if communes OR units are modified
   observeEvent({

      inputVals$selectedCommunes
      inputVals$selectedUnit},{

      session$doBookmark()

   })

   # Exclude everything but bookmarkingWhitelist above
   ExcludedIDs <- reactiveVal(value = NULL)

   observe({
      toExclude <- setdiff(names(input), bookmarkingWhitelist) # utils.R
      setBookmarkExclude(toExclude)
      ExcludedIDs(toExclude)
   })

   # Update url with bookmarking state
   onBookmarked(function(url) {
      updateQueryString(url)
   })

   ## Tabs redirecting ----
   # in app_ui.R we have actionButtons to redirect in 'mode_about_the_app.R' data tabs

   # To avoid repetitive coding, we make a tribble of module-input events and target tabpanels
   #  only the last tabpanel is required, the others can be hard-coded in the purrr::walk
   #  since all redirect buttons end up in 'À propos' > 'about-tabset' area
   # <h6> tags are necessary since the titles of each panels (mod_about_the_app.R) have h6 wrappers
   # would be best to use a robust <id> argument but it's not implemented yet !

   subpanels_tribble <- dplyr::tribble(~observe_input, ~tabpanel_name,
                  "consumption_charts-elec_data_help",  "<h6>Distribution d'électricité</h6>",
                  "production_charts-elec_data_help",   "<h6>Production d'électricité</h6>",
                  "regener_needs-rgr_needs_help",       "<h6>Chaleur bâtiments</h6>",
                  "regener_cons-rgr_cons_help",         "<h6>Chaleur bâtiments</h6>",
                  "regener_misc-rgr_misc_help",         "<h6>Chaleur bâtiments</h6>",
                  "subsidies_building-subsidies_building_help", "<h6>Subventions bâtiments</h6>",
                  "subsidies_measure-subsidies_measure_help",  "<h6>Subventions bâtiments</h6>")

   # Code below is to generate redirections from methodological accordions in each module to mod_about_the_app.R
   # pwalk -> our tribble -> observeEvent -> input[[observe_input]] (.x) -> selected -> tabpanel_name (.y)
   purrr::pwalk(subpanels_tribble,
                ~ shiny::observeEvent(
                   input[[.x]], {

                     # First we redirect to navbar's 'A propos'
                     bslib::nav_select("nav", "À propos", session)
                     # Then we update to the first nav_panel 'Données' (in 'about-' ns module)
                     bslib::nav_select("about-tabset", selected = "Données", session)
                     # Last we update the nested nav_panel with subpanels_tribble$tabpanel_name
                     bslib::nav_select(id = "about-nested_tabset", selected = .y, session)
                   }
                   )) #End pwalk


   # When redirected to input$nav from a nav_select(), the dropdown remains open
   # We add this code to wait 2s before closing it so the user knows where it comes from

   observe({
     if (input$nav %in% "À propos") {
       Sys.sleep(2)
       session$sendCustomMessage(
         type = "toggleDropdown",
         message = list(msg = "hide dropdown"))
     }
   })



   # Inputs module ----

    # This retrieves the inputs saved in mod_inputs.R
   inputVals <- mod_inputs_server("inputs_1")

   ### Browser dimensions ----
   # height/width values are stored in inputVals because it's convenient
   # these are used for some dynamic plots sizing

   observe({

      inputVals$web_width <- shinybrowser::get_width()  # store width
      inputVals$web_height <- shinybrowser::get_height() # store height

   })

# Data retrievals ----
#  These next steps take the mod_inputs.R data (unit converted!) and creates objects
   # that may be further changed (subset_prod_data gets additionnal filtering if selected)
   # or that may be unchanged (e.g. regener data).

  ## Subset_elec_cons_data ----
   ## Subset data for consumption data (fed into mod_elec_charts_server("consumption_charts", ...))

    subset_elec_cons_data <- reactive({

      # explicitely require communes to be selected
      validate(
        need(inputVals$selectedCommunes, "Sélectionner au moins une commune pour générer un résultat.")
      )

      # waiting on these to get initialized (renderUIs)
      req(inputVals$min_selected_elec_cons,
          inputVals$max_selected_elec_cons,
          inputVals$elec_cons_dataset,
          inputVals$selectedUnit)

      # further filter cons_dataset with selected min/max values and convert to selectedUnit()
       # CONVERSION TEST IN PROGRESS
      inputVals$elec_cons_dataset |>
        dplyr::filter(annee >= inputVals$min_selected_elec_cons,
                      annee <= inputVals$max_selected_elec_cons)

    })


   ## Subset_elec_prod_data ----
   ## Subset data for production data (fed into mod_elec_charts_server("production_charts", ...))
   subset_elec_prod_data <- reactive({

     # explicitly require communes to be selected
     validate(
       need(inputVals$selectedCommunes,req_communes_phrase) # utils_helpers.R
     )
     # waiting on these to get initialized (renderUIs)
     req(inputVals$min_selected_elec_prod,
         inputVals$max_selected_elec_prod,
         inputVals$techs_selected,
         inputVals$elec_prod_dataset)

     # prod by commune filtered with commune pickerInput(), years from sliderInput(), techs from pickerInput()

     inputVals$elec_prod_dataset |>
       dplyr::filter(annee >= inputVals$min_selected_elec_prod,
                     annee <= inputVals$max_selected_elec_prod) |>
       dplyr::filter(categorie %in% inputVals$techs_selected)

   }) # End reactive()

   ## Subset regener (x3)----
   # subset_rgr1 : regener by commune, cons, ae, use

   subset_rgr_cons_1 <- reactive({

      # explicitly require communes to be selected
      validate(
         need(inputVals$selectedCommunes,req_communes_phrase) # utils_helpers.R
      )

      # waiting on these to get initialized (renderUIs)
      req(inputVals$selectedUnit)

      # No filter needed yet for years, only year conversion
      inputVals$rgr_1
   })

   # subset_rgr1 : regener by commune, cons, ae, aff

   subset_rgr_cons_2 <- reactive({

      # explicitly require communes to be selected
      validate(
         need(inputVals$selectedCommunes,req_communes_phrase) # utils_helpers.R
      )

      # waiting on these to get initialized (renderUIs)
      req(inputVals$selectedUnit)

      # No filter needed yet for years, only year conversion
      inputVals$rgr_2

   })

      # subset_rgr_needs : regener by commune, needs, use
   subset_rgr_needs <- reactive({

      # explicitly require communes to be selected
      validate(
         need(inputVals$selectedCommunes,req_communes_phrase) # utils_helpers.R
      )

      # waiting on these to get initialized (renderUIs)
      req(inputVals$selectedUnit)

      # No filter needed yet for years, only year conversion
      inputVals$rgr_needs

   })


      # subset_rgr_misc : regener by commune and misc columns
      # Note : we don't need to apply conver_units() so we would not need the
      # code below here, but this code is for consistency with other data imports
   subset_rgr_misc <- reactive({

      # explicitly require communes to be selected
      validate(
         need(inputVals$selectedCommunes,req_communes_phrase) # utils_helpers.R
      )

      # waiting on these to get initialized (renderUIs)
      req(inputVals$selectedUnit)

      inputVals$rgr_misc

   })


  ## Subset subsidies data (x2) ----

   subset_subsidies_building <- reactive({

      # explicitly require communes to be selected
      validate(
         need(inputVals$selectedCommunes, req_communes_phrase) # utils_helpers.R
      )

      inputVals$subsidies_building
   })


   subset_subsidies_measure <- reactive({

      # explicitly require communes to be selected
      validate(
         need(inputVals$selectedCommunes, req_communes_phrase) # utils_helpers.R
      )

      inputVals$subsidies_measure
   })

   # Upload communes ----

   observeEvent(inputVals$uploadedCommunesTimed,{

     ## UpdateSelectizeInput ----
     updateSelectizeInput(session = session,
                          inputId = "inputs_1-selected_communes",
                          choices = choices_canton_communes,
                          # We remove the first timestamp item (see mod_upload_communes.R/mod_inputs.R)
                          selected = inputVals$uploadedCommunesTimed[-1])

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

   # END RENDER LEAFLET BASE; START PROXYLEAFLET WORK


   # Define leaflet proxy to draw the base map once only and build on its proxy
   proxy <- leaflet::leafletProxy("map")

   # Create empty vector to hold all click ids
   selected <- reactiveValues(groups = vector())

   # Handles the highlight/removal of a polygon on CLICK EVENTS

   observeEvent(input$map_shape_click, {
     if(input$map_shape_click$group == "regions"){

       selected$groups <- c(selected$groups, input$map_shape_click$id)
       proxy |> leaflet::showGroup(group = input$map_shape_click$id)

     } else {
       selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
       proxy |> leaflet::hideGroup(group = input$map_shape_click$group)
     }
     # Update selectInput for bilateral communication map/widget
     # inputId got customized with the NS of the respective input module (inputs_1-)
     # this allows to update a widget from another module

     ## UpdateSelectizeInput ----
     updateSelectizeInput(session = session,
                          inputId = "inputs_1-selected_communes",
                          choices = choices_canton_communes,
                          selected = selected$groups)
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
       proxy |> leaflet::hideGroup(group = removed_via_selectInput)
     }

     if(length(added_via_selectInput) > 0){
       selected$groups <- inputVals$selectedCommunes
       proxy |> leaflet::showGroup(group = added_via_selectInput)
     }
   }, ignoreNULL = FALSE) # Don't trigger when input is NULL

   # END PROXY LEAFLET WORK
   # END MAP SELECTOR

   # Output modules ----

   ## tabCons: chart server logic ----
   mod_elec_charts_server("consumption_charts",
                          inputVals = inputVals,
                          subsetData = subset_elec_cons_data,
                          selectedUnit = inputVals$selectedUnit,
                          legend_title = "Secteur",
                          var_year = "annee",
                          var_commune = "commune",
                          var_rank_2 = "secteur", # might be NULL if needed
                          var_values = "consommation",
                          color_palette = colors_sectors, # app's defaults is blue if no var_rank_2 supplied
                          # name of fct to create dt table
                          fct_table_dt_type = create_cons_table_dt,
                          # name of dl prefix to supply to download module
                          dl_prefix = "profil_energie_elec_cons_",
                          # documentation file from utils_helpers.R
                          doc_vars = elec_cons_doc)


   ## tabProd: chart server logic ----
   mod_elec_charts_server("production_charts",
                          inputVals = inputVals,
                          subsetData = subset_elec_prod_data,
                          selectedUnit = inputVals$selectedUnit,
                          legend_title = NULL,
                          var_year = "annee",
                          var_commune = "commune",
                          var_rank_2 = "categorie",
                          var_values = "production",
                          color_palette = colors_categories,
                          # name of fct to create dt table
                          fct_table_dt_type = create_prod_table_dt,
                          # name of dl prefix to supply to download module
                          dl_prefix = "profil_energie_elec_prod_",
                          # documentation file from utils_helpers.R
                          doc_vars = elec_prod_doc)
  #
  #  ## tabRegener: chart server logic ----
  #  ### mod regener_cons ----
  #
   mod_regener_cons_charts_server("regener_cons",
                             inputVals = inputVals,
                             subset_rgr_cons_1 = subset_rgr_cons_1,
                             subset_rgr_cons_2 = subset_rgr_cons_2,
                             dl_prefix = "profil_energie_conso_bat_",
                             doc_vars = regener_doc # utils_helpers.R
                             )


  #  ### mod regener_needs ----
   mod_regener_needs_charts_server("regener_needs",
                                   inputVals = inputVals,
                                   subsetData = subset_rgr_needs, # filtered data for communes and selected years
                                   legend_title = "Usage", # for legend of barplot (either secteur/technologies)
                                   var_year = "statut", # 'etat' instead of 'annee' better reflects the dataset
                                   var_commune = "commune", # 'commune'
                                   var_rank_2 = "type", # categorical var ('secteur'/'categorie', ...)
                                   var_values = "besoins", # prod/consumption/besoins
                                   color_palette = colors_rg_type, # utils_helpers.R
                                   fct_table_dt_type = create_rg_needs_table_dt, # table function to pass (data specific)
                                   dl_prefix = "profil_energie_besoins_bat_",# when DL the data (mod_download_data.R) : prod_(...) or cons_(...)
                                   doc_vars = regener_doc # utils_helpers.R
                                   )

   ### mod regener_misc ----
   mod_regener_misc_charts_server("regener_misc",
                                  inputVals = inputVals,
                                  subsetData = subset_rgr_misc,
                                  selectedUnit = selectedUnit,
                                  dl_prefix = "profil_energie_regener_autres_",
                                  doc_vars = regener_doc)


  ## tabSubsidies: chart server logic ----

   ## mod tabSubsidiesBuilding ----
   mod_subsidies_building_charts_server("subsidies_building",
                               subsetData = subset_subsidies_building,
                               inputVals = inputVals,
                               dl_prefix = "profil_energie_subventions_bat_",
                               doc_vars = NULL # for now
                               )

   ###  mod tabSubsidiesMeasure ----
   mod_subsidies_measure_charts_server("subsidies_measure",
                               subsetData = subset_subsidies_measure,
                               inputVals = inputVals,
                               dl_prefix = "profil_energie_subventions_mesure_",
                               doc_vars = NULL # for now
   )



  #  ## tabMap: boxes for statistics ----


  ### VD Box ----
  #  # Must be dynamically rendered because it depends on selectedUnit (reactive)

   observe({


     req(inputVals$selectedUnit)

     mod_stats_box_server("vd_box",
                                   title = strong("Synthèse : Canton de Vaud"),
                                   selectedUnit = inputVals$selectedUnit,

                                   # Computed in utils_helpers.R (using years below) then converted if needed
                                   elec_prod_value = elec_prod_vd_last_year |>
                                     convert_units(unit_to = inputVals$selectedUnit),

                                   cons_rg_value = cons_rg_vd_last_year |>
                                     convert_units(unit_to = inputVals$selectedUnit),

                                   subsidies_value = subsidies_m01_vd_last_year,

                                   elec_cons_value = elec_cons_vd_last_year |>
                                     convert_units(unit_to = inputVals$selectedUnit),

                                   # Computed in utils_helpers.R for both statboxes
                                   year_elec_prod = last_year_elec_prod,
                                   year_elec_cons = last_year_elec_cons,
                                   year_rgr = last_year_rgr,
                                   year_subsidies = last_year_subsidies

                                   )

  })


  ### Communes box ----
  # This is dependant on a UI's conditionalpanel toggled with output.commune below
  # So that we remove the statbox if no more commune is selected (it was persisting without it)

   observe({

     req(inputVals$selectedUnit)

     check_selected_communes <- !is.null(inputVals$selectedCommunes)


     mod_stats_box_server("communes_box",
                                   title = strong("Synthèse : Commune(s) sélectionnée(s)"),
                                   selectedUnit = inputVals$selectedUnit,

                                   # Computed in utils_helpers.R (using years below) then converted if needed
                                   elec_prod_value = ifelse(
                                     check_selected_communes,
                                     inputVals$elec_prod_last_year,
                                     0),
                                   cons_rg_value = ifelse(
                                     check_selected_communes,
                                     inputVals$max_year_rg_cons,
                                     0),
                                   subsidies_value = ifelse(
                                     check_selected_communes,
                                     inputVals$max_year_subsidies_m01,
                                     0),
                                   elec_cons_value = ifelse(
                                     check_selected_communes,
                                     inputVals$elec_cons_last_year,
                                     0),

                                   # Computed in utils_helpers.R for both statboxes
                                   year_elec_prod = last_year_elec_prod,
                                   year_elec_cons = last_year_elec_cons,
                                   year_rgr = last_year_rgr,
                                   year_subsidies = last_year_subsidies
                                   )
   })


   ## tabReport ----
   # Module for producing rmd report based on downloadable_report.Rmd
   mod_download_rmd_server("report",
                           inputVals = inputVals)

   ## tabInfo ----
   # Module for producing the text about the app
   mod_about_the_app_server("about")


   }


#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny sf
#' @importFrom purrr pwalk pluck
#' @noRd
app_server <- function(input, output, session) {

  ## |---------------------------------------------------------------|
  ##          PRINT AREA FOR TESTING
  ## |---------------------------------------------------------------|

  # observe({
  #   print(state$value$nav)
  #   print(query$nav)
  #
  # })

  ## |---------------------------------------------------------------|
  ##          /PRINT AREA
  ## |---------------------------------------------------------------|

  # Record logs ----
  ## We record session and errors (no inputs/outputs)

  # make sure the logs subfolder exists to store logs below
  # note to myself : if logs don't work this is probably due to lacking write authorisations !

  # RUN: sudo chown -R rserver:rserver /srv/shiny-server/> in terminal if needed
  if(!dir.exists("./logs")){
    dir.create("./logs")
  }

  shinylogs::track_usage(
    what = c("session", "input"),
    storage_mode = shinylogs::store_json(path = "logs/"),#, # store in the /logs subfolder
    exclude_input_regex = "^(?!nav$|inputs_1-selected_communes$).*" #,
    #exclude_users = "h7q1n0" # myself locally
  )

  # Dev message ----

  welcome_modal() # defined in fct_helpers.R

  observeEvent(input$modal_info, {
    bslib::nav_select(id = "nav", selected = "À propos", session)
    removeModal() # Close the modal after redirecting
  })

  observeEvent(input$modal_news, {
    bslib::nav_select(id = "nav", selected = "Nouveautés", session)
    removeModal() # Close the modal after redirecting
  })

  # introJS ----

  observeEvent(input$introjs, {
    session$sendCustomMessage(type = "startIntro", list())
    removeModal()
  })

  # Bookmarking ----
  # This code allows returning back one step from the browser !

  # Track whether the app is initializing
  initializing <- TRUE

  # Load the correct tab on startup or URL change
  observeEvent(session$clientData$url_search, {

    query <- parseQueryString(session$clientData$url_search)

    if (!is.null(query$nav) && query$nav != input$nav) {
      bslib::nav_select("nav", query$nav, session)  # Switch to the tab from the URL
    }

    # Mark initialization as complete, scoping assignment to write in parent function
    initializing <<- FALSE

    }, ignoreInit = FALSE)  # Trigger on app start


  # Update the URL when switching tabs
  observeEvent(input$nav, {
    # Prevent URL update during initialization or redundant updates
    if (!initializing) {
      updateQueryString(paste0("?nav=", input$nav), mode = "push")
    }
  }, ignoreInit = TRUE)  # Ignore the initial tab value




  # Redirect click logo --> Accueil ----

  observeEvent(
    input$clickLogoToHomepage_click, {
      bslib::nav_select(
        id = "nav",
        select = "Accueil",
        session = session
      )
    }
  )

  # Redirect to method tabs ----

  # Code below is to generate redirections from methodological accordions in each module to mod_about_the_app.R
  # pwalk -> our tribble -> observeEvent -> input[[observe_input]] (.x) -> selected -> tabpanel_name (.y)

  purrr::pwalk(subset(subpanels_tribble, select = c("observe_input", "about_nav_panel", "navset_id", "about_tabpanel_name")), # selecting all cols result in unnused arguments
               .f = \(observe_input, about_nav_panel, navset_id, about_tabpanel_name) shiny::observeEvent(
                 input[[observe_input]], {

                   # 1. Redirect to navbar's 'A propos'
                   bslib::nav_select(id = "nav",
                                     selected = "À propos", session)

                   # 2. Redirect to 'Données' nav_panel inside tabset_main inside 'about' named mod_about_the_app.R module
                   bslib::nav_select(id = "about-tabset_main",
                                     selected = "Données", session)

                   # # 3. Redirect to <navset_name> which may vary according to which general theme the button is clicked from (Energie, Climat, ...)
                   bslib::nav_select(id = "about-navset_donnees",
                                     selected = about_nav_panel, session)

                   # 4. Redirect to the final nav_panel name inside the dynamic navset_<theme>
                   bslib::nav_select(id = paste0("about-", navset_id),
                                     selected = about_tabpanel_name, session)

                 }
               )) #End pwalk


  # When redirected to input$nav from a nav_select(), the dropdown remains open... annoying.
  # Case 1) if redirected from data tabs to 'A propos' (from redirections above) --> keep the dropdown open 2 sec to inform the user
  # Case 2) if redirect from statboxes clicks to datatabs (mod_stats_box.R) --> simply remove the dropdown display because Sys.Sleep() delays display of plots, etc.

  observeEvent(
    input$nav, {
      session$sendCustomMessage(
        type = "toggleDropdown",
        message = list(msg = "hide dropdown"))
    }
  )


  # Inputs module ----

  # This retrieves the inputs saved in mod_inputs.R
  inputVals <- mod_inputs_server("inputs_1")

  # Upload communes ----

  observeEvent(inputVals$uploadedCommunesTimed,{

    ## UpdateSelectizeInput ----
    # only when uploaded communes via widget !
    updateSelectizeInput(session = session,
                         inputId = "inputs_1-selected_communes",
                         choices = choices_canton_communes,
                         # We remove the first timestamp item (see mod_upload_communes.R/mod_inputs.R)
                         selected = inputVals$uploadedCommunesTimed[-1])

  })

  ### Table of content ('Accueil') ----

  mod_table_content_server("toc", parent = session)

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
  observeEvent(inputVals$selectedCommunesDirect, {

    if("Canton de Vaud" %in% inputVals$selectedCommunesDirect){

      proxy |>
        # Contour for Canton VD, orange + light orange fill
        leaflet::addPolylines(data = sf_canton, # utils_helpers.R ; static
                              layerId = "highlight_canton", # layer id to remove if unselected
                              color = "#3A862D",
                              fill = T, fillColor = "#75AA6C",
                              # Disable clikable events otherwise it will crash (!)
                              options = leaflet::pathOptions(interactive = FALSE)
        )
    }else {
      # If Canton de Vaud is not in the selection, we remove the canton highlight Polyline
      proxy |>
        leaflet::removeShape(layerId = "highlight_canton")

    }

    ## Polygons highlighting (Communes selected only) ----
    removed_via_selectInput <- setdiff(selected$groups, inputVals$selectedCommunesDirect)
    added_via_selectInput <- setdiff(inputVals$selectedCommunesDirect, selected$groups)

    if(length(removed_via_selectInput) > 0){
      selected$groups <- inputVals$selectedCommunesDirect
      proxy |> leaflet::hideGroup(group = removed_via_selectInput)
    }

    if(length(added_via_selectInput) > 0){
      selected$groups <- inputVals$selectedCommunesDirect
      proxy |> leaflet::showGroup(group = added_via_selectInput)
    }
  }, ignoreNULL = FALSE) # Don't trigger when input is NULL

  # END PROXY LEAFLET WORK
  # END MAP SELECTOR

  # Output modules energy ----

  ## mod_elec_charts (cons) ----
  mod_elec_charts_server("consumption_charts",
                         inputVals = inputVals,
                         subsetData = reactive({inputVals$energyDatasets$elec_cons}),
                         legend_title = "Secteur : ",
                         var_year = "annee",
                         var_commune = "commune",
                         var_cat = "secteur", # might be NULL if needed
                         var_values = "consommation",
                         color_palette = colors_sectors, # app's defaults is blue if no var_cat supplied
                         icons_palette = cons_icons,
                         dl_prefix = "elec_cons_",
                         # documentation file from utils_helpers.R
                         doc_vars = doc_datasets$elec_cons)


  ## mod_elec_charts (prod) ----
  mod_elec_charts_server("production_charts",
                         inputVals = inputVals,
                         subsetData = reactive({inputVals$energyDatasets$elec_prod}),
                         legend_title = NULL,
                         var_year = "annee",
                         var_commune = "commune",
                         var_cat = "categorie",
                         var_values = c("production", "injection", "autoconsommation", "puissance_electrique_installee"),
                         color_palette = colors_categories,
                         icons_palette = prod_icons,
                         dl_prefix = "elec_prod_",
                         # documentation file from utils_helpers.R
                         doc_vars = doc_datasets$elec_prod)



  ## mod ng_charts ----

  mod_ng_charts_server("ng_cons_charts",
                       inputVals = inputVals,
                       subsetData = reactive({inputVals$energyDatasets$ng_cons}),
                       var_commune = "commune",
                       var_year = "annee",
                       var_cat = "secteur",
                       var_values = "consommation",
                       color_palette = colors_sectors,
                       dl_prefix = "conso_gaz_",
                       doc_vars = doc_datasets$ng_cons # utils_helpers.R
  )

  ## mod regener_cons_charts ----
  mod_regener_cons_charts_server("regener_cons",
                                 inputVals = inputVals,
                                 subset_rgr_cons_year = reactive({inputVals$energyDatasets$regener_cons_ae_year}),
                                 subset_rgr_cons_use = reactive({inputVals$energyDatasets$regener_cons_ae_use}),
                                 subset_rgr_cons_aff = reactive({inputVals$energyDatasets$regener_cons_ae_aff}),
                                 dl_prefix = "conso_bat_",
                                 doc_vars = doc_datasets$regener # utils_helpers.R
  )


  ## mod regener_needs_charts ----
  mod_regener_needs_charts_server("regener_needs",
                                  inputVals = inputVals,
                                  subsetData = reactive({inputVals$energyDatasets$regener_needs}), # filtered data for communes and selected years
                                  legend_title = "Usage : ", # for legend of barplot (either secteur/technologies)
                                  var_year = "statut", # DATASET SPECIFICITY : we don't plot usual year on X axis, but <statut> (actuel vs théorique)
                                  var_commune = "commune", # 'commune'
                                  var_cat = "type", # categorical var ('secteur'/'categorie', ...)
                                  var_values = "besoins", # prod/consumption/besoins
                                  color_palette = colors_rg_type, # utils_helpers.R
                                  dl_prefix = "besoins_bat_",# when DL the data (mod_download_data.R) : prod_(...) or cons_(...)
                                  doc_vars = doc_datasets$regener # utils_helpers.R
  )

  ## mod regener_misc_charts ----
  mod_regener_misc_charts_server("regener_misc",
                                 inputVals = inputVals,
                                 subsetData = reactive({inputVals$energyDatasets$regener_misc}),
                                 dl_prefix = "regener_autres_",
                                 doc_vars = doc_datasets$regener # utils_helpers.R
  )


  ## mod_subsidies_building_charts ----
  mod_subsidies_building_charts_server("subsidies_building",
                                       subsetData = reactive({inputVals$energyDatasets$subsidies_by_building}),
                                       inputVals = inputVals,
                                       dl_prefix = "subventions_bat_",
                                       doc_vars = doc_datasets$subsidies # utils_helpers.R
  )

  ##  mod_subsidies_measure_charts ----
  mod_subsidies_measure_charts_server("subsidies_measure",
                                      subsetData = reactive({inputVals$energyDatasets$subsidies_by_measure}),
                                      inputVals = inputVals,
                                      dl_prefix = "subventions_mesure_",
                                      doc_vars = doc_datasets$subsidies # utils_helpers.R
  )


  # Output modules adaptation ----

  ## canopy (generic) ----

  mod_generic_charts_server("adaptation_canopy",
                            parent = session,
                            subsetData = reactive({inputVals$adaptationDatasets$surface_canopee}),
                            inputVals = inputVals,
                            var_commune = "commune",
                            var_year = "annee",
                            var_values = c("part_surface", "surface_urbaine"),
                            ggiraph_geom = "col",
                            unit = c("%", "ha"),
                            coerce_dodge = FALSE, # these should not be stacked --> don't make sense
                            var_cat = "categorie",
                            color_palette = surface_canopee_palette, # default_palette, dedicated one, or one color
                            legend_title = "Hectares : ",
                            dl_prefix = "canopee_urbaine_",
                            doc_vars = NULL # for now
  )

  ## natural hazards (generic) ----
  mod_generic_charts_server("buildings_exposure_hazards",
                            parent = session,
                            subsetData = reactive({inputVals$adaptationDatasets$batiment_danger}),
                            inputVals = inputVals,
                            var_commune = "commune",
                            var_year = "annee",
                            var_values = "nombre_batiment",
                            ggiraph_geom = "col",
                            unit = NULL,
                            coerce_dodge = FALSE,
                            var_cat = "categorie", # ask OCDC to change dataset var name...
                            color_palette = batiment_danger_palette, # default_palette, dedicated one, or one color
                            legend_title = "Nombre de bâtiments : ",
                            dl_prefix = "bat_danger_",
                            doc_vars = NULL # for now
  )


  # Output modules mobility ----

  ## EV share ----

  mod_generic_charts_server("part_voit_elec",
                            parent = session,
                            subsetData = reactive({inputVals$mobilityDatasets$part_voit_elec}),
                            inputVals = inputVals,
                            var_commune = "commune",
                            var_year = "annee",
                            var_values = "part_voit_elec",
                            ggiraph_geom = "col",
                            unit = "%",
                            coerce_dodge = FALSE,
                            var_cat = NULL,
                            color_palette = "#3c9ba7", # default_palette, dedicated one, or one color
                            legend_title = NULL,
                            dl_prefix = "part_ve_",
                            doc_vars = NULL # for now
  )

  ## Motorisation rate ----
  mod_generic_charts_server("taux_motorisation",
                            parent = session,
                            subsetData = reactive({inputVals$mobilityDatasets$taux_motorisation}),
                            inputVals = inputVals,
                            var_commune = "commune",
                            var_year = "annee",
                            var_values = "taux_motorisation",
                            ggiraph_geom = "line",
                            unit = "vhc/1000 habitants",
                            coerce_dodge = FALSE,
                            var_cat = NULL,
                            color_palette = "#f5b093", # default_palette, dedicated one, or one color
                            legend_title = NULL,
                            dl_prefix = "taux_motorisation_",
                            doc_vars = NULL # for now
  )

  ## Desserte quality ----
  mod_generic_charts_server("qualite_desserte",
                            parent = session,
                            subsetData = reactive({inputVals$mobilityDatasets$qualite_desserte}),
                            inputVals = inputVals,
                            var_commune = "commune",
                            var_year = "annee",
                            var_values = "qualite_desserte",
                            var_cat = "type",
                            ggiraph_geom = "line",
                            unit = NULL,
                            coerce_dodge = FALSE,
                            color_palette = qualite_desserte_palette, # dedicated one, or one color
                            legend_title = "Type d'indice",
                            dl_prefix = "desserte_tp_",
                            doc_vars = NULL # for now
  )

  ## tabInfo ----
  # Module for producing the text about the app
  mod_about_the_app_server("about")


}


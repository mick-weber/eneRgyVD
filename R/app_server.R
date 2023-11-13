#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny sf
#' @importFrom purrr pwalk pluck
#' @noRd
app_server <- function(input, output, session) {

   # Record logs ----
   ## We record session and errors (no inputs/outputs)

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
      selectedUnit$unit_to},{

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

   # To avoid repetitive coding, we make a tribble of input events and target tabpanels
   #  only the last tabpanel is required, the others can be hard-coded in the purrr::walk

   subpanels_tribble <- dplyr::tribble(~observe_input, ~tabpanel_name,
                  "cons_data_help", "Consommation d'électricité",
                  "prod_data_help", "Production d'électricité",
                  "rg_needs_help",  "Chaleur bâtiments",
                  "rg_cons_help",   "Chaleur bâtiments",
                  "rg_misc_help",   "Chaleur bâtiments",
                  "subsidies_help", "Subventions bâtiments")

   # Code below is to generate updatebs4TabItems redirections
   # pwalk -> our tribble -> observeEvent -> input[[observe_input]] (.x) -> selected -> tabpanel_name (.y)
   purrr::pwalk(subpanels_tribble,
                ~ shiny::observeEvent(
                   input[[.x]], {

                      # First we redirect to sidebar's tabInfo
                   bs4Dash::updateTabItems(session, "sidebarMenu", "tabInfo")
                      # Then we update to the first tabPanel 'Données' (in 'about-' mod)
                   bs4Dash::updatebs4TabItems(session, "about-tabset", selected = "Données")
                      # Last we update the nested tabPanel with subpanels_tribble$tabpanel_name
                   bs4Dash::updatebs4TabItems(session, "about-nested_tabset", selected = .y)
                   }
                   )) #End pwalk



   # Inputs module ----

    # This returns the reactiveVal() selected unit to convert the dataframes from
   selectedUnit <- mod_unit_converter_server("unit_converter")

    # This retrieves the inputs saved in mod_inputs.R
   inputVals <- mod_inputs_server("inputs_1",
                                  selectedUnit = selectedUnit)

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

  ## Subset_cons_data ----
   ## Subset data for consumption data (fed into mod_elec_charts_server("consumption_charts", ...))

   # !!CONS_ELEC removed!! # subset_cons_data <- reactive({
   # !!CONS_ELEC removed!! #
   # !!CONS_ELEC removed!! #   # explicitely require communes to be selected
   # !!CONS_ELEC removed!! #   validate(
   # !!CONS_ELEC removed!! #     need(inputVals$selectedCommunes, "Sélectionner au moins une commune pour générer un résultat.")
   # !!CONS_ELEC removed!! #   )
   # !!CONS_ELEC removed!! #
   # !!CONS_ELEC removed!! #   # waiting on these to get initialized (renderUIs)
   # !!CONS_ELEC removed!! #   req(inputVals$min_selected_cons,
   # !!CONS_ELEC removed!! #       inputVals$max_selected_cons,
   # !!CONS_ELEC removed!! #       inputVals$cons_dataset,
   # !!CONS_ELEC removed!! #       selectedUnit$unit_to)
   # !!CONS_ELEC removed!! #
   # !!CONS_ELEC removed!! #   # further filter cons_dataset with selected min/max values and convert to selectedUnit()
   # !!CONS_ELEC removed!! #    # CONVERSION TEST IN PROGRESS
   # !!CONS_ELEC removed!! #   inputVals$cons_dataset %>%
   # !!CONS_ELEC removed!! #     dplyr::filter(annee >= inputVals$min_selected_cons,
   # !!CONS_ELEC removed!! #                   annee <= inputVals$max_selected_cons)  %>%
   # !!CONS_ELEC removed!! #       convert_units(colnames = "consommation",
   # !!CONS_ELEC removed!! #                     unit_from = "kWh",
   # !!CONS_ELEC removed!! #                     unit_to = selectedUnit$unit_to)
   # !!CONS_ELEC removed!! #
   # !!CONS_ELEC removed!! # })

   ## Subset_prod_data ----
   ## Subset data for production data (fed into mod_elec_charts_server("production_charts", ...))
   subset_prod_data <- reactive({

     # explicitly require communes to be selected
     validate(
       need(inputVals$selectedCommunes,req_communes_phrase) # utils_helpers.R
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
      req(selectedUnit$unit_to)

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
      req(selectedUnit$unit_to)

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
      req(selectedUnit$unit_to)

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
      req(selectedUnit$unit_to)

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


  ## Sunburst filter data prod/cons ----

   # PROD
  subset_sunburst_prod_data <- reactive({

    req(subset_prod_data())

    subset_prod_data() %>%
      filter(annee == inputVals$max_selected_prod)

  })

   # CONS
# !!CONS_ELEC removed!! #  subset_sunburst_cons_data <- reactive({
# !!CONS_ELEC removed!! #
# !!CONS_ELEC removed!! #    req(subset_cons_data())
# !!CONS_ELEC removed!! #
# !!CONS_ELEC removed!! #    subset_cons_data() %>%
# !!CONS_ELEC removed!! #      filter(annee == inputVals$max_selected_cons)
# !!CONS_ELEC removed!! #
# !!CONS_ELEC removed!! #  })



   # mod_map (leaflet module) ----
   # This returns selected$groups from mod_map_server(),
   # which stores the communes selected in the map & selectizeInput combined

   map_selected_items <- mod_map_server("map", inputVals = inputVals)

   observeEvent(map_selected_items(),
                {
                   # map_selected_items <- map_selected_items()

                      updateSelectInput(session = session,
                                        inputId = "inputs_1-selected_communes",
                                        selected = map_selected_items())

                })


   # Output modules ----

   ## tabCons: chart server logic ----
   # !!CONS_ELEC removed!! # mod_elec_charts_server("consumption_charts",
   # !!CONS_ELEC removed!! #                        inputVals = inputVals,
   # !!CONS_ELEC removed!! #                        subsetData = subset_cons_data,
   # !!CONS_ELEC removed!! #                        selectedUnit = selectedUnit,
   # !!CONS_ELEC removed!! #                        # args for create_bar_plotly() & create_sunburst_plotly()
   # !!CONS_ELEC removed!! #                        sunburstData = subset_sunburst_cons_data,
   # !!CONS_ELEC removed!! #                        legend_title = "Secteur",
   # !!CONS_ELEC removed!! #                        var_year = "annee",
   # !!CONS_ELEC removed!! #                        var_commune = "commune",
   # !!CONS_ELEC removed!! #                        var_rank_2 = "secteur",
   # !!CONS_ELEC removed!! #                        var_values = "consommation",
   # !!CONS_ELEC removed!! #                        color_palette = colors_sectors,
   # !!CONS_ELEC removed!! #                        third_rank = FALSE,
   # !!CONS_ELEC removed!! #                        var_rank_3_1 = NULL, var_rank_3_2 = NULL,
   # !!CONS_ELEC removed!! #                        # name of fct to create dt table
   # !!CONS_ELEC removed!! #                        fct_table_dt_type = create_cons_table_dt,
   # !!CONS_ELEC removed!! #                        # name of dl prefix to supply to download module
   # !!CONS_ELEC removed!! #                        dl_prefix = "cons_elec_",
   # !!CONS_ELEC removed!! #                        # documentation file from utils_helpers.R
   # !!CONS_ELEC removed!! #                        doc_vars = elec_cons_doc)

   ## tabProd: chart server logic ----
   mod_elec_charts_server("production_charts",
                          inputVals = inputVals,
                          subsetData = subset_prod_data,
                          selectedUnit = selectedUnit,
                          # args for create_bar_plotly() & create_sunburst_plotly()
                          sunburstData = subset_sunburst_prod_data,
                          legend_title = "Technologies",
                          var_year = "annee",
                          var_commune = "commune",
                          var_rank_2 = "categorie",
                          var_values = "production",
                          color_palette = colors_categories,
                          third_rank = TRUE,
                          var_rank_3_1 = "injection",
                          var_rank_3_2 = "autoconsommation",
                          # name of fct to create dt table
                          fct_table_dt_type = create_prod_table_dt,
                          # name of dl prefix to supply to download module
                          dl_prefix = "prod_elec_",
                          # documentation file from utils_helpers.R
                          doc_vars = elec_prod_doc)

   ## tabRegener: chart server logic ----
   ### mod regener_cons ----

   mod_regener_cons_charts_server("regener_cons",
                             inputVals = inputVals,
                             selectedUnit = selectedUnit,
                             subset_rgr_cons_1 = subset_rgr_cons_1,
                             subset_rgr_cons_2 = subset_rgr_cons_2)
   ### mod regener_needs ----
   mod_regener_needs_charts_server("regener_needs",
                                   inputVals = inputVals,
                                   subsetData = subset_rgr_needs, # filtered data for communes and selected years
                                   selectedUnit = selectedUnit, # unit selected in mod_unit_converter.R
                                   legend_title = "Usage", # for legend of barplot (either secteur/technologies)
                                   var_year = "statut", # 'etat' instead of 'annee' better reflects the dataset
                                   var_commune = "commune", # 'commune'
                                   var_rank_2 = "type", # categorical var ('secteur'/'categorie', ...)
                                   var_values = "besoins", # prod/consumption/besoins
                                   color_palette = colors_rg_type, # utils_helpers.R
                                   third_rank = FALSE, # boolean
                                   var_rank_3_1 = NULL, var_rank_3_2 = NULL,
                                   fct_table_dt_type = create_rg_needs_table_dt, # table function to pass (data specific)
                                   dl_prefix = "besoins_bat_",# when DL the data (mod_download_data.R) : prod_(...) or cons_(...)
                                   doc_vars = regener_doc # utils_helpers.R
                                   )

   ### mod regener_misc ----
   mod_regener_misc_charts_server("regener_misc",
                                  inputVals = inputVals,
                                  subsetData = subset_rgr_misc,
                                  selectedUnit = selectedUnit,
                                  dl_prefix = "regener_autres_",
                                  doc_vars = regener_doc)


   ## tabSubsidies: chart server logic ----

   ### mod tabSubsidiesBuilding ----
   mod_subsidies_building_charts_server("subsidies_building",
                               subsetData = subset_subsidies_building,
                               inputVals = inputVals,
                               dl_prefix = "subventions_bat_",
                               doc_vars = NULL # for now
                               )

   ###  mod tabSubsidiesMeasure ----
   mod_subsidies_measure_charts_server("subsidies_measure",
                               subsetData = subset_subsidies_measure,
                               inputVals = inputVals,
                               dl_prefix = "subventions_mesure_",
                               doc_vars = NULL # for now
   )



   ## tabMap: boxes for statistics ----
   ### VD Box ----
   # Must be dynamically rendered because it depends on selectedUnit (reactive)

   output$vd_box <- renderUI({

   req(selectedUnit$unit_to)

   mod_collapse_stats_box_server("vd_box",
                                 title = "Synthèse : Canton de Vaud",
                                 selectedUnit = selectedUnit,
                                 prod_elec_value = prod_elec_vd_last_year |>
                                    convert_units(unit_to = selectedUnit$unit_to), # utils_helpers.R

                                 # !! CONS_ELEC removed !! # cons_elec_value = cons_elec_vd_last_year, # utils_helpers.R

                                 cons_rg_value = cons_rg_vd_last_year |>
                                    convert_units(unit_to = selectedUnit$unit_to), # utils_helpers.R

                                 year = last_common_elec_year) # utils_helpers.R

   })



  ### Communes box ----
  # Must be dynamically rendered because it depends on selectedUnit (reactive)
   output$communes_box <- renderUI({

     req(inputVals$selectedCommunes, selectedUnit$unit_to)

     mod_collapse_stats_box_server("communes_box",
                                   title = "Synthèse : Commune(s) sélectionnée(s)",
                                   selectedUnit = selectedUnit,

                                   prod_elec_value = inputVals$common_year_elec_prod, # mod_inputs.R

                                   # !! CONS_ELEC removed !! # cons_elec_value = inputVals$common_year_elec_cons, # mod_inputs.R

                                   cons_rg_value = inputVals$max_year_rg_cons,
                                   year = last_common_elec_year) # utils_helpers.R
   })


   ## tabReport ----
   # Module for producing rmd report based on downloadable_report.Rmd
   mod_download_rmd_server("rmd",
                           inputVals = inputVals,
                           selectedUnit = selectedUnit)
   ## tabInfo ----
   # Module for producing the text about the app
   mod_about_the_app_server("about")

}


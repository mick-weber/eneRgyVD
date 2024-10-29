#' sideboard_inputs UI Function
#'
#' @description A shiny Module which populates the sideboard of the dashboard.
#' Some of the widgets are dynamic, so we also create the reactive data in the top server part.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_inputs_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(

    # selectizeInput() for municipalities ----
    shiny::selectizeInput(inputId = ns("selected_communes"),
                          label = "Sélection des communes",
                          choices = choices_canton_communes,
                          selected = NULL,
                          multiple = TRUE,
                          options = list(placeholder = "Plusieurs acceptées")
    ),
      # conditionalPanel uploadCommunes widget ----

      # IF tab Accueil
      shiny::conditionalPanel(
        condition="input.nav == 'Accueil'",

        # We open a div to wrap the label + widget so that they don't get distinguished by the 'gap' spacer from bslib
        tags$div(
          br(),
          # Label as for selectizeInput for esthetics (form-label bs5 class) + add tooltip
          tags$p("Importer des communes",

                 bslib::tooltip(
                   id = "tooltip_import_communes",
                   placement = "right",
                   options = list(customClass = "customTooltips"), # custom.scss
                   trigger = bsicons::bs_icon("info-circle"),
                   "Cette fonctionnalité permet d'importer un fichier csv avec des numéros OFS de communes pour automatiser une sélection de communes."),
                 style = "margin-bottom:0.5rem !important;"),

          mod_upload_communes_ui(ns("uploaded_communes"))
        )
      ),# End div


    ## |---------------------------------------------------------------|
    ##          This section is source of many sorrows !!
    ## |---------------------------------------------------------------|
    # we must remember to update these conditionalPanel conditions when we change the names of each nav_panel() and navset_card_pill() !!
    # otherwise the plots etc. fail to render because they can't access the required conditional input values ! (selected year, etc.)


    # uiOutput for elec consumption ----

    shiny::conditionalPanel(
      condition="input.nav == 'Electricité' && input.navset_elec == 'Distribution d\\\'électricité'", # 2 conditions + triple escape : 2 for R, 1 for JS

      shiny::uiOutput(ns("elec_cons_year"))

    ), # End conditionalPanel

    # uiOutput() for elec production ----

    shiny::conditionalPanel(
      condition="input.nav == 'Electricité' && input.navset_elec == 'Production d\\\'électricité'", # 2 conditions + triple escape : 2 for R, 1 for JS

      shiny::uiOutput(ns("prod_year_n_techs"))

    ), # End conditionalPanel

    # uiOutput() for regener ----
    shiny::conditionalPanel(
      condition = "input.nav == 'Chaleur des bâtiments' && ['Besoins des bâtiments', 'Consommation des bâtiments'].includes(input.navset_regener)", # 2 conditions !
      shiny::uiOutput(ns("regener_year_selector"))

    ), # End conditionalPanel


    # uiOutput() for ng_cons ----
    shiny::conditionalPanel(
      condition = "input.nav == 'Gaz naturel'", # 1 condition !
      shiny::uiOutput(ns("ng_cons_year_selector"))

    ), # End conditionalPanel


    # Sidebar bottom ----

    tags$div(style = "margin-top: auto;",

             ## 1. Downoad all widget ----
             mod_download_all_data_ui(ns("download_all_data")),

             ##  2. Unit converter widget ----
             mod_unit_converter_ui(ns("unit_converter"))
             )# End div()

  ) # End tagList
} # End UI

#' sideboard_inputs Server Functions
#'
#' @noRd
mod_inputs_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # 0. Retrieve units ----
    selectedUnits <- mod_unit_converter_server("unit_converter")


    ## |---------------------------------------------------------------|
    ##          REWRITE DATASETS AND INPUTVALS
    ## |---------------------------------------------------------------|


    # Initialize inputVals ----
    inputVals <- reactiveValues(
      energyDatasets = reactiveValues(),
      mobilityDatasets = reactiveValues(),
      adaptationDatasets = reactiveValues()
    )


    # store energyDatasets ----
    observe({

      req(input$selected_communes)
      req(selectedUnits$energy_unit)
      req(selectedUnits$co2_unit)

      inputVals$energyDatasets <- energy_datasets |>
        purrr::map(\(energy_df){
          energy_df |>
            dplyr::filter(commune %in% input$selected_communes) |>
            convert_units(colnames = c("consommation", "besoins",
                                       "production", "injection", "autoconsommation",
                                       "puissance_electrique_installee"),
                          unit_from = "kWh",
                          unit_to = selectedUnits$energy_unit) |>
            convert_units(colnames = c("co2_direct",
                                       unit_from = "tCO2", # initial value in dataset
                                       unit_to = selectedUnits$co2_unit
                                       ))
        })
    })

    # store mobilityDatasets ----
    observe({
      req(input$selected_communes)

      inputVals$mobilityDatasets <- mobility_datasets |>
        purrr::map(\(mobility_df){
          mobility_df |>
            dplyr::filter(commune %in% input$selected_communes)
          # no convert_units() call here
        })
    })

    # store adaptationDatasets----
    observe({
      req(input$selected_communes)
      req(selectedUnits$co2_unit)

      inputVals$adaptationDatasets <- adaptation_datasets |>
        purrr::map(\(adaptation_df){
          adaptation_df |>
            dplyr::filter(commune %in% input$selected_communes) |>
            convert_units(colnames = NULL, # no value yet
                          unit_from = "tCO2", # expected initial value in dataset
                          unit_to = selectedUnits$co2_unit)
        })
    })





    ## |---------------------------------------------------------------|
    ##          END REWRITE DATASETS AND INPUTVALS
    ## |---------------------------------------------------------------|

#
#     ## Subset datasets + convert units
#     # 1. tabCons inputs ----
#
#      subset_elec_cons <- reactive({
#
#        req(input$selected_communes)
#
#        elec_cons |>
#          filter(commune %in% input$selected_communes) |>
#          convert_units(colnames = "consommation",
#                        unit_from = "kWh", # initial value in dataset
#                        unit_to = selectedUnits$energy_unit)
#
#      })
#
#
#     # 2. tabProd inputs ----
#
#     ## Reactive subset data  ----
#     # This is only used to feed the dynamic UI (year/techs available)
#     # The 'usable' dataset for plots etc. is computed in app_server.R and is further filtered by
#     # the values of the year sliderInput() and techs pickerInput()
#
#     subset_elec_prod <- reactive({
#
#       req(input$selected_communes)
#       req(selectedUnits$energy_unit)
#
#       elec_prod |>
#         filter(commune %in% input$selected_communes) |>
#         convert_units(colnames = contains(c("injection", "production", "autoconso", "puissance")),
#                       unit_from = "kWh", # initial value in dataset
#                       unit_to = selectedUnits$energy_unit)
#     })
#
#
#     # 3. tabRegener inputs ----
#     # Subset aggregated regener data with the currently selected commune(s)
#
#     ## 1/4: cons/use ----
#     # regener by commune, consumption, ae, use
#     # I may rename these later for more explicit names...
#     subset_rgr_cons_1 <- reactive({
#
#       req(input$selected_communes)
#       req(selectedUnits$energy_unit)
#       req(selectedUnits$co2_unit)
#
#       regener_cons_ae_use |>
#         filter(commune %in% input$selected_communes) |>
#         convert_units(colnames = "consommation",
#                       unit_from = "kWh", # initial value in dataset
#                       unit_to = selectedUnits$energy_unit)  |>
#         convert_units(colnames = contains("co2"),
#                       unit_from = "tCO2", # initial value in dataset
#                       unit_to = selectedUnits$co2_unit)
#
#     })
#
#     ## 2/4: cons/aff ----
#     # regener by commune, consumption, ae, aff
#     # I may rename these later for more explicit names...
#     subset_rgr_cons_2 <- reactive({
#
#       req(input$selected_communes)
#       req(selectedUnits$energy_unit)
#       req(selectedUnits$co2_unit)
#
#       regener_cons_ae_aff |>
#         filter(commune %in% input$selected_communes) |>
#         convert_units(colnames = "consommation",
#                       unit_from = "kWh", # initial value in dataset
#                       unit_to = selectedUnits$energy_unit)  |>
#         convert_units(colnames = contains("co2"),
#                       unit_from = "tCO2", # initial value in dataset
#                       unit_to = selectedUnits$co2_unit)
#
#     })
#
#     ## 3/4 : needs ----
#     # regener by commune, needs
#
#     subset_rgr_needs <- reactive({
#
#       req(input$selected_communes)
#       req(selectedUnits$energy_unit)
#
#       regener_needs |>
#         dplyr::filter(commune %in% input$selected_communes) |>
#         convert_units(colnames = contains("besoins"),
#                       unit_from = "kWh", # initial value in dataset
#                       unit_to = selectedUnits$energy_unit)
#
#     })
#
#     ## 4/4 : misc ----
#     # other regener data
#
#     subset_rgr_misc <- reactive({
#
#       req(input$selected_communes)
#
#       regener_misc |>
#         dplyr::filter(commune %in% input$selected_communes)
#
#     })
#
#
#     # 4. Subsidies inputs ----
#
#     # 1/2
#     subset_subsidies_building <- reactive({
#
#       req(input$selected_communes)
#
#       subsidies_by_building |>
#         dplyr::filter(commune %in% input$selected_communes)
#     })
#
#     subset_subsidies_measure <- reactive({
#       req(input$selected_communes)
#
#       subsidies_by_measure |>
#         dplyr::filter(commune %in% input$selected_communes)
#     })
#
#     # 5. NG distribution inputs ----
#
#     subset_ng_cons <- reactive({
#
#       req(input$selected_communes)
#       req(selectedUnits$energy_unit)
#
#       ng_cons |>
#         dplyr::filter(commune %in% input$selected_communes) |>
#         convert_units(colnames = "consommation",
#                       unit_from = "kWh", # initial value in dataset
#                       unit_to = selectedUnits$energy_unit)
#
#     })
#
#
#
#     # 10. tabAdaptation ----
#
#     subset_generic_test_data <- reactive({
#       req(input$selected_communes)
#
#       generic_data |>
#         dplyr::filter(commune %in% input$selected_communes)
#
#     })
#
#
#
#     # Uploaded communes ----
#     ## '_timed' because we have a timestamp as the first element to force reactivity
#     ##    when reuploading the same file (see mod_upload_communes.R)
#
#     uploaded_communes_timed <- mod_upload_communes_server("uploaded_communes")
#
#     # Initiate & populate inputVals : ----
#     # Initializing the inputVals items
#
#     inputVals <- reactiveValues()
#
# # inputVals communes & unit ----
#
#     # communes
#     observe({
#       inputVals$selectedCommunes <- input$selected_communes
#     })
#
#     # uploaded communes
#     ## we keep the timestamp to maintain reactivity updates and avoid unwanted lazy eval
#
#     observe({
#       inputVals$uploadedCommunesTimed <- uploaded_communes_timed()
#       })
#
#     # units selected
#     observe({
#       inputVals$energyUnit <- selectedUnits$energy_unit
#     })
#     observe({
#       inputVals$co2Unit <- selectedUnits$co2_unit
#     })
#
#
# # inputVals datasets & min/max/year values ----
#     # Other inputs not influenced by selectInputs() else than commune
#
#     ## datasets ----
#     observe({
#
#       # store the commune cons dataset already filtered
#     inputVals$elec_cons_dataset <- subset_elec_cons()
#
#       # store the commune prod dataset already filtered
#     inputVals$elec_prod_dataset <- subset_elec_prod()
#
#       # store the regener commune dataset already filtered
#
#       inputVals$rgr_1 <- subset_rgr_cons_1()
#       inputVals$rgr_2 <- subset_rgr_cons_2()
#
#       inputVals$rgr_needs <- subset_rgr_needs()
#       inputVals$rgr_misc <- subset_rgr_misc()
#
#       # store subsidies dataset already filtered
#
#       inputVals$subsidies_building <- subset_subsidies_building()
#       inputVals$subsidies_measure <- subset_subsidies_measure()
#
#       inputVals$ng_cons_dataset <- subset_ng_cons()
#
#       # store generic data already filtered
#
#       inputVals$generic_test_data <- subset_generic_test_data()
#
#
#     ## min/max/year available values ----
#
#       inputVals$min_avail_elec_cons <- min(subset_elec_cons()$annee)
#       inputVals$max_avail_elec_cons <- max(subset_elec_cons()$annee)
#
#       # store min & max !available! years to feed sliderInput()
#       inputVals$min_avail_elec_prod <- min(subset_elec_prod()$annee)
#       inputVals$max_avail_elec_prod <- max(subset_elec_prod()$annee)
#
#       # store list of !available! techs to feed pickerInput()
#       inputVals$techs_avail <- subset_elec_prod() |>
#         dplyr::distinct(categorie) |>
#         dplyr::pull()
#
#       # store min & max years from regener dataset (this does not need to by commune specific)
#       inputVals$min_regener_year <- min_regener_year # utils_helpers.R
#       inputVals$max_regener_year <- max_regener_year # utils_helpers.R
#
#       # store min & max years from ng_cons dataset
#       # since missing vars are possible for a selection, set a fallback value outside the subset selected
#       # remove annoying warnings saying that Inf/+Inf is returned for the first min/max term
#       inputVals$min_avail_ng_cons <- min(min(subset_ng_cons()$annee), min(ng_cons$annee)) |> suppressWarnings()
#       inputVals$max_avail_ng_cons <- max(max(subset_ng_cons()$annee), max(ng_cons$annee)) |> suppressWarnings()
#
#
#     })# End observe
#
#     ### Statbox subsets, communes only (!) ----
#     # --> we exclude Cantonal row which value is separated inside a dedicated statbox
#
#     observe({
#
#       req(subset_elec_prod(),
#           subset_elec_cons(),
#           subset_rgr_cons_1(),
#           subset_subsidies_measure()
#           )
#
#       # Statbox value for current selection's aggregated electricity production
#       inputVals$elec_prod_last_year <- subset_elec_prod() |>
#         dplyr::filter(annee == last_year_elec_prod) |>
#         dplyr::filter(!commune == "Canton de Vaud") |> # remove cantonal row
#         dplyr::summarise(production = sum(production, na.rm = T)) |>
#         dplyr::pull(production)
#
#       # Statbox value for current selection's aggregated electricity consumption
#
#       inputVals$elec_cons_last_year <- subset_elec_cons() |>
#         dplyr::filter(annee == last_year_elec_cons) |>
#         dplyr::filter(!commune == "Canton de Vaud")|>
#         dplyr::summarise(consommation = sum(consommation, na.rm = T)) |>
#         dplyr::pull(consommation)
#
#       # Statbox value for current selection's aggregated buildings thermal consumption
#
#       inputVals$max_year_rg_cons <- subset_rgr_cons_1() |>
#         dplyr::filter(etat == last_year_rgr) |>
#         dplyr::filter(!commune == "Canton de Vaud") |>
#         dplyr::summarise(consommation=sum(consommation, na.rm = T)) |>
#         dplyr::pull(consommation)
#
#
#       # Statbox value for current selection's aggregated sum of M01 measures
#
#       inputVals$max_year_subsidies_m01 <- subset_subsidies_measure() |>
#         dplyr::filter(annee == last_year_subsidies) |>
#         dplyr::filter(!commune == "Canton de Vaud") |>
#         dplyr::filter(mesure == "M01") |>
#         dplyr::summarise(nombre = sum(nombre, na.rm = TRUE)) |>
#         dplyr::pull(nombre)
#
#     })# End observe
#
#
#     ## Render dynamic UI for renderUIs ----
#
#     ### tabCons dynamic select ----
#
#      output$elec_cons_year <- shiny::renderUI({
#
#        req(input$selected_communes)
#
#        shiny::tagList(
#
#          shiny::sliderInput(ns("elec_cons_year"),
#                             label = "Choix des années",
#                             min = inputVals$min_avail_elec_cons,
#                             max = inputVals$max_avail_elec_cons,
#                             value = c(inputVals$min_avail_elec_cons,
#                                       inputVals$max_avail_elec_cons),
#                             step = 1L, sep = "", ticks = T, dragRange = T
#                             )
#
#          )# End tagList
#      })# End renderUi
#
#     ### tabRegener dynamic select ----
#
#     output$regener_year_selector <- shiny::renderUI({
#
#       req(input$selected_communes)
#
#                shiny::selectInput(ns("regener_year"),
#                                   label = "Année (graphique)",
#                                   choices = c(inputVals$max_regener_year:inputVals$min_regener_year),
#                                   selected = inputVals$max_regener_year,
#                                   multiple = FALSE)
#
#     })
#
#
#     ### tabProd dynamic select ----
#
#     output$prod_year_n_techs <- shiny::renderUI({
#
#         req(input$selected_communes)
#         req(selectedUnits$energy_unit)
#
#       shiny::tagList(
#
#         shiny::sliderInput(ns("elec_prod_year"),
#                            label = "Choix des années",
#                            min = inputVals$min_avail_elec_prod,
#                            max = inputVals$max_avail_elec_prod,
#                            value = c(inputVals$min_avail_elec_prod,
#                                      inputVals$max_avail_elec_prod),
#                            step = 1L, sep = "", ticks = TRUE, dragRange = TRUE),
#
#
#         # For SELECTABLE technologies : these checkboxes are linked to other server parts
#         tags$div(class = "fs-sidebar",
#         shinyWidgets::prettyCheckboxGroup(inputId = ns("prod_techs"),
#                                           label = "Choix des technologies",
#                                           choices = inputVals$techs_avail,
#                                           selected = inputVals$techs_avail,
#                                           inline = FALSE,
#                                           bigger = FALSE,
#                                           shape = "round",
#                                           status =  "default",
#                                           icon = icon("check"),
#                                           animation = "jelly"),
#         # For NON-SELECTABLE technologies : a title with a unordered list (see custom.css classes)
#         tags$p(#class = "sidebar_na_title",
#                "Non représentées :"),
#         tags$ul(class = "sidebar_na_list",
#                 setdiff(categories_diren, inputVals$techs_avail) |> # unavailable techs
#                   purrr::map(tags$li) # map into list items of ul()
#         )# End tags$ul
#       )# End tags$div
#       )# End tagList
#     }) # End renderUI
#
#     ### ng_cons dynamic select ----
#
#     output$ng_cons_year_selector <- shiny::renderUI({
#
#       req(input$selected_communes)
#
#       shiny::sliderInput(ns("ng_cons_year"),
#                          label = "Choix des années",
#                          min = inputVals$min_avail_ng_cons,
#                          max = inputVals$max_avail_ng_cons,
#                          value = c(inputVals$min_avail_ng_cons,
#                                    inputVals$max_avail_ng_cons),
#                          step = 1L, sep = "", ticks = T, dragRange = T)
#     })
#
# # [inputVals 3/3] -----
#
#     # We eventually complete inputVals with the values from renderUI() above
#     observe({
#
#       # Cons elec selected inputs
#       inputVals$min_selected_elec_cons <- input$elec_cons_year[1] # current min year selected for elec consumption
#       inputVals$max_selected_elec_cons <- input$elec_cons_year[2] # current max year selected for elec consumption
#
#       # Prod elec selected inputs
#       inputVals$min_selected_elec_prod <- input$elec_prod_year[1] # current min year selected for elec production
#       inputVals$max_selected_elec_prod <- input$elec_prod_year[2] # current max year selected for elec production
#       inputVals$techs_selected <- input$prod_techs      # current selected technologies for elec production
#
#       # RegEner selected inputs
#       inputVals$max_selected_regener <- input$regener_year # current (unique) year selected for regener data
#
#       # NG cons selected inputs
#       inputVals$min_selected_ng_cons <- input$ng_cons_year[1] # current min year selected for ng consumption
#       inputVals$max_selected_ng_cons <- input$ng_cons_year[2] # current max year selected for ng consumption
#     })
#
#     # mod_download_all_data ----
#     mod_download_all_data_server("download_all_data", inputVals = inputVals)

    # Returning inputVals ----
    return(inputVals)



  }) # End moduleServer
} # End server




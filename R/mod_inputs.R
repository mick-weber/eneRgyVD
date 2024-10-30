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

      #shiny::uiOutput(ns("elec_cons_year"))
      shiny::sliderInput(ns("elec_cons_year"),
                         label = "Choix des années",
                         min = min(energy_datasets$elec_prod$annee),
                         max = max(energy_datasets$elec_prod$annee),
                         value = c(min(energy_datasets$elec_prod$annee),
                                  max(energy_datasets$elec_prod$annee)),
                         step = 1L, sep = "", ticks = T, dragRange = T
      )



    ), # End conditionalPanel

    # uiOutput() for elec production ----

    shiny::conditionalPanel(
      condition="input.nav == 'Electricité' && input.navset_elec == 'Production d\\\'électricité'", # 2 conditions + triple escape : 2 for R, 1 for JS

      #shiny::uiOutput(ns("prod_year_n_techs"))

      shiny::sliderInput(ns("elec_prod_year"),
                         label = "Choix des années",
                         # reactive choices from current subset
                         min = min(energy_datasets$elec_prod$annee),
                         max = max(energy_datasets$elec_prod$annee),
                         value = c(min(energy_datasets$elec_prod$annee),
                                   max(energy_datasets$elec_prod$annee)),
                         step = 1L, sep = "", ticks = TRUE, dragRange = TRUE)


    ), # End conditionalPanel

    # uiOutput() for regener ----
    shiny::conditionalPanel(
      condition = "input.nav == 'Chaleur des bâtiments' && ['Besoins des bâtiments', 'Consommation des bâtiments'].includes(input.navset_regener)", # 2 conditions !

      #shiny::uiOutput(ns("regener_year_selector"))
      shiny::selectInput(ns("regener_needs_year"),
                         label = "Année (graphique)",
                         # static choices from utils_helpers.R -> no reactivity needed
                         choices = c(min(energy_datasets$regener_needs$etat):max(energy_datasets$regener_needs$etat),
                         selected = max(energy_datasets$regener_needs$etat),
                         multiple = FALSE))

    ), # End conditionalPanel


    # uiOutput() for ng_cons ----
    shiny::conditionalPanel(
      condition = "input.nav == 'Gaz naturel'", # 1 condition !

      #shiny::uiOutput(ns("ng_cons_year_selector"))
      shiny::sliderInput(ns("ng_cons_year"),
                         label = "Choix des années",
                         # reactive choices from current subset
                         min = min(energy_datasets$ng_cons$annee),
                         max = max(energy_datasets$ng_cons$annee),
                         value = c(min(energy_datasets$ng_cons$annee),
                                   max(energy_datasets$ng_cons$annee)),
                         step = 1L, sep = "", ticks = T, dragRange = T)


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


    # Store filtered datasets in inputVals ----
    # Note : we do it here and not in app_server.R so we don't have to pass all inputs in inputVals, retrieve them in app_server.R and filter there...

    ## 1. energyDatasets ----
    observe({

      validate(need(input$selected_communes, req_communes_phrase)) # utils_helpers.R
      req(selectedUnits$energy_unit)
      req(selectedUnits$co2_unit)

      inputVals$energyDatasets <- energy_datasets |>
        # Filter communes and convert units as needed
        purrr::map(\(df){
                      df |>
                        dplyr::filter(commune %in% input$selected_communes) |>
                        convert_units(colnames = c("consommation", "besoins",
                                                   "production", "injection", "autoconsommation",
                                                   "puissance_electrique_installee"),
                                      unit_from = "kWh", # initial value in all dataset
                                      unit_to = selectedUnits$energy_unit) |>
                        convert_units(colnames = c("co2_direct"),
                                      unit_from = "tCO2", # initial value in all dataset
                                      unit_to = selectedUnits$co2_unit
                        )
                    }) |>
        # Filter years selectively with respective (if any) slider/selectInputs
        purrr::map2(names(energy_datasets),
                    \(df, name_df){
                      if(name_df == "elec_prod"){df |> dplyr::filter(dplyr::between(annee, input$elec_prod_year[1], input$elec_prod_year[2]))}else
                        if(name_df == "elec_cons"){df |> dplyr::filter(dplyr::between(annee, input$elec_cons_year[1], input$elec_cons_year[2]))}else
                          if(name_df == "ng_cons"){df |> dplyr::filter(dplyr::between(annee, input$ng_cons_year[1], input$ng_cons_year[2]))}else
                            if(grepl(name_df, pattern = "regener")){df |> dplyr::filter(etat == input$regener_needs_year)}else
                              # else just return the df unfiltered
                              {df}
                    })
    })

    ## 2. mobilityDatasets ----
    observe({

      validate(need(input$selected_communes, req_communes_phrase)) # utils_helpers.R

      inputVals$mobilityDatasets <- mobility_datasets |>
        purrr::map(\(mobility_df){
          mobility_df |>
            dplyr::filter(commune %in% input$selected_communes)
          # no convert_units() call here
          # no input widgets filter here neither
        })
    })

    ## 3. adaptationDatasets----
    observe({

      validate(need(input$selected_communes, req_communes_phrase)) # utils_helpers.R
      req(selectedUnits$co2_unit)

      inputVals$adaptationDatasets <- adaptation_datasets |>
        purrr::map(\(adaptation_df){
          adaptation_df |>
            dplyr::filter(commune %in% input$selected_communes) # |>
          # no convert_units() call here
          # no input widgets filter here neither
        })
    })

     # Uploaded communes ----
     ## '_timed' because we have a timestamp as the first element to force reactivity
     ##    when reuploading the same file (see mod_upload_communes.R)

    uploaded_communes_timed <- mod_upload_communes_server("uploaded_communes")

# inputVals communes & unit ----

    # communes
    observe({
      inputVals$selectedCommunes <- input$selected_communes
    })

    # uploaded communes
    ## we keep the timestamp to maintain reactivity updates and avoid unwanted lazy eval

    observe({
      inputVals$uploadedCommunesTimed <- uploaded_communes_timed()
      })
    # units selected
    observe({
      inputVals$energyUnit <- selectedUnits$energy_unit
    })
    observe({
      inputVals$co2Unit <- selectedUnits$co2_unit
    })
#


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
    ## Render dynamic UI for renderUIs ----

    ### tabCons dynamic select ----

     # output$elec_cons_year <- shiny::renderUI({
     #
     #   req(input$selected_communes)
     #
     #   shiny::tagList(
     #
     #     shiny::sliderInput(ns("elec_cons_year"),
     #                        label = "Choix des années",
     #                        min = min(inputVals$energyDatasets$elec_prod$annee),
     #                        max = max(inputVals$energyDatasets$elec_prod$annee),
     #                        value = c(min(inputVals$energyDatasets$elec_prod$annee),
     #                                  max(inputVals$energyDatasets$elec_prod$annee)),
     #                        step = 1L, sep = "", ticks = T, dragRange = T
     #                        )
     #
     #     )# End tagList
     # })# End renderUi

    ### tabRegener dynamic select ----

    # output$regener_year_selector <- shiny::renderUI({
    #
    #   req(input$selected_communes)
    #
    #            shiny::selectInput(ns("regener_year"),
    #                               label = "Année (graphique)",
    #                               # static choices from utils_helpers.R -> no reactivity needed
    #                               choices = c(min_regener_year:max_regener_year),
    #                               selected = max_regener_year,
    #                               multiple = FALSE)
    #
    # })


    # ### tabProd dynamic select ----
    #
    # output$prod_year_n_techs <- shiny::renderUI({
    #
    #     req(input$selected_communes)
    #     req(selectedUnits$energy_unit)
    #     req(inputVals$energyDatasets)
    #
    #   shiny::tagList(
    #
    #     shiny::sliderInput(ns("elec_prod_year"),
    #                        label = "Choix des années",
    #                        # reactive choices from current subset
    #                        min = min(inputVals$energyDatasets$elec_prod$annee),
    #                        max = max(inputVals$energyDatasets$elec_prod$annee),
    #                        value = c(min(inputVals$energyDatasets$elec_prod$annee),
    #                                  max(inputVals$energyDatasets$elec_prod$annee)),
    #                        step = 1L, sep = "", ticks = TRUE, dragRange = TRUE),
#
#
#         # For SELECTABLE technologies : these checkboxes are linked to other server parts
#         tags$div(class = "fs-sidebar",
#         shinyWidgets::prettyCheckboxGroup(inputId = ns("prod_techs"),
#                                           label = "Choix des technologies",
#                                           # reactive choices from current subset
#                                           choices = unique(inputVals$energyDatasets$elec_prod$categorie),
#                                           selected = unique(inputVals$energyDatasets$elec_prod$categorie),
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
#                 setdiff(categories_diren, unique(inputVals$energyDatasets$elec_prod$categorie)) |> # unavailable techs
#                   purrr::map(tags$li) # map into list items of ul()
#         )# End tags$ul
#       )# End tags$div
#       )# End tagList
#     }) # End renderUI

    ### ng_cons dynamic select ----

#     output$ng_cons_year_selector <- shiny::renderUI({
#
#       req(input$selected_communes)
#
#       shiny::sliderInput(ns("ng_cons_year"),
#                          label = "Choix des années",
#                          # reactive choices from current subset
#                          min = min(inputVals$energyDatasets$ng_cons$annee),
#                          max = max(inputVals$energyDatasets$ng_cons$annee),
#                          value = c(min(inputVals$energyDatasets$ng_cons$annee),
#                                    max(inputVals$energyDatasets$ng_cons$annee)),
#                          step = 1L, sep = "", ticks = T, dragRange = T)
#     })
# #
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


    # mod_download_all_data ----
    mod_download_all_data_server("download_all_data", inputVals = inputVals)

    # Returning inputVals ----
    return(inputVals)



  }) # End moduleServer
} # End server




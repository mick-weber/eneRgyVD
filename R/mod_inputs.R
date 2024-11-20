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
    # make a div so that introjs can target the full item
    tags$div(id = "introjs_select_communes",
             shiny::selectizeInput(inputId = ns("selected_communes"),
                                   label = tags$p("Sélection des communes", style = "font-weight:500;margin-bottom:0px;"),
                                   choices = choices_canton_communes,
                                   selected = NULL,
                                   multiple = TRUE,
                                   options = list(
                                     placeholder = "Plusieurs acceptées",
                                     plugins = list("remove_button")
                                   )
             )
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
               style = "font-weight:500;",
               bslib::tooltip(
                 id = "tooltip_import_communes",
                 placement = "right",
                 options = list(customClass = "customTooltips"), # custom.scss
                 trigger = bsicons::bs_icon("info-circle"),
                 "Cette fonctionnalité permet d'importer un fichier csv avec des numéros OFS de communes pour automatiser une sélection de communes."
                 ),
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
    uiOutput(ns("elec_cons_widget")), # End conditionalPanel

    # uiOutput() for elec production ----
    uiOutput(ns("elec_prod_widget")),

    # uiOutput() for regener ----
    uiOutput(ns("regener_widget")),

    # uiOutput() for ng_cons ----
    uiOutput(ns("ng_cons_widget")),

    # Sidebar bottom ----

    tags$div(style = "margin-top: auto;",

             ##  1. Unit converter widget ----
             shiny::conditionalPanel(
               condition="input.nav != 'Accueil'",
               mod_unit_converter_ui(ns("unit_converter"))
             ),# End conditionalPanel


             ## 1. Downoad all widget ----
             tags$div(
               id = "introjs_download_all",
               mod_download_all_data_ui(ns("download_all_data")),
             )
    )# End div()


  ) # End tagList
} # End UI

#' sideboard_inputs Server Functions
#'
#' @noRd
mod_inputs_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # # # TEST PRINT #
    # observe({
    #   print(input$selected_communes)
    #   print(inputVals$selectedCommunes)
    # })
    # # /TEST PRINT

    # 0. Retrieve units ----
    selectedUnits <- mod_unit_converter_server("unit_converter")

    # Uploaded communes ----
    ## '_timed' because we have a timestamp as the first element to force reactivity
    ##    when reuploading the same file (see mod_upload_communes.R)

    uploaded_communes_timed <- mod_upload_communes_server("uploaded_communes")



    # Render UI sidebar widgets ----
    # They must be rendered dynamically (and not in UI directly) so that we can pass req(input$selected_communes)
    # Additional display conditions are stored in conditionalPanel()

    output$elec_cons_widget <- renderUI({

      req(input$selected_communes)

      shiny::conditionalPanel(
        condition="input.nav == 'Electricité' && input.navset_elec == 'Distribution d\\\'électricité'", # 2 conditions + triple escape : 2 for R, 1 for JS

        shiny::sliderInput(ns("elec_cons_year"),
                           label = "Choix des années",
                           min = min(energy_datasets$elec_cons$annee),
                           max = max(energy_datasets$elec_cons$annee),
                           value = c(min(energy_datasets$elec_cons$annee),
                                     max(energy_datasets$elec_cons$annee)),
                           step = 1L, sep = "", ticks = T, dragRange = T
        )
      )
    })


    output$elec_prod_widget <- renderUI({

      req(input$selected_communes)

      shiny::conditionalPanel(
        condition="input.nav == 'Electricité' && input.navset_elec == 'Production d\\\'électricité'", # 2 conditions + triple escape : 2 for R, 1 for JS

        shiny::sliderInput(ns("elec_prod_year"),
                           label = "Choix des années",
                           # reactive choices from current subset
                           min = min(energy_datasets$elec_prod$annee),
                           max = max(energy_datasets$elec_prod$annee),
                           value = c(min(energy_datasets$elec_prod$annee),
                                     max(energy_datasets$elec_prod$annee)),
                           step = 1L, sep = "", ticks = TRUE, dragRange = TRUE)


      ) # End conditionalPanel

    })


    output$regener_widget <- renderUI({

      req(input$selected_communes)

      shiny::conditionalPanel(
        condition = "input.nav == 'Chaleur des bâtiments' && ['Besoins des bâtiments', 'Consommation des bâtiments'].includes(input.navset_regener)", # 2 conditions !

        #shiny::uiOutput(ns("regener_year_selector"))
        shiny::selectInput(ns("regener_needs_year"),
                           label = "Année (graphique)",
                           # static choices from utils_helpers.R -> no reactivity needed
                           choices = c(min(energy_datasets$regener_needs$etat):max(energy_datasets$regener_needs$etat)),
                           selected = max(energy_datasets$regener_needs$etat),
                           multiple = FALSE)

      ) # End conditionalPanel

    })


    output$ng_cons_widget <- renderUI({

      req(input$selected_communes)

      shiny::conditionalPanel(
        condition = "input.nav == 'Gaz naturel'", # 1 condition !

        shiny::sliderInput(ns("ng_cons_year"),
                           label = "Choix des années",
                           # reactive choices from current subset
                           min = min(energy_datasets$ng_cons$annee),
                           max = max(energy_datasets$ng_cons$annee),
                           value = c(min(energy_datasets$ng_cons$annee),
                                     max(energy_datasets$ng_cons$annee)),
                           step = 1L, sep = "", ticks = T, dragRange = T)


      ) # End conditionalPanel

    })


    # Initialize inputVals ----
    inputVals <- reactiveValues(

      uploadedCommunesTimed = NULL,
      energyUnit = NULL,
      co2Unit = NULL,
      max_selected_regener = NULL,

      # nested reactiveValues to avoid triggering everything when one dataset changes (see 'stratégie du petit r' from ThinkR)
      energyDatasets = reactiveValues(),
      mobilityDatasets = reactiveValues(),
      adaptationDatasets = reactiveValues()
    )

    ## Store communes & unit ----
    # Create a reactive value for the debounced communes
    debouncedCommunes <- debounce(reactive(input$selected_communes), millis = 500)

    # communes

    observe({inputVals$selectedCommunesDirect <- input$selected_communes})
    observe({inputVals$selectedCommunes <- debouncedCommunes()})

    # uploaded communes (we keep the timestamp to maintain reactivity updates and avoid unwanted lazy eval)
    observe({inputVals$uploadedCommunesTimed <- uploaded_communes_timed()})
    # units selected
    observe({inputVals$energyUnit <- selectedUnits$energy_unit})
    observe({inputVals$co2Unit <- selectedUnits$co2_unit})


    ## Store filtered datasets in inputVals ----
    # Note : we do it here and not in app_server.R so we don't have to pass all inputs in inputVals, retrieve them in app_server.R and filter there...

    ## 1. energyDatasets ----

    # Special treatment for regener : we want to store the slider value so it's available only for the plot code (not made for multiple years)
    observe({
      inputVals$max_selected_regener <- input$regener_needs_year
    })


    observe({

      # req all inputs needed to filter & convert the datasets
      req(debouncedCommunes())#req(input$selected_communes)
      req(selectedUnits$energy_unit)
      req(selectedUnits$co2_unit)

      req(input$elec_prod_year)
      req(input$elec_cons_year)
      req(input$ng_cons_year)

      inputVals$energyDatasets <- energy_datasets |>
        # Filter communes and convert units as needed
        purrr::map(\(df){
          df |>
            dplyr::filter(commune %in% debouncedCommunes()) |>
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
                            #if(grepl(name_df, pattern = "regener")){df |> dplyr::filter(etat == input$regener_needs_year)}else
                            # else (regener, others dfs, just return the df unfiltered
                          {df}
                    })
    })

    ## 2. mobilityDatasets ----
    observe({

      # req all inputs needed to filter & convert the datasets
      req(debouncedCommunes())

      inputVals$mobilityDatasets <- mobility_datasets |>
        purrr::map(\(mobility_df){
          mobility_df |>
            dplyr::filter(commune %in% debouncedCommunes())
          # no convert_units() call here
          # no input widgets filter here neither
        })
    })

    ## 3. adaptationDatasets----
    observe({

      # req all inputs needed to filter & convert the datasets
      req(debouncedCommunes())
      req(selectedUnits$co2_unit)

      inputVals$adaptationDatasets <- adaptation_datasets |>
        purrr::map(\(adaptation_df){
          adaptation_df |>
            dplyr::filter(commune %in% debouncedCommunes()) # |>
          # no convert_units() call here
          # no input widgets filter here neither
        })
    })

    ### Statbox subsets, communes only (!) ----
    # --> we exclude Cantonal row which value is separated inside a dedicated statbox

    observe({

      req(inputVals$energyDatasets$elec_prod,
          inputVals$energyDatasets$elec_cons,
          inputVals$energyDatasets$elec_prod,
          inputVals$energyDatasets$regener_cons_ae_use,
          inputVals$energyDatasets$subsidies_by_measure
      )

      # Statbox value for current selection's aggregated electricity production
      inputVals$elec_prod_last_year <- inputVals$energyDatasets$elec_prod |>
        dplyr::filter(annee == last_year_elec_prod) |>
        dplyr::filter(!commune == "Canton de Vaud") |> # remove cantonal row
        dplyr::summarise(production = sum(production, na.rm = T)) |>
        dplyr::pull(production)

      # Statbox value for current selection's aggregated electricity consumption
      inputVals$elec_cons_last_year <- inputVals$energyDatasets$elec_cons |>
        dplyr::filter(annee == last_year_elec_cons) |>
        dplyr::filter(!commune == "Canton de Vaud")|>
        dplyr::summarise(consommation = sum(consommation, na.rm = T)) |>
        dplyr::pull(consommation)

      # Statbox value for current selection's aggregated buildings thermal consumption
      inputVals$max_year_rg_cons <- inputVals$energyDatasets$regener_cons_ae_use |>
        dplyr::filter(etat == last_year_rgr) |>
        dplyr::filter(!commune == "Canton de Vaud") |>
        dplyr::summarise(consommation=sum(consommation, na.rm = T)) |>
        dplyr::pull(consommation)

      # Statbox value for current selection's aggregated sum of M01 measures
      inputVals$max_year_subsidies_m01 <- inputVals$energyDatasets$subsidies_by_measure |>
        dplyr::filter(annee == last_year_subsidies) |>
        dplyr::filter(!commune == "Canton de Vaud") |>
        dplyr::filter(mesure == "M01") |>
        dplyr::summarise(nombre = sum(nombre, na.rm = TRUE)) |>
        dplyr::pull(nombre)

      # Statbox value for current selection's aggregated NG consumption
      inputVals$ng_cons_last_year <- inputVals$energyDatasets$ng_cons |>
        dplyr::filter(annee == last_year_ng_cons) |>
        dplyr::filter(!commune == "Canton de Vaud")|>
        dplyr::summarise(consommation = sum(consommation, na.rm = T)) |>
        dplyr::pull(consommation)

    })# End observe

    # mod_download_all_data ----
    #mod_download_all_data_server("download_all_data", inputVals = inputVals)

    # Returning inputVals ----
    return(inputVals)

  }) # End moduleServer
} # End server




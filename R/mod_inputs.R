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
                                     plugins = list("remove_button",
                                                    "clear_button")
                                   )
             )
    ),

    ## |---------------------------------------------------------------|
    ##          This section is source of many sorrows !!
    ## |---------------------------------------------------------------|
    # we must remember to update these conditionalPanel conditions when we change the names of each nav_panel() and navset_card_pill() !!
    # otherwise the plots etc. fail to render because they can't access the required conditional input values ! (selected year, etc.)

    # Energy
    uiOutput(ns("elec_cons_years_picker")),
    uiOutput(ns("elec_prod_years_picker")),
    uiOutput(ns("ng_cons_years_picker")),
    uiOutput(ns("subsidies_years_picker")),

    # Mobility
    uiOutput(ns("part_voit_elec_years_picker")),
    uiOutput(ns("qualite_desserte_years_picker")),
    uiOutput(ns("taux_motorisation_years_picker")),

    # Climate


    # uiOutput() for regener ----
    uiOutput(ns("regener_widget")),


    ## |---------------------------------------------------------------|
    ##          Sidebar bottom footer
    ## |---------------------------------------------------------------|

    # Div with bottom elements ----
    tags$div(style = "margin-top: auto;",

             ## Upload communes widget (conditional) ----
             shiny::conditionalPanel(
               condition="input.nav == 'Accueil'",
               mod_upload_communes_ui(ns("uploaded_communes"))
             ),

             ##  Unit converter widget (conditional) ----
             shiny::conditionalPanel(
               condition="input.nav != 'Accueil'",
               mod_unit_converter_ui(ns("unit_converter"))
             ),# End conditionalPanel

             ##  Downoad all widget (fixed) ----
             hr(), # separator
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


    # Energy

    output$elec_cons_years_picker <- renderUI({

      req(input$selected_communes)

      shiny::conditionalPanel(style = "display: none;", # avoid flickering upon init
                              condition = "input.nav == 'Electricité' && input.navset_elec == 'Distribution d\\\'électricité'",
                              shinyWidgets::airYearpickerInput(
                                inputId = ns("elec_cons_years"),
                                label = "Sélection des années",
                                separator = " à ",
                                range = TRUE,
                                width = "90%",
                                addon = "right",
                                addonAttributes = list(class = "btn disabled"),
                                value = elec_cons_years,
                                highlightedDates = elec_cons_years,
                                update_on = "change",
                                minDate = elec_cons_years[1],
                                maxDate = elec_cons_years[2],
                                clearButton = TRUE,
                                language = "fr",
                                placeholder = "Filtrer les années"
                              )
      )
    })



    output$elec_prod_years_picker <- renderUI({

      req(input$selected_communes)

      shiny::conditionalPanel(style = "display: none;", # avoid flickering upon init
                              condition = "input.nav == 'Electricité' && input.navset_elec == 'Production d\\\'électricité'",
                              shinyWidgets::airYearpickerInput(
                                inputId = ns("elec_prod_years"),
                                label = "Sélection des années",
                                separator = " à ",
                                range = TRUE,
                                width = "90%",
                                addon = "right",
                                addonAttributes = list(class = "btn disabled"),
                                value = elec_prod_years,
                                minDate = elec_prod_years[1],
                                maxDate = elec_prod_years[2],
                                clearButton = TRUE,
                                language = "fr",
                                placeholder = "Filtrer les années"
                              )
      )
    })

    output$ng_cons_years_picker <- renderUI({

      req(input$selected_communes)

      shiny::conditionalPanel(style = "display: none;", # avoid flickering upon init
                              condition = "input.nav == 'Gaz naturel'",
                              shinyWidgets::airYearpickerInput(
                                inputId = ns("ng_cons_years"),
                                label = "Sélection des années",
                                separator = " à ",
                                range = TRUE,
                                width = "90%",
                                addon = "right",
                                addonAttributes = list(class = "btn disabled"),
                                value = ng_cons_years,
                                minDate = ng_cons_years[1],
                                maxDate = ng_cons_years[2],
                                clearButton = TRUE,
                                language = "fr",
                                placeholder = "Filtrer les années"
                              )
      )
    })

    output$subsidies_years_picker <- renderUI({

      req(input$selected_communes)

      shiny::conditionalPanel(style = "display: none;", # avoid flickering upon init
                              condition = "input.nav == 'Subventions bâtiments'",
                              shinyWidgets::airYearpickerInput(
                                inputId = ns("subsidies_years"),
                                label = "Sélection des années",
                                separator = " à ",
                                range = TRUE,
                                width = "90%",
                                addon = "right",
                                addonAttributes = list(class = "btn disabled"),
                                value = subsidies_years,
                                minDate = subsidies_years[1],
                                maxDate = subsidies_years[2],
                                clearButton = TRUE,
                                language = "fr",
                                placeholder = "Filtrer les années"
                              )
      )
    })

    # Mobility

    output$part_voit_elec_years_picker <- renderUI({

      req(input$selected_communes)

      shiny::conditionalPanel(style = "display: none;", # avoid flickering upon init
        condition = "input.nav == 'Véhicules électriques'",
        shinyWidgets::airYearpickerInput(
          inputId = ns("part_voit_elec_years"),
          label = "Sélection des années",
          separator = " à ",
          range = TRUE,
          width = "90%",
          addon = "right",
          addonAttributes = list(class = "btn disabled"),
          value = part_voit_elec_years,
          minDate = part_voit_elec_years[1],
          maxDate = part_voit_elec_years[2],
          clearButton = TRUE,
          language = "fr",
          placeholder = "Filtrer les années"
        )
      )
    })

    output$qualite_desserte_years_picker <- renderUI({

      req(input$selected_communes)

      shiny::conditionalPanel(style = "display: none;", # avoid flickering upon init
        condition = "input.nav == 'Transports publics'",
        shinyWidgets::airYearpickerInput(
          inputId = ns("qualite_desserte_years"),
          label = "Sélection des années",
          separator = " à ",
          range = TRUE,
          width = "90%",
          addon = "right",
          addonAttributes = list(class = "btn disabled"),
          value = qualite_desserte_years,
          minDate = qualite_desserte_years[1],
          maxDate = qualite_desserte_years[2],
          clearButton = TRUE,
          language = "fr",
          placeholder = "Filtrer les années"
        )
      )
    })

    output$taux_motorisation_years_picker <- renderUI({

      req(input$selected_communes)

      shiny::conditionalPanel(style = "display: none;", # avoid flickering upon init
        condition = "input.nav == 'Taux de motorisation'",
        shinyWidgets::airYearpickerInput(
          inputId = ns("taux_motorisation_years"),
          label = "Sélection des années",
          separator = " à ",
          range = TRUE,
          width = "90%",
          addon = "right",
          addonAttributes = list(class = "btn disabled"),
          value = taux_motorisation_years,
          minDate = taux_motorisation_years[1],
          maxDate = taux_motorisation_years[2],
          clearButton = TRUE,
          language = "fr",
          placeholder = "Filtrer les années"
        )
      )
    })

    output$regener_widget <- renderUI({

      req(input$selected_communes)

      shiny::conditionalPanel( #style = "display: none;", # avoid flickering upon init
        condition = "input.nav == 'Chaleur des bâtiments' && ['Besoins des bâtiments', 'Consommation des bâtiments'].includes(input.navset_regener)", # 2 conditions !

        #shiny::uiOutput(ns("regener_year_selector"))
        shiny::selectInput(ns("regener_needs_year"),
                           label = tags$p("Année (graphique)", style = "font-weight:500;margin-bottom:0px;"),
                           # static choices from utils_helpers.R -> no reactivity needed
                           choices = c(min_regener_year:max_regener_year),
                           selected = max_regener_year,
                           multiple = FALSE)

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
    debouncedCommunes <- debounce(reactive(input$selected_communes), millis = 300)

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
      req(debouncedCommunes())
      req(selectedUnits$energy_unit)
      req(selectedUnits$co2_unit)

      req(length(input$elec_prod_years)>1)
      req(length(input$elec_cons_years)>1)
      req(length(input$ng_cons_years)>1)
      req(length(input$subsidies_years)>1)


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
                      if(name_df == "elec_prod"){df |> dplyr::filter(dplyr::between(
                        annee,
                        lubridate::year(input$elec_prod_years[1]),
                        lubridate::year(input$elec_prod_years[2])))}else
                        if(name_df == "elec_cons"){df |> dplyr::filter(dplyr::between(
                          annee,
                          lubridate::year(input$elec_cons_years[1]),
                          lubridate::year(input$elec_cons_years[2])))}else
                          if(name_df == "ng_cons"){df |> dplyr::filter(dplyr::between(
                            annee,
                            lubridate::year(input$ng_cons_years[1]),
                            lubridate::year(input$ng_cons_years[2])))}else
                              if(stringr::str_detect(name_df, pattern = "subsidies")){df |> dplyr::filter(dplyr::between(
                                .data[[ifelse("etat" %in% names(df), "etat", "annee")]],
                                lubridate::year(input$subsidies_years[1]),
                                lubridate::year(input$subsidies_years[2])))}else
                          {df}
                    })
    })

    ## 2. mobilityDatasets ----
    observe({

      # req all inputs needed to filter & convert the datasets
      req(debouncedCommunes())
      # req mobility_dataset
      req(input$part_voit_elec_years)
      req(input$taux_motorisation_years)
      req(input$qualite_desserte_years)

      inputVals$mobilityDatasets <- mobility_datasets |>
        purrr::map(\(mobility_df){
          mobility_df |>
            dplyr::filter(commune %in% debouncedCommunes())
          # no convert_units() call here
          # no input widgets filter here neither
        }) |> purrr::map2(names(mobility_datasets),
                          \(df, name_df){

                            if(name_df == "part_voit_elec"){df |> dplyr::filter(dplyr::between(annee,
                                                                                               lubridate::year(input$part_voit_elec_years[1]),
                                                                                               lubridate::year(input$part_voit_elec_years[2])))}else
                              if(name_df == "taux_motorisation"){df |> dplyr::filter(dplyr::between(annee,
                                                                                                    lubridate::year(input$taux_motorisation_years[1]),
                                                                                                    lubridate::year(input$taux_motorisation_years[2])))}else
                                if(name_df == "qualite_desserte"){df |> dplyr::filter(dplyr::between(annee,
                                                                                                     lubridate::year(input$qualite_desserte_years[1]),
                                                                                                     lubridate::year(input$qualite_desserte_years[2])))}else
                                {df}

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

    # mod_download_all_data ----
    mod_download_all_data_server("download_all_data", inputVals = inputVals)

    # Returning inputVals ----
    return(inputVals)

  }) # End moduleServer
} # End server




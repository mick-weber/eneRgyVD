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
                   label = "Sélection par commune(s)",
                   choices = communes_names,
                   selected = NULL,
                   multiple = TRUE,
                   options = list(placeholder = "Plusieurs communes acceptées")),

    # selectizeInput() for district zoom ----
    # IF tabMap : Select input for zooming on the districts (WIP feature)
    shiny::conditionalPanel(
       condition="input.sidebarMenu == 'tabMap'",
    shiny::selectizeInput(inputId = ns("selected_district"),
                   label = "Zoom sur un district",
                   choices = districts_names,
                   selected = 0,
                   multiple = FALSE)),

    # uiOutput for tabCons ----

    shiny::conditionalPanel(
      condition="input.sidebarMenu == 'tabCons'",

      shiny::uiOutput(ns("cons_year"))

    ), # End conditionalPanel

    # uiOutput() for tabProd ----
    # IF tabProd : 2 widgets in a single uiOutput call for years and technologies
    # --> uiOutput/renderUI because its parameters are reactive

    shiny::conditionalPanel(
      condition="input.sidebarMenu == 'tabProd'",

      shiny::uiOutput(ns("prod_year_n_techs"))

    ) # End conditionalPanel
  ) # End tagList
} # End UI

#' sideboard_inputs Server Functions
#'
#' @noRd
mod_inputs_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # tabCons inputs ----

    subset_elec_cons <- reactive({

      req(input$selected_communes)

      elec_cons_communes %>%
        filter(commune %in% input$selected_communes)

    })

    # tabProd inputs ----

    ## Reactive subset data  ----
    # This is only used to feed the dynamic UI (year/techs available)
    # The 'usable' dataset for plots etc. is computed in app_server.R and is further filtered by
    # the values of the year sliderInput() and techs pickerInput()

    subset_elec_prod <- reactive({

      req(input$selected_communes)

      elec_prod_communes %>%
        filter(commune %in% input$selected_communes)

    })

    ## Storing all useful values in inputVals ----
    # [inputVals 0/3] Initializing the inputVals item

    inputVals <- reactiveValues()

    # [inputVals 1/3] Communes and district
    # These are updated separately first for the district zoom feature to be fully operational.

    observe({

      inputVals$selectedCommunes <- input$selected_communes
      inputVals$selectedDistrict <- input$selected_district
    })

    # [inputVals 2/3] Other inputs not influenced by selectInputs() else than commune

    observe({
      # store the commune cons dataset already filtered
      inputVals$cons_dataset <- subset_elec_cons()

      # store the commune prod dataset already filtered
      inputVals$prod_dataset <- subset_elec_prod()

      # store min & max !available! years from consumption data to feed sliderInput()

      inputVals$min_avail_cons <- min(subset_elec_cons()$annee)
      inputVals$max_avail_cons <- max(subset_elec_cons()$annee)

      # store min & max !available! years to feed sliderInput()
      # CHANGE PARAMS WITH PROD LATER  ($min_avail_prod and max_avail_prod for consistency with cons)
      inputVals$min_avail_prod <- min(subset_elec_prod()$annee)
      inputVals$max_avail_prod <- max(subset_elec_prod()$annee)

      # store list of !available! techs to feed pickerInput()
      inputVals$techs_avail <- subset_elec_prod() %>%
        dplyr::distinct(categorie_diren) %>%
        dplyr::pull()

    }) # End observe

    # inputVals 4/3 TESTING ; WHEN WORKING ADD THIS IN APPROPRIATE PLACE
    # store the elec consumption and production values of the current selection
    #   for the 'last_common_elec_year' year defined in utils_helpers.R

    observe({

      req(subset_elec_prod(), subset_elec_cons())

      inputVals$common_year_elec_prod <- subset_elec_prod() %>%
        dplyr::filter(annee == last_common_elec_year) %>%
        dplyr::summarise(production_totale = sum(production_totale, na.rm = T)) %>%
        dplyr::pull(production_totale) # kWh

      inputVals$common_year_elec_cons <- subset_elec_cons() %>%
        dplyr::filter(annee == last_common_elec_year) %>%
        dplyr::summarise(consommation_kwh = sum(consommation_kwh, na.rm = T)) %>%
        dplyr::pull(consommation_kwh) # kWh

    })


    ## Render dynamic UI for renderUIs ----

    # renderUI for when tabCons is selected

    output$cons_year <- shiny::renderUI({

      req(input$selected_communes)

      shiny::tagList(
        shiny::sliderInput(ns("cons_year"), label = "Choix des années",
                           min = inputVals$min_avail_cons,
                           max = inputVals$max_avail_cons,
                           value = c(inputVals$min_avail_cons, inputVals$max_avail_cons),
                           step = 1L, sep = "", ticks = T))
    })


    # renderUI for when tabProd is selected
    output$prod_year_n_techs <- shiny::renderUI({

        req(input$selected_communes)

      shiny::tagList(
        shiny::sliderInput(ns("prod_year"), label = "Choix des années",
                    min = inputVals$min_avail_prod,
                    max = inputVals$max_avail_prod,
                    value = c(inputVals$min_avail_prod, inputVals$max_avail_prod),
                    step = 1L, sep = "", ticks = T),

        shinyWidgets::pickerInput(ns("prod_techs"), label = "Choix des technologies",
                                  choices = inputVals$techs_avail,
                                  selected = inputVals$techs_avail,
                                  multiple = T,
                                  options=shinyWidgets::pickerOptions(
                                    title = "Technologies disponibles",
                                    actionsBox = TRUE,
                                    deselectAllText = "Tout déselectionner",
                                    selectAllText = "Tout sélectionner",
                                    noneSelectedText = "Aucune sélection"),
                                  choicesOpt = list(
                                    # we apply css iteratively on each element using length(techs)
                                    # https://stackoverflow.com/questions/54081254/pickerinput-font-or-background-color
                                    style = rep(("color: black; background: white;"),
                                                length(inputVals$techs_avail)))
                                  ) # End pickerInput()
      ) # End tagList
    }) # End renderUI


    # [inputVals 3/3]
    # We eventually complete inputVals with the values from renderUI() above
    observe({

      inputVals$min_selected_cons <- input$cons_year[1] # current min year selected for elec consumption
      inputVals$max_selected_cons <- input$cons_year[2] # current max year selected for elec consumption

      inputVals$min_selected_prod <- input$prod_year[1] # current min year selected for elec production
      inputVals$max_selected_prod <- input$prod_year[2] # current max year selected for elec production
      inputVals$techs_selected <- input$prod_techs      # current selected technologies for elec production

    })


    # Returning all the input values ----

    return(inputVals)


  }) # End moduleServer
} # End server




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
                          choices = choices_canton_communes,
                          selected = NULL,
                          multiple = TRUE,
                          options = list(placeholder = "Plusieurs acceptées")
    ),

    # fileInput() when tabMap selected ----
    shiny::conditionalPanel(
      condition="input.sidebarMenu == 'tabMap'",

    mod_upload_communes_ui(ns("uploaded_communes"))

    ),

    # selectizeInput() for district zoom ----
    # IF tabMap : Select input for zooming on the districts (WIP feature)
    shiny::conditionalPanel(
      condition="input.sidebarMenu == 'tabMap'",
      shiny::selectizeInput(inputId = ns("selected_district"),
                            label = "Zoom sur un district",
                            choices = districts_names,
                            selected = 0,
                            multiple = FALSE)
    ),

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

    ), # End conditionalPanel

    # uiOutput() for tabRegener ----
    # Use js array for all tabs because we can't target the overarching 'tabRegener' it does not work
    shiny::conditionalPanel(
      condition="['tabRegenerNeeds', 'tabRegenerCons', 'tabRegenerMisc'].includes(input.sidebarMenu)",
      shiny::uiOutput(ns("regener_year_selector"))

    ) # End conditionalPanel
  ) # End tagList
} # End UI

#' sideboard_inputs Server Functions
#'
#' @noRd
mod_inputs_server <- function(id,
                              selectedUnit # app_server.R <- mod_unit_converter.R
                              ){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Important note ----
    ## Unit conversion is made here, at the root of inputVals, for each dataset

    # 1. tabCons inputs ----

    # !! CONS_ELEC removed !! # subset_elec_cons <- reactive({
    # !! CONS_ELEC removed !! #
    # !! CONS_ELEC removed !! #   req(input$selected_communes)
    # !! CONS_ELEC removed !! #
    # !! CONS_ELEC removed !! #   elec_cons_communes %>%
    # !! CONS_ELEC removed !! #     filter(commune %in% input$selected_communes) |>
    # !! CONS_ELEC removed !! #     convert_units(colnames = "consommation",
    # !! CONS_ELEC removed !! #                   unit_from = "kWh",
    # !! CONS_ELEC removed !! #                   unit_to = selectedUnit$unit_to)
    # !! CONS_ELEC removed !! #
    # !! CONS_ELEC removed !! # })


    # 2. tabProd inputs ----

    ## Reactive subset data  ----
    # This is only used to feed the dynamic UI (year/techs available)
    # The 'usable' dataset for plots etc. is computed in app_server.R and is further filtered by
    # the values of the year sliderInput() and techs pickerInput()

    subset_elec_prod <- reactive({

      req(input$selected_communes)

      elec_prod %>%
        filter(commune %in% input$selected_communes) |>
        convert_units(colnames = contains(c("injection", "production", "autoconso", "puissance")),
                      unit_from = "kWh",
                      unit_to = selectedUnit$unit_to)
    })


    # 3. tabRegener inputs ----
    # Subset aggregated regener data with the currently selected commune(s)

    ## 1/4: cons/use ----
    # regener by commune, consumption, ae, use
    # I may rename these later for more explicit names...
    subset_rgr_cons_1 <- reactive({

      req(input$selected_communes)

      regener_cons_ae_use %>%
        filter(commune %in% input$selected_communes) %>%
        convert_units(colnames = "consommation",
                      unit_from = "kWh",
                      unit_to = selectedUnit$unit_to)

    })

    ## 2/4: cons/aff ----
    # regener by commune, consumption, ae, aff
    # I may rename these later for more explicit names...
    subset_rgr_cons_2 <- reactive({

      req(input$selected_communes)

      regener_cons_ae_aff %>%
        filter(commune %in% input$selected_communes) %>%
        convert_units(colnames = "consommation",
                      unit_from = "kWh",
                      unit_to = selectedUnit$unit_to)

    })

    ## 3/4 : needs ----
    # regener by commune, needs

    subset_rgr_needs <- reactive({

      req(input$selected_communes)

      regener_needs %>%
        dplyr::filter(commune %in% input$selected_communes) %>%
        convert_units(colnames = contains("besoins"),
                      unit_from = "kWh",
                      unit_to = selectedUnit$unit_to)

    })

    ## 4/4 : misc ----
    # other regener data

    subset_rgr_misc <- reactive({

      req(input$selected_communes)

      regener_misc %>%
        dplyr::filter(commune %in% input$selected_communes)

    })


    # 4. tabSubsidies inputs ----

    # 1/2
    subset_subsidies_building <- reactive({
      req(input$selected_communes)

      subsidies_by_building |>
        dplyr::filter(commune %in% input$selected_communes)
    })

    subset_subsidies_measure <- reactive({
      req(input$selected_communes)

      subsidies_by_measure |>
        dplyr::filter(commune %in% input$selected_communes)
    })


    # Uploaded communes ----

    uploaded_communes <- mod_upload_communes_server("uploaded_communes")

    # Storing in inputVals : ----
    # Initializing the inputVals items

    inputVals <- reactiveValues()

# [inputVals 1/3] ----

    observe({

      inputVals$selectedCommunes <- input$selected_communes
      inputVals$selectedDistrict <- input$selected_district

    })

    observe({
      inputVals$uploadedCommunes <- uploaded_communes()
    })

# [inputVals 2/3] ----
    # Other inputs not influenced by selectInputs() else than commune

    observe({
      # store the commune cons dataset already filtered

      # !!CONS_ELEC removed!! #inputVals$cons_dataset <- subset_elec_cons()

      # store the commune prod dataset already filtered
      inputVals$prod_dataset <- subset_elec_prod()

      # store the regener commune dataset already filtered

      inputVals$rgr_1 <- subset_rgr_cons_1()
      inputVals$rgr_2 <- subset_rgr_cons_2()

      inputVals$rgr_needs <- subset_rgr_needs()
      inputVals$rgr_misc <- subset_rgr_misc()

      # store subsidies dataset already filtered

      inputVals$subsidies_building <- subset_subsidies_building()
      inputVals$subsidies_measure <- subset_subsidies_measure()

      # store min & max !available! years from consumption data to feed sliderInput()

      # !!CONS_ELEC removed!! #  inputVals$min_avail_cons <- min(subset_elec_cons()$annee)
      # !!CONS_ELEC removed!! # inputVals$max_avail_cons <- max(subset_elec_cons()$annee)

      # store min & max !available! years to feed sliderInput()
      inputVals$min_avail_prod <- min(subset_elec_prod()$annee)
      inputVals$max_avail_prod <- max(subset_elec_prod()$annee)

      # store list of !available! techs to feed pickerInput()
      inputVals$techs_avail <- subset_elec_prod() %>%
        dplyr::distinct(categorie) %>%
        dplyr::pull()

      # store min & max years from regener dataset (this does not need to by commune specific)
      inputVals$min_regener_year <- min_regener_year # utils_helpers.R
      inputVals$max_regener_year <- max_regener_year # utils_helpers.R


    })# End observe



    ### Statbox subsets, communes only (!) ----
    # --> we exclude Cantonal row which value is separated inside a dedicated statbox

    observe({

      req(subset_elec_prod(),
          # !!CONS_ELEC removed!! # subset_elec_cons(),
          subset_rgr_cons_1())

      # Statbox value for current selection's aggregated electricity production
      inputVals$common_year_elec_prod <- subset_elec_prod() %>%
        dplyr::filter(annee == last_common_elec_year) %>%
        dplyr::filter(!commune == "Canton de Vaud") %>% # remove cantonal row
        dplyr::summarise(production = sum(production, na.rm = T)) %>%
        dplyr::pull(production)

      # Statbox value for current selection's aggregated electricity consumption

      # !!CONS_ELEC removed!! # inputVals$common_year_elec_cons <- subset_elec_cons() %>%
      # !!CONS_ELEC removed!! #   dplyr::filter(annee == last_common_elec_year) %>%
      # !!CONS_ELEC removed!! # dplyr::filter(!commune == "Canton de Vaud")%>%
      # !!CONS_ELEC removed!! #   dplyr::summarise(consommation = sum(consommation, na.rm = T)) %>%
      # !!CONS_ELEC removed!! #   dplyr::pull(consommation)

      # Statbox value for current selection's aggregated buildings thermal consumption

      inputVals$max_year_rg_cons <- subset_rgr_cons_1() %>%
        dplyr::filter(etat == max_regener_year) %>%
        dplyr::filter(!commune == "Canton de Vaud") %>%
        dplyr::summarise(consommation=sum(consommation, na.rm = T)) %>%
        dplyr::pull(consommation)

    })# End observe


    ## Render dynamic UI for renderUIs ----

    ### tabCons dynamic select ----

    # !!CONS_ELEC removed!! # output$cons_year <- shiny::renderUI({
    # !!CONS_ELEC removed!! #
    # !!CONS_ELEC removed!! #   req(input$selected_communes)
    # !!CONS_ELEC removed!! #
    # !!CONS_ELEC removed!! #   shiny::tagList(
    # !!CONS_ELEC removed!! #
    # !!CONS_ELEC removed!! #     tags$div(class = 'customSliderInput', # custom.css -> go green
    # !!CONS_ELEC removed!! #
    # !!CONS_ELEC removed!! #     shiny::sliderInput(ns("cons_year"), label = "Choix des années",
    # !!CONS_ELEC removed!! #                        min = inputVals$min_avail_cons,
    # !!CONS_ELEC removed!! #                        max = inputVals$max_avail_cons,
    # !!CONS_ELEC removed!! #                        value = c(inputVals$min_avail_cons, inputVals$max_avail_cons),
    # !!CONS_ELEC removed!! #                        step = 1L, sep = "", ticks = T)
    # !!CONS_ELEC removed!! #
    # !!CONS_ELEC removed!! #     ))# End tagList
    # !!CONS_ELEC removed!! # })# End renderUi

    ### tabRegener dynamic select ----

    output$regener_year_selector <- shiny::renderUI({

      req(input$selected_communes)

      tags$div(class = 'customSliderInput', # custom.css -> go green

               shiny::sliderInput(ns("regener_year"), label = "Choix des années",
                                  min = inputVals$min_regener_year,
                                  max = inputVals$max_regener_year,
                                  value = c(inputVals$min_regener_year,inputVals$max_regener_year),
                                  step = 1L, sep = "", ticks = T))

    })


    ### tabProd dynamic select ----

    output$prod_year_n_techs <- shiny::renderUI({

        req(input$selected_communes)

      shiny::tagList(

        tags$div(class = 'customSliderInput', # custom.css -> go green

        shiny::sliderInput(ns("prod_year"), label = "Choix des années",
                    min = inputVals$min_avail_prod,
                    max = inputVals$max_avail_prod,
                    value = c(inputVals$min_avail_prod, inputVals$max_avail_prod),
                    step = 1L, sep = "", ticks = T)),


        # For SELECTABLE technologies : these checkboxes are linked to other server parts
        shinyWidgets::prettyCheckboxGroup(inputId = ns("prod_techs"),
                                          label = "Choix des technologies",
                                         choices = inputVals$techs_avail,
                                         selected = inputVals$techs_avail,
                                         inline = FALSE,
                                         bigger = FALSE,
                                         shape = "round",
                                         status =  "default",
                                         icon = icon("check"),
                                         animation = "jelly"),
        # For NON-SELECTABLE technologies : a title with a unordered list (see custom.css classes)
          tags$p(class = "sidebar_na_title",
                 "Non représentées :"),
          tags$ul(class = "sidebar_na_list",
                  setdiff(categories_diren, inputVals$techs_avail) %>% # unavailable techs
                  purrr::map(tags$li) # map into list items of ul()
        )# End tags$ul
      ) # End tagList
    }) # End renderUI


# [inputVals 3/3] -----

    # We eventually complete inputVals with the values from renderUI() above
    observe({

                        # Cons elec selected inputs
# !!CONS_ELEC removed!! # inputVals$min_selected_cons <- input$cons_year[1] # current min year selected for elec consumption
# !!CONS_ELEC removed!! # inputVals$max_selected_cons <- input$cons_year[2] # current max year selected for elec consumption

      # Prod elec selected inputs
      inputVals$min_selected_prod <- input$prod_year[1] # current min year selected for elec production
      inputVals$max_selected_prod <- input$prod_year[2] # current max year selected for elec production
      inputVals$techs_selected <- input$prod_techs      # current selected technologies for elec production


      # RegEner selected inputs
      inputVals$min_selected_regener <- input$regener_year[1]
      inputVals$max_selected_regener <- input$regener_year[2]

    })

    # Returning all the input values ----

    return(inputVals)


  }) # End moduleServer
} # End server




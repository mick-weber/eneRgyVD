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


    # IF tab Carte : add upload communes widget
    shiny::conditionalPanel(
      condition="input.nav == 'Carte'",
      mod_upload_communes_ui(ns("uploaded_communes"))
    ),

    # uiOutput for tabCons ----

    shiny::conditionalPanel(
      condition="input.nav == 'Consommation'",

      shiny::uiOutput(ns("cons_year"))

    ), # End conditionalPanel

    # uiOutput() for tabProd ----
    # IF tabProd : 2 widgets in a single uiOutput call for years and technologies
    # --> uiOutput/renderUI because its parameters are reactive

    shiny::conditionalPanel(
      condition="input.nav == 'Production'",

      shiny::uiOutput(ns("prod_year_n_techs"))

    ), # End conditionalPanel

    # uiOutput() for tabRegener ----
    # Use js array for all tabs because we can't target the overarching 'tabRegener' it does not work
    shiny::conditionalPanel(
      condition="['Besoins', 'Consommations', 'Autres'].includes(input.nav)",
      shiny::uiOutput(ns("regener_year_selector"))

    ), # End conditionalPanel


    # Unit converter ----

    tags$div(style = "margin-top: auto;",
             # Unit converter widget
             bslib::accordion(open = FALSE,
                              class = "fs-sidebar-header rotatedSVG",
                              bslib::accordion_panel(title = "Changer d'unité",
                                                     icon = bsicons::bs_icon("calculator-fill"),
                                                           shinyWidgets::prettyRadioButtons(inputId = ns("selected_unit"),
                                                                                            label = NULL,
                                                                                            choices = c("kWh", "MWh", "GWh", "TJ"),
                                                                                            selected = "MWh",
                                                                                            inline = FALSE,
                                                                                            status =  "default",
                                                                                            icon = icon("check"),
                                                                                            animation = "jelly")
             )),
             ),# End div()




  ) # End tagList
} # End UI

#' sideboard_inputs Server Functions
#'
#' @noRd
mod_inputs_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Important note ----
    ## Unit conversion is made here, at the root of inputVals, for each dataset

    # 1. tabCons inputs ----

     subset_elec_cons <- reactive({

       req(input$selected_communes)

       elec_cons %>%
         filter(commune %in% input$selected_communes) |>
         convert_units(colnames = "consommation",
                       unit_from = "kWh",
                       unit_to = input$selected_unit)

     })


    # 2. tabProd inputs ----

    ## Reactive subset data  ----
    # This is only used to feed the dynamic UI (year/techs available)
    # The 'usable' dataset for plots etc. is computed in app_server.R and is further filtered by
    # the values of the year sliderInput() and techs pickerInput()

    subset_elec_prod <- reactive({

      req(input$selected_communes)
      req(input$selected_unit)

      elec_prod %>%
        filter(commune %in% input$selected_communes) |>
        convert_units(colnames = contains(c("injection", "production", "autoconso", "puissance")),
                      unit_from = "kWh",
                      unit_to = input$selected_unit)
    })


    # 3. tabRegener inputs ----
    # Subset aggregated regener data with the currently selected commune(s)

    ## 1/4: cons/use ----
    # regener by commune, consumption, ae, use
    # I may rename these later for more explicit names...
    subset_rgr_cons_1 <- reactive({

      req(input$selected_communes)
      req(input$selected_unit)

      regener_cons_ae_use %>%
        filter(commune %in% input$selected_communes) %>%
        convert_units(colnames = "consommation",
                      unit_from = "kWh",
                      unit_to = input$selected_unit)

    })

    ## 2/4: cons/aff ----
    # regener by commune, consumption, ae, aff
    # I may rename these later for more explicit names...
    subset_rgr_cons_2 <- reactive({

      req(input$selected_communes)
      req(input$selected_unit)

      regener_cons_ae_aff %>%
        filter(commune %in% input$selected_communes) %>%
        convert_units(colnames = "consommation",
                      unit_from = "kWh",
                      unit_to = input$selected_unit)

    })

    ## 3/4 : needs ----
    # regener by commune, needs

    subset_rgr_needs <- reactive({

      req(input$selected_communes)
      req(input$selected_unit)

      regener_needs %>%
        dplyr::filter(commune %in% input$selected_communes) %>%
        convert_units(colnames = contains("besoins"),
                      unit_from = "kWh",
                      unit_to = input$selected_unit)

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

# [inputVals communes & unit] ----

    # communes
    observe({
      inputVals$selectedCommunes <- input$selected_communes
    })

    # uploaded communes

    observe({
      inputVals$uploadedCommunes <- uploaded_communes()
    })

    # unit selected
    observe({
      inputVals$selectedUnit <- input$selected_unit
    })

# [inputVals datasets & subsets] ----
    # Other inputs not influenced by selectInputs() else than commune

    observe({

      # store the commune cons dataset already filtered
    inputVals$cons_dataset <- subset_elec_cons()

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

      inputVals$min_avail_cons <- min(subset_elec_cons()$annee)
      inputVals$max_avail_cons <- max(subset_elec_cons()$annee)

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
          subset_elec_cons(),
          subset_rgr_cons_1(),
          subset_subsidies_measure()
          )

      # Statbox value for current selection's aggregated electricity production
      inputVals$common_year_elec_prod <- subset_elec_prod() %>%
        dplyr::filter(annee == last_common_elec_year) %>%
        dplyr::filter(!commune == "Canton de Vaud") %>% # remove cantonal row
        dplyr::summarise(production = sum(production, na.rm = T)) %>%
        dplyr::pull(production)

      # Statbox value for current selection's aggregated electricity consumption

      inputVals$common_year_elec_cons <- subset_elec_cons() %>%
        dplyr::filter(annee == last_common_elec_year) %>%
        dplyr::filter(!commune == "Canton de Vaud")%>%
        dplyr::summarise(consommation = sum(consommation, na.rm = T)) %>%
        dplyr::pull(consommation)

      # Statbox value for current selection's aggregated buildings thermal consumption

      inputVals$max_year_rg_cons <- subset_rgr_cons_1() %>%
        dplyr::filter(etat == max_regener_year) %>%
        dplyr::filter(!commune == "Canton de Vaud") %>%
        dplyr::summarise(consommation=sum(consommation, na.rm = T)) %>%
        dplyr::pull(consommation)


      inputVals$max_year_subsidies_m01 <- subset_subsidies_measure() |>
        dplyr::filter(annee == last_subsidies_year) |>
        dplyr::filter(!commune == "Canton de Vaud") |>
        dplyr::filter(mesure == "M01") |>
        dplyr::summarise(nombre = sum(nombre, na.rm = TRUE)) |>
        dplyr::pull(nombre)

    })# End observe


    ## Render dynamic UI for renderUIs ----

    ### tabCons dynamic select ----

     output$cons_year <- shiny::renderUI({

       req(input$selected_communes)

       shiny::tagList(

         tags$div(class = 'customSliderInput', # custom.css -> go green

         shiny::sliderInput(ns("cons_year"), label = "Choix des années",
                            min = inputVals$min_avail_cons,
                            max = inputVals$max_avail_cons,
                            value = c(inputVals$min_avail_cons, inputVals$max_avail_cons),
                            step = 1L, sep = "", ticks = T)

         ))# End tagList
     })# End renderUi

    ### tabRegener dynamic select ----

    output$regener_year_selector <- shiny::renderUI({

      req(input$selected_communes)

      tags$div(class = 'customSliderInput', # custom.css -> go green

               shiny::sliderInput(ns("regener_year"), label = "Choix des années",
                                  min = inputVals$min_regener_year,
                                  max = inputVals$max_regener_year,
                                  value = c(inputVals$min_regener_year,
                                            inputVals$max_regener_year),
                                  step = 1L, sep = "", ticks = T))

    })


    ### tabProd dynamic select ----

    output$prod_year_n_techs <- shiny::renderUI({

        req(input$selected_communes)
        req(input$selected_unit)

      shiny::tagList(

        tags$div(class = 'customSliderInput', # custom.css -> go green

        shiny::sliderInput(ns("prod_year"), label = "Choix des années",
                    min = inputVals$min_avail_prod,
                    max = inputVals$max_avail_prod,
                    value = c(inputVals$min_avail_prod, inputVals$max_avail_prod),
                    step = 1L, sep = "", ticks = T)),


        # For SELECTABLE technologies : these checkboxes are linked to other server parts
        tags$div(class = "fs-sidebar",
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
        tags$p(#class = "sidebar_na_title",
               "Non représentées :"),
        tags$ul(class = "sidebar_na_list",
                setdiff(categories_diren, inputVals$techs_avail) %>% # unavailable techs
                  purrr::map(tags$li) # map into list items of ul()
        )# End tags$ul
      )# End tags$div
      )# End tagList
    }) # End renderUI


# [inputVals 3/3] -----

    # We eventually complete inputVals with the values from renderUI() above
    observe({

                        # Cons elec selected inputs
      inputVals$min_selected_cons <- input$cons_year[1] # current min year selected for elec consumption
      inputVals$max_selected_cons <- input$cons_year[2] # current max year selected for elec consumption

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




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
                   label = "Commune(s)",
                   choices = communes_names,
                   selected = NULL,
                   multiple = TRUE,
                   options = list(placeholder = "Sélectionner une ou plusieurs communes")),

    # selectizeInput() for district zoom ----
    # IF tabMap : Select input for zooming on the districts (WIP feature)
    shiny::conditionalPanel(
       condition="input.sidebarMenu == 'tabMap'",
    shiny::selectizeInput(inputId = ns("selected_district"),
                   label = "Zoom par district",
                   choices = districts_names,
                   selected = 0,
                   multiple = FALSE)),

    # uiOutput() for tabProd ----
    # IF tabProd : 2 widgets in a single uiOutput call for years and technologies
    # --> uiOutput/renderUI because its parameters are reactive

    shiny::conditionalPanel(
      condition="input.sidebarMenu == 'tabProd'",

      shiny::uiOutput(ns("prod_year_n_techs")),

      # radioGroupButtons() for tabProd ----
      # To select the type of plot. Params are not reactive so we can code it here
      # instead of the dynamic uiOutputs above

      shinyWidgets::radioGroupButtons(
        inputId = ns("tabProd_plot_type"),
        label = "Type de graphique",
        choices = c(`<i class='fa fa-bar-chart'></i>` = "bar",
                    `<i class='fa fa-chart-area'></i>` = "area",
                    `<i class='fa fa-pie-chart'></i>` = "sunburst"),
        justified = TRUE)

    ) # End conditionalPanel
  ) # End tagList
} # End UI

#' sideboard_inputs Server Functions
#'
#' @noRd
mod_inputs_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

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

    ## Storing all values in inputVals ----
    # saving inputVals to populate the widgets in renderUI()
    # Note : inputVals is completed later again once renderUI() are rendered

    inputVals <- reactiveValues()

    observe({

      # store the selectizeInputs() inputs
      inputVals$selectedCommunes <- input$selected_communes
      inputVals$selectedDistrict <- input$selected_district

      # store min & max !available! years to feed sliderInput()
      inputVals$min_avail <- min(subset_elec_prod()$annee)
      inputVals$max_avail <- max(subset_elec_prod()$annee)

      # store list of !available! techs to feed pickerInput()
      inputVals$techs_avail <- subset_elec_prod() %>%
        dplyr::distinct(categorie_diren) %>%
        dplyr::pull()

      # store which radioGroupButton() is currently selected

      inputVals$prod_plot_type <- input$tabProd_plot_type

    }) # End observe. Note : inputVals is completed later again once renderUI() are rendered


    ## Render dynamic UI for renderUIs ----
    # renderUI for when tabProd is selected
    output$prod_year_n_techs <- shiny::renderUI({

        req(input$selected_communes)

      shiny::tagList(
        shiny::sliderInput(ns("prod_year"), label = "Choix des années",
                    min = inputVals$min_avail,
                    max = inputVals$max_avail,
                    value = c(inputVals$min_avail, inputVals$max_avail),
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


    # We complete inputVals with the values from renderUI() above
    observe({

      inputVals$min_selected <- input$prod_year[1]
      inputVals$max_selected <- input$prod_year[2]
      inputVals$techs_selected <- input$prod_techs

    })


    # Returning all the input values ----

    return(inputVals)


  }) # End moduleServer
} # End server

## To be copied in the UI
# mod_sideboard_inputs_ui("inputs_1")

## To be copied in the server
# mod_sideboard_inputs_server("inputs_1")



  # testing module
  # nameApp <- function() {
  #     ui <- fluidPage(
  #       mod_inputs_ui("id")
  #     )
  #     server <- function(input, output, session) {
  #       mod_inputs_server("id")
  #     }
  #     shinyApp(ui, server)
  # }
  # nameApp()


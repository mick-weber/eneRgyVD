#' sideboard_inputs UI Function
#'
#' @description A shiny Module.
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
      # To select the type of plot. Params are not reactive so we code it here instead of uiOutput above

      shinyWidgets::radioGroupButtons(
        inputId = ns("tabProd_plot"),
        label = "Type de graphique",
        choices = c(`<i class='fa fa-bar-chart'></i>` = "bar",
                    `<i class='fa fa-chart-area'></i>` = "area",
                    `<i class='fa fa-pie-chart'></i>` = "pie"),
        justified = TRUE)

    ),





  )
}

#' sideboard_inputs Server Functions
#'
#' @noRd
mod_inputs_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # tabProd inputs ----

    # reactive dataset filtered by input$selected_communes
    subset_elec_prod <- reactive({

      req(input$selected_communes)

      elec_prod_communes %>%
        filter(commune %in% input$selected_communes)

    })

    # saving inputVals to populate the widgets in renderUI()

    inputVals <- reactiveValues()

    observe({

      # store the selectizeInputs() values
      inputVals$selectedCommunes <- input$selected_communes
      inputVals$selectedDistrict <- input$selected_district

      # min & max years for subsetted data
      inputVals$min <- min(subset_elec_prod()$annee)
      inputVals$max <- max(subset_elec_prod()$annee)

      # list of available techs for subsetted data
      inputVals$techs <- subset_elec_prod() %>%
        dplyr::distinct(categorie_diren) %>%
        pull()

    })

    # renderUI for when tabProd is selected
    output$prod_year_n_techs <- shiny::renderUI({

        req(input$selected_communes)

      shiny::tagList(
        shiny::sliderInput("prod_year", label = "Choix des années",
                    min = inputVals$min,
                    max = inputVals$max,
                    value = c(inputVals$min, inputVals$max),
                    step = 1L, sep = "", ticks = T),

        shinyWidgets::pickerInput("prod_techs", label = "Choix des technologies",
                                  choices = inputVals$techs,
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
                                                length(inputVals$techs)))

        ))

      })

    # Returning values ----

    return(inputVals)


  })
}

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


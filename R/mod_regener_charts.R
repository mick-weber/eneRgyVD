#' regener_charts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_regener_charts_ui <- function(id){
  ns <- NS(id)
  tagList(


    # TABSETS for better readability of plot / table
    bs4Dash::tabsetPanel(
      id = "tabset1",
      shiny::tabPanel(title = "Graphique",

                      # radioGroupButtons() for tab ----

                      shiny::wellPanel(style = "background: transparent",
                                       shinyWidgets::radioGroupButtons(
                                         inputId = ns("tab_plot_type"),
                                         label = "Sélection du type de graphique",
                                         choices = c(`<i class='fa fa-bar-chart'></i>` = "flow",
                                                     `<i class='fa fa-pie-chart'></i>` = "bar"),
                                         justified = TRUE,
                                         width = "25%")

                      ),# End wellPanel

                      # # breathing
                      # br(),

                      # Conditional plotly (bar/sunburst) ----

                      # ggalluvial plot
                      shiny::plotOutput(ns("chart_alluvial"), height = "auto") %>%
                        shinycssloaders::withSpinner(color= main_color) # color defined in utils_helpers.R


      ),# End tabPanel 'Graphique'

      shiny::tabPanel(title = "Table",
                      column(width = 11,
                             # breathing
                             br(),
                             # Download module
                             # mod_download_data_ui(ns("table_download")),

                             # breathing
                             br(),
                             # DT table
                             #DT::dataTableOutput(ns("table_1")) %>%
                              #  shinycssloaders::withSpinner(color= main_color)
                      )# End column
      )# End tabPanel 'Table'
    )# End tabsetPanel




  )
}

#' regener_charts Server Functions
#'
#' @noRd
mod_regener_charts_server <- function(id, data, var_commune){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    n_facets <- reactiveValues(alluvial=1) #define it ouside, initialize with 1 facet

    # Pre-create the plot before calling renderPlot

    ggplot_alluvial <- reactive({

      data %>%
        create_alluvial_chart(var_commune = var_commune)

    })

    # Accessing the number of facets

    observe({

      req(ggplot_alluvial())

      out <- ggplot2::ggplot_build(ggplot_alluvial())
      n_facets$alluvial <- length(levels(out$data[[1]]$PANEL))

    })


    # renderPlot inside observe: required to access n_facets$alluvial above dynamically for height management

    observe({

      output$chart_alluvial <- shiny::renderPlot({

        validate(
          need(n_facets$alluvial>0, "Sélectionner au moins une commune pour générer un résultat.")
        )

        ggplot_alluvial() # plot call above


      },height = ceiling(n_facets$alluvial/2)*400) # 1,2,4,6,8,... change height every two facets+

    })# End observe
  })
}

## To be copied in the UI
# mod_regener_charts_ui("regener_charts_1")

## To be copied in the server
# mod_regener_charts_server("regener_charts_1")

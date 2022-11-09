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
                      # breating
                      br(),

                      # radioGroupButtons() for tab ----

                      shinyWidgets::radioGroupButtons(
                        inputId = ns("tab_plot_type"),
                        label = "Sélection du type de graphique",
                        choices = c(`<i class='fa fa-bars-staggered'></i>` = "flow",
                                    `<i class='fa fa-bar-chart'></i>` = "bar"),
                        justified = TRUE,
                        width = "25%"),

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

                             # radioGroupButtons() for tab ----
                             shinyWidgets::radioGroupButtons(
                               inputId = ns("tab_table_type"),
                               label = "Sélection du type de table",
                               choices = c(`<i class='fa fa-bars-staggered'></i>` = "flow",
                                           `<i class='fa fa-bar-chart'></i>` = "bar"),
                               justified = TRUE,
                               width = "25%"),

                             # Download module
                             mod_download_data_ui(ns("table_download")),

                             # breathing
                             br(),
                             # DT table
                             DT::dataTableOutput(ns("table_1")) %>%
                              shinycssloaders::withSpinner(color= main_color)
                      )# End column
      )# End tabPanel 'Table'
    )# End tabsetPanel




  )
}

#' regener_charts Server Functions
#'
#' @noRd
mod_regener_charts_server <- function(id,
                                      selectedUnit,
                                      inputVals, # for facet height
                                      subset_rgr_1, # conso->use
                                      subset_rgr_2 # conso->aff
                                      ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # tabs and renderPlot() ----

    observe({

      if(input$tab_plot_type == "flow"){

        # Alluvial plot 1 : conso -> usage
      output$chart_alluvial <- shiny::renderPlot({

        subset_rgr_1 %>%
          create_alluvial_chart(var_commune = "Commune",
                                var_flow = "Consommation",
                                var_from = "AE",
                                label_from = "Consommation",
                                var_to = "Usage",
                                label_to = "Usage")




      },height = ifelse(test = is.null(inputVals$selectedCommunes),
                        yes = 400, # if no commune selected, default width = 400
                        no = ceiling(length(inputVals$selectedCommunes)/2)*400)) # change height every two facets+


      }# End if tab...
      else if(input$tab_plot_type == "bar"){

        # Alluvial plot 2 : conso -> aff
        output$chart_alluvial <- shiny::renderPlot({

          subset_rgr_2 %>%
            create_alluvial_chart(var_commune = "Commune",
                                  var_flow = "Consommation",
                                  var_from = "AE",
                                  label_from = "Consommation",
                                  var_to = "Affectation",
                                  label_to = "Affectation")


        }, height = ifelse(test = is.null(inputVals$selectedCommunes),
                           yes = 400, # if no commune selected, default width = 400
                           no = ceiling(length(inputVals$selectedCommunes)/2)*400))
      }# End elseif

      })# End observe

    # tabs and renderTable ----
    observe({

      if(input$tab_table_type == "flow"){

        # Alluvial table 1 : conso -> usage
        output$table_1 <- DT::renderDataTable({

            create_regener_table_dt(data = subset_rgr_1(),
                                    unit = selectedUnit$unit_to)

        })


      }# End if tab...
      else if(input$tab_table_type == "bar"){

        # Alluvial table 2 : conso -> aff
        output$table_1 <- DT::renderDataTable({

            create_regener_table_dt(data = subset_rgr_2(),
                                    unit = selectedUnit$unit_to)
        })
      }# End elseif

    })# End observe

    # store the data in a reactive, according to which table type is selected

    download_data <- reactive({

      if(input$tab_table_type == "flow"){

        subset_rgr_1() %>% # from app_server.R
          # Add the currently selected unit in the colnames (conversion is already done)
          add_colname_units(unit = selectedUnit$unit_to) # fct_helpers.R

      }
      else if(input$tab_table_type == "bar"){

        subset_rgr_2() %>% # from app_server.R
          # Add the currently selected unit in the colnames (conversion is already done)
          add_colname_units(unit = selectedUnit$unit_to) # fct_helpers.R


      }

    })

    # Module to download DT table data
    mod_download_data_server("table_download",
                             data = download_data, # see if() above
                             dl_prefix = "regener_",
                             doc_vars = iris) # dl preffix for file name, passed into app_server.R


  })
}

## To be copied in the UI
# mod_regener_charts_ui("regener_charts_1")

## To be copied in the server
# mod_regener_charts_server("regener_charts_1")

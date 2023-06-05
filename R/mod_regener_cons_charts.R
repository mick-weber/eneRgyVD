#' regener_charts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_regener_cons_charts_ui <- function(id){
  ns <- NS(id)
  tagList(


    # TABSETS for better readability of plot / table
    bs4Dash::tabsetPanel(
      id = "tabset_rg_cons",
      shiny::tabPanel(title = "Graphique",
                      # breating
                      br(),
                      column(width = 10,
                      # Disclaimer for regener cons data (in a column for better display)
                      tags$p("Ces graphiques illustrent comment la consommation de différents agents énergétiques
 se répartit pour satisfaire les besoins en chaleur du bâtiment (chauffage et eau chaude sanitaire) selon l'usage ou l'affectation principale des bâtiments.",
 strong("La chaleur de procédés et l'électricité pour un usage autre que calorifique ne sont pas compris."),
 "Il s'agit d'estimations théoriques fondées sur des données empiriques. Les communes jouent notamment un rôle central
 pour garantir que les données reflètent bien la réalité des agents énergétiques en vigueur."),

 tags$p(paste0( # regener_current_year -> utils_helpers.R
   "Ces données reflètent la situation en ", regener_current_year, ". Une mise à disposition historisée de ces données est en cours d'élaboration et sera disponible à moyen terme."
   )),
                      ),# End column
                      # radioGroupButtons() for tab ----

                      shinyWidgets::radioGroupButtons(
                        inputId = ns("tab_plot_type"),
                        label = "Type de consommation",
                        choices = c(`<i class='fa fa-fire'></i> Par usage` = "flow",
                                    `<i class='fa fa-house'></i> Par affectation` = "bar"),
                        justified = TRUE,
                        width = "25%"),

                      # # breathing
                      # br(),

                      # Conditional plotly (bar/sunburst) ----

                      # ggalluvial plot
                      shiny::plotOutput(ns("chart_alluvial"), height = "auto") %>%
                        shinycssloaders::withSpinner(type = 6,
                                                     color= main_color) # color defined in utils_helpers.R


      ),# End tabPanel 'Graphique'

      shiny::tabPanel(title = "Table",
                      column(width = 11,
                             # breathing
                             br(),

                             # radioGroupButtons() for tab ----
                             shinyWidgets::radioGroupButtons(
                               inputId = ns("tab_table_type"),
                               label = "Type de consommation",
                               choices = c(`<i class='fa fa-fire'></i> Par usage` = "flow",
                                           `<i class='fa fa-house'></i> Par affectation` = "bar"),
                               justified = TRUE,
                               width = "25%"),

                             # Download module
                             mod_download_data_ui(ns("table_download")),

                             # breathing
                             br(),
                             # DT table
                             DT::dataTableOutput(ns("table_1"))
                      )# End column
      )# End tabPanel 'Table'
    )# End tabsetPanel




  )
}

#' regener_charts Server Functions
#'
#' @noRd
mod_regener_cons_charts_server <- function(id,
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


        subset_rgr_1() %>%
          lump_alluvial_factors(var_commune = "commune",
                                var_flow = "consommation",
                                var_from = "ae",
                                var_to = "usage") %>%
          create_alluvial_chart(var_commune = "commune",
                                var_flow = "consommation",
                                var_from = "ae",
                                label_from = "Consommation",
                                var_to = "usage",
                                label_to = "Usage")



      },height = ifelse(test = is.null(inputVals$selectedCommunes),
                        yes = 400, # if no commune selected, default width = 400
                        no = ceiling(length(inputVals$selectedCommunes)/2)*400)) # change height every two facets+


      }# End if tab...
      else if(input$tab_plot_type == "bar"){

        # Alluvial plot 2 : conso -> aff
        output$chart_alluvial <- shiny::renderPlot({

          subset_rgr_2() %>%
            lump_alluvial_factors(var_commune = "commune",
                                  var_flow = "consommation",
                                  var_from = "ae",
                                  var_to = "affectation") %>%
            create_alluvial_chart(var_commune = "commune",
                                  var_flow = "consommation",
                                  var_from = "ae",
                                  label_from = "Consommation",
                                  var_to = "affectation",
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
                                    unit = selectedUnit$unit_to,
                                    DT_dom = "frtip" # remove default button in DT extensions
                                    )

        })


      }# End if tab...
      else if(input$tab_table_type == "bar"){

        # Alluvial table 2 : conso -> aff
        output$table_1 <- DT::renderDataTable({

            create_regener_table_dt(data = subset_rgr_2(),
                                    unit = selectedUnit$unit_to,
                                    DT_dom = "frtip" # remove default button in DT extensions
                                    )
        })
      }# End elseif

    })# End observe

    # store the data in a reactive, according to which table type is selected

    download_data <- reactive({

      if(input$tab_table_type == "flow"){

        subset_rgr_1() %>% # from app_server.R
          # Add the currently selected unit in the colnames (conversion is already done)
          rename_fr_colnames() %>% # fct_helpers.R
          add_colname_units(unit = selectedUnit$unit_to)  # fct_helpers.R

          } else if(input$tab_table_type == "bar"){

        subset_rgr_2() %>% # from app_server.R
          # Add the currently selected unit in the colnames (conversion is already done)
          rename_fr_colnames() %>%  # fct_helpers.R
          add_colname_units(unit = selectedUnit$unit_to) # fct_helpers.R

      }

    })

    # Module to download DT table data
    mod_download_data_server("table_download",
                             inputVals = inputVals,
                             data = download_data, # see if() above
                             dl_prefix = "regener_",
                             doc_vars = regener_doc) # dl prefix for file name, passed into app_server.R


  })
}

## To be copied in the UI
# mod_regener_cons_charts_ui("regener_charts_1")

## To be copied in the server
# mod_regener_cons_charts_server("regener_charts_1")

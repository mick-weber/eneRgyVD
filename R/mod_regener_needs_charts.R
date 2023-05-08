#' regener_needs_charts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_regener_needs_charts_ui <- function(id){
  ns <- NS(id)


  tagList(
    # TABSETS for better readability of plot / table
    bs4Dash::tabsetPanel(
      id = "tabset_rg_needs",
      # tabPanel 'Graphique' ----
      shiny::tabPanel(title = "Graphique",
                      # breating
                      br(),
                      column(width = 10,
                             # Disclaimer for regener needs data (in a column for better display)
                             tags$p(
                               "Ces graphiques illustrent la répartition des besoins énergétiques théoriques pour la chaleur des bâtiments, soit
                               l'eau chaude sanitaire et chauffage des locaux.",
                               strong("Ne sont pas compris la chaleur des procédés industriels et l'électricité pour un usage autre que calorifique."),
                               "Plus d'informations, notamment sur les besoins optimisés, en cliquant sur 'Méthodologie' ci-dessus.")
                      ),# End column


                      fluidRow( # to display the plot buttons + materialswitches inline

                      ## radioGroupButtons() for tab ----
                      shinyWidgets::radioGroupButtons(
                        inputId = ns("tab_plot_type"),
                        label = "Sélection du type de graphique",
                        choices = c(`<i class='fa fa-bar-chart'></i>` = "bar",
                                    `<i class='fa fa-pie-chart'></i>` = "sunburst"),
                        justified = TRUE,
                        width = "25%"),

                        # materialSwitch 1/2 for bar plot
                        shiny::conditionalPanel(
                          # Both conditions: toggle must be TRUE and the bar plot button must be selected
                          condition = "output.commune && input.tab_plot_type == 'bar'",
                          ns = ns,

                          tags$div(style = "padding-left:80px;padding-top:40px;", # align with facets
                                   tags$div(
                                     shinyWidgets::materialSwitch(
                                       inputId = ns("stacked_status"),
                                       value = FALSE,
                                       status = "success",
                                       label = strong("Barres empilées"), inline = TRUE),
                                     tags$span(strong("adjacentes"))
                                   ))# End 2x tags$div()
                        ),# End conditionalPanel 1/2

                        # Spaces between the two toggles
                        HTML("&nbsp;"),HTML("&nbsp;"),HTML("&nbsp;"),
                        HTML("&nbsp;"),HTML("&nbsp;"),HTML("&nbsp;"),

                        # materialSwitch 2/2 for bar plot
                        shiny::conditionalPanel(
                          # Both conditions: toggle must be TRUE and the bar plot button must be selected
                          condition = "output.toggle && input.tab_plot_type == 'bar'",
                          ns = ns,

                          tags$div(
                            style = "padding-left:30px;padding-top:40px;border-left:1px solid lightgrey;", # separator with prev toggle
                            shinyWidgets::materialSwitch(
                              inputId = ns("toggle_status"),
                              value = FALSE,
                              label = strong("Axe vertical commun"),
                              status = "success",
                              inline = TRUE),
                            tags$span(strong("indépendant"))
                          )# End tags$div
                        )# End 2nd conditionalPanel

                      ),# End fluidrow for plot buttons + materialswitches

                      # Simple text to inform how the sunburst year works, if selected
                      shiny::conditionalPanel(
                        condition = "input.tab_plot_type == 'sunburst'",
                        ns = ns,

                        tags$p("Seuls les besoins actuels théoriques sont affichés sur ce graphique.")

                      ),# End conditionalPanel

                      # # breathing
                      # br(),

                      ## Conditional plotly (bar/sunburst) ----
                      # Rendered server side so that we can check if sunburst, then we apply a css class for padding
                      uiOutput(ns("plot_render_ui"))



      ),# End tabPanel 'Graphique'

      # tabPanel 'Table' ----
      shiny::tabPanel(title = "Table",

                      column(width = 11,
                             # breathing
                             br(),
                             tags$p("Plus d'informations, notamment sur les besoins optimisés, en cliquant sur 'Méthodologie' ci-dessus."),
                             br(),
                             # Download module
                             mod_download_data_ui(ns("table_download")),
                             # DT table
                             DT::dataTableOutput(ns("table_1"))
                      )# End column
      )# End tabPanel 'Table'
    )# End tabsetPanel
  )# End tagList

}

#' regener_needs_charts Server Functions
#'
#' @noRd
mod_regener_needs_charts_server <- function(id,
                                            inputVals,
                                            subsetData, # filtered data for communes and selected years
                                            selectedUnit, # unit selected in mod_unit_converter.R
                                            legend_title, # for legend of barplot (either secteur/technologies)
                                            target_year, # which current year for the sunburst
                                            var_year, # 'annee'
                                            var_commune, # 'commune'
                                            var_rank_2, # categorical var ('secteur'/'categorie', ...)
                                            var_values, # prod/consumption kwh
                                            color_palette, # utils_helpers.R
                                            third_rank, # boolean
                                            var_rank_3_1, # var 1/2 to pivot for the last level of sunburst, if third_rank exists
                                            var_rank_3_2, # var 2/2
                                            fct_table_dt_type, # table function to pass (data specific)
                                            dl_prefix,# when DL the data (mod_download_data.R) : prod_(...) or cons_(...)
                                            doc_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # We process subsetData() in a nice, wide format for the table and sunburst
    subsetData_wide <- reactive({

      subsetData() %>%
        tidyr::pivot_wider(names_from = "statut",
                           values_from = "besoins") # not passed as arguments (could be if needed)
    })

    # We also process subsetData_wide() in a compatible format for create_sunburst_plotly()
    subsetData_sunburst <- reactive({

      subsetData_wide() %>%  # not passed as arguments (could be if needed)
        dplyr::rename(besoins = `Besoins actuels`) #

    })

    # Initialize toggle free_y condition for conditionalPanel in ui
    output$toggle <- reactive({
      length(inputVals$selectedCommunes) > 1 # Returns TRUE if more than 1 commune, else FALSE
    })

    # Initialize toggle stacked condition for conditionalPanel in ui
    output$commune <- reactive({
      length(inputVals$selectedCommunes) > 0 # Returns TRUE if at least one commune is selected, else FALSE
    })

    # We don't suspend output$toggle when hidden (default is TRUE)
    outputOptions(output, 'toggle', suspendWhenHidden = FALSE)

    outputOptions(output, 'commune', suspendWhenHidden = FALSE)

    # Render plot selectively based on radioButton above
    # Note we're nesting renderPlotly inside renderUI to access input$tab_plot_type for css class

    # renderUI ----

    output$plot_render_ui <- renderUI({

      if(input$tab_plot_type == "bar"){

        # Update the initialized FALSE toggle_status with the input$toggle_status
        # WIP with selectedUnit$unit_to

        # ...PLOTLY BAR PLOT ----
        output$chart_1 <-

            plotly::renderPlotly({

          # fct is defined in fct_helpers.R
          create_bar_plotly(data = subsetData(),
                            n_communes = length(inputVals$selectedCommunes),
                            var_year = var_year,
                            var_commune = var_commune,
                            unit = selectedUnit$unit_to,
                            var_rank_2 = var_rank_2,
                            var_values = var_values,
                            color_palette = color_palette, # defined in utils_helpers.R
                            dodge = input$stacked_status, # if T -> 'dodge', F -> 'stack'
                            free_y = input$toggle_status, # reactive(input$toggle_status)
                            legend_title = legend_title,  # links to ifelse in facet_wrap(scales = ...)
                            web_width = inputVals$web_width, # px width of browser when app starts
                            web_height = inputVals$web_height # px height of browser when app starts
                            )
        })# End renderPlotly

      }# End if
      else if(input$tab_plot_type == "sunburst"){


        # ...PLOTLY SUNBURST PLOT ----
        output$chart_1 <- plotly::renderPlotly({

          create_sunburst_plotly(data_sunburst = subsetData_sunburst(), # must be adapted later
                                 unit = selectedUnit$unit_to,
                                 var_year = var_year, # var name
                                 var_values = var_values, # var name
                                 var_commune = var_commune, # var name
                                 var_rank_2 = var_rank_2, # var name
                                 third_rank = third_rank, # we do have a third layer (rank_3_1+rank_3_2)
                                 var_rank_3_1 = var_rank_3_1,
                                 var_rank_3_2 = var_rank_3_2) # var names pivotted
        })# End renderPlotly
      } # End else if

      # We create a div so that we can pass a class. If sunburst, the class adds left-padding. If not, barClass -> custom.css
      tags$div(class = ifelse(input$tab_plot_type == "sunburst", "sunburstClass", "barClass"),
               plotly::plotlyOutput(ns("chart_1")) %>%
                 shinycssloaders::withSpinner(type = 6,
                                              color= main_color) # color defined in utils_helpers.R
      )#End div

    })# End renderUI

    # Renders the DT table ----
    output$table_1 <- DT::renderDataTable({

      fct_table_dt_type(data = subsetData_wide(), # see pivot_wider() at the top of the server
                        unit = selectedUnit$unit_to)

    })# End renderDT

    # store the data in a reactive (not sure why we can't pass subsetData() it directly, but otherwise this won't work)
    download_data <- reactive({


      subsetData() %>% # We let the data in a long format
        # Add the currently selected unit in the colnames (conversion is already done)
        # add energy units in brackets for energy/power related columns
        rename_fr_colnames() %>%  # fct_helpers.R
        add_colname_units(unit = selectedUnit$unit_to) # fct_helpers.R

    })

    # Module to download DT table data ----
    mod_download_data_server("table_download",
                             inputVals = inputVals,
                             data = download_data,
                             dl_prefix = dl_prefix,
                             doc_vars = doc_vars) # dl preffix for file name, passed into app_server.R

  })
}

## To be copied in the UI
# mod_regener_needs_charts_ui("regener_needs_charts_1")

## To be copied in the server
# mod_regener_needs_charts_server("regener_needs_charts_1")

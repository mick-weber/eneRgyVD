#' subsidies_charts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_subsidies_charts_ui <- function(id){
  ns <- NS(id)
  tagList(
    # TABSETS for better readability of plot / table
    bs4Dash::tabsetPanel(
      id = "tabset_subsidies",

      ## Graphique tabPanel ----

      shiny::tabPanel(title = "Graphique",
                      # breating
                      br(),
                      column(width = 10,
                             # Disclaimer for regener cons data (in a column for better display)
                             tags$p("Ces graphiques illustrent le nombre de bâtiments ayant reçu des subventions du Programme Bâtiment vaudois.
                                    L'état à la fin de chaque année est représenté. Le total des subventions octroyées d'une année ne peut pas être
                                    inférieur au total de l'année précédente. La SRE correspond à la surface de référence énergétique des bâtiments ayant
                                    reçu une subvention."),
                      ),# End column

                      fluidRow(

                      # radioGroupButtons() for tab ----
                      shinyWidgets::radioGroupButtons(
                        inputId = ns("tab_plot_type"),
                        label = "Type de représentation",
                        choices = c(`<i class='fa fa-house'></i> Par nombre de bâtiments` = "n_egid",
                                    `<i class='fa fa-layer-group'></i> Par m<sup>2</sup> de SRE` = "sre"),
                        justified = TRUE,
                        width = "25%"),

                      # Spaces between the two toggles
                      HTML("&nbsp;"),HTML("&nbsp;"),HTML("&nbsp;"),
                      HTML("&nbsp;"),HTML("&nbsp;"),HTML("&nbsp;"),

                      # materialSwitch 1/1 for bar plot
                      shiny::conditionalPanel(
                        # Both conditions: toggle must be TRUE and the bar plot button must be selected
                        condition = "output.toggle",
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
                      )# End conditionalPanel
                      ), # End fluidRow

                      # breathing
                      br(),

                      # Plotly bar (only one viz) ----
                      # plotly barplot in server according to which flow/bar is selected

                      plotly::plotlyOutput(ns("plot_subsidies")) |>
                        shinycssloaders::withSpinner(type = 6,
                                                     color= main_color) # color defined in utils_helpers.R

      ),# End tabPanel 'Graphique'

      ## Table tabPanel ----

      shiny::tabPanel(title = "Table",
                      column(width = 11,
                             # breathing
                             br(),

                             # Download module
                             mod_download_data_ui(ns("table_download")),

                             # breathing
                             br(),
                             # DT table
                             DT::dataTableOutput(ns("table_1"))
                      )# End column
      )# End tabPanel 'Table'
    )# End tabsetPanel
  )# End tagList
}

#' subsidies_charts Server Functions
#'
#' @noRd
mod_subsidies_charts_server <- function(id,
                                        subsetData, # passed from inputVals, bit redundant but clear
                                        # selectedUnit not needed here !
                                        inputVals,
                                        dl_prefix = dl_prefix,
                                        doc_vars = doc_vars
                                        ){
  moduleServer(id, function(input, output, session){

    ns <- session$ns

    # Initialize toggle free_y condition for conditionalPanel in ui
    output$toggle <- reactive({
      length(inputVals$selectedCommunes) > 1 # Returns TRUE if more than 1 commune, else FALSE
    })

    # We don't suspend output$toggle when hidden (default is TRUE)
    outputOptions(output, 'toggle', suspendWhenHidden = FALSE)

    # Plot logic ----

        output$plot_subsidies <- plotly::renderPlotly({

          if(input$tab_plot_type == "sre"){


          subsetData() |>
            # Important : aggregate to remove details needed only in tables
            dplyr::group_by(commune, etat, subv_type) |>
            dplyr::summarise(N_EGID = sum(N_EGID),
                             SRE = sum(SRE)) |>
            # Then plot :
            create_bar_plotly(n_communes = length(inputVals$selectedCommunes),
                              var_year = "etat",
                              var_commune = "commune",
                              var_values = "SRE",
                              var_rank_2 = "subv_type",
                              unit = "m<sup>2</sup>",
                              legend_title = "",
                              color_palette = colors_subsidies_type,
                              dodge = FALSE, # we don't allow user to dodge w/ toggle button
                              free_y = input$toggle_status, # reactive(input$toggle_status)
                              web_width = inputVals$web_width, # px width of browser when app starts
                              web_height = inputVals$web_height # px height of browser when app starts
            )

          }else if(input$tab_plot_type == "n_egid"){

          subsetData() |>
            # Important : aggregate to remove details needed only in tables
            dplyr::group_by(commune, etat, subv_type) |>
            dplyr::summarise(N_EGID = sum(N_EGID),
                             SRE = sum(SRE)) |>
            # Then plot :
            create_bar_plotly(n_communes = length(inputVals$selectedCommunes),
                              var_year = "etat",
                              var_commune = "commune",
                              var_values = "N_EGID",
                              var_rank_2 = "subv_type",
                              unit = "Bâtiments",
                              legend_title = "",
                              color_palette = colors_subsidies_type,
                              dodge = FALSE, # we don't allow user to dodge w/ toggle button
                              free_y = input$toggle_status, # reactive(input$toggle_status)
                              web_width = inputVals$web_width, # px width of browser when app starts
                              web_height = inputVals$web_height # px height of browser when app starts

            )# End create_bar_plotly
          }# End else if
        })# End renderPlot

    # Table logic ----

    # DT table, detailed (plot is aggregated) with both N_EGID & SRE
    output$table_1 <- DT::renderDataTable({

      create_subsidies_table_dt(data = subsetData(),
                                DT_dom = "frtip" # remove default button in DT extensions
      )
    })

    # Download logic ----

    # store the data in a reactive (not sure why we can't pass subsetData() it directly, but otherwise this won't work)

    download_data <- reactive({


      # Make colnames nicelly formatted and add the current unit
      subsetData() |>
        rename_misc_colnames() |>  # fct_helpers.R
        rename_fr_colnames()       # fct_helpers.R


    })

    # Module to download DT table data
    mod_download_data_server("table_download",
                             inputVals = inputVals,
                             data = download_data,
                             dl_prefix = dl_prefix,
                             doc_vars = doc_vars) # dl prefix for file name, passed from app_server.R




  })# End moduleServer
}# End server

## To be copied in the UI
# mod_subsidies_charts_ui("subsidies_charts_1")

## To be copied in the server
# mod_subsidies_charts_server("subsidies_charts_1")


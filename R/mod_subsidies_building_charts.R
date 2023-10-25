#' subsidies_charts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_subsidies_building_charts_ui <- function(id){
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
                             tags$p("Ces graphiques illustrent le nombre de bâtiments ayant reçu des subventions du Programme Bâtiment vaudois depuis 2017.
                                    Les données précédant 2017 ne sont pas diffusées et représentent une minorité des subventions versées.
                                    L'état à la fin de chaque année est présenté, en cumulant les subventions des années précédentes.
                                    Le total des subventions octroyées d'une année ne peut donc pas être inférieur au total de l'année précédente.
                                    La SRE correspond à la surface de référence énergétique des bâtiments ayant reçu une subvention.
                                    Pour simplifier, le terme 'chauffage renouvelable' englobe également les pompes à chaleur (PAC) et le chauffage à distance (CAD)."),
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

                             tags$p("Les données sont davantage détaillées que pour les graphiques ce qui permet
                                    de discerner quel type de chauffage est subventionné avec la colonne `Détail chauffage`.
                                    La colonne `Type de subvention` permet de retrouver les données des graphiques."),

                             # Download module
                             mod_download_data_ui(ns("table_download")),

                             # DT table
                             DT::dataTableOutput(ns("table_1"))
                      )# End column
      )# End tabPanel 'Table'
    )# End tabsetPanel
  )# End tagList
}

#' subsidies_building_charts Server Functions
#'
#' @noRd
mod_subsidies_building_charts_server <- function(id,
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

    # Important : aggregate to remove `detail_chauffage` from plots
    subsetData_agg <- reactive({
      subsetData() |>
      dplyr::group_by(commune, etat, subv_type) |>
      dplyr::summarise(N_EGID = sum(N_EGID),
                       SRE = sum(SRE))
    })

    # Plot accordingly to which radioGroupButton (sre/n_egid) is selected
        output$plot_subsidies <- plotly::renderPlotly({

          if(input$tab_plot_type == "sre"){

            # Then plot aggregated data :
            subsetData_agg() |>
            create_bar_plotly(n_communes = length(inputVals$selectedCommunes),
                              var_year = "annee",
                              var_commune = "commune",
                              var_values = "SRE",
                              var_rank_2 = "subv_type",
                              unit = "m<sup>2</sup>",
                              legend_title = "",
                              color_palette = subsidies_building_colors,
                              dodge = FALSE, # we don't allow user to dodge w/ toggle button
                              free_y = input$toggle_status, # reactive(input$toggle_status)
                              web_width = inputVals$web_width, # px width of browser when app starts
                              web_height = inputVals$web_height # px height of browser when app starts
            )

          }else if(input$tab_plot_type == "n_egid"){

            # Then plot aggregated data :
            subsetData_agg() |>
            create_bar_plotly(n_communes = length(inputVals$selectedCommunes),
                              var_year = "etat",
                              var_commune = "commune",
                              var_values = "N_EGID",
                              var_rank_2 = "subv_type",
                              unit = "Bâtiments",
                              legend_title = "",
                              color_palette = subsidies_building_colors,
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
                                var_year = "etat",
                                var_rank_2 = "subv_type",
                                icon_list = subsidies_building_icons,
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


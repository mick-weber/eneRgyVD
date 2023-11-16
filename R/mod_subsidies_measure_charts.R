#' subsidies_measure_charts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_subsidies_measure_charts_ui <- function(id){
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
                             tags$p("Ces graphiques illustrent le nombre de subventions versées par type et année depuis 2017.
                                    Plusieurs subventions pouvant être accordées à un même bâtiment sur une ou plusieurs années,
                                    il ne faut pas interpréter une subvention comme un bâtiment subventionné. Une vision agrégée par
                                    bâtiments subventionnés est disponible dans l'onglet Subventions par bâtiments"),
                      ),# End column

                      fluidRow(

                        # Spaces between the two toggles
                        # HTML("&nbsp;"),HTML("&nbsp;"),HTML("&nbsp;"),
                        # HTML("&nbsp;"),HTML("&nbsp;"),HTML("&nbsp;"),

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

                             # DT table
                             DT::dataTableOutput(ns("table_1"))

                      )# End column
      )# End tabPanel 'Table'
    )# End tabsetPanel
  )# End tagList
}

#' subsidies_measure_charts Server Functions
#'
#' @noRd
mod_subsidies_measure_charts_server <- function(id,
                                                subsetData, # passed from inputVals, bit redundant but clear
                                                # selectedUnit not needed here !
                                                inputVals,
                                                dl_prefix = dl_prefix,
                                                doc_vars = doc_vars
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Initialize toggle free_y condition for conditionalPanel in ui
    output$toggle <- reactive({
      length(inputVals$selectedCommunes) > 1 # Returns TRUE if more than 1 commune, else FALSE
    })

    # We don't suspend output$toggle when hidden (default is TRUE)
    outputOptions(output, 'toggle', suspendWhenHidden = FALSE)

    # Plot logic ----

    output$plot_subsidies <- plotly::renderPlotly({

      # Plotly but factor lumped for clarity :
      subsetData() |>
        dplyr::mutate(mesure_simplifiee = forcats::fct_lump_n(f = mesure_simplifiee,
                                                   n = 3,
                                                   w = nombre,
                                                   other_level = "Autres mesures")) |>
        dplyr::group_by(commune, annee, mesure_simplifiee) |>
        dplyr::summarise(nombre = sum(nombre, na.rm = TRUE)) |>
          create_bar_plotly(n_communes = length(inputVals$selectedCommunes),
                            var_year = "annee",
                            var_commune = "commune",
                            var_values = "nombre",
                            var_rank_2 = "mesure_simplifiee",
                            unit = "Subventions",
                            legend_title = "",
                            color_palette = RColorBrewer::brewer.pal(
                              n = 4, # 3fcts + lump
                              name = "Pastel1"),
                            dodge = FALSE, # we don't allow user to dodge w/ toggle button
                            free_y = input$toggle_status, # reactive(input$toggle_status)
                            web_width = inputVals$web_width, # px width of browser when app starts
                            web_height = inputVals$web_height # px height of browser when app starts
          )# End create_bar_plotly
    })# End renderPlot

    # Table logic ----

    # DT table, detailed (plot is aggregated) with both N_EGID & SRE
    output$table_1 <- DT::renderDataTable({

      create_subsidies_table_dt(data = subsetData(),
                                var_year = "annee",
                                var_rank_2 = "mesure",
                                icon_list = return_icons_subsidies(which = "measure"),
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


  })
}

## To be copied in the UI
# mod_subsidies_measure_charts_ui("subsidies_measure_charts_1")

## To be copied in the server
# mod_subsidies_measure_charts_server("subsidies_measure_charts_1")

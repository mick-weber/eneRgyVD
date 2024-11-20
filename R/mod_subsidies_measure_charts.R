#' subsidies_measure_charts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_subsidies_measure_charts_ui <- function(id,
                                            title,
                                            title_complement){
  ns <- NS(id)
  tagList(

    # div to handle title + accordion layout
    tags$div(
      # Large+ screens : inline, flex layout, justified items
      #  smaller screens : row by row (default layout without fill)
      class = "d-lg-flex justify-content-between",
      # Title
      h4(title, style = "padding-right:10px;"),


      # Methodology accordion
      bslib::accordion(
        class = "customAccordion", # custom.scss : lg screens = 70% width; smaller screens = 100% width
        bslib::accordion_panel(
          title = "Méthodologie",
          div(paste(generic_method_warning, # text in utils_helpers.R
                    specific_subsidies_warning)),
          br(),
          actionButton(ns("subsidies_measure_help"), label = "Plus de détails sur les données")
        ),
        open = FALSE)
    ),

    # utils_text_and_links.R
    title_complement,

    # TABSETS for better readability of plot / table
    bslib::navset_pill(
      id = ns("tabset_subsidies"),

      ## Graphique tabPanel ----

      bslib::nav_panel(title = "Graphique",
                       icon = bsicons::bs_icon("bar-chart-fill"),

                       # breating
                       br(),

                       bslib::layout_column_wrap(width = 1/3,
                                                 class = "d-flex align-items-end",

                         # materialSwitch 1/1 for bar plot
                         shiny::conditionalPanel(
                           # Both conditions: toggle must be TRUE and the bar plot button must be selected
                           condition = "output.toggle",
                           ns = ns,
                           tags$div(
                             class = "d-flex justify-content-center",
                             shinyWidgets::materialSwitch(
                               inputId = ns("toggle_status"),
                               value = FALSE,
                               label = strong("Axe vertical commun", class = "align-middle"),
                               status = "success",
                               inline = TRUE),
                             tags$span(strong("indépendant", class = "align-middle"))
                           )# End tags$div
                         )# End conditionalPanel

                       ), # End layout_column_wrap

                       # Plotly bar (only one viz) ----
                       # plotly barplot in server according to which flow/bar is selected

                       plotly::plotlyOutput(ns("plot_subsidies")) |>
                         shinycssloaders::withSpinner(type = 6,
                                                      color= main_color) # color defined in utils_helpers.R

      ),# End tabPanel 'Graphique'

      ## Table tabPanel ----

      bslib::nav_panel(title = "Table",
                       icon = bsicons::bs_icon("table"),
                       # breathing
                       br(),

                       p(shiny::HTML("Note : les données sont plus détaillées que celles affichées dans l'onglet <strong>Graphique</strong>.")),

                       br(),

                       # Download module
                       mod_download_data_ui(ns("table_download")),

                       # DT table
                       DT::dataTableOutput(ns("table_1"))

      )# End tabPanel 'Table'
    )# End nav_menu
  )# End tagList
}

#' subsidies_measure_charts Server Functions
#'
#' @noRd
mod_subsidies_measure_charts_server <- function(id,
                                                subsetData, # passed from inputVals, bit redundant but clear
                                                # energyUnit not needed here !
                                                inputVals,
                                                dl_prefix = dl_prefix,
                                                doc_vars = doc_vars
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Initialize toggle free_y condition for conditionalPanel in ui
    output$toggle <- reactive({length(unique(subsetData()$commune)) > 1})

    # We don't suspend output$toggle when hidden (default is TRUE)
    outputOptions(output, 'toggle', suspendWhenHidden = FALSE)

    # Plot logic ----

    output$plot_subsidies <- plotly::renderPlotly({

      validate(need(inputVals$selectedCommunes, req_communes_phrase))

      # Plotly but factor lumped for clarity :
      subsetData() |>
        dplyr::mutate(mesure_simplifiee = forcats::fct_lump_n(f = mesure_simplifiee,
                                                              n = 3,
                                                              w = nombre,
                                                              # other_level should match utils_helpers.R subsidies_measure_palette_plot value for color matching !
                                                              other_level = "Autres mesures (voir table)")) |>
        dplyr::group_by(commune, annee, mesure_simplifiee) |>
        dplyr::summarise(nombre = sum(nombre, na.rm = TRUE)) |>
        create_bar_plotly(n_communes = dplyr::n_distinct(subsetData()$commune),
                          var_year = "annee",
                          var_commune = "commune",
                          var_values = "nombre",
                          var_cat = "mesure_simplifiee",
                          unit = "subventions",
                          legend_title = NULL,
                          color_palette = subsidies_measure_simplifiee_colors,
                          dodge = FALSE, # we don't allow user to dodge w/ toggle button
                          free_y = input$toggle_status, # reactive(input$toggle_status)
                          web_width = inputVals$web_width, # px width of browser when app starts
                          web_height = inputVals$web_height # px height of browser when app starts
        )# End create_bar_plotly
    })# End renderPlot

    # Table logic ----

    # DT table, detailed (plot is aggregated) with both N_EGID & SRE
    output$table_1 <- DT::renderDataTable({

      validate(need(inputVals$selectedCommunes, req_communes_phrase))

      create_subsidies_table_dt(data = subsetData(),
                                var_year = "annee",
                                var_cat = "mesure",
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

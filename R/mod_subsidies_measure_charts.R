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
      class = "d-flex justify-content-start",
      # Title
      h4(title, style = "padding-right:3vw;"),

      # Methodology button
      actionButton(ns("subsidies_measure_help"),
                   class = "btnCustom",
                   label = tags$span(style = "font-weight:500;",
                                     "Source et méthode",
                                     bslib::tooltip(
                                       id = ns("tooltip_data_help"),
                                       placement = "right",
                                       options = list(customClass = "customTooltips"), # custom.scss
                                       trigger = phosphoricons::ph(title = NULL, "info"),
                                       generic_method_warning # utils_text_and_links.R
                                     )
                   ))
    ),

    # utils_text_and_links.R
    title_complement,

    # TABSETS for better readability of plot / table
    bslib::navset_pill(
      id = ns("tabset_subsidies"),

      ## Graphique tabPanel ----

      bslib::nav_panel(title = "Graphique",
                       icon = phosphoricons::ph(title = NULL, "chart-bar"),

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

                       ggiraph::girafeOutput(ns("plot_subsidies")) |>
                         shinycssloaders::withSpinner(type = 6,
                                                      color= main_color) # color defined in utils_helpers.R

      ),# End tabPanel 'Graphique'

      ## Table tabPanel ----

      bslib::nav_panel(title = "Table",
                       icon = phosphoricons::ph(title = NULL, "table"),
                       # breathing
                       br(),

                       p(shiny::HTML("Note : les données sont plus détaillées que celles affichées dans l'onglet <strong>Graphique</strong>.")),

                       br(),

                       # Download module
                       mod_download_data_ui(ns("table_download")),

                       # rt table
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

    output$plot_subsidies <- ggiraph::renderGirafe({

      validate(need(inputVals$selectedCommunes, req_communes_phrase))
      validate(need(nrow(subsetData()) > 0, message = req_communes_not_available))
      req(subsetData())

      # Compute number of rows
      num_facets <- length(unique(subsetData()$commune))
      num_columns <- 2
      num_rows <- ceiling(num_facets / num_columns)  # Calculate rows needed for 2 columns

      # Dynamic height and width ratios (unitless)
      base_height_per_row <- 2  # Adjust height ratio per row

      # Save units passed to create_plot_ggiraph()
      height_svg <- 2 + (num_rows * base_height_per_row)  # Height grows with the number of rows
      width_svg <- 15  # Keep width static for two columns layout

      # If selected commune(s) yields in 0 rows, then state it's not available instead of plotting error
      validate(
        need(nrow(subsetData()) > 0, message = req_communes_not_available)
      )

      # Plotly but factor lumped for clarity :

      subsetData() |>
        dplyr::mutate(mesure_simplifiee = forcats::fct_lump_n(f = mesure_simplifiee,
                                                              n = 3,
                                                              w = nombre,
                                                              # other_level should match utils_helpers.R subsidies_measure_palette_plot value for color matching !
                                                              other_level = "Autres mesures (voir table)")) |>
        dplyr::group_by(commune, annee, mesure_simplifiee) |>
        dplyr::summarise(nombre = sum(nombre, na.rm = TRUE)) |>
        create_plot_ggiraph(# data piped
                           n_communes = dplyr::n_distinct(subsetData()$commune),
                           var_year = "annee",
                           var_commune = "commune",
                           unit = "subventions",
                           var_cat = "mesure_simplifiee",
                           var_values = "nombre",
                           geom = "col",
                           color_palette = subsidies_measure_simplifiee_colors, # defined in utils_helpers.R
                           dodge = FALSE, # if T -> 'dodge', F -> 'stack'
                           free_y = input$toggle_status, # reactive(input$toggle_status)
                           legend_title = NULL, # links to ifelse in facet_wrap(scales = ...)
                           height_svg = height_svg, # px width of browser when app starts
                           width_svg = width_svg # px height of browser when app starts
        )

    })# End renderPlot

    # Table logic ----

    # rt table, detailed (plot is aggregated) with both N_EGID & SRE
    output$table_1 <- DT::renderDataTable({

      validate(need(inputVals$selectedCommunes, req_communes_phrase))

      make_table_dt(data = subsetData(),
                    var_commune = "commune",
                    var_year = "annee",
                    var_values = c("nombre"),
                    var_cat = "mesure",
                    icons_palette = subsidies_measure_icons,
                    na_string = "(Non disponible)",
                    unit = NULL # no unit to apply
      )
    })

    # Download logic ----

    # store the data in a reactive (not sure why we can't pass subsetData() it directly, but otherwise this won't work)

    download_data <- reactive({

      # Make colnames nicelly formatted and add the current unit
      subsetData() |>
        rename_columns_output()
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

#' elec_charts UI Function
#'
#' @description A shiny Module which produces the different chart for the tab of the app. It
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_elec_charts_ui <- function(id,
                               title){
  ns <- NS(id)
  tagList(

    # Header ----
    # div to handle title + accordion layout
    tags$div(
      # Large+ screens : inline, flex layout, justified items
      #  smaller screens : row by row (default layout without fill)
      class = "d-lg-flex justify-content-between",
      # Title (w small padding to avoid it touching the accordion)
      h4(title, style = "padding-right:10px;"),


      # Methodology accordion
      bslib::accordion(
        class = "customAccordion", # custom.scss : lg screens = 70% width; smaller screens = 100% width
        bslib::accordion_panel(
          title = "Méthodologie",
          div(paste(generic_method_warning, # text in utils_helpers.R
                    specific_elec_warning)),
              br(),
          # Since multiple mod_elec_charts can exist, we play with the namespace to land the correct 'mod_about_the_app.R' page
          # This is done in app_server.R, `subpanels_tribble` object

          actionButton(ns("elec_data_help"), label = "Plus de détails")
        ),
        open = FALSE)
    ),#End div

    # Pills ----

    bslib::navset_pill(header = br(), # blank line to space content (alternative would be to add padding)

                       ### Graph ----
                       bslib::nav_panel(title = "Graphique",
                                 icon = bsicons::bs_icon("bar-chart-fill"),


                                bslib::layout_column_wrap(width = 1/4, # each col = 25% of avail. width
                                                          class = "d-flex align-items-end",


                                   # materialSwitch 1/2 for bar plot
                                   shiny::conditionalPanel(
                                     # Commune condition in server must be reached
                                     condition = "output.commune",
                                     ns = ns,

                                     tags$div(
                                       class = "d-flex justify-content-center",
                                              tags$div(
                                                shinyWidgets::materialSwitch(
                                                  inputId = ns("stacked_status"),
                                                  value = FALSE,
                                                  status = "success",
                                                  label = strong("Barres empilées"), inline = TRUE),
                                                tags$span(strong("adjacentes"))
                                              ))# End 2x tags$div()
                                   ),# End conditionalPanel 1/2

                                   # materialSwitch 2/2 for bar plot
                                   shiny::conditionalPanel(
                                     # Toggle condition in server must be reached
                                     condition = "output.toggle",
                                     ns = ns,

                                     tags$div(
                                       class = "d-flex justify-content-center",
                                       shinyWidgets::materialSwitch(
                                         inputId = ns("toggle_status"),
                                         value = FALSE,
                                         label = strong("Axe vertical commun"),
                                         status = "success",
                                         inline = TRUE),
                                       tags$span(strong("indépendant"))
                                     )# End tags$div
                                   ),# End 2nd conditionalPanel

                                ),#End layout_column_wrap() for buttons


                                 # !! Since sunburst is removed we can directly use renderPlotly
                                 uiOutput(ns("plot_render_ui"))




                       ),# End nav_panel('Graphique')

                       ### Table ----
                       bslib::nav_panel(title = "Table",
                                 icon = bsicons::bs_icon("table"),

                                 # Download buttons
                                 mod_download_data_ui(ns("table_download")),

                                 # DT table
                                 DT::dataTableOutput(ns("table_1"))


                       )# End nav_panel() 'Table'
    )#End navset_pill()
  )# End tagList()
}

#' elec_charts Server Functions
#'
#' @noRd
mod_elec_charts_server <- function(id,
                                   inputVals,
                                   subsetData, # filtered data for communes and selected years
                                   selectedUnit, # unit selected in mod_unit_converter.R
                                   legend_title, # for legend of barplot (either secteur/technologies)
                                   target_year, # which current year for the sunburst
                                   var_year, # 'annee'
                                   var_commune, # 'commune'
                                   var_rank_2, # categorical var ('secteur'/'categorie', NULL, ...)
                                   var_values, # prod/consumption kwh
                                   color_palette, # utils_helpers.R
                                   fct_table_dt_type, # table function to pass (data specific)
                                   dl_prefix,# when DL the data (mod_download_data.R) : prod_(...) or cons_(...)
                                   doc_vars){ # the non-reactive documentation file for variables description
  moduleServer(id, function(input, output, session){

    ns <- session$ns


    # Make debounced inputs ----
    # For barplot functions only, this avoids flickering plots when many items are selected/removed

    subsetData_d <- reactive({subsetData()}) |> debounce(debounce_plot_time)
    inputVals_communes_d <- reactive({inputVals$selectedCommunes}) |> debounce(debounce_plot_time)


    # Initialize toggle free_y condition for conditionalPanel in ui
    output$toggle <- reactive({
      length(inputVals_communes_d()) > 1 # Returns TRUE if more than 1 commune, else FALSE
    })

    # Initialize toggle stacked condition for conditionalPanel in ui
    output$commune <- reactive({
      length(inputVals_communes_d()) > 0 # Returns TRUE if at least one commune is selected, else FALSE
    })

    # We don't suspend output$toggle when hidden (default is TRUE)
    outputOptions(output, 'toggle', suspendWhenHidden = FALSE)
    outputOptions(output, 'commune', suspendWhenHidden = FALSE)

    # Plot logic ----

    # Render plot selectively based on radioButton above
    # Note we're nesting renderPlotly inside renderUI to access input$tab_plot_type for css class

    output$plot_render_ui <- renderUI({

        # Update the initialized FALSE toggle_status with the input$toggle_status
        # PLOTLY BAR PLOT

        output$chart_1 <- plotly::renderPlotly({

          # fct is defined in fct_helpers.R
          create_bar_plotly(data = subsetData_d(),
                            n_communes = length(inputVals_communes_d()),
                            var_year = var_year,
                            var_commune = var_commune,
                            unit = inputVals$selectedUnit,
                            var_rank_2 = var_rank_2,
                            var_values = var_values,
                            color_palette = color_palette, # defined in utils_helpers.R
                            dodge = input$stacked_status, # if T -> 'dodge', F -> 'stack'
                            free_y = input$toggle_status, # reactive(input$toggle_status)
                            legend_title = legend_title, # links to ifelse in facet_wrap(scales = ...)
                            web_width = inputVals$web_width, # px width of browser when app starts
                            web_height = inputVals$web_height # px height of browser when app starts
          )

        })# End renderPlotly


      # We create a div so that we can pass a class. If sunburst, the class adds left-padding. If not,

      tags$div(
               plotly::plotlyOutput(ns("chart_1")) |>
                 shinycssloaders::withSpinner(type = 6,
                                              color= main_color) # color defined in utils_helpers.R
      )

    })# End renderUI

    # Table logic ----
    # Renders the DT table
    output$table_1 <- DT::renderDataTable({

      fct_table_dt_type(data = subsetData(),
                        unit = inputVals$selectedUnit,
                        DT_dom = "frtip" # no buttons extension for DT table
      )

    })# End renderDT

    # Download logic ----
    # store the data in a reactive (not sure why we can't pass subsetData_d() it directly, but otherwise this won't work)

    download_data <- reactive({


      # Make colnames nicelly formatted and add the current unit
      subsetData() |>
        rename_fr_colnames()  |>  # fct_helpers.R
        add_colname_units(unit = inputVals$selectedUnit)  # fct_helpers.R

    })

    # Module to download DT table data
    mod_download_data_server("table_download",
                             inputVals = inputVals,
                             data = download_data,
                             dl_prefix = dl_prefix,
                             doc_vars = doc_vars) # dl preffix for file name, passed into app_server.R
  }) # End ModuleServer
} # End server

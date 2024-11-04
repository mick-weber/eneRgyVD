#' ng_charts UI Function
#'
#' @description A shiny Module made for natural gas distribution dataset
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ng_charts_ui <- function(id,
                             title,
                             title_complement){
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
                    specific_ng_warning)),
          br(),
          # Since multiple mod_elec_charts can exist, we play with the namespace to land the correct 'mod_about_the_app.R' page
          # This is done in app_server.R, `subpanels_tribble` object

          actionButton(ns("ng_cons_help"), label = "Plus de détails sur les données")
        ),
        open = FALSE)
    ),#End div

    # utils_text_and_links.R
    title_complement,

    # Pills ----

    bslib::navset_pill(
      header = br(), # blank line to space content (alternative would be to add padding)

      ### Graph ----
      bslib::nav_panel(title = "Graphique",
                       icon = bsicons::bs_icon("bar-chart-fill"),


                       bslib::layout_columns(col_widths = c(-2, 4, 4, -2),
                                             class = "fs-materialSwitch",


                                             # materialSwitch 1/2 for bar plot
                                             shiny::conditionalPanel(
                                               # Commune condition in server must be reached
                                               condition = "output.commune",
                                               ns = ns,

                                               tags$div(
                                                 tags$div(
                                                   class = "align-middle",
                                                   shinyWidgets::materialSwitch(
                                                     inputId = ns("stacked_status"),
                                                     value = FALSE,
                                                     status = "success",
                                                     label = strong("Barres empilées", class = "align-middle"),
                                                     inline = TRUE),
                                                   tags$span(strong("adjacentes", class = "align-middle"))
                                                 ))# End 2x tags$div()
                                             ),# End conditionalPanel 1/2

                                             # materialSwitch 2/2 for bar plot
                                             shiny::conditionalPanel(
                                               # Toggle condition in server must be reached
                                               condition = "output.toggle",
                                               ns = ns,

                                               tags$div(
                                                 shinyWidgets::materialSwitch(
                                                   inputId = ns("toggle_status"),
                                                   value = FALSE,
                                                   label = strong("Axe vertical commun", class = "align-middle"),
                                                   status = "success",
                                                   inline = TRUE),
                                                 tags$span(strong("indépendant", class = "align-middle"))
                                               )# End tags$div
                                             ),# End 2nd conditionalPanel

                       ),#End layout_column_wrap() for buttons


                       # renderPlotly
                       plotly::plotlyOutput(ns("chart_1")) |>
                         shinycssloaders::withSpinner(type = 6,
                                                      color= main_color) # color defined in utils_helpers.R




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

#' ng_charts Server Functions
#'
#' @noRd
mod_ng_charts_server <- function(id,
                                 subsetData, # passed from inputVals, bit redundant but clear
                                 inputVals,
                                 var_commune,
                                 var_year,
                                 var_cat,
                                 var_values,
                                 color_palette,
                                 dl_prefix = dl_prefix,
                                 doc_vars = doc_vars
){ # the non-reactive documentation file for variables description
  moduleServer(id, function(input, output, session){
    ns <- session$ns


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
    ## Make debounced inputs ----
    # For barplot functions only, this avoids flickering plots when many items are selected/removed

    output$chart_1 <- plotly::renderPlotly({

      validate(need(inputVals$selectedCommunes, req_communes_phrase))
      validate(need(nrow(subsetData_d()) > 0, message = req_communes_not_available))

      create_bar_plotly(data = subsetData_d(),
                        n_communes = length(inputVals_communes_d()),
                        var_commune = var_commune,
                        var_year = var_year,
                        var_values = var_values,
                        var_cat = var_cat,
                        unit = inputVals$energyUnit,
                        legend_title = "Secteur",
                        color_palette = color_palette,
                        dodge = input$stacked_status, # if T -> 'dodge', F -> 'stack'
                        free_y = input$toggle_status, # reactive(input$toggle_status)
                        web_width = inputVals$web_width, # px width of browser when app starts
                        web_height = inputVals$web_height # px height of browser when app starts
      )# End create_bar_plotly
    })# End renderPlot

    # Table logic ----

    # DT table
    output$table_1 <- DT::renderDataTable({

      validate(need(inputVals$selectedCommunes, req_communes_phrase))

      # ng needs are the same as electricity consumption
      create_cons_table_dt(data = subsetData(),
                              energy_unit = inputVals$energyUnit,
                              DT_dom = "frtip" # remove default button in DT extensions
      )
    })

    # Download logic ----

    # store the data in a reactive (not sure why we can't pass subsetData it directly, but otherwise this won't work)

    download_data <- reactive({

      # Make colnames nicelly formatted and add the current unit
      subsetData() |>
        rename_fr_colnames() |>        # fct_helpers.R
        add_colname_units(unit = inputVals$energyUnit)

    })

    # Module to download DT table data
    mod_download_data_server("table_download",
                             inputVals = inputVals,
                             data = download_data,
                             dl_prefix = dl_prefix,
                             doc_vars = doc_vars) # dl prefix for file name, passed from app_server.R


  })
} # End server


## To be copied in the UI
# mod_ng_charts_ui("generic_charts_1")

## To be copied in the server
# mod_ng_charts_server("generic_charts_1")

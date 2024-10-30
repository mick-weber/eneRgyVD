#' regener_needs_charts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_regener_needs_charts_ui <- function(id,
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
      # Title
      h4(title, style = "padding-right:10px;"),


      # Methodology accordion
      bslib::accordion(
        class = "customAccordion", # custom.scss : lg screens = 70% width; smaller screens = 100% width
        bslib::accordion_panel(
          title = "Méthodologie",
          div(paste(generic_method_warning, # text in utils_helpers.R
                    specific_rgr_warning)),
          br(),
          actionButton(ns("rgr_needs_help"), label = "Plus de détails sur les données")
        ),
        open = FALSE)

    ),#End div

    # utils_text_and_links.R
    title_complement,

    # Pills ----

    bslib::navset_pill(header = br(), # blank line to space content (alternative would be to add padding)

                       ### Graph ----
                       bslib::nav_panel(title = "Graphique",
                                        icon = bsicons::bs_icon("bar-chart-fill"),

                                        tags$p(class = "text-muted justify-content-center pb-2",
                                        "L'année affichée correspond à l'année la plus récente sélectionnée dans la barre latérale : ",
                                               shiny::textOutput(ns("current_year_txt"), inline = TRUE)
                                               ),


                                        bslib::layout_columns(col_widths = c(-2, 4, 4, -2),
                                                                    class = "fs-materialSwitch",

                                                                  # materialSwitch 1/2 for bar plot
                                                                  shiny::conditionalPanel(
                                                                    # Both conditions: toggle must be TRUE and the bar plot button must be selected
                                                                    condition = "output.commune",
                                                                    ns = ns,

                                                                             tags$div(
                                                                               shinyWidgets::materialSwitch(
                                                                                 inputId = ns("stacked_status"),
                                                                                 value = FALSE,
                                                                                 status = "success",
                                                                                 label = strong("Barres empilées", class = "align-middle"),
                                                                                 inline = TRUE),
                                                                               tags$span(strong("adjacentes", class = "align-middle"))
                                                                             )
                                                                  ),# End conditionalPanel 1/2

                                                                  # materialSwitch 2/2 for bar plot
                                                                  shiny::conditionalPanel(
                                                                    # Both conditions: toggle must be TRUE and the bar plot button must be selected
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


                                        # Conditional plotly (bar) ----
                                        # We'll apply a css class for padding. The renderUI can be changed to plotly render funs
                                        # since sunburst is removed
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

#' regener_needs_charts Server Functions
#'
#' @noRd
mod_regener_needs_charts_server <- function(id,
                                            inputVals,
                                            subsetData, # filtered data for communes and selected years
                                            legend_title, # for legend of barplot (either secteur/technologies)
                                            target_year, # which current year for the sunburst
                                            var_year, # 'annee'
                                            var_commune, # 'commune'
                                            var_cat, # categorical var ('secteur'/'categorie', ...)
                                            var_values, # prod/consumption kwh
                                            color_palette, # utils_helpers.R
                                            fct_table_dt_type, # table function to pass (data specific)
                                            dl_prefix,# when DL the data (mod_download_data.R) : prod_(...) or cons_(...)
                                            doc_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # renderText() latest year for UI's textOutput() ----

    output$current_year_txt <- renderText({

      req(inputVals$max_selected_regener)

      inputVals$max_selected_regener

    })

    # Preparing data ----
    # We process subsetData() in a nice, wide format for the TABLE and SUNBURST
    subsetData_wide <- reactive({

      subsetData() |>
        # this creates vars `Besoins actuels` & `Besoins optimaux` from var `besoins`
        tidyr::pivot_wider(names_from = "statut",
                           values_from = "besoins") # not passed as arguments (could be if needed)
    })


    # We process subsetData() for create_bar_plotly() below
    #  we only plot on the latest year selected in the regener_year selector

    subsetData_barplot <- reactive({

      req(inputVals$max_selected_regener)

      subsetData() |>
        dplyr::filter(etat == inputVals$max_selected_regener)

    })


    # Make debounced inputs ----
    # For barplot functions only, this avoids flickering plots when many items are selected/removed

    subsetData_barplot_d <- reactive({subsetData_barplot()}) |> shiny::debounce(debounce_plot_time)
    inputVals_communes_d <- reactive({inputVals$selectedCommunes}) |> debounce(debounce_plot_time)

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

      # Update the initialized FALSE toggle_status with the input$toggle_status
        # WIP with inputVals$energyUnit

        # ...PLOTLY BAR PLOT ----
        output$chart_1 <- plotly::renderPlotly({

          validate(need(inputVals$selectedCommunes, req_communes_phrase))

          # fct_helpers.R
          create_bar_plotly(data = subsetData_barplot_d(),
                            n_communes = length(inputVals_communes_d()),
                            var_year = var_year,
                            var_commune = var_commune,
                            unit = inputVals$energyUnit,
                            var_cat = var_cat,
                            var_values = var_values,
                            color_palette = color_palette, # defined in utils_helpers.R
                            dodge = input$stacked_status, # if T -> 'dodge', F -> 'stack'
                            free_y = input$toggle_status, # reactive(input$toggle_status)
                            legend_title = legend_title,  # links to ifelse in facet_wrap(scales = ...)
                            web_width = inputVals$web_width, # px width of browser when app starts
                            web_height = inputVals$web_height # px height of browser when app starts
                            )
        })# End renderPlotly

      # We create a div so that we can pass a class. If sunburst, the class adds left-padding. If not, barClass -> custom.css
      tags$div(class = ifelse(input$tab_plot_type == "sunburst", "sunburstClass", "barClass"),
               plotly::plotlyOutput(ns("chart_1")) |>
                 shinycssloaders::withSpinner(type = 6,
                                              color= main_color) # color defined in utils_helpers.R
      )#End div

    })# End renderUI

    # Renders DT table ----
    output$table_1 <- DT::renderDataTable({

      validate(need(inputVals$selectedCommunes, req_communes_phrase))

      fct_table_dt_type(data = subsetData_wide(), # see pivot_wider() at the top of the server
                        unit = inputVals$energyUnit,
                        DT_dom = "frtip" # remove default button in DT extensions
                        )

    })# End renderDT

    # store the data in a reactive (not sure why we can't pass subsetData() it directly, but otherwise this won't work)
    download_data <- reactive({

      # We send data in wide format too (Besoins actuels/optimaux)
      subsetData_wide() |>
        # Add the currently selected unit in the colnames (conversion is already done)
        rename_fr_colnames() |>  # fct_helpers.R
        # Add energy units in brackets for energy/power related columns
        add_colname_units(unit = inputVals$energyUnit) # fct_helpers.R

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

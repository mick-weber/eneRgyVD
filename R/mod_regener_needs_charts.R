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
      class = "d-flex justify-content-start",
      # Title
      h4(title, style = "padding-right:3vw;"),

      # Methodology button
      actionButton(ns("rgr_needs_help"),
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
                   )
      )
    ),#End div

    # utils_text_and_links.R
    title_complement,

    # Pills ----

    bslib::navset_pill(header = br(), # blank line to space content (alternative would be to add padding)

                       ### Graph ----
                       bslib::nav_panel(title = "Graphique",
                                        icon = phosphoricons::ph(title = NULL, "chart-bar"),

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
                                        icon = phosphoricons::ph(title = NULL, "table"),

                                        # Download buttons
                                        mod_download_data_ui(ns("table_download")),

                                        # rt table
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
                                            dl_prefix,# when DL the data (mod_download_data.R) : prod_(...) or cons_(...)
                                            doc_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # renderText() latest year for UI's textOutput() ----

    output$current_year_txt <- renderText({

      req(inputVals$selectedCommunes)
      req(inputVals$max_selected_regener)

      inputVals$max_selected_regener

    })

    # Preparing data ----
    # We process subsetData() in a nice, wide format for the TABLE
    subsetData_wide <- reactive({

      req(inputVals$selectedCommunes)

      subsetData() |>
        # this creates vars `Besoins actuels` & `Besoins optimaux` from var `besoins`
        tidyr::pivot_wider(names_from = "statut",
                           values_from = "besoins") # not passed as arguments (could be if needed)
    })


    # We process subsetData() for create_bar_plotly() below
    #  we only plot on the latest year selected in the regener_year selector

    subsetData_barplot <- reactive({

      req(inputVals$max_selected_regener)
      req(inputVals$selectedCommunes)

      subsetData() |>
        dplyr::filter(etat == inputVals$max_selected_regener)

    })

    # Initialize toggle free_y condition for conditionalPanel in ui
    output$toggle <- reactive({length(unique(subsetData_barplot()$commune)) > 1})

    # Initialize toggle stacked condition for conditionalPanel in ui
    output$commune <- reactive({length(unique(subsetData_barplot()$commune)) > 0})

    # We don't suspend output$toggle when hidden (default is TRUE)
    outputOptions(output, 'toggle', suspendWhenHidden = FALSE)
    outputOptions(output, 'commune', suspendWhenHidden = FALSE)

    # Render plot selectively based on radioButton above
    # Note we're nesting renderPlotly inside renderUI to access input$tab_plot_type for css class

    # renderUI ----

    output$plot_render_ui <- renderUI({

      validate(need(inputVals$selectedCommunes, req_communes_phrase))
      req(subsetData())

      # Plot logic ----

      output$chart_1 <- ggiraph::renderGirafe({

        validate(need(inputVals$selectedCommunes, req_communes_phrase))

        # Compute number of rows
        num_facets <- length(unique(subsetData()$commune))
        num_columns <- 2
        num_rows <- ceiling(num_facets / num_columns)  # Calculate rows needed for 2 columns

        # Dynamic height and width ratios (unitless)
        base_height_per_row <- 2  # Adjust height ratio per row

        # Save units passed to create_plot_ggiraph()
        height_svg <- 2 + (num_rows * base_height_per_row)  # Height grows with the number of rows
        width_svg <- 15  # Keep width static for two columns layout

        # fct is defined in fct_helpers.R
        create_plot_ggiraph(data = subsetData_barplot(),
                           n_communes = dplyr::n_distinct(subsetData_barplot()$commune),
                           var_year = var_year, # note that <statut> is passed here instead of usual <annee> or <etat>
                           var_commune = var_commune,
                           unit = inputVals$energyUnit,
                           var_cat = var_cat,
                           var_values = var_values,
                           geom = "col",
                           color_palette = color_palette, # defined in utils_helpers.R
                           dodge = input$stacked_status, # if T -> 'dodge', F -> 'stack'
                           free_y = input$toggle_status, # reactive(input$toggle_status)
                           legend_title = legend_title, # links to ifelse in facet_wrap(scales = ...)
                           height_svg = height_svg, # px width of browser when app starts
                           width_svg = width_svg # px height of browser when app starts
        )
      })# End renderGirafe

      # We create a div so that we can pass a class. If sunburst, the class adds left-padding. If not, barClass -> custom.css
               ggiraph::girafeOutput(ns("chart_1")) |>
                 shinycssloaders::withSpinner(type = 6,
                                              color= main_color) # color defined in utils_helpers.R

    })# End renderUI

    # Renders rt table ----
    output$table_1 <- DT::renderDataTable({

      validate(need(inputVals$selectedCommunes, req_communes_phrase))

      make_table_dt(data = subsetData_wide(), # see pivot_wider() at the top of the server
                    var_commune = "commune",
                    var_year = "etat",
                    var_values = c("Besoins actuels", "Besoins optimaux"),# created above in subsetData_wide()
                    var_cat = "type",
                    icons_palette = regener_icons_type,
                    unit = inputVals$energyUnit
      )

    })# End renderDT

    # store the data in a reactive (not sure why we can't pass subsetData() it directly, but otherwise this won't work)
    download_data <- reactive({

      # We send data in wide format too (Besoins actuels/optimaux)
      subsetData_wide() |>
        add_colname_unit(colnames = dplyr::contains("besoins"),
                          unit = inputVals$energyUnit) |>
        rename_columns_output()

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

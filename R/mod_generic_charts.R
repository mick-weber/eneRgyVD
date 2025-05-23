#' generic_charts UI Function
#'
#' @description A shiny Module made for generic datasets
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_generic_charts_ui <- function(id,
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
      # Title (w small padding to avoid it touching the accordion)
      h4(title, style = "padding-right:3vw;"),


      # Methodology button
      actionButton(ns("generic_data_help"),
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
    ),#End div

    # If any : pass title_complement from utils_text_and_links (beware of css styling, width and font-size)
    title_complement,

    # Pills ----

    bslib::navset_pill(id = ns("generic_mod"),
      header = br(), # blank line to space content (alternative would be to add padding)

                       ### Graph ----
                       bslib::nav_panel(title = "Graphique",
                                        icon = phosphoricons::ph(title = NULL, "chart-bar"),

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


                                        # !! Since sunburst is removed we can directly use renderPlotly
                                        ggiraph::girafeOutput(ns("generic_chart"))|>
                                          shinycssloaders::withSpinner(type = 6,
                                                                       color= main_color) # color defined in utils_helpers.R




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

#' generic_charts Server Functions
#'
#' @noRd
mod_generic_charts_server <- function(id,
                                      parent,
                                      subsetData, # passed from inputVals, bit redundant but clear
                                      # energyUnit not needed here !
                                      inputVals,
                                      var_commune,
                                      var_year,
                                      var_cat,
                                      var_values,
                                      ggiraph_geom,
                                      unit,
                                      coerce_dodge,
                                      color_palette,
                                      icons_palette = NULL,
                                      legend_title,
                                      dl_prefix = dl_prefix,
                                      doc_vars = doc_vars
                                      ){ # the non-reactive documentation file for variables description
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    ## |---------------------------------------------------------------|
    ##          DEV WORK : generic year selector
    ## |---------------------------------------------------------------|

    # test <- reactive({
    #   unique(sort(subsetData()[[var_year]]))
    # })
    #
    # observeEvent(test(), {
    #   print("Hey I've changed ! ")
    # })


    ## |---------------------------------------------------------------|
    ##          /DEV WORK
    ## |---------------------------------------------------------------|




    # Initialize toggle stacked condition for conditionalPanel in ui
    # stack/dodge widget will only show if <coerce_dodge> is FALSE OR if <var_cat> is NULL ANS if at least 1 commune is selected
    # this is because sometimes we want to force the dodge (thus remove widget, see create_plot_ggiraph logic below), and
    # sometimes we want to remove the widget when dodge/stack is pointless if there's no <var_cat> available
      output$commune <- reactive({
        if(is.null(var_cat) | coerce_dodge == TRUE | ggiraph_geom == "line"){
          FALSE  # this will never show the widget
        }else{
          length(unique(subsetData()$commune)) > 0}
      })

    # Initialize toggle free_y condition for conditionalPanel in ui
    output$toggle <- reactive({dplyr::n_distinct(subsetData()$commune) > 1 }) # Returns TRUE if more than 1 commune, else FALSE

    # We don't suspend output$toggle when hidden (default is TRUE)
    outputOptions(output, 'toggle', suspendWhenHidden = FALSE)
    outputOptions(output, 'commune', suspendWhenHidden = FALSE)

    # Plot logic ----

    output$generic_chart <- ggiraph::renderGirafe({

      validate(need(inputVals$selectedCommunes, req_communes_phrase))
      validate(need(nrow(subsetData()) > 0, message = req_communes_not_available))
      req(subsetData())

      # Compute number of rows
      num_facets <- length(inputVals$selectedCommunes)
      num_columns <- 2
      num_rows <- ceiling(num_facets / num_columns)  # Calculate rows needed for 2 columns

      # Dynamic height and width ratios (unitless)
      base_height_per_row <- 2  # Adjust height ratio per row

      # Save units passed to create_plot_ggiraph()
      height_svg <- 2 + (num_rows * base_height_per_row)  # Height grows with the number of rows
      width_svg <- 15  # Keep width static for two columns layout

      # fct is defined in fct_helpers.R
      create_plot_ggiraph(data = subsetData(),
                         n_communes = dplyr::n_distinct(subsetData()$commune),
                         var_year = var_year,
                         var_commune = var_commune,
                         unit = unit,
                         var_cat = var_cat,
                         var_values = var_values, # if more than one var_values is passed, plot only the first one.
                         geom = ggiraph_geom,
                         color_palette = color_palette, # defined in utils_helpers.R
                         dodge = if(coerce_dodge == TRUE){TRUE}else{input$stacked_status}, # if T -> 'dodge', F -> 'stack'
                         free_y = input$toggle_status, # reactive(input$toggle_status)
                         legend_title = legend_title, # links to ifelse in facet_wrap(scales = ...)
                         height_svg = height_svg, # px width of browser when app starts
                         width_svg = width_svg # px height of browser when app starts
      )
    })# End renderGirafe

    # Table logic ----

    # rt table, detailed (plot is aggregated) with both N_EGID & SRE
    output$table_1 <- DT::renderDataTable({

      validate(need(inputVals$selectedCommunes, req_communes_phrase))

      make_table_dt(data = subsetData(),
                    var_commune = var_commune,
                    var_year = var_year,
                    var_values = var_values,
                    var_cat = var_cat,
                    unit = unit,
                    icons_palette = icons_palette,
      )
    })

    # Download logic ----

    # store the data in a reactive (not sure why we can't pass subsetData() it directly, but otherwise this won't work)

    download_data <- reactive({

      # Make colnames nicelly formatted and add the current unit
      subsetData() |>
        add_colname_unit(colnames = var_values, unit = unit) |>
        rename_columns_output()
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
# mod_generic_charts_ui("generic_charts_1")

## To be copied in the server
# mod_generic_charts_server("generic_charts_1")

#' subsidies_charts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_subsidies_building_charts_ui <- function(id,
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
      actionButton(ns("subsidies_building_help"),
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

                                                 # radioGroupButtons() for tab ----
                                                 shinyWidgets::radioGroupButtons(
                                                   inputId = ns("tab_plot_type"),
                                                   label = h6(strong("Représentation")),
                                                   choices = c(`<i class='fa fa-house'></i> Par nombre de bâtiments` = "n_egid",
                                                               `<i class='fa fa-layer-group'></i> Par m<sup>2</sup> de SRE` = "sre"),
                                                   justified = TRUE,
                                                   individual = TRUE,
                                                   width = "100%"),


                                                 # materialSwitch 1/1 for bar plot
                                                 shiny::conditionalPanel(
                                                   # Both conditions: toggle must be TRUE and the bar plot button must be selected
                                                   condition = "output.toggle",
                                                   ns = ns,
                                                   tags$div(
                                                     class = "fs-materialSwitch",
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

                       # breathing
                       br(),

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

                       tags$p(HTML("Les données sont davantage détaillées que pour les graphiques ce qui permet
                                    de discerner quel type de chauffage est subventionné avec la colonne <i>Détail chauffage</i>.
                                    La colonne <i>Type de subvention</i> permet de retrouver les données des graphiques.")),

                       br(),

                       # Download module
                       mod_download_data_ui(ns("table_download")),

                       # rt table
                       DT::dataTableOutput(ns("table_1"))

      )# End nav_panel 'Table'
    )# End nav_menu
  )# End tagList
}

#' subsidies_building_charts Server Functions
#'
#' @noRd
mod_subsidies_building_charts_server <- function(id,
                                                 subsetData, # passed from inputVals, bit redundant but clear
                                                 # energyUnit not needed here !
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

      if(input$tab_plot_type == "sre"){

        # Then plot aggregated data :
        subsetData_agg() |>
          create_plot_ggiraph(
            n_communes = dplyr::n_distinct(subsetData_agg()$commune),
            var_year = "etat",
            var_commune = "commune",
            var_values = "SRE",
            var_cat = "subv_type",
            unit = "m<sup>2</sup>",
            legend_title = "",
            geom = "col",
            color_palette = subsidies_building_colors,
            dodge = FALSE, # we don't allow user to dodge w/ toggle button
            free_y = input$toggle_status, # reactive(input$toggle_status)
            height_svg = height_svg, # px width of browser when app starts
            width_svg = width_svg # px height of browser when app starts
          )

      }else if(input$tab_plot_type == "n_egid"){

        # Then plot aggregated data :
        subsetData_agg() |>
          create_plot_ggiraph(n_communes =  dplyr::n_distinct(subsetData_agg()$commune),
                             var_year = "etat",
                             var_commune = "commune",
                             var_values = "N_EGID",
                             var_cat = "subv_type",
                             unit = "Bâtiments",
                             legend_title = "",
                             geom = "col",
                             color_palette = subsidies_building_colors,
                             dodge = FALSE, # we don't allow user to dodge w/ toggle button
                             free_y = input$toggle_status, # reactive(input$toggle_status)
                             height_svg = height_svg, # px width of browser when app starts
                             width_svg = width_svg # px height of browser when app starts

          )# End create_bar_plotly
      }# End else if
    })# End renderPlot

    # Table logic ----

    # rt table, detailed (plot is aggregated) with both N_EGID & SRE
    output$table_1 <- DT::renderDataTable({

      validate(need(inputVals$selectedCommunes, req_communes_phrase))

      make_table_dt(data = subsetData(),
                    var_commune = "commune",
                    var_year = "etat",
                    var_values = c("N_EGID", "SRE"),
                    var_cat = "subv_type",
                    na_string = "Non disponible",
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




  })# End moduleServer
}# End server

## To be copied in the UI
# mod_subsidies_charts_ui("subsidies_charts_1")

## To be copied in the server
# mod_subsidies_charts_server("subsidies_charts_1")

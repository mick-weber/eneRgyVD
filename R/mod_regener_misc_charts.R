#' regener_misc_charts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_regener_misc_charts_ui <- function(id,
                                       title,
                                       title_complement){
  ns <- NS(id)
  tagList(


    tags$div(
      # Large+ screens : inline, flex layout, justified items
      #  smaller screens : row by row (default layout without fill)
      class = "d-flex justify-content-start",
      # Title
      h4(title, style = "padding-right:3vw;"),

      # Methodology button
      actionButton(ns("rgr_misc_help"),
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

    ),# End div

    # utils_text_and_links.R
    title_complement,

    bslib::navset_pill(header = br(),

    ### Table ----
    bslib::nav_panel(title = "Table",
                     icon = phosphoricons::ph(title = NULL, "table"),

                     # Download buttons
                     mod_download_data_ui(ns("table_download")),

                     # rt table
                     DT::dataTableOutput(ns("table_1"))
  )# End nav_panel
    )# End navset_pill
  )# End tagList
}

#' regener_misc_charts Server Functions
#'
#' @noRd
mod_regener_misc_charts_server <- function(id,
                                           inputVals,
                                           subsetData,
                                           dl_prefix,
                                           doc_vars){
  moduleServer(id, function(input, output, session){

    ns <- session$ns

    # Renders the rt table ----
    output$table_1 <- DT::renderDataTable({

      validate(need(inputVals$selectedCommunes, req_communes_phrase))

      make_table_dt(
        data = subsetData(),
        var_commune = "commune",
        var_year = "etat",
        var_values = NULL,
        var_cat = NULL,
        unit = NULL
      )

    })# End rt table

    # Download data : rename cols before export (also don)
      download_data <- reactive({
        subsetData() |>
          rename_columns_output() # fct_helpers.R, used in create_rg_misc_table_dt too
      })


    # Download module
    mod_download_data_server("table_download",
                             inputVals = inputVals,
                             data = download_data,
                             dl_prefix = dl_prefix,
                             doc_vars = doc_vars) # dl prefix for file name, passed into app_server.R

  })
}

## To be copied in the UI
# mod_regener_misc_charts_ui("regener_misc_charts_1")

## To be copied in the server
# mod_regener_misc_charts_server("regener_misc_charts_1")

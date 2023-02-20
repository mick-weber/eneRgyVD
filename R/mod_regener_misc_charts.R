#' regener_misc_charts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_regener_misc_charts_ui <- function(id){
  ns <- NS(id)
  tagList(

    # Download module
    mod_download_data_ui(ns("table_download")),

    # DT table
    DT::dataTableOutput(ns("table_1")) %>%
      shinycssloaders::withSpinner(color= main_color)

  )
}

#' regener_misc_charts Server Functions
#'
#' @noRd
mod_regener_misc_charts_server <- function(id,
                                           subsetData,
                                           selectedUnit, # quite irrelevant but might be useful later
                                           dl_prefix,
                                           doc_vars){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Renders the DT table ----
    output$table_1 <- DT::renderDataTable({

      create_rg_misc_table_dt(data = subsetData())

    })# End DT table


    # Download data ----
    download_data <- reactive({


      subsetData() %>% # We let the data in a long format
        # Add the currently selected unit in the colnames (conversion is already done)
        # add energy units in brackets for energy/power related columns
        rename_fr_colnames() %>%  # fct_helpers.R
        add_colname_units(unit = selectedUnit$unit_to) # fct_helpers.R, not needed actually

    })

    # module download data
    mod_download_data_server("table_download",
                             data = download_data,
                             dl_prefix = dl_prefix,
                             doc_vars = doc_vars) # dl preffix for file name, passed into app_server.R




  })
}

## To be copied in the UI
# mod_regener_misc_charts_ui("regener_misc_charts_1")

## To be copied in the server
# mod_regener_misc_charts_server("regener_misc_charts_1")
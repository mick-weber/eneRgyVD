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


    tags$div(
      # Large+ screens : inline, flex layout, justified items
      #  smaller screens : row by row (default layout without fill)
      class = "d-lg-flex justify-content-between",
      # Title
      h4("Autres informations des bâtiments"),


      # Methodology accordion
      bslib::accordion(
        class = "customAccordion", # custom.scss : lg screens = 70% width; smaller screens = 100% width
        bslib::accordion_panel(
          title = "Méthodologie",
          div(paste(generic_method_warning, # text in utils_helpers.R
                    specific_rgr_warning)),
          br(),
          actionButton(ns("rgr_misc_help"), label = "Plus de détails sur les données")
        ),
        open = FALSE)

    ),# End div

    bslib::navset_pill(header = br(),

    ### Table ----
    bslib::nav_panel(title = "Table",
                     icon = bsicons::bs_icon("table"),

                     # Download buttons
                     mod_download_data_ui(ns("table_download")),

                     # DT table
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
                                           selectedUnit, # quite irrelevant but might be useful later
                                           dl_prefix,
                                           doc_vars){
  moduleServer(id, function(input, output, session){

    ns <- session$ns

    # Renders the DT table ----
    output$table_1 <- DT::renderDataTable({

      create_rg_misc_table_dt(data = subsetData(),
                              DT_dom = "frtip" # remove default button in DT extensions
                              )

    })# End DT table

    # Download data : rename cols before export (also don)
      download_data <- reactive({
        subsetData() |>
          rename_misc_colnames() # fct_helpers.R, used in create_rg_misc_table_dt too
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

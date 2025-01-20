#' download_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_data_ui <- function(id){
  ns <- NS(id)
  tagList(

    h6(strong("Télécharger les données")),

    bslib::layout_columns(col_widths = 2, # each button will take ~15% of avail. width (2/12)

                          shiny::uiOutput(ns("download_ui_csv"), fill = TRUE),   # explicitly take 100% avail width
                          shiny::uiOutput(ns("download_ui_excel"), fill = TRUE)  # explicitly take 100% avail width

    )# End layout_columns_wrap
    )# End tagList
}

#' download_data Server Functions
#'
#' @noRd
mod_download_data_server <- function(id,
                                     data,
                                     inputVals,
                                     dl_prefix,
                                     doc_vars){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Render uiOutput buttons ----
    ## CSV button ----
    output$download_ui_csv <- shiny::renderUI({

      req(inputVals$selectedCommunes)

      shiny::downloadButton(outputId = ns("download_csv"),
                          label = "CSV", class = "btn-primary btnCustom")
      })

    ## Excel button ----
    output$download_ui_excel <- shiny::renderUI({

      req(inputVals$selectedCommunes)

      shiny::downloadButton(outputId = ns("download_excel"),
                          label = "XLSX", class = "btn-primary btnCustom") # class defined in custom.css
    })


    # Download handlers ----
    ## CSV handler ----
    output$download_csv <- downloadHandler(
      filename = paste0(dl_prefix, Sys.Date(), ".csv"),
      content = function(file){

        # Write data csv format + handle confidential NAs
        readr::write_excel_csv2(data(), file = file,
                                na = "(Confidentiel)"
                                ) # https://www.rdocumentation.org/packages/readr/versions/1.3.0/topics/write_delim
        }
    )
    ## XLSX handler ----

    # We add documentation for XLSX since it's easy (CSV would require two separate files which must be in a ZIP...)

    download_sheets <- reactive({

      list(donnees = data(),
           doc = doc_vars)
      })


    output$download_excel <- downloadHandler(
      filename = paste0(dl_prefix, Sys.Date(), ".xlsx"),
      content = function(file){

        # Write XLSX sheets + handle confidential NAs
        openxlsx::write.xlsx(x = download_sheets(),
                             file = file,
                             keepNA = TRUE,
                             na.string = "(Confidentiel)")

        }
      )
})
}




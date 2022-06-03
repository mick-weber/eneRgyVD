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
    h4("Télécharger les données"),
    shiny::downloadButton(outputId = ns("download_csv"),
                          label = "CSV", class = "dlButton"),
    shiny::downloadButton(outputId = ns("download_excel"),
                          label = "XLSX", class = "dlButton" ) # class defined in custom.css
  )
}

#' download_data Server Functions
#'
#' @noRd
mod_download_data_server <- function(id, data, dl_prefix){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # CSV handler
    output$download_csv <- downloadHandler(
      filename = paste0(dl_prefix, Sys.Date(), ".csv"),
      content = function(file){
        readr::write_delim(data, file = file, delim = ";")
      }
    )
    # XLSX handler
    output$download_excel <- downloadHandler(
      filename = paste0(dl_prefix, Sys.Date(), ".xlsx"),
      content = function(file){
        writexl::write_xlsx(data, path = file)
        }
      )
})
}

## To be copied in the UI
# mod_download_data_ui("download_data_1")

## To be copied in the server
# mod_download_data_server("download_data_1")

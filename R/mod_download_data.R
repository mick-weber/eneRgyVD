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
mod_download_data_server <- function(id, data, dl_prefix, doc_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # CSV handler
    output$download_csv <- downloadHandler(
      filename = paste0(dl_prefix, Sys.Date(), ".csv"),
      content = function(file){
        readr::write_excel_csv2(data(), file = file) # https://www.rdocumentation.org/packages/readr/versions/1.3.0/topics/write_delim
      }
    )
    # XLSX handler

    # We add documentation for XLSX since it's easy (CSV would require two separate files which must be in a ZIP...)

    download_sheets <- reactive({
      list(donnees = data(),
           doc = doc_vars)})


    output$download_excel <- downloadHandler(
      filename = paste0(dl_prefix, Sys.Date(), ".xlsx"),
      content = function(file){
        writexl::write_xlsx(download_sheets(), path = file)
        }
      )
})
}

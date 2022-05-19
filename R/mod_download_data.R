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
                          label = "XLSX", class = "dlButton" ),
    # create css class for 'dlButton' above
    # this should be put in a styles.css file once the app is more advanced
    tags$head(tags$style(".dlButton{background-color:#ffffff;} .dlButton{color: #3A862D;}"))

  )
}

#' download_data Server Functions
#'
#' @noRd
mod_download_data_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # CSV handler
    output$download_csv <- downloadHandler(
      filename = paste("prod_elec_", Sys.Date(), ".csv"),
      content = function(file){
        write.csv(data, file = file, row.names = FALSE, sep = ";")
      }
    )
    # XLSX handler
    output$download_excel <- downloadHandler(
      filename = paste("prod_elec", Sys.Date(), ".xlsx"),
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

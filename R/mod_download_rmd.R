#' download_rmd UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

# this code is outside server part, as suggested by H.Wickham below
# https://github.com/hadley/mastering-shiny/blob/master/rmarkdown-report/app.R
report_path <- tempfile(fileext = ".Rmd")
file.copy("downloadable_report.Rmd", report_path, overwrite = TRUE)


mod_download_rmd_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::downloadButton(
      outputId = ns("report"),
      label = "Générer un rapport"
    )

  )
}

#' download_rmd Server Functions
#'
#' @noRd
mod_download_rmd_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$report <- downloadHandler(
      filename = "report.html",
      content = function(file) {
        params <- list(communes = c("Morges", "Aigle"))

        id <- showNotification(
          "Rendu du rapport...",
          duration = NULL,
          closeButton = FALSE
        )
        on.exit(removeNotification(id), add = TRUE)

        rmarkdown::render(report_path,
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )

  })
}

## To be copied in the UI
# mod_download_rmd_ui("download_rmd_1")

## To be copied in the server
# mod_download_rmd_server("download_rmd_1")

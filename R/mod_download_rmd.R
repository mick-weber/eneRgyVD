#' download_rmd UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_download_rmd_ui <- function(id){
  ns <- NS(id)
  tagList(

    br(),
    br(),
    p("En cliquant sur le button ci-dessous, un rapport HTML sera automatiquement généré pour : "),
    shiny::htmlOutput(ns("selected_communes")), # htmlOutput because we style it in server
    br(),
    p("Celui-ci contient les éléments suivants :"),
    # [u]nordered [l]ist of [l]ist [i]tems
    tags$ul(
      tags$li("Des chiffres-clés (élaborer...)"),
      tags$li("Les données & graphiques relatifs à la production d'électricité."),
      tags$li("Les données & graphiques relatifs à la consommation d'électricité."),
    ),
    br(),

    # Add dynamic download button here (only if commune(s) selected)
    uiOutput(ns("dl_button")),

    br(),
    br(),
    br(),
    p("Ce type de rapport peut être ouvert avec n'importe quel navigateur web, même hors-ligne.",
      style = "color:grey;"),

  )
}

#' download_rmd Server Functions
#'
#' @noRd
mod_download_rmd_server <- function(id, inputVals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$selected_communes <- renderPrint({

      validate(need(inputVals$selectedCommunes,
               "Sélectionner au moins une commune pour générer un rapport."))

        # Nicely format the selected commune(s)
        tags$strong(knitr::combine_words(words = inputVals$selectedCommunes,
                             sep = ", ",
                             and = " et ",
                             oxford_comma = F), style = "font-size:medium;")

    })


    output$dl_button <- shiny::renderUI({

      req(inputVals$selectedCommunes)

      shiny::downloadButton(
      outputId = ns("report"),
      label = "Générer un rapport",
      class = "dlButton" # class defined in custom.css
      )
    })


    output$report <- downloadHandler(
      filename = paste0("eneRgyVD_rapport_",Sys.Date(),".html"),
      content = function(file) {

        # tempReport <- file.path(tempdir(), "downloadable_report.Rmd")
        # file.copy("downloadable_report.Rmd", tempReport, overwrite = TRUE)


        params <- list(communes = inputVals$selectedCommunes,
                       prod_data = inputVals$prod_dataset,
                       cons_data = inputVals$cons_dataset)

        id <- showNotification(
          "Rendu du rapport html. Cette opération peut prendre quelques secondes.",
          duration = NULL,
          closeButton = FALSE
        )
        on.exit(removeNotification(id), add = TRUE)

        rmarkdown::render(report_path, # utils_helpers.R !
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

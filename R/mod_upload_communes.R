#' upload_communes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_upload_communes_ui <- function(id){
  ns <- NS(id)
  tagList(


    bslib::accordion(open = FALSE,
                     class = "fs-sidebar-header", # custom.css
                     bslib::accordion_panel(title = "Fichier de communes",
                                            icon = bsicons::bs_icon("cloud-arrow-up-fill"),

                                            shiny::fileInput(ns("file_communes"),
                                                             label = p("Importer un fichier", br(),
                                                                       tags$i(
                                                                       "La première colonne doit contenir les numéros OFS.
                                                                       Séparateur point-virgule ';' uniquement. \n", # new line
                                                                              style = "font-size:0.8rem; font-weight:normal;"),

                                                                       tags$i(tags$a(href= "www/exemple_import_communes.csv",
                                                                                     target="_blank", "Télécharger un exemple",
                                                                                     download = "exemple_import_communes.csv",
                                                                                     style = "color:#3a862d; font-size:0.8rem;"))
                                                             ),
                                                             buttonLabel = "Importer...",
                                                             placeholder = ".csv",
                                                             multiple = FALSE,
                                                             accept = c("text/csv",
                                                                        "text/comma-separated-values,text/plain",
                                                                        ".csv")
                                                             )# End fileInput()
                     )# End accordion_panel
    )# End accordion()
  )# End tagList
}

#' upload_communes Server Functions
#'
#' @noRd
mod_upload_communes_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns



    input_communes_numbers <- reactive({
      req(input$file_communes)

      tryCatch(
        {
          input_file <- read.csv(input$file_communes$datapath,
                                 sep = ";")


          input_communes <- communes_names_id[names(communes_names_id) %in% input_file[[1]]] |>
            unname() # keep only commune names and no number

        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )

      return(input_communes)


    })


    return(input_communes_numbers)


  }
  )}

## To be copied in the UI
# mod_upload_communes_ui("upload_communes_1")

## To be copied in the server
# mod_upload_communes_server("upload_communes_1")

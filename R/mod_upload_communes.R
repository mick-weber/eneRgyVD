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
                                                             label = p(
                                                               # add tooltip to provide details about the format
                                                                       bslib::tooltip(
                                                                         id = ns("tooltip_upload"),
                                                                         options = list(customClass = "customTooltips"),
                                                                         trigger = tags$span(
                                                                           class = "link-primary",
                                                                           "Format requis",
                                                                           bsicons::bs_icon("info-circle")),
                                                                            shiny::markdown("La **première** colonne du fichier .csv (séparateur **point-virgule ';'** uniquement)
                                                                          doit contenir les **numéros OFS** des communes à sélectionner.
                                                                          <br> Attention aux fusions de communes !"),

                                                                       ),

                                                                       br(),

                                                                       tags$i(tags$a(href= "www/exemple_import_communes.csv",
                                                                                     target="_blank", "Télécharger un exemple",
                                                                                     download = "exemple_import_communes.csv",
                                                                                     class = "link-primary",
                                                                                     style = "font-size:1rem;"))
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

    input_communes_timed <- reactive({

      req(input$file_communes)

      tryCatch(
        {
          input_file <- utils::read.csv(input$file_communes$datapath,
                                 sep = ";")

          # communes_names_id -> utils_helpers.R ; this code implicitely removes dupe ids !
          input_communes <- communes_names_id[names(communes_names_id) %in% input_file[[1]]] |> # [[1]] -> first col is read only
            unname() # keep only commune names and no number to update selectInput in mod_inputs.R

        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )

      # Add a <chr> time stamp upfront, so that reactivity triggers even when we reload the same file !
      return(c(as.character(Sys.time()), input_communes))

    })

    # Notify how many communes are retrieved

    observeEvent(input_communes_timed(),{

      # Prepare notification
      unique_communes <- length(input_communes_timed())-1 # ignore the time stamp with -1 !

      if(unique_communes > 0){
      notify_text <- paste0("Importation de communes : ",
                             unique_communes,
                             " communes distinctes ont été importées.")
      type = "message"

      }else{

        notify_text <- paste0("Erreur : aucune commune n'a pu être importée.
                             Vérifier le fichier d'entrée ou consulter le fichier d'exemple.")
        type <- "error"
      }

    # Actually notify
    showNotification(notify_text,
                     type = type)

    })

    # return value to module call
    return(input_communes_timed) # mod_inputs.R

  }

  )}

## To be copied in the UI
# mod_upload_communes_ui("upload_communes_1")

## To be copied in the server
# mod_upload_communes_server("upload_communes_1")

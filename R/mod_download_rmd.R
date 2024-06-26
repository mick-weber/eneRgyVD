#' download_rmd UI Function
#'
#' @description This modules places the UI for the tabReport section
#'  One button to download the report, one to download all the data at once,
#'  and some textOutput to display which communes are currently selected.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import promises

mod_download_rmd_ui <- function(id){
  ns <- NS(id)
  tagList(


    bslib::layout_columns(col_widths = c(-1, 8, -3),

    # 1/2 Report ----

    br(),
    h4(strong("Générer un rapport")),
    shiny::htmlOutput(ns("selected_communes")), # htmlOutput because we style it in server

    # Add dynamic download button here (only if commune(s) selected)
    uiOutput(ns("dl_button")),

    # 2/2 Download all data----

    h4(strong("Télécharger toutes les données")),
    # moved to server side
    # p("En cliquant sur le button ci-dessous, toutes les données pour la sélection seront téléchargées : "),
    shiny::htmlOutput(ns("download_all_sentence")),
    uiOutput(ns("dl_all_button_ui"))



    )# End layout_columns
  )# End tagList
}

#' download_rmd Server Functions
#' @description Server logic to generate the report and download all the available data
#' for the currently selected communes.
#'
#' @noRd

mod_download_rmd_server <- function(id,
                                    inputVals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # 1/2 : Report ----

    # Conditionally display report details text when a commune is selected

    output$selected_communes <- renderPrint({

      validate(need(inputVals$selectedCommunes,
               "Sélectionner au moins une commune pour générer un rapport."))

      shiny::tagList(
        # Introduce the report feature
      p("En cliquant sur le button ci-dessous, un rapport HTML sera automatiquement généré pour : "),
        # Nicely format the selected commune(s)
      h4(tags$strong(knitr::combine_words(words = inputVals$selectedCommunes,
                             sep = ", ",
                             and = " et ",
                             oxford_comma = F),
                     # Green font
                     style = "color:#3A862D;")),
      p("Celui-ci contient les éléments suivants :"),
      # [u]nordered [l]ist of [l]ist [i]tems
      tags$ul(
        tags$li("Les données & graphiques relatifs à la production d'électricité."),
        tags$li("Les données & graphiques relatifs à la consommation théorique des bâtiments."),
        tags$li("D'autres informations seront progressivement ajoutées ...")
      ),# End ul
      br(),
      p("Ce type de rapport peut être ouvert avec n'importe quel navigateur web, même hors-ligne.",
        br(),
        "La génération du rapport peut prendre quelques secondes...",
        style = "color:grey;"),

      )# End tagList

    })# End renderPrint


    # Download button for rmd

    output$dl_button <- shiny::renderUI({

      req(inputVals$selectedCommunes)

      shiny::downloadButton(
      outputId = ns("report"),
      label = "Générer un rapport",
      class = "btn btn-primary btnCustomWidth" # btnCustomWidth class defined in custom.css
      )
    })

    # Download handler

    output$report <- downloadHandler(
      filename = paste0("rapport_profil_energie_",Sys.Date(),".html"),
      content = function(file) {

        # Define params to pass into rmarkdown::render() below
        params <- list(communes = inputVals$selectedCommunes,
                       web_width = 1900, # manually set
                       web_height = 1000, # manually set
                       unit = inputVals$selectedUnit,
                       elec_prod_data = inputVals$elec_prod_dataset,
                       elec_cons_data = inputVals$elec_cons_dataset,
                       regener_data_0 = inputVals$rgr_needs,
                       regener_data_1 = inputVals$rgr_1,
                       regener_data_2 = inputVals$rgr_2,
                       regener_data_3 = inputVals$rgr_misc,
                       subsidies_building_data = inputVals$subsidies_building,
                       subsidies_measure_data = inputVals$subsidies_measure)

        ## Render (future) report ----

        # Async feature to download the report

        render_async <- function(file_path,
                                 output_file,
                                 params){
          promises::future_promise(seed = NULL, # to remove warnings
                                   {
            rmarkdown::render(file_path,
                   output_file = output_file,
                   params = params,
                   envir = new.env(parent = globalenv())) # we create a new env to avoid knit conflicts

          })
        }

        # Add notification (custom.css for styling)
        showNotification(paste0("Génération du rapport..."),
                         type = "message")

        # Render report asynchronously
        render_async(file_path = report_path,
                     params = params,
                     output_file = file)

      }
    )# End downloadHandler


    # 2/2 : Download all data----


    output$download_all_sentence <- shiny::renderPrint({

      validate(need(inputVals$selectedCommunes,
                    "Sélectionner au moins une commune pour télécharger toutes les données."))

      p("En cliquant sur le button ci-dessous, toutes les données pour la sélection seront téléchargées au format xlsx : ")

    })


    output$dl_all_button_ui <- shiny::renderUI({

      req(inputVals$selectedCommunes)

      shiny::downloadButton(
        outputId = ns("download_all_excel"),
        label = "Tout télécharger",
        class = "btn btn-primary btnCustomWidth" # btnCustomWidth class defined in custom.css
      )
    })


    # XLSX handler to download all objects.
    # ! We rename colnames here too ! Check if still required !

    download_all_sheets <- reactive({


      # List all pertinent inputVals$<datasets> from mod_inputs.R
      list(
        # Cons elec renamed+units
        elec_cons = inputVals$elec_cons_dataset |>
        rename_fr_colnames() |>
        add_colname_units(unit = inputVals$selectedUnit),

        # Prod elec renamed+units
        elec_prod = inputVals$elec_prod_dataset |>
          rename_fr_colnames() |>
          add_colname_units(unit = inputVals$selectedUnit),

        # Regener renamed+units
        regener_besoins = inputVals$rgr_needs |>
          rename_fr_colnames() |>
          add_colname_units(unit = inputVals$selectedUnit),

        # Regener renamed+units
        regener_cons_use = inputVals$rgr_1 |>
          rename_fr_colnames() |>
          add_colname_units(unit = inputVals$selectedUnit),

        # Regener renamed+units
        regener_cons_aff = inputVals$rgr_2 |>
          rename_fr_colnames() |>
          add_colname_units(unit = inputVals$selectedUnit),

        # Regener misc renamed
        regener_autres = inputVals$rgr_misc |>
          rename_misc_colnames(),

        # Subsidies building
        subventions_bat = inputVals$subsidies_building |>
          rename_misc_colnames() |>
          rename_fr_colnames(),

        # Subsidies measure
        subventions_mesure = inputVals$subsidies_measure|>
          rename_misc_colnames() |>
          rename_fr_colnames()

      )
      })

    # When button (server-side) is clicked; download all sheets as xlsx
    output$download_all_excel <- downloadHandler(
      filename = paste0("profil_energie_global_", Sys.Date(), ".xlsx"),
      content = function(file){
        writexl::write_xlsx(download_all_sheets(), path = file)
      }
    )

  })
}

## To be copied in the UI
# mod_download_rmd_ui("download_rmd_1")

## To be copied in the server
# mod_download_rmd_server("download_rmd_1")

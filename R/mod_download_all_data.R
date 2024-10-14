#' download_all_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_all_data_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::accordion(open = FALSE,
                     class = "fs-sidebar-header rotatedSVG mb-2", # adds spacing with next accordion()
                     bslib::accordion_panel(title = "Tout télécharger",
                                            icon = bsicons::bs_icon("file-earmark-spreadsheet-fill"),

                                            tags$p(style = "font-size:1rem;", "Exporter toutes les données du profil climatique au format Excel"),

                                            shiny::uiOutput(ns("render_ui_button") )#, fill = TRUE)  # explicitly take 100% avail width

                     )
    )
  )
}

#' download_all_data Server Functions
#'
#' @noRd
mod_download_all_data_server <- function(id,
                                         inputVals){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ## Excel button ----
    output$render_ui_button <- shiny::renderUI({


      # Check if selected communes : if not, we show a dummy disabled button
      # could be implemented nicely with shinyjs, but this creates a dependency just for this...

      if(isTruthy(inputVals$selectedCommunes)){

        # Real download button
          shiny::downloadButton(outputId = ns("download_all_excel"),
                                label = "Télécharger",  class = "btn-sm") # class defined in custom.css
      }else{

        # Dummy download button + warning text
        tagList(
          tags$p(style = "font-size:1rem;color:red;", "Aucun territoire sélectionné !"),
          shiny::downloadButton(outputId = ns("dummy_disabled"),
                                label = "Télécharger",
                              class = "disabled btn-sm")
        )
      }

        })

    ## Prepare datasets ----
    download_all_sheets <- reactive({


      # List all pertinent inputVals$<datasets> from mod_inputs.R
      list(
        # Cons elec renamed+units
        elec_cons = inputVals$elec_cons_dataset |>
          rename_fr_colnames() |>
          add_colname_units(unit = inputVals$energyUnit),

        # Prod elec renamed+units
        elec_prod = inputVals$elec_prod_dataset |>
          rename_fr_colnames() |>
          add_colname_units(unit = inputVals$energyUnit),

        # Regener renamed+units
        regener_besoins = inputVals$rgr_needs |>
          rename_fr_colnames() |>
          add_colname_units(unit = inputVals$energyUnit),

        # Regener renamed+units
        regener_cons_use = inputVals$rgr_1 |>
          rename_fr_colnames() |>
          add_colname_units(unit = inputVals$energyUnit) |>
          add_colname_units(unit = inputVals$co2Unit),

        # Regener renamed+units
        regener_cons_aff = inputVals$rgr_2 |>
          rename_fr_colnames() |>
          add_colname_units(unit = inputVals$energyUnit) |>
          add_colname_units(unit = inputVals$co2Unit),

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
          rename_fr_colnames(),

        # NG consumption
        gaz_cons = inputVals$ng_cons_dataset |>
          rename_fr_colnames() |>
          add_colname_units(unit = inputVals$energyUnit)


        # Add other datasets with associated functions here !


      )
    })


    ## Handle download ----

    # When button (server-side) is clicked; download all sheets as xlsx
    output$download_all_excel <- downloadHandler(
      filename = paste0("profil_climatique_global_", Sys.Date(), ".xlsx"),
      content = function(file){
        writexl::write_xlsx(download_all_sheets(), path = file)
      }
    )

      })
}

## To be copied in the UI
# mod_download_all_data_ui("download_all_data_1")

## To be copied in the server
# mod_download_all_data_server("download_all_data_1")

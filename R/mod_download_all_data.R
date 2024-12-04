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

    tags$p("Tout télécharger",
           style = "font-weight:500;",
           bslib::tooltip(
             id = "tooltip_download_all",
             placement = "right",
             options = list(customClass = "customTooltips"), # custom.scss
             trigger = bsicons::bs_icon("info-circle"),
             "Exporter toutes les données du profil climatique au format Excel (un onglet par donnée)"
           ),
           style = "margin-bottom:0.5rem !important;"),

    shiny::uiOutput(ns("render_ui_button"))

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
      # could be implemented nicely with shinyjs, but this creates an extra dependency just for this...

      if(isTruthy(inputVals$selectedCommunes)){

        # Real download button

        shiny::downloadButton(outputId = ns("download_all_excel"),
                              class = "btnDownloadAll",
                              icon = icon("file-excel", class = "fa-solid me-2"),
                              label = "Prêt à l'export"
        )

      }else{

        # Dummy download button + warning text
          shiny::actionButton(inputId = ns("dummy_disabled"),
                              label = "Attente de sélection" ,
                              icon = icon("ban", class = "me-1"),
                              disabled = TRUE,
                              class = "btnDownloadAll"
          )

      }

    })

    ## Prepare datasets ----
    download_all_sheets <- reactive({


      # List all pertinent inputVals$<datasets>$... from mod_inputs.R
      # <sheet_name> = inputVals$dataset$...

      list(

        ## |---------------------------------------------------------------|
        ##          ENERGY DATASETS ----
        ## |---------------------------------------------------------------|

        # Cons elec renamed+units
        elec_cons = inputVals$energyDatasets$elec_cons |>
          rename_fr_colnames() |>
          add_colname_units(unit = inputVals$energyUnit),

        # Prod elec renamed+units
        elec_prod = inputVals$energyDatasets$elec_prod |>
          rename_fr_colnames() |>
          add_colname_units(unit = inputVals$energyUnit),

        # Regener renamed+units
        regener_besoins = inputVals$energyDatasets$regener_needs |>
          rename_fr_colnames() |>
          add_colname_units(unit = inputVals$energyUnit),

        # Regener renamed+units
        regener_cons_use = inputVals$energyDatasets$regener_cons_ae_use |>
          rename_fr_colnames() |>
          add_colname_units(unit = inputVals$energyUnit) |>
          add_colname_units(unit = inputVals$co2Unit),

        # Regener renamed+units
        regener_cons_aff = inputVals$energyDatasets$regener_cons_ae_aff |>
          rename_fr_colnames() |>
          add_colname_units(unit = inputVals$energyUnit) |>
          add_colname_units(unit = inputVals$co2Unit),

        # Regener misc renamed
        regener_autres = inputVals$energyDatasets$regener_misc |>
          rename_misc_colnames(),

        # Subsidies building
        subventions_bat = inputVals$energyDatasets$subsidies_by_building |>
          rename_misc_colnames() |>
          rename_fr_colnames(),

        # Subsidies measure
        subventions_mesure = inputVals$energyDatasets$subsidies_by_measure|>
          rename_misc_colnames() |>
          rename_fr_colnames(),

        # NG consumption
        gaz_cons = inputVals$energyDatasets$ng_cons |>
          rename_fr_colnames() |>
          add_colname_units(unit = inputVals$energyUnit),

        ## |---------------------------------------------------------------|
        ##          ADAPTATION CLIMAT DATASETS ----
        ## |---------------------------------------------------------------|

        # Canopy area
        canopee = inputVals$adaptationDatasets$taux_canopee |>
          rename_fr_colnames(),

        # Buildings natural hazards
        batiment_danger = inputVals$adaptationDatasets$batiment_danger |>
          rename_fr_colnames(),

        ## |---------------------------------------------------------------|
        ##          MOBILITY DATASETS ----
        ## |---------------------------------------------------------------|

        # EVs
        part_ve = inputVals$mobilityDatasets$part_voit_elec |>
          rename_fr_colnames(),

        # Public transportation quality
        qualite_desserte = inputVals$mobilityDatasets$qualite_desserte |>
          rename_fr_colnames()

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

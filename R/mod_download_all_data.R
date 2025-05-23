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
             trigger = phosphoricons::ph(title = NULL, "info"),
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
                              label = tagList(phosphoricons::ph(title = NULL, "file-xls", height = "1.66em"), 'Téléchargement prêt !'),
                              icon = NULL #see ph above
        )
      }else{

        # Dummy download button + warning text
          shiny::actionButton(inputId = ns("dummy_disabled"),
                              class = "btnDownloadAll",
                              label = tagList(phosphoricons::ph(title = NULL, "prohibit", height = "1.66em"), 'Attente de sélection'),
                              icon = NULL, #see ph above
                              disabled = TRUE
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

        # # Cons elec renamed+units
        elec_cons = inputVals$energyDatasets$elec_cons |>
          add_colname_unit(colnames = "consommation",
                           unit = inputVals$energyUnit) |>
          rename_columns_output(),

        # Prod elec renamed+units
        elec_prod = inputVals$energyDatasets$elec_prod |>
          add_colname_unit(colnames = c("puissance_electrique_installee",
                                        "injection",
                                        "autoconsommation",
                                        "production"),
                           unit = rep(inputVals$energyUnit, 4)) |>
          rename_columns_output(),
        #add_colname_unit(unit = inputVals$energyUnit),


        # NG consumption
        gaz_cons = inputVals$energyDatasets$ng_cons |>
          add_colname_unit(colnames = "consommation",
                           unit = inputVals$energyUnit) |>
          rename_columns_output(),

        # Regener needs renamed+units
        regener_besoins = inputVals$energyDatasets$regener_needs |>
          add_colname_unit(colnames = "besoins",
                           unit = inputVals$energyUnit) |>
          rename_columns_output(),

        # Regener renamed+units
        regener_cons_year = inputVals$energyDatasets$regener_cons_ae_year |>
          add_colname_unit(colnames = "consommation",
                           unit = inputVals$energyUnit) |>
          add_colname_unit(colnames = "co2_direct",
                           unit = inputVals$co2Unit) |>
          rename_columns_output(),

        # Regener renamed+units
        regener_cons_use = inputVals$energyDatasets$regener_cons_ae_use |>
          add_colname_unit(colnames = "consommation",
                            unit = inputVals$energyUnit) |>
          add_colname_unit(colnames = "co2_direct",
                            unit = inputVals$co2Unit) |>
          rename_columns_output(),

        # Regener renamed+units
        regener_cons_aff = inputVals$energyDatasets$regener_cons_ae_aff |>
          add_colname_unit(colnames = "consommation",
                           unit = inputVals$energyUnit) |>
          add_colname_unit(colnames = "co2_direct",
                           unit = inputVals$co2Unit) |>
          rename_columns_output(),

        # Regener misc renamed
        regener_autres = inputVals$energyDatasets$regener_misc |>
          # add_colname_unit() |>
          rename_columns_output(),

        # Subsidies building
        subventions_bat = inputVals$energyDatasets$subsidies_by_building |>
          # add_colname_unit() |>
          rename_columns_output(),

        # Subsidies measure
        subventions_mesure = inputVals$energyDatasets$subsidies_by_measure |>
          rename_columns_output(),

        ## |---------------------------------------------------------------|
        ##          ADAPTATION CLIMAT DATASETS ----
        ## |---------------------------------------------------------------|

        # Canopy area
        canopee = inputVals$adaptationDatasets$surface_canopee |>
          # add_colname_unit() |>
          rename_columns_output(),

        # Buildings natural hazards
        batiment_danger = inputVals$adaptationDatasets$batiment_danger |>
          # add_colname_unit() |>
          rename_columns_output(),

        ## |---------------------------------------------------------------|
        ##          MOBILITY DATASETS ----
        ## |---------------------------------------------------------------|

        # Electric vehicles
        part_ve = inputVals$mobilityDatasets$part_voit_elec |>
          # add_colname_unit() |>
          rename_columns_output(),

        # Motorisation
        taux_motorisation = inputVals$mobilityDatasets$taux_motorisation |>
          add_colname_unit(colnames = "taux_motorisation", unit = "v/1000hab.") |>
          rename_columns_output(),

        # Public transportation quality
        qualite_desserte = inputVals$mobilityDatasets$qualite_desserte |>
          # add_colname_unit() |>
          rename_columns_output()

      )
    })


    ## Handle download ----

    # When button (server-side) is clicked; download all sheets as xlsx
    output$download_all_excel <- downloadHandler(
      filename = paste0("profil_climatique_global_", Sys.Date(), ".xlsx"),
      content = function(file){
        #writexl::write_xlsx(download_all_sheets(), path = file)
        openxlsx::write.xlsx(x = download_all_sheets(),
                             file = file,
                             keepNA = TRUE,
                             na.string = "(Confidentiel ou non-disponible)")
      }
    )

  })
}

## To be copied in the UI
# mod_download_all_data_ui("download_all_data_1")

## To be copied in the server
# mod_download_all_data_server("download_all_data_1")

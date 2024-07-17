library(shiny)
library(tidyverse)

source("./R/fct_helpers.R")
source("./R/utils_helpers.R")
source("./R/mod_download_data.R")

## |---------------------------------------------------------------|
##          fake loading data in utils_helpers.R
## |---------------------------------------------------------------|
generic_data <- readRDS("P:/DIREN/11 POLITIQUE ENERG/11.08 Indicateurs et stat/12 Dashboard/EnergyVD/upgrade_profil_climatique/examples_donnees_climatique/part_canop.rds") |>
  dplyr::mutate(annee = 2022) |>
  rename(commune = libelle,
         generic_value = surface_de_canopee_3m_dans_espace_bati_percent) |>
  select(commune, generic_value, annee)


generic_data |> save(file = "dummy_climat.rda")

## |---------------------------------------------------------------|
##          Make fake generic datatable function
## |---------------------------------------------------------------|
create_generic_table_dt <- function(data,
                                    DT_dom = "frtip"){

  data |>
    DT::datatable(options = list(dom = DT_dom))
}


## |---------------------------------------------------------------|
##          Make faked argument objects
## |---------------------------------------------------------------|
subsetData <- canop |> filter(commune %in% c("Aclens", "Lausanne") & annee == 2022)
selectedUnit = NULL


## |---------------------------------------------------------------|
##          UI calling module
## |---------------------------------------------------------------|

ui <- fluidPage(

  bslib::page_navbar(

    bslib::nav_menu("Climat",
                    icon = icon("earth"),


                    # cons elec ----
                    bslib::nav_panel("Générique",

  mod_generic_charts_ui(id = "test",
                        title = "Donnée générique climat",
                        title_complement = NULL # not needed
  )
  )
)
))

## |---------------------------------------------------------------|
##          SERVER calling module
## |---------------------------------------------------------------|
server <- function(input, output, session){
 mod_generic_charts_server(
   id = "test",
   inputVals = ,
   subsetData = subsetData , # filtered data for communes and selected years
   selectedUnit = , # unit selected in mod_unit_converter.R
   legend_title = "Generic legend title", # for legend of barplot (either secteur/technologies)
   var_year = "annee", # 'annee'
   var_commune = "commune", # 'commune'
   var_rank_2 = NULL, # categorical var ('secteur'/'categorie', NULL, ...)
   var_values = "surface_canopee", # prod/consumption kwh
   color_palette = NULL, # utils_helpers.R
   fct_table_dt_type = create_generic_table_dt, # table function to pass (data specific)
   dl_prefix = "OLA",# when DL the data (mod_download_data.R) : prod_(...) or cons_(...)
   doc_vars = NULL
 )
}

shinyApp(ui = ui, server= server)

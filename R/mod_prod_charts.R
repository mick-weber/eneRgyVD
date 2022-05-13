#' prod_charts UI Function
#'
#' @description A shiny Module which produces the different chart for the tabProd tab of the app. It
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_prod_charts_ui <- function(id){
  ns <- NS(id)
  tagList(

    # WIP
    plotly::plotlyOutput(ns("chart_1"), height = "900px", width = "1200px") %>%
      shinycssloaders::withSpinner(color= main_color), # color defined in utils_helpers.R

  )
}

#' prod_charts Server Functions
#'
#' @noRd
mod_prod_charts_server <- function(id, inputVals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Build reactive dataset here
    # We could do it in app_server.R but since the dataset is only used in this tab for now...
     subset_elec_prod <- reactive({

       validate(
         need(inputVals$selectedCommunes, "Sélectionner au moins une commune !")
         )

       req(inputVals$min_selected, inputVals$max_selected, inputVals$techs_selected)

       elec_prod_communes %>%
         dplyr::filter(commune %in% inputVals$selectedCommunes)  %>%
         dplyr::filter(annee >= inputVals$min_selected,
                       annee <= inputVals$max_selected) %>%
         dplyr::filter(categorie_diren %in% inputVals$techs_selected)


     }) # End reactive()

     # We could debounce the subset dataframe here to avoid trigerring multiple renderPlotly calls
     subset_elec_prod_d <- subset_elec_prod %>% debounce(0)

     # According to which radioGroupButton() is selected through inputVals$prod_plot_type
     # We plot one or the other type of plot (2/3 done)
    # note: observeEvent is required since we need to acces prod_plot_type in a reactive consumer



       # Render facetted plotly barplot
       output$chart_1 <- plotly::renderPlotly({

         if(inputVals$prod_plot_type == "bar"){
        # PLOTLY BAR PLOT
           create_bar_plotly(data = subset_elec_prod_d()) # defined in fct_helpers.R

         } else if(inputVals$prod_plot_type == "sunburst"){
        # PLOTLY SUNBURST PLOT
           create_sunburst_plotly(data = subset_elec_prod_d(), # created just above
                                  year_var = "annee", # var name
                                  year = inputVals$max_selected, # takes the upper range of year slider
                                  values_tot = "production_totale", # var name
                                  rank_1 = "commune", # var name
                                  rank_2 = "categorie_diren", # var name
                                  rank_3_1 = "injection_totale", rank_3_2 = "autoconso_totale") # var names pivotted
         }

         # PLOTLY AREA PLOT TO BE ADDED LATER

       })# End renderPlotly()
  }) # End ModuleServer()
} # End server

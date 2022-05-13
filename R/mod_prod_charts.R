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
    plotly::plotlyOutput(ns("chart_1")) %>%
      shinycssloaders::withSpinner(color= main_color), # color defined in utils_helpers.R
    plotly::plotlyOutput(ns("chart_2")) %>%
      shinycssloaders::withSpinner(color= main_color) # color defined in utils_helpers.R

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


     })

     # We can debounce the subset dataframe here to avoid trigerring multiple renderPlotly calls

     subset_elec_prod_d <- subset_elec_prod %>% debounce(0)


     output$chart_1 <- plotly::renderPlotly({

       create_bar_plotly(data = subset_elec_prod_d()) # defined in fct_helpers.R

    })


     output$chart_2 <- plotly::renderPlotly({

       create_sunburst_plotly(data = subset_elec_prod_d(),
                              year_var = "annee", # var name
                              year = inputVals$max_selected, # takes the upper range of year slider
                              values_tot = "production_totale", # var name
                              rank_1 = "commune", # var name
                              rank_2 = "categorie_diren", # var name
                              rank_3_1 = "injection_totale", rank_3_2 = "autoconso_totale") # var name

     })


  })
}

## To be copied in the UI
# mod_prod_charts_ui("prod_charts_1")

## To be copied in the server
# mod_prod_charts_server("prod_charts_1")

# remove afterwards
# elec_prod_communes %>%
#   filter(commune %in% c("Morges", "Lausanne")) %>%
#   ggplot()+
#   geom_col(aes(x = annee, y = production_totale, fill = categorie_diren), position = "dodge")+
#   ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = "'", accuracy = 1))+
#   ggplot2::scale_fill_manual(values = colors_categories)+ # palette defined in utils_helpers.R
#   ggplot2::labs( x = "", y = "kWh")+
#   ggplot2::theme_bw()+
#   ggplot2::facet_wrap(facets = vars(commune))

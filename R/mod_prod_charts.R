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
      shinycssloaders::withSpinner(color= main_color) # defined in utils_helpers.R


  )
}

#' prod_charts Server Functions
#'
#' @noRd
mod_prod_charts_server <- function(id, inputVals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Build reactive dataset (or save it directly in inputVals maybe)
     subset_elec_prod <- reactive({

       req(inputVals$selectedCommunes)

       elec_prod_communes %>%
         filter(commune %in% inputVals$selectedCommunes)

     })

     # We can debounce the subset dataframe here to avoid trigerring multiple renderPlotly calls

     subset_elec_prod_d <- subset_elec_prod %>% debounce(0)


     output$chart_1 <- plotly::renderPlotly({

       # for later : put in utils_helpers.R under : create_plotly_prod_bar(subset_elec_prod_d())

       ggplot <- subset_elec_prod_d() %>%
         ggplot2::ggplot()+
         ggplot2::geom_col(aes(x = as.factor(annee), y = production_totale, fill = categorie_diren),
                           position = "dodge")+
         ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = "'", accuracy = 1))+
         ggplot2::scale_fill_manual(values = colors_categories)+ # palette defined in utils_helpers.R
         ggplot2::labs( x = "", y = "kWh")+
         ggplot2::theme_bw()+
         ggplot2::facet_wrap(facets = vars(commune))

        # turn to plotly object
       ggplot %>% plotly::ggplotly()

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

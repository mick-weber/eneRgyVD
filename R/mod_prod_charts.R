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
    # TABSETS for better readability of plot / table
    shiny::tabsetPanel(
      shiny::tabPanel(title = "Graphique",

                      # radioGroupButtons() for tabProd ----

                      shiny::wellPanel(style = "background: white",
                                       shinyWidgets::radioGroupButtons(
                                         inputId = ns("tabProd_plot_type"),
                                         label = "Sélection du type de graphique",
                                         choices = c(`<i class='fa fa-bar-chart'></i>` = "bar",
                                                     `<i class='fa fa-pie-chart'></i>` = "sunburst"),
                                         justified = TRUE,
                                         width = "25%"),

                                       # prettyToggle
                                       shiny::conditionalPanel(
                                         # Both conditions: toggle must be TRUE and the bar plot button must be selected
                                         condition = "output.toggle && input.tabProd_plot_type == 'bar'",
                                         ns = ns,

                                         shinyWidgets::prettyToggle(
                                           inputId = ns("toggle_status"),
                                           label_on = "Axe des ordonnées libéré !",
                                           label_off = "Libérer l'axe des ordonnées ?",
                                           bigger = T,
                                           shape = "curve",
                                           animation = "pulse")
                                       )# End conditionalPanel
                      ),# End wellPanel

                      # # breathing
                      # br(),

                      # Conditional plotly (bar/sunburst) ----
                      plotly::plotlyOutput(ns("chart_1"), width = "1150px", height = "auto") %>%
                        shinycssloaders::withSpinner(color= main_color), # color defined in utils_helpers.R


      ),# End tabPanel 'Graphique'

      shiny::tabPanel(title = "Table",
                      column(width = 11,
                             # breathing
                             br(),
                             # Download module
                             mod_download_data_ui(ns("table_download")),
                             # breathing
                             br(),
                             # DT table
                             DT::dataTableOutput(ns("table_1")) %>%
                               shinycssloaders::withSpinner(color= main_color)
                      )# End column
      )# End tabPanel 'Table'
    )# End tabsetPanel
  )# End tagList
}

#' prod_charts Server Functions
#'
#' @noRd
mod_prod_charts_server <- function(id, inputVals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Reactive dataset
    # We could do it in app_server.R but since the dataset is only used in this tab for now...

     subset_elec_prod <- reactive({

        # explicitely require communes to be selected
       validate(
         need(inputVals$selectedCommunes, "Sélectionner au moins une commune pour générer un résultat.")
         )
       # waiting on these to get initialized (renderUIs)
       req(inputVals$min_selected,
           inputVals$max_selected,
           inputVals$techs_selected,
           inputVals$prod_dataset)

    # prod by commune filtered with commune pickerInput(), years from sliderInput(), techs from pickerInput()
       # TESTING, WE DONT NEED THIS IF WE TAKE DIRECTLY INPUTVALS$PROD_DATASET
       # elec_prod_communes %>%
       # dplyr::filter(commune %in% inputVals$selectedCommunes)  %>%

       inputVals$prod_dataset %>%
       dplyr::filter(annee >= inputVals$min_selected,
                       annee <= inputVals$max_selected) %>%
         dplyr::filter(categorie_diren %in% inputVals$techs_selected)

     }) # End reactive()

     # We CAN debounce the subset dataframe here to avoid trigerring multiple renderPlotly calls
     subset_elec_prod_d <- subset_elec_prod %>% debounce(0)

     # Initialize toggle condition for conditionalPanel in ui
     output$toggle <- reactive({
       # req(inputVals$selectedCommunes)
       length(inputVals$selectedCommunes) > 1 # Returns TRUE if more than 1 commune, else FALSE
     })
      # We don't suspend output$toggle when hidden (default is TRUE)
      # https://stackoverflow.com/questions/35136029/hide-show-outputs-shiny-r
     outputOptions(output, 'toggle', suspendWhenHidden = FALSE)

     # Render plot selectively based on radioButton above
     observe({

       if(input$tabProd_plot_type == "bar"){

        # Update the initialized FALSE toggle_status with the input$toggle_status

         # PLOTLY BAR PLOT
         output$chart_1 <- plotly::renderPlotly({

           # fct is defined in fct_helpers.R
           create_bar_plotly(data = subset_elec_prod_d(),
                             free_y = reactive(input$toggle_status)) # links to ifelse in facet_wrap(scales = ...)
         })# End renderPlotly
       }# End if
       else if(input$tabProd_plot_type == "sunburst"){

         # PLOTLY SUNBURST PLOT
         output$chart_1 <- plotly::renderPlotly({
           create_sunburst_plotly(data = subset_elec_prod_d(), # created just above
                                  year_var = "annee", # var name
                                  year = inputVals$max_selected, # takes the upper range of year slider
                                  values_tot = "production_totale", # var name
                                  rank_1 = "commune", # var name
                                  rank_2 = "categorie_diren", # var name
                                  rank_3_1 = "injection_totale", rank_3_2 = "autoconso_totale") # var names pivotted
         })# End renderPlotly
       }# End else if
     })# End observe

     mod_download_data_server("table_download", data = subset_elec_prod_d())


       output$table_1 <- DT::renderDataTable({

         create_table_dt(data = subset_elec_prod_d())

       })# End renderDT
  }) # End ModuleServer
} # End server

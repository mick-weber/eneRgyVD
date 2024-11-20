#' regener_charts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_regener_cons_charts_ui <- function(id,
                                       title,
                                       title_complement){
  ns <- NS(id)
  tagList(

    # Header ----
    # div to handle title + accordion layout
    tags$div(
      # Large+ screens : inline, flex layout, justified items
      #  smaller screens : row by row (default layout without fill)
      class = "d-lg-flex justify-content-between",
      # Title
      h4(title, style = "padding-right:10px;"),

      # Methodology accordion
      bslib::accordion(
        class = "customAccordion", # custom.scss : lg screens = 70% width; smaller screens = 100% width
        bslib::accordion_panel(
          title = "Méthodologie",
          div(paste(generic_method_warning, # text in utils_helpers.R
                    specific_rgr_warning)),
          br(),
          actionButton(ns("rgr_cons_help"), label = "Plus de détails sur les données")
        ),
        open = FALSE)

    ),#End div

    # utils_text_and_links.R
    title_complement,

    bslib::navset_pill(
      id = ns("tabset_rgr_cons"),
      header = br(), # blank line to space content (alternative would be to add padding)

    # nav_panel for better readability of plot / table

    bslib::nav_panel(title = "Graphique",
                     icon = bsicons::bs_icon("bar-chart-fill"),

 tags$p(class = "text-muted justify-content-center pb-2",
   "L'année affichée correspond à l'année la plus récente sélectionnée dans la barre latérale : ",
      shiny::textOutput(ns("current_year_txt"), inline = TRUE)),

                      # radioGroupButtons() for tab ----

 bslib::layout_column_wrap(width = 1/3, # 33% of avail. width
                           class = "d-flex align-items-end",

                      shinyWidgets::radioGroupButtons(
                        inputId = ns("tab_plot_type"),
                        label = h6(strong("Affichage de la consommation")),
                        choices = c(`<i class='fa fa-fire'></i> Par usage` = "use",
                                    `<i class='fa fa-house'></i> Par affectation` = "aff"),
                        justified = TRUE,
                        individual = TRUE,
                        width = "100%")

 ),# End layout_column_wrap

                      # Alluvial plot ----
                      # ggalluvial plot
                        uiOutput(ns("plot_render_ui")) |>
                              shinycssloaders::withSpinner(type = 6, color= main_color)

                      # shiny::plotOutput(ns("chart_alluvial"), height = "auto") |>
                         # shinycssloaders::withSpinner(type = 6,
                         #                              color= main_color) # color defined in utils_helpers.R


      ),# End nav_panel 'Graphique'

      bslib::nav_panel(title = "Table",
                       icon = bsicons::bs_icon("table"),

                       bslib::layout_column_wrap(width = 1/3, # each col = 33% of avail. width

                             # radioGroupButtons() for tab ----
                             shinyWidgets::radioGroupButtons(
                               inputId = ns("tab_table_type"),
                               label = h6(strong("Affichage de la consommation")),
                               choices = c(`<i class='fa fa-fire'></i> Par usage` = "use",
                                           `<i class='fa fa-house'></i> Par affectation` = "aff"),
                               justified = TRUE,
                               individual = TRUE,
                               width = "100%")
                             ),# End layout_column_wrap

                       br(),
                             # Download module
                             mod_download_data_ui(ns("table_download")),

                             # DT table
                       DT::dataTableOutput(ns("table_1")) |>
                         shinycssloaders::withSpinner(type = 6,
                                                      color= main_color) # color defined in utils_helpers.R
      )# End nav_panel 'Table'
    )# End navset_pill
  )# End tagList
}

#' regener_charts Server Functions
#'
#' @noRd
mod_regener_cons_charts_server <- function(id,
                                      inputVals, # for facet height
                                      subset_rgr_cons_1, # conso->use
                                      subset_rgr_cons_2, # conso->aff
                                      doc_vars = doc_vars,
                                      dl_prefix
                                      ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # renderText() latest year for UI's textOutput() ----

    output$current_year_txt <- renderText({

      req(inputVals$selectedCommunes)
      inputVals$max_selected_regener

    })

    # debounce + filter for the latest year for the plots (not the table) ----
    # subset_rgr_cons_1 -> subset_rgr_cons_1_last_year

    subset_rgr_cons_1_last_year <- reactive({

      req(inputVals$max_selected_regener)
      req(inputVals$selectedCommunes)

      subset_rgr_cons_1() |>
        dplyr::filter(etat == inputVals$max_selected_regener)

    })

    # subset_rgr_cons_2 -> subset_rgr_cons_2_last_year

    subset_rgr_cons_2_last_year <- reactive({

      req(inputVals$max_selected_regener)
      req(inputVals$selectedCommunes)

      subset_rgr_cons_2() |>
        dplyr::filter(etat == inputVals$max_selected_regener)

    })


    # tabs and renderPlot() ----

    output$plot_render_ui <- renderUI({

      validate(need(inputVals$selectedCommunes, req_communes_phrase))


      if(input$tab_plot_type == "use"){
      output$chart_1 <- renderPlot({

            subset_rgr_cons_1_last_year()  |>
              lump_alluvial_factors(var_commune = "commune",
                                    var_flow = "consommation",
                                    var_from = "ae",
                                    var_to = "usage") |>
              create_alluvial_chart(var_commune = "commune",
                                    var_flow = "consommation",
                                    var_from = "ae",
                                    label_from = "Consommation",
                                    var_to = "usage",
                                    label_to = "Usage")

      }, height = ifelse(test = is.null(dplyr::n_distinct(subset_rgr_cons_1_last_year()$commune)),
                         yes = 400, # if no commune selected, default width = 400
                         no = ceiling(dplyr::n_distinct(subset_rgr_cons_1_last_year()$commune)/2)*400))

      }else if(input$tab_plot_type == "aff"){


        output$chart_1 <- renderPlot({
          subset_rgr_cons_2_last_year()  |>
            lump_alluvial_factors(var_commune = "commune",
                                  var_flow = "consommation",
                                  var_from = "ae",
                                  var_to = "affectation") |>
            create_alluvial_chart(var_commune = "commune",
                                  var_flow = "consommation",
                                  var_from = "ae",
                                  label_from = "Consommation",
                                  var_to = "affectation",
                                  label_to = "Affectation")
        }, height = ifelse(test = is.null(dplyr::n_distinct(subset_rgr_cons_2_last_year()$commune)),
                           yes = 400, # if no commune selected, default width = 400
                           no = ceiling(dplyr::n_distinct(subset_rgr_cons_2_last_year()$commune)/2)*400))

      }

      # We create a div so that we can optionnaly pass a class
      tags$div(
        style = "margin-top:2vh;", # otherwise alluvial plots are too close to input$tab_plot_type
        plotOutput(ns("chart_1")) |>
          shinycssloaders::withSpinner(type = 6,
                                       color= main_color) # color defined in utils_helpers.R
      )

      })# End renderUI


    # tabs and renderTable ----
    observe({

      if(input$tab_table_type == "use"){

        # Alluvial table 1 : conso -> usage
        output$table_1 <- DT::renderDataTable({

            validate(need(inputVals$selectedCommunes, req_communes_phrase))

            create_regener_table_dt(data = subset_rgr_cons_1(),
                                    energy_unit = inputVals$energyUnit,
                                    co2_unit = inputVals$co2Unit,
                                    DT_dom = "frtip" # remove default button in DT extensions
                                    )
        })

      }# End if tab...
      else if(input$tab_table_type == "aff"){

        # Alluvial table 2 : conso -> aff
        output$table_1 <- DT::renderDataTable({

          validate(need(inputVals$selectedCommunes, req_communes_phrase))

          create_regener_table_dt(data = subset_rgr_cons_2(),
                                  energy_unit = inputVals$energyUnit,
                                  co2_unit = inputVals$co2Unit,
                                  DT_dom = "frtip" # remove default button in DT extensions
          )
        })
      }# End elseif

    })# End observe

    # store the data in a reactive, according to which table type is selected

    download_data <- reactive({

      if(input$tab_table_type == "use"){

        subset_rgr_cons_1() |> # from app_server.R
          # Add the currently selected unit in the colnames (conversion is already done)
          rename_fr_colnames() |> # fct_helpers.R
          add_colname_units(unit = inputVals$energyUnit) |>
          add_colname_units(unit = inputVals$co2Unit) # fct_helpers.R

          } else if(input$tab_table_type == "aff"){

        subset_rgr_cons_2() |> # from app_server.R
          # Add the currently selected unit in the colnames (conversion is already done)
          rename_fr_colnames() |>  # fct_helpers.R
          add_colname_units(unit = inputVals$energyUnit) |>
          add_colname_units(unit = inputVals$co2Unit) # fct_helpers.R

      }

    })

    # Module to download DT table data
    mod_download_data_server("table_download",
                             inputVals = inputVals,
                             data = download_data, # see if() above
                             dl_prefix = dl_prefix,
                             doc_vars = doc_vars) # dl prefix for file name, passed into app_server.R


  })
}

## To be copied in the UI
# mod_regener_cons_charts_ui("regener_charts_1")

## To be copied in the server
# mod_regener_cons_charts_server("regener_charts_1")

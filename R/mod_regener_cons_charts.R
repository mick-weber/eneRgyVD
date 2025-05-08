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
      class = "d-flex justify-content-start",
      # Title
      h4(title, style = "padding-right:3vw;"),

      # Methodology button
      actionButton(ns("rgr_cons_help"),
                   class = "btnCustom",
                   label = tags$span(style = "font-weight:500;",
                                     "Source et méthode",
                                     bslib::tooltip(
                                       id = ns("tooltip_data_help"),
                                       placement = "right",
                                       options = list(customClass = "customTooltips"), # custom.scss
                                       trigger = phosphoricons::ph(title = NULL, "info"),
                                       generic_method_warning # utils_text_and_links.R
                                     )
                   ))
    ),#End div

    # utils_text_and_links.R
    title_complement,

    bslib::navset_pill(
      id = ns("tabset_rgr_cons"),
      header = br(), # blank line to space content (alternative would be to add padding)

      # nav_panel for better readability of plot / table

      bslib::nav_panel(title = "Graphique",
                       icon = phosphoricons::ph(title = NULL, "chart-bar"),

                       # Disclaimer that only the latest year is used : only for use/year panels below, not yearly (makes no se)
                       shiny::conditionalPanel(
                         glue::glue("['use', 'aff'].includes(input['{ns('tab_plot_type')}'])"),
                                               tags$p(class = "text-muted justify-content-center pb-2",
                                                      "L'année affichée correspond à l'année la plus récente sélectionnée dans la barre latérale : ",
                                                      shiny::textOutput(ns("current_year_txt"), inline = TRUE))
                       ),

                       bslib::layout_columns(col_widths = c(-2, 8, -2),
                                             class = "fs-materialSwitch",

                                             # materialSwitch 2/2 for bar plot
                                             shiny::conditionalPanel(
                                               # Toggle condition in server must be reached
                                               condition = "output.toggle",
                                               ns = ns,

                                               tags$div(
                                                 shinyWidgets::materialSwitch(
                                                   inputId = ns("toggle_status"),
                                                   value = FALSE,
                                                   label = strong("Axe vertical commun", class = "align-middle"),
                                                   status = "success",
                                                   inline = TRUE),
                                                 tags$span(strong("indépendant", class = "align-middle"))
                                               )# End tags$div
                                             )# End 2nd conditionalPanel

                       ),#End layout_column_wrap() for buttons

                       # radioGroupButtons() for tab ----

                       bslib::layout_column_wrap(width = 1/3, # 33% of avail. width
                                                 class = "d-flex align-items-end",

                                                 shinyWidgets::radioGroupButtons(
                                                   inputId = ns("tab_plot_type"),
                                                   label = h6(strong("Affichage de la consommation")),
                                                   choices = c(
                                                     `<i class='fa fa-fire'></i> Par année` = "year",
                                                     `<i class='fa fa-fire'></i> Par usage` = "use",
                                                     `<i class='fa fa-house'></i> Par affectation` = "aff"),
                                                   justified = TRUE,
                                                   individual = TRUE,
                                                   width = "100%")

                       ),# End layout_column_wrap

                       # Dynamic plot rendering
                       uiOutput(ns("plot_render_ui")) |>
                         shinycssloaders::withSpinner(type = 6, color= main_color)


      ),# End nav_panel 'Graphique'

      bslib::nav_panel(title = "Table",
                       icon = phosphoricons::ph(title = NULL, "table"),

                       bslib::layout_column_wrap(width = 1/3, # each col = 33% of avail. width

                                                 # radioGroupButtons() for tab ----
                                                 shinyWidgets::radioGroupButtons(
                                                   inputId = ns("tab_table_type"),
                                                   label = h6(strong("Affichage de la consommation")),
                                                   choices = c(
                                                     `<i class='fa fa-fire'></i> Par année` = "year",
                                                     `<i class='fa fa-fire'></i> Par usage` = "use",
                                                     `<i class='fa fa-house'></i> Par affectation` = "aff"),
                                                   justified = TRUE,
                                                   individual = TRUE,
                                                   width = "100%")
                       ),# End layout_column_wrap

                       br(),
                       # Download module
                       mod_download_data_ui(ns("table_download")),

                       # rt table
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
                                           subset_rgr_cons_year,# conso->year
                                           subset_rgr_cons_use, # conso->use
                                           subset_rgr_cons_aff, # conso->aff
                                           doc_vars = doc_vars,
                                           dl_prefix
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Initialize toggle free_y condition for conditionalPanel in ui
    output$toggle <- reactive({length(unique(subset_rgr_cons_year()$commune)) > 1})
    # We don't suspend output$toggle when hidden (default is TRUE)
    outputOptions(output, 'toggle', suspendWhenHidden = FALSE)

    # renderText() latest year for UI's textOutput() ----

    output$current_year_txt <- renderText({

      req(inputVals$selectedCommunes)
      inputVals$max_selected_regener

    })

    # filter for the latest year for the plots (not the table) ----

    subset_rgr_cons_use_last_year <- reactive({

      req(inputVals$max_selected_regener)
      req(inputVals$selectedCommunes)
      req(subset_rgr_cons_use())

      subset_rgr_cons_use() |>
        dplyr::filter(etat == inputVals$max_selected_regener)

    })

    # subset_rgr_cons_aff -> subset_rgr_cons_aff_last_year

    subset_rgr_cons_aff_last_year <- reactive({

      req(inputVals$max_selected_regener)
      req(inputVals$selectedCommunes)
      req(subset_rgr_cons_aff())

      subset_rgr_cons_aff() |>
        dplyr::filter(etat == inputVals$max_selected_regener)

    })



    output$plot_render_ui <- renderUI({
      validate(need(inputVals$selectedCommunes, req_communes_phrase))  # Ensure input is valid
      # Make sure the data is available
      req(subset_rgr_cons_use_last_year())
      req(subset_rgr_cons_aff_last_year())
      req(subset_rgr_cons_year())

      # Compute number of rows
      num_facets <- length(unique(subset_rgr_cons_year()$commune))
      num_columns <- 2
      num_rows <- ceiling(num_facets / num_columns)  # Calculate rows needed for 2 columns

      # Dynamic height and width for svg/px
      base_height_per_row_svg <- 2  # Adjust height ratio per row
      base_height_per_row_px <- 300  # Adjust height ratio per row

      # height/width for svg (ggiraph) and px (alluvial)
      height_svg <- 2 + (num_rows * base_height_per_row_svg)  # Height grows with the number of rows
      width_svg <- 15  # Keep width static for two columns layout

      height_px <- num_rows * base_height_per_row_px


      ## renderGirafe ----
      output$bar_plot <- ggiraph::renderGirafe({

        req(subset_rgr_cons_year())

        create_plot_ggiraph(data = subset_rgr_cons_year(),
                            n_communes = dplyr::n_distinct(subset_rgr_cons_year()$commune),
                            var_year = "etat",
                            var_commune = "commune",
                            unit = inputVals$energyUnit,
                            var_cat = "ae",
                            var_values = "consommation",
                            geom = "col",
                            color_palette = colors_ae, # defined in utils_helpers.R
                            dodge = FALSE, # if T -> by default we stack, no user interaction allowed
                            free_y = input$toggle_status,
                            legend_title = NULL,
                            height_svg = height_svg, # see above, heuristic
                            width_svg = width_svg # see above, heuristic
        )
      })



      ## renderPlot 1/2 ----
      output$alluvial_use_plot <- renderPlot({

        req(subset_rgr_cons_use_last_year())

        subset_rgr_cons_use_last_year()  |>
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

      }, height = height_px)

      ## renderPlot 2/2 ----
      output$alluvial_aff_plot <- renderPlot({

        req(subset_rgr_cons_aff_last_year())

        subset_rgr_cons_aff_last_year()  |>
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
      }, height = height_px)

      #Plot rendering based on tab selection

      if(input$tab_plot_type == "year"){
        ggiraph::girafeOutput(ns("bar_plot"))
      } else if(input$tab_plot_type == "use"){
        tags$div(style = "margin-top:2vh;",
                 plotOutput(ns("alluvial_use_plot"))
        )
      } else if(input$tab_plot_type == "aff"){
        tags$div(style = "margin-top:2vh;",
                 plotOutput(ns("alluvial_aff_plot"))
        )
      }
    })


    # tabs and renderTable ----
    observe({

      if(input$tab_table_type == "year"){

        # Bar plot table 1 : conso by year
        output$table_1 <- DT::renderDataTable({

          validate(need(inputVals$selectedCommunes, req_communes_phrase))

          make_table_dt(data = subset_rgr_cons_year(),
                        var_commune = "commune",
                        var_year = "etat",
                        var_values = c("consommation", "pct_commune", "co2_direct"),
                        var_cat = "ae",
                        unit = list(inputVals$energyUnit, NULL, inputVals$co2Unit),
                        icons_palette = regener_icons
          )
        })
      }
      else if(input$tab_table_type == "use"){

        # Alluvial table 1 : conso -> usage
        output$table_1 <- DT::renderDataTable({

          validate(need(inputVals$selectedCommunes, req_communes_phrase))

          make_table_dt(data = subset_rgr_cons_use(),
                        var_commune = "commune",
                        var_year = "etat",
                        var_values = c("consommation", "co2_direct"),
                        var_cat = "ae",
                        unit = list(inputVals$energyUnit, inputVals$co2Unit),
                        icons_palette = regener_icons
          )
        })

      }# End if tab...
      else if(input$tab_table_type == "aff"){

        # Alluvial table 2 : conso -> aff
        output$table_1 <- DT::renderDataTable({

          validate(need(inputVals$selectedCommunes, req_communes_phrase))

          make_table_dt(data = subset_rgr_cons_use(),
                        var_commune = "commune",
                        var_year = "etat",
                        var_values = c("consommation", "co2_direct"),
                        var_cat = "ae",
                        unit = c(inputVals$energyUnit, inputVals$co2Unit),
                        icons_palette = regener_icons
          )
        })
      }# End elseif

    })# End observe

    # store the data in a reactive, according to which table type is selected

    download_data <- reactive({

      if(input$tab_table_type == "year"){

        subset_rgr_cons_year() |> # from app_server.R
          # Add the currently selected unit in the colnames (conversion is already done)
          add_colname_unit(colnames = c("consommation", "co2_direct"),
                           unit = c(inputVals$energyUnit,inputVals$co2Unit )) |>
          rename_columns_output()

      }else if(input$tab_table_type == "use"){

        subset_rgr_cons_use() |> # from app_server.R
          # Add the currently selected unit in the colnames (conversion is already done)
          add_colname_unit(colnames = c("consommation", "co2_direct"),
                           unit = c(inputVals$energyUnit,inputVals$co2Unit )) |>
          rename_columns_output()

      } else if(input$tab_table_type == "aff"){

        subset_rgr_cons_aff() |> # from app_server.R
          add_colname_unit(colnames = c("consommation", "co2_direct"),
                           unit = c(inputVals$energyUnit,inputVals$co2Unit )) |>
          rename_columns_output()
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

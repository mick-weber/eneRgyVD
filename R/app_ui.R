#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd



app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboardPlus::dashboardPage(
      skin = "green",

      # Header ----
      shinydashboard::dashboardHeader(title = "eneRgy VD : prototype",
                                      titleWidth = 300),

      # Sidebar ----
      shinydashboard::dashboardSidebar(
        width = 300,
        shinydashboard::sidebarMenu(id = "sidebarMenu",
                                    shinydashboard::menuItem("Sélection des communes", tabName = "tabMap", icon = icon("globe", lib = "glyphicon")),
                                    shinydashboard::menuItem("Consommation", tabName = "tabConso", icon = icon("flash", lib = "glyphicon")),
                                    shinydashboard::menuItem("Production", tabName = "tabProd", icon = icon("flash", lib = "glyphicon")),
                                    shinydashboard::menuItem("Rapport", tabName = "tabReport", icon = icon("file", lib = "glyphicon"))
        ),

        ## SelectInput module ----
        # Renders the sidebar inputs dynamically according to which tab is selected
        mod_inputs_ui("inputs_1")
      ),

      # Body ----
      shinydashboard::dashboardBody(
        # Change background of dashboardBody() with html
        tags$head(tags$style(HTML('.content-wrapper {background-color:white;}'))),

        ## tabMap ----
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            tabName = "tabMap",
            fluidRow(
              column(width = 8,
                     # Leaflet select map
                     leaflet::leafletOutput("map", height = "500px", width = "800px") %>%
                       shinycssloaders::withSpinner(color=main_color), # defined in utiles_helpers.R
              ),# End column

              # Next to leaflet map
              column(width = 4,
                     # TEST AREA FOR SHINYDASHBOARDPLUS NESTED BOX WITH DESCRIPTIONBLOCKS
                     shinydashboardPlus::box(
                       solidHeader = FALSE,
                       title = "Le canton en quelques chiffres",
                       background = NULL,
                       width = 12,
                       status = "danger",
                       footer = fluidRow(
                         column(
                           width = 6,
                           shinydashboardPlus::descriptionBlock(
                             numberColor = "green",
                             number = "17%",
                             header = "$35,210.43",
                             text = "TOTAL REVENUE"
                           )
                         ),
                         column(
                           width = 6,
                           shinydashboardPlus::descriptionBlock(
                             header = "1200",
                             text = "GOAL COMPLETION"
                           )
                         )
                       )
                     )



                     # / TEST AREA FOR SHINYDASHBOARDPLUS NESTED BOX WITH DESCRIPTIONBLOCKS

              )# End column
            ),#End fluidRow

            # Below leaflet map
            fluidRow(
              column(width = 12,
                     shinydashboard::valueBox(10 * 2, "New Orders", icon = icon("plug"), width = 2),
                     shinydashboard::valueBox(10 * 2, "New Orders", icon = icon("flash"), width = 2),
                     shinydashboard::valueBox(10 * 2, "New Orders", icon = icon("battery-half"), width = 2),
                     shinydashboard::valueBox(10 * 2, "New Orders", icon = icon("battery-full"), width = 2)
              )
            )
          ),# End tabItem
          ## tabConso
          shinydashboard::tabItem(
            tabName = "tabConso",
            h2("À venir: consommation d'électricité par commune et secteur")
          ),
          ## tabProd
          shinydashboard::tabItem(
            tabName = "tabProd",
            h2("Production d'électricité par commune"),
            br(),
            # CALL MODULE
            mod_prod_charts_ui("prod_chart1")
          ),
          shinydashboard::tabItem(
            tabName = "tabReport",
            h2("À venir: téléchargement d'un rapport spécifique à une commune")
          )
        )


      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "eneRgyVD"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}


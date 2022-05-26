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
      shinydashboardPlus::dashboardHeader(title = "eneRgy VD : prototype",
                                      titleWidth = 300,

      leftUi = mod_unit_converter_ui("unit_converter")

      ),

      # Sidebar ----
      shinydashboard::dashboardSidebar(
        width = 300,
        shinydashboard::sidebarMenu(id = "sidebarMenu",
                                    shinydashboard::menuItem("Sélection des communes", tabName = "tabMap", icon = icon("globe", lib = "glyphicon")),
                                    shinydashboard::menuItem("Consommation", tabName = "tabConso", icon = icon("flash", lib = "glyphicon")),
                                    shinydashboard::menuItem("Production", tabName = "tabProd", icon = icon("flash", lib = "glyphicon")),
                                    shinydashboard::menuItem("Rapport", tabName = "tabReport", icon = icon("file", lib = "glyphicon")),
                                    shinydashboard::menuItem("À propos", tabName = "tabInfo", icon = icon("info-sign", lib = "glyphicon"))
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
                     # Title for select map
                     h4("Sélectionnez des communes sur la carte ou dans la zone latérale"),
                     # Leaflet select map
                     leaflet::leafletOutput("map", height = "500px", width = "800px") %>%
                       shinycssloaders::withSpinner(color=main_color), # defined in utiles_helpers.R
              ),# End column

              # Next to leaflet map
              column(width = 4,
                     # Module for collapsible VD box
                     mod_vd_collapse_box_ui("vd_box")

              )# End column
            ),#End fluidRow
            # breathing
            br(),
            # Below leaflet map ; for selected communes statistics
            fluidRow(
              column(width = 12,
                     # Module for calling communes boxes
                    mod_communes_boxes_ui("communes_box")
            )
          )# End fluidRow
          ),# End tabItem
          ## tabConso
          shinydashboard::tabItem(
            tabName = "tabConso",
            h2("À venir: consommation d'électricité par commune et secteur")
          ),# End tabItem
          ## tabProd
          shinydashboard::tabItem(
            tabName = "tabProd",
            h2("Production d'électricité par commune"),
            # breathing
            br(),

            # Module for producing prod charts
            mod_prod_charts_ui("prod_chart1")

          ),# End tabItem

          shinydashboard::tabItem(
            tabName = "tabReport",
            h2("À venir: téléchargement d'un rapport spécifique à une commune")
          ),# End tabItem

          shinydashboard::tabItem(
            tabName = "tabInfo",
            h2("Documentation sur l'application à venir")
          )# End tabItem
        )# End tabItems
      )# End dashboardBody
    )# End dashboardPage
  )# End tagList
}# End UI

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


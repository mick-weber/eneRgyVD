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
            # Leaflet select map
            leaflet::leafletOutput("map", height = "900px", width = "1200px") %>%
              shinycssloaders::withSpinner(color=main_color) # defined in utiles_helpers.R
            ),
        ## tabConso
          shinydashboard::tabItem(
            tabName = "tabConso",
            h2("Will be implemented later on")
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
          h2("Download report coming soon")
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


#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinydashboard
#' @noRd



app_ui <- function(request) {

  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic
    shinydashboardPlus::dashboardPage(
      skin = "green",
      # Header ----
      shinydashboardPlus::dashboardHeader(title = "eneRgy VD (v0.2)",
                                          titleWidth = 300,
                                          shinydashboard::dropdownMenu(type = "notifications", badgeStatus = NULL,
                                                                       icon = icon("calendar", lib = "glyphicon"),
                                                                       headerText = "Dernières mises à jour",
                                                                       shinydashboard::notificationItem(icon = icon("upload", lib = "glyphicon"),
                                                                                                        status = "info",
                                                                                                        text = "Juin 2022 : données production électricité 2021"),
                                                                       shinydashboard::notificationItem(icon = icon("upload", lib = "glyphicon"),
                                                                                                        status = "info",
                                                                                                        text = "Juin 2022 : données consommation électricité 2020")
                                          ),# End dropdownMenu 'updates'
                                          shinydashboard::dropdownMenu(type = "notifications", badgeStatus = NULL,
                                                                       icon = icon("envelope", lib = "font-awesome"),
                                                                       headerText = "Retours et suggestions",
                                                                       shinydashboard::notificationItem(text = "Nous contacter par e-mail",
                                                                                                        href = paste0("mailto:", mail_address), # defined in utils_helpers.R
                                                                                                        icon = icon("envelope", lib = "font-awesome"), status = "info")
                                          ),# End dropdownMenu 'contact'

                                           # Add unit converter drop-down next to the app's title (left)
                                           leftUi = mod_unit_converter_ui("unit_converter")

      ),# End dashboardHeader

      # Sidebar ----
      shinydashboard::dashboardSidebar(
        width = 300,
        shinydashboard::sidebarMenu(id = "sidebarMenu",
                                    shinydashboard::menuItem("Carte des communes", tabName = "tabMap", icon = icon("globe", lib = "glyphicon")),
                                    shinydashboard::menuItem("Consommation", tabName = "tabCons", icon = icon("flash", lib = "glyphicon")),
                                    shinydashboard::menuItem("Production", tabName = "tabProd", icon = icon("flash", lib = "glyphicon")),
                                    shinydashboard::menuItem("Rapport", tabName = "tabReport", icon = icon("file", lib = "glyphicon")),
                                    shinydashboard::menuItem("À propos", tabName = "tabInfo", icon = icon("info-sign", lib = "glyphicon"))
        ),# End sidebarMenu

        ## SelectInput module ----
        # Renders the sidebar inputs dynamically according to which tab is selected
       mod_inputs_ui("inputs_1")

      ),

      # Body ----
      shinydashboard::dashboardBody(

        ## tabMap ----
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            tabName = "tabMap",
            fluidRow(
              column(width = 8,
                     # Title for select map
                     h4("Sélectionnez des communes sur la carte ou dans la zone latérale"),
                     # Leaflet select map
                      leaflet::leafletOutput("map") %>% # height defined in custom.css #map
                        shinycssloaders::withSpinner(color=main_color), # defined in utiles_helpers.R



              ),# End column

              # Next to leaflet map
              column(width = 4,
                     # Module for collapsible VD box
                     mod_collapse_stats_box_ui("vd_box"),

                     # Dynamic module for collapsible communes box (condition in renderUI)
                     uiOutput("communes_box")

              )# End column
            ),#End fluidRow
            # breathing
            br(),
            # Below leaflet map area
            fluidRow(
              column(width = 12,
                     # Later : add Module to call more statistics ?

                    # ...

            )
          )# End fluidRow
          ),# End tabItem
          ## tabCons ----
          shinydashboard::tabItem(
            tabName = "tabCons",
            h4(strong("Consommation d'électricité par commune")),
            # breathing
            br(),

            mod_elec_charts_ui("consumption_charts")

          ),# End tabItem
          ## tabProd ----
          shinydashboard::tabItem(
            tabName = "tabProd",
            h4(strong("Production d'électricité par commune")),
            # breathing
            br(),
            # Module for producing prod charts

            mod_elec_charts_ui("production_charts")

          ),# End tabItem
          ## tabReport ----
          shinydashboard::tabItem(
            tabName = "tabReport",
            # Tab's title is here, but the rest of text is in the module to ease the reading of app_ui.R
            h4(strong("Générer un rapport énergétique")),

            # Module for producing rmd report based on downloadable_report.Rmd. Renders the button.
            mod_download_rmd_ui("rmd")

          ),# End tabItem
          ## tabInfo ----
          shinydashboard::tabItem(
            tabName = "tabInfo",
            # Tab's title is here, but the rest of text is in the module to ease the reading of app_ui.R
            h4(strong("À propos")),
            br(),

            # Module for producing the tab content (html mostly)

            mod_about_the_app_ui("about")

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


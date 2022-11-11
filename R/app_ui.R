#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinydashboard bs4Dash
#' @noRd

app_ui <- function(request) {

  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic
    bs4Dash::dashboardPage(
      dark = NULL,
      # Custom theme
      freshTheme = eneRgy_theme, # utils_helpers.R
      # Header ----
      bs4Dash::dashboardHeader(titleWidth = 300, status = "primary",
                               title = bs4Dash::dashboardBrand(
                                 title = "eneRgy VD",
                                 image = NULL, # add path to logo if needed
                                 color = "primary",
                                 href = link_diren # utils_helpers.R
                               ),

                               leftUi =  mod_unit_converter_ui("unit_converter"),



                               rightUi = shiny::tagList(bs4Dash::dropdownMenu(type = "notifications", badgeStatus = NULL,
                                                                              icon = icon("calendar", lib = "glyphicon"),
                                                                              headerText = "Dernières mises à jour",
                                                                              bs4Dash::notificationItem(icon = icon("upload", lib = "glyphicon"),
                                                                                                        status = "info",
                                                                                                        text = "06.22: données prod élec 2021"),
                                                                              bs4Dash::notificationItem(icon = icon("upload", lib = "glyphicon"),
                                                                                                        status = "info",
                                                                                                        text = "06.22: données conso élec 2020")
                               ),# End dropdownMenu 'updates'
                               bs4Dash::dropdownMenu(type = "notifications", badgeStatus = NULL,
                                                     icon = icon("envelope", lib = "font-awesome"),
                                                     headerText = "Retours et suggestions",
                                                     bs4Dash::notificationItem(text = "Nous contacter par e-mail",
                                                                               href = paste0("mailto:", mail_address), # defined in utils_helpers.R
                                                                               icon = icon("envelope", lib = "font-awesome"), status = "info")
                               )# End dropdownMenu 'contact'
                               )# End tagList (rightUi)


                               ),# End dashboardHeader

      # Sidebar ----
      bs4Dash::dashboardSidebar(
        id = "tabs",
        minified = FALSE,
        width = 300,
        bs4Dash::sidebarMenu(id = "sidebarMenu",
                             ## menuItems ----
                                    bs4Dash::menuItem("Carte des communes", tabName = "tabMap", icon = icon("globe", lib = "glyphicon")),
                                    bs4Dash::menuItem("Consommation", tabName = "tabCons", icon = icon("flash", lib = "glyphicon")),
                                    bs4Dash::menuItem("Production", tabName = "tabProd", icon = icon("flash", lib = "glyphicon")),
                                    bs4Dash::menuItem("Chaleur bâtiments", tabName = "tabRegener", icon = icon("fire", lib = "glyphicon")),
                                    bs4Dash::menuItem("Rapport", tabName = "tabReport", icon = icon("file", lib = "glyphicon")),
                                    bs4Dash::menuItem("À propos", tabName = "tabInfo", icon = icon("info-sign", lib = "glyphicon"))
        ),# End sidebarMenu

        ## Widgets module ----
        # Renders the sidebar inputs dynamically according to which tab is selected
       mod_inputs_ui("inputs_1"),

       # Sidebar footer ()
       tags$footer(
       tags$a(
         "@DGE-DIREN 2022 (v.02)",
         target = "_blank",
         href = link_diren
       ),
       style = "position: absolute; bottom:0; width: 80%; color: white; text-align: center;")




      ),# End dashboardSidebar

      # Footer ----

      # None, messes the layout of plots

      # Body ----
      bs4Dash::dashboardBody(

        ## tabMap ----
        bs4Dash::tabItems(
          bs4Dash::tabItem(
            tabName = "tabMap",
            fluidRow(
              column(width = 8,
                     # Title for select map
                     h4(strong("Sélectionnez des communes sur la carte ou dans la zone latérale")),
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
          bs4Dash::tabItem(
            tabName = "tabCons",
            h4(strong("Consommation d'électricité par commune")),
            # breathing
            br(),

            mod_elec_charts_ui("consumption_charts")

          ),# End tabItem
          ## tabProd ----
          bs4Dash::tabItem(
            tabName = "tabProd",
            h4(strong("Production d'électricité par commune")),
            # breathing
            br(),

            # Module for producing prod charts
            mod_elec_charts_ui("production_charts")

          ),# End tabItem

          ## tabRegener
          bs4Dash::tabItem(
            tabName = "tabRegener",
            h4(strong("Consommation théorique des bâtiments")),
            # breathing
            br(),

            # Module for producing regener plots
            mod_regener_charts_ui("regener_charts")


          ),# End tabItem

          ## tabReport ----
          bs4Dash::tabItem(
            tabName = "tabReport",
            # Tab's title is here, but the rest of text is in the module to ease the reading of app_ui.R
            h4(strong("Générer un rapport énergétique")),

            # Module for producing rmd report based on downloadable_report.Rmd. Renders the button.
            mod_download_rmd_ui("rmd")

          ),# End tabItem
          ## tabInfo ----
          bs4Dash::tabItem(
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


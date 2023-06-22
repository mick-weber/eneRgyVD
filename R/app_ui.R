#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {


  shiny::tagList(

    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Shinybrowser identifies the web browser for use in app_server.R/fct_helpers.R
    shinybrowser::detect(),

    # Your application UI logic
    bs4Dash::dashboardPage(
      dark = NULL,
      # Custom theme
      freshTheme = eneRgy_theme, # utils_helpers.R
      # Header ----
      bs4Dash::dashboardHeader(titleWidth = 300, status = "primary",
                               sidebarIcon = icon("arrows-left-right-to-line"),

                               # Title                   # Title is custom made inside sidebar directly

                               ## leftUi ----

                               leftUi = tags$li(
                                 style = NULL,
                                 h4(strong("Profil énergétique des communes vaudoises"),
                                    class = "adaptiveTitle"),
                                 class = "dropdown"# required
                               ),

                               ## rightUi ----

                               rightUi =

                                 ### unit converter ----

                               shiny::tagList(
                                 mod_unit_converter_ui("unit_converter"),

                                 ### notifs & contact ----

                                 tags$li(
                                   class = "dropdown", # required

                                   #### notifs dropdownmenu
                                   bs4Dash::dropdownMenu(type = "notifications", badgeStatus = NULL, href = NULL,
                                                         icon = icon("calendar", lib = "glyphicon"),
                                                         headerText = "Dernières mises à jour",

                                                         tags$div(class = "disableLink", # wrap in a div to pass the disableLink class (css)

                                                                  purrr::pmap(notif_msg, .f = ~ bs4Dash::notificationItem(icon = shiny::icon(glue::glue("{..1}")),
                                                                                                                          status = glue::glue("{..2}"),
                                                                                                                          text = glue::glue("{..3}"))

                                                                  )# End pmap
                                                         )# End div

                                                         # bs4Dash::notificationItem(icon = icon("upload", lib = "glyphicon"),
                                                         #                           status = "info",
                                                         #                           text = "03.23: Ajout données production + chaleur"),
                                                         # bs4Dash::notificationItem(icon = icon("upload", lib = "glyphicon"),
                                                         #                           status = "info",
                                                         #                           text = "03.23: Mise en ligne du profil")
                                   ),# End dropdownMenu notifs

                                   #### contact dropdownmenu

                                   bs4Dash::dropdownMenu(type = "notifications", badgeStatus = NULL,
                                                         icon = icon("envelope", lib = "font-awesome"),
                                                         headerText = "Retours et suggestions",
                                                         bs4Dash::notificationItem(text = "Nous contacter par e-mail",
                                                                                   href = paste0("mailto:", mail_address), # defined in utils_helpers.R
                                                                                   icon = icon("envelope", lib = "font-awesome"), status = "info")
                                    )# End dropdownMenu 'contact'
                                 )# End tags$li
                               )# End tagList (rightUi)


      ),# End dashboardHeader

      # Sidebar ----
      bs4Dash::dashboardSidebar(
        id = "tabs",
        minified = FALSE,
        width = sidebar_width, # utils_helpers.R
        bs4Dash::sidebarMenu(id = "sidebarMenu",

                             ## Title (custom) ----

                             tags$div(class = "titleClassDGE",
                                      tags$a(
                                        href = link_dge,
                                        target = "_blank",
                                        "Direction générale de l'environnement"
                                      )),
                             tags$div(class = "titleClassDIREN",
                                      tags$a(href = link_diren,
                                             target = "_blank",
                                             "Direction de l'énergie",
                                      )
                             ),# End tags$div for title

                             br(), # between title and menuItems

                             ## menuItems ----


                             ### Sélection subMenu ----
                             h6("Sélection", style = "color:white;"), #menuItem header

                             bs4Dash::menuItem("Carte des communes", tabName = "tabMap", icon = icon("earth-americas")),

                             ### Données subMenu ----
                             h6("Données", style = "color:white;"), #menuItem header

                             # !!CONS_ELEC removed!! # bs4Dash::menuItem("Consommation", tabName = "tabCons", icon = icon("bolt")),
                             bs4Dash::menuItem("Production", tabName = "tabProd", icon = icon("bolt")),
                             bs4Dash::menuItem("Chaleur bâtiments", tabName = "tabRegener", icon = icon("fire"),
                                               bs4Dash::menuSubItem("Besoins", tabName = "tabRegenerNeeds"),
                                               bs4Dash::menuSubItem("Consommation", tabName = "tabRegenerCons"),
                                               bs4Dash::menuSubItem("Autres", tabName = "tabRegenerMisc")),

                             ### Divers subMenu ----
                             h6("Divers", style = "color:white;"), #menuItem header

                             bs4Dash::menuItem("Rapport", tabName = "tabReport", icon = icon("file-code")),
                             bs4Dash::menuItem("À propos", tabName = "tabInfo", icon = icon("circle-info")),
                             tags$div(
                               class = "separateMenu",
                               bs4Dash::menuItem("Guide utilisateur", tabName = "tabScribe", icon = icon("question-circle"))
                             )
        ),# End sidebarMenu

        ## Widgets module ----
        # Renders the sidebar inputs dynamically according to which tab is selected
        mod_inputs_ui("inputs_1"),

        # Sidebar footer ()
        tags$footer(
          tags$a(
            "@DGE-DIREN 2023",
            target = "_blank", # new tab
            href = link_diren
          ), # css below should be in custom.css
          style = "font-size:0.85rem;position: fixed;height:20px;bottom: 0;left: 250px;width:calc(100% - 250px);color: darkgrey;text-align: right;background-color: transparent;"
        )


      ),# End dashboardSidebar

      # Body ----
      bs4Dash::dashboardBody(

        ## tabMap incl.StatBoxes ----
        bs4Dash::tabItems(
          bs4Dash::tabItem(
            tabName = "tabMap",
            fluidRow(
              column(width = 8,
                     # Title for select map
                     h5(strong("Sélectionnez des communes sur la carte ou dans la zone latérale puis naviguez dans les onglets")),
                     # Leaflet select map
                     leaflet::leafletOutput("map") %>% # height defined in custom.css #map
                       shinycssloaders::withSpinner(type = 6,
                                                    color = main_color), # defined in utiles_helpers.R



              ),# End column

              # Next to leaflet map
              column(width = 4,

                     br(), # to align boxes below with leafletOutput

                     # Module for collapsible VD box
                     uiOutput("vd_box"),

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
          # !!CONS_ELEC removed!! # bs4Dash::tabItem(
          # !!CONS_ELEC removed!! #   tabName = "tabCons",
          # !!CONS_ELEC removed!! #   fluidRow(h4(strong("Consommation d'électricité par commune")),
          # !!CONS_ELEC removed!! #            HTML('&nbsp;'), HTML('&nbsp;'),
          # !!CONS_ELEC removed!! #            actionButton("cons_data_help", label = "Consulter la méthodologie", icon = icon("info-sign", lib = "glyphicon"), class = "infoButton")), # no label
          # !!CONS_ELEC removed!! #   # breathing
          # !!CONS_ELEC removed!! #   br(),
          # !!CONS_ELEC removed!! #   # to about_the_app module (test)
          # !!CONS_ELEC removed!! #
          # !!CONS_ELEC removed!! #
          # !!CONS_ELEC removed!! #   mod_elec_charts_ui("consumption_charts")
          # !!CONS_ELEC removed!! #
          # !!CONS_ELEC removed!! # ),# End tabItem

          ## tabProd ----
          bs4Dash::tabItem(
            tabName = "tabProd",
            fluidRow(h4(strong("Production d'électricité par commune")),
                     HTML('&nbsp;'), HTML('&nbsp;'),
                     HTML('&nbsp;'),HTML('&nbsp;'),
                     bs4Dash::box(title = actionButton("prod_data_help", # custom.css padding header
                                                       label = "Consulter la méthodologie",
                                                       icon = icon("info-sign", lib = "glyphicon"),
                                                       class = "infoButton"),
                                  collapsed = TRUE,
                                  width = 6,
                                  paste(generic_method_warning, # text in utils_helpers.R
                                        specific_prod_elec_warning)
                     ),
            ), #End fluidRow
            h5("Important: les données d'une catégorie pour laquelle la commune recense moins de trois installations sont retirées."),

            # breathing
            br(),

            # Module for producing prod charts
            mod_elec_charts_ui("production_charts")

          ),# End tabItem

          ## tabRegener ----
          ### Needs subitem ----

          bs4Dash::tabItem(
            tabName = "tabRegenerNeeds",

            fluidRow(h4(strong("Besoins théoriques des bâtiments")),
                     HTML('&nbsp;'), HTML('&nbsp;'),
                     HTML('&nbsp;'),HTML('&nbsp;'),
                     bs4Dash::box(title = actionButton("rg_needs_help", # custom.css padding header
                                                       label = "Consulter la méthodologie",
                                                       icon = icon("info-sign", lib = "glyphicon"),
                                                       class = "infoButton"),
                                  collapsed = TRUE,
                                  width = 6,
                                  paste(generic_method_warning, # text in utils_helpers.R
                                        specific_rgr_warning)
                     ),
            ),

            # breathing
            br(),

            # Module for producing regener need plots
            mod_regener_needs_charts_ui("regener_needs")


          ),# End tabItem


          ### Cons subitem ----
          bs4Dash::tabItem(
            tabName = "tabRegenerCons",

            fluidRow(h4(strong("Consommation théorique des bâtiments")),
                     HTML('&nbsp;'), HTML('&nbsp;'),
                     HTML('&nbsp;'),HTML('&nbsp;'),
                     bs4Dash::box(title = actionButton("rg_cons_help", # custom.css padding header
                                                       label = "Consulter la méthodologie",
                                                       icon = icon("info-sign", lib = "glyphicon"),
                                                       class = "infoButton"),
                                  collapsed = TRUE,
                                  width = 6,
                                  paste(generic_method_warning, # text in utils_helpers.R
                                        specific_rgr_warning)
                     ),
            ),

            # breathing
            br(),

            # Module for producing regener plots
            mod_regener_cons_charts_ui("regener_cons")

          ),# End tabItem

          ### Misc subitem----
          bs4Dash::tabItem(
            tabName = "tabRegenerMisc",

            fluidRow(h4(strong("Autres informations des bâtiments")),
                     HTML('&nbsp;'), HTML('&nbsp;'),
                     HTML('&nbsp;'),HTML('&nbsp;'),
                     bs4Dash::box(title = actionButton("rg_misc_help", # custom.css padding header
                                                       label = "Consulter la méthodologie",
                                                       icon = icon("info-sign", lib = "glyphicon"),
                                                       class = "infoButton"),
                                  collapsed = TRUE,
                                  width = 6,
                                  paste(generic_method_warning, # text in utils_helpers.R
                                        specific_rgr_warning)
                     ),
            ),

            # breathing
            br(),

            # Module for producing regener plots
            mod_regener_misc_charts_ui("regener_misc")


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

          ),# End tabItem

          ## tabScribe ----
          bs4Dash::tabItem(
            tabName = "tabScribe",
            # h4(strong("")) # No title --> inside the Scribe

            br(),
            # Simple iframe embed (no module needed here)
            tags$iframe(src="https://scribehow.com/embed/Guide_utilisateur_du_profil_energetique__EgMdqpUuS52wylthvDs2Xg?as=scrollable", #&skipIntro=true
                        width = "100%", height = "800",
                        frameborder='no' # no unnecessary padding
            )
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
      app_title = "Profil énergie VD"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}


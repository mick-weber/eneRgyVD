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

    # Navbar page
    bslib::page_navbar(

      id = "nav", # conditionalPanels will refer to this as input.nav == <condition>
      fillable = TRUE,

      # This script (see app_server.R too) closes nav_menu's after being redirected by a click (2s delay)
      # It is used for redirects from app_server.R to mod_about_the_app.R
      # ! This may cause some unexpected behaviour since all dropdown-menu are targeted by the script !
      # see here : https://github.com/rstudio/bslib/issues/621
      tags$script("
        Shiny.addCustomMessageHandler(
          'toggleDropdown',
          function toggleDropdown(msg) {
            $('.dropdown-menu').removeClass('show')
          });
        "
      ),

      # Footer
      footer = p("DGE-DIREN @ 2024", class = "fw-lighter",
                 style = "position: fixed;bottom:0;right:1%;font-size:1rem;"),
      # Custom theme
      theme = profil_theme, # utils_helpers.R
      # Title
      title = strong("Profil énergétique des communes vaudoises",
                     class = "h4 text-wrap",
                     style = "font-family: 'Arial', sans-serif;"),
      # Browser title
      # window_title = "Profil énergie VD",
      # Sidebar
      sidebar = bslib::sidebar(

        class = "shadow", # add some shadow
        width = "300px",
          bg =  "#343A40",

        # add brand
        tags$div(
        tags$img(src="www/vd-logo-black.svg", class = "customLogo"),
        br(),
        "Direction de l'énergie",
        hr()
        ),# End div brand,

        mod_inputs_ui("inputs_1")

      ),# End sidebar()

      # Navigation panels ----
      ## Carte ----
      bslib::nav_panel("Carte",
                       icon = icon("map"),
                       bslib::layout_column_wrap(
                         height_mobile = "200vh", # when mobile we allow 2x scren height of scrollable area (map+2statboxes)
                         fill = TRUE,
                         width = NULL,
                         style = htmltools::css(grid_template_columns = "2fr 1fr"),

                         # 1st column
                         bslib::card(full_screen = TRUE,

                                     bslib::card_header(strong("Carte interactive des communes"), bslib::tooltip(
                                       id = "tooltip_map_card",
                                       placement = "right",
                                       options = list(customClass = "customTooltips"), # custom.scss
                                       trigger = bsicons::bs_icon("info-circle"),

                                       "En cas de fusions communales, un décalage de quelques semaines peut être nécessaire
                                        pour mettre à jour les données avec les nouveaux périmètres communaux"),

                                       class = "bg-secondary"),

                                     bslib::card_body(
                                       leafletOutput("map"))
                         ),# End card() map

                         # 2nd column

                         tagList(

                           mod_collapse_stats_box_ui("vd_box"),

                           mod_collapse_stats_box_ui("communes_box")

                         )
                       )# End layout_column_wrap

      ),# End nav_panel('Carte')

      ## cons/prod elec----
      bslib::nav_menu("Electricité", icon = icon("bolt"),


                      # cons elec ----
                      bslib::nav_panel("Distribution",


                                       # Module for producing cons elec charts
                                       mod_elec_charts_ui("consumption_charts",
                                                          title = "Distribution d'électricité par commune",
                                                          title_complement = HTML("La <strong>distribution</strong> ne doit pas être confondue à la <strong>consommation finale</strong> car il manque l'autoconsommation (notamment photovoltaïque) ou encore l'électricité du réseau des CFF.
                                                                                   L'autoconsommation estimée est disponible dans la <strong>table des données de production d'électricité</strong>.")
                                                          )

                      ),

                      # prod elec ----
                      bslib::nav_panel("Production",

                                       # Module for producing prod elec charts
                                       mod_elec_charts_ui("production_charts",
                                                          title = "Production d'électricité par commune",
                                                          title_complement = NULL # not needed
                                                          )

                      )

      ),# End nav_menu Electricité


      ## Buildings heat ----
      bslib::nav_menu("Chaleur des bâtiments", icon = icon("fire"),
                      bslib::nav_panel("Besoins",

                                       mod_regener_needs_charts_ui("regener_needs")

                                       ),
                      bslib::nav_panel("Consommations",

                                       mod_regener_cons_charts_ui("regener_cons")
                                       ),
                      bslib::nav_panel("Informations bâtiments",

                                       mod_regener_misc_charts_ui("regener_misc")
                      )

      ),#End nav_menu() 'Chaleur des bâtiments'


      ## Subsidies ----
      bslib::nav_menu("Subventions", icon = icon("file-pen"),

                      bslib::nav_panel("Vue par bâtiments",

                                       mod_subsidies_building_charts_ui("subsidies_building")
                      ),
                      bslib::nav_panel("Vue par subventions",

                                       mod_subsidies_measure_charts_ui("subsidies_measure")

                      )

      ),#End nav_menu() 'Subventions'

      ## Misc ----
      bslib::nav_menu("Divers",
        ### Report ----
        bslib::nav_panel("Rapport", icon = icon("file-code"),

                         mod_download_rmd_ui("report")

                         ),

        ## News ----

        bslib::nav_panel("Nouveautés",
                         icon = bsicons::bs_icon("star"),

                         mod_news_ui("news")
                         ),

        ### About ----
        bslib::nav_panel("À propos",icon = icon("circle-info"),

                         mod_about_the_app_ui("about")

                         )

      ),#End nav_menu() 'Divers'


      #### (spacer) ----
      bslib::nav_spacer(),

      ## Useful links ----
      bslib::nav_menu(
        align = "right",
        title = "Liens utiles", # utils_helpers.R for links

        ### Contact ----
        bslib::nav_item(tags$a(bsicons::bs_icon("envelope-at-fill"), "Contact",

                               # add objet to mail manually, better UX
                               href = paste0("mailto:", mail_address, "?subject=Question profil énergétique"), target = "_blank")),
        ### DGE-DIREN ----
        bslib::nav_item(tags$a(bsicons::bs_icon("link"), "DGE-DIREN",
                               href = link_diren, target = "_blank")),

        ### GitHub ----
        bslib::nav_item(tags$a(bsicons::bs_icon("github"), "GitHub",
                               href = link_github, target = "_blank"))

      )
    )#End page_navbar()
  )# End tagList()
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


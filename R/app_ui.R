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
      title = strong("Profil énergétique des communes vaudoises"),
      # Browser title
      window_title = "Profil énergie VD",
      # Sidebar
      sidebar = bslib::sidebar(

        width = "300px",
        bg =  "#343A40",
        mod_inputs_ui("inputs_1")

      ),# End sidebar()

      # Navigation panels ----
      ## Carte ----
      bslib::nav_panel("Carte",
                       icon = icon("map"),
                       bslib::layout_column_wrap(
                         fill = TRUE,
                         width = NULL,
                         style = htmltools::css(grid_template_columns = "2fr 1fr"),
                         bslib::card(full_screen = TRUE,
                               bslib::card_header(class = "bg-secondary", "Carte des communes"),
                              bslib::card_body(
                                leafletOutput("map"))
                         )# End card() map
                       )# add tagList statboxes here


                       ),

      ## Prod elec----
      bslib::nav_panel("Production", icon = icon("bolt"),

                       # Module for producing prod elec charts
                       mod_elec_charts_ui("production_charts")

                       ),

      ## Buildings heat ----
      bslib::nav_menu("Chaleur des bâtiments", icon = icon("fire"),
                      bslib::nav_panel("Besoins "),
                      bslib::nav_panel("Consommations"),
                      bslib::nav_panel("Autres")

      ),#End nav_menu() 'Chaleur des bâtiments'


      ## Subsidies ----
      bslib::nav_menu("Subventions", icon = icon("file-pen"),
                      bslib::nav_panel("Vue par bâtiments"),
                      bslib::nav_panel("Vue par subventions")

      ),#End nav_menu() 'Subventions'

      ## Misc ----
      bslib::nav_menu("Divers",
        bslib::nav_panel("Rapport", icon = icon("file-code")),

        bslib::nav_panel("À propos",icon = icon("circle-info"),

                         mod_about_the_app_ui("about")

                         )

      ),#End nav_menu() 'Divers'


      ###Spacer ----
      bslib::nav_spacer(),

      ## Useful links ----
      bslib::nav_menu(
        align = "right",
        title = "Liens utiles", # utils_helpers.R for links
        bslib::nav_item(tags$a(bsicons::bs_icon("envelope-at-fill"), "Contact", href = paste0("mailto:", mail_address), target = "_blank")),
        bslib::nav_item(tags$a(bsicons::bs_icon("link"), "DGE-DIREN", href = link_diren, target = "_blank")),
        bslib::nav_item(tags$a(bsicons::bs_icon("github"), "GitHub", href = link_github, target = "_blank"))

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


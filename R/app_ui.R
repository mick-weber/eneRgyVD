#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {

  shiny::tagList(

    ## |---------------------------------------------------------------|
    ##          Scripts and JS resources
    ## |---------------------------------------------------------------|
    # First proceed to load JS scripts / librairies as needed
    tags$head(
      # Add description for web content
      tags$meta(name = "description",
                content = "Découvrez la situation énergétique et climatique de votre commune à l'aide de données mises à disposition par le Canton de Vaud."
                ),

      # Add introJS library for the guided tour
      tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/intro.js/minified/introjs.min.css"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/intro.js/minified/intro.min.js")

    ),# End tags$head

    # Leave this function for adding external resources in inst/app :
    #   loads : tags$script(src = "www/introjs_script.js"), # this script handles the introjs feature
    #   loads : tags$script(src = "www/toggleDropdowns.js") # this script closes nav_menu's after being redirected by a click. It is used for redirects from app_server.R to mod_about_the_app.R

    golem_add_external_resources(),

    ## |---------------------------------------------------------------|
    ##        UI app starts here
    ## |---------------------------------------------------------------|
    # Top navbar ----
    tags$nav(
      class = "navbar navbar-expand-md bg-primary",
      style = "color:white;padding: 2px; width: 100%; position: relative; z-index: 1000;",

      # Tweak the title spacers and font-size
      tags$p("Profil climatique des communes vaudoises", class = "ms-4 py-0 my-0 fs-5", style = "font-weight:500;"),

      tags$div(
        # dropdown with useful links, collapses on small widths
        id = "topNavbar",
        class = "collapse navbar-collapse",
        style = "margin-right:2vw;",
        tags$ul(
          class = "navbar-nav ms-auto",
          tags$li(class = "nav-item dropdown",
                  tags$a(
                    class = "nav-link text-white dropdown-toggle", href = "#", id = "topNavDropdown",
                    role = "button", `data-bs-toggle` = "dropdown",
                    "Liens utiles"
                  ),
                  tags$ul(
                    class = "dropdown-menu dropdown-menu-end",
                    tags$a(class = "dropdown-item", phosphoricons::ph(title = NULL, "envelope"),
                           "Contact", href = paste0("mailto:", mail_address, "?subject=Question profil climatique"), target = "_blank"),
                    tags$a(class = "dropdown-item", phosphoricons::ph(title = NULL, "link-simple"),
                           "Plan énergie climat", href = link_pecc, target = "_blank"),
                    tags$a(class = "dropdown-item", phosphoricons::ph(title = NULL, "link-simple"),
                           "OCDC", title = "Office cantonal de la durabilité et du climat", href = link_ocdc, target = "_blank"),
                    tags$a(class = "dropdown-item", phosphoricons::ph(title = NULL, "link-simple"),
                           "DGE", title = "Direction générale de l'environnement", href = link_dge, target = "_blank"),
                    tags$a(class = "dropdown-item", phosphoricons::ph(title = NULL, "link-simple"),
                           "DGE-DIREN", title = "Direction de l'énergie", href = link_diren, target = "_blank"),
                    tags$a(class = "dropdown-item", phosphoricons::ph(title = NULL, "link-simple"),
                           "Cartostat DGMR", title = "Guichet cartographique DGMR", href = link_cartostat_dgmr, target = "_blank"),
                    tags$a(class = "dropdown-item", phosphoricons::ph(title = NULL, "github-logo"),
                           "GitHub",  href = link_github, target = "_blank")
                  )
          )
        )
      )# End div
    ),

    # Main navbar ----
    bslib::page_navbar(

      id = "nav", # conditionalPanels will refer to this as input.nav == <condition>
      fillable = TRUE,
      navbar_options = list(class = "navbar-expand-xl",
                            style = "@media (max-width:1199px) {.navbar-header .navbar-toggle{order:2 !important;}}"), #instead of default lg we collapse earlier to avoid navbar on two rows
      # Footer
      footer = p("DGE-DIREN @ 2025", class = "fw-lighter",
                 style = "position: fixed;bottom:0;right:1%;font-size:1rem;"),
      # Custom theme
      theme = profil_theme, # utils_helpers.R
      # Title not needed here, provided in app_ui.R
      title = NULL, #strong("Profil climatique des communes vaudoises"),
      # Browser title
      window_title = "Profil climatique vaudois",

      # Sidebar ----
      sidebar = bslib::sidebar(
        id = "customSidebar",
        open = "always",
        class = "shadow rounded-end-3", # add some shadow + rounded + see custom.scss
        bg =  "#343A40",
        mod_inputs_ui("inputs_1")

      ),# End sidebar()

      # Logo VD left of main sidebar
      bslib::nav_item(
        tags$div(
          id = "clickLogoToHomepage",
          title = "Retour à l’accueil",  # tooltip
          # When clicked, add a reactive input (id from div is not listened by server). See app_server.R.
          # With server code this redirects to 'Accueil' tab
          onclick = "Shiny.setInputValue(id = 'clickLogoToHomepage_click', value = 'clickLogoToHomepage_click', {priority : 'event'})",
          style = "cursor: pointer;display: flex; align-items: center; gap: 10px;margin-right:3rem;",
          tags$img(src = "www/vd-logo-black.svg", height = "50px", class = "navbar-brand"),
          tags$div(
            style = "font-size:0.9rem;",
            tags$p(class = "m-0", "Office cantonal de la durabilité et du climat"),
            tags$p(class = "m-0", "Direction générale de l'environnement")
          )
        )
      ),

      # Navigation panels ----
      ## Carte ----
      bslib::nav_panel("Accueil",
                       bslib::layout_column_wrap(
                         height_mobile = "200vh", # when mobile we allow 2x scren height of scrollable area (map+2statboxes)
                         fill = TRUE,
                         width = NULL,
                         style = htmltools::css(grid_template_columns = "2fr 1fr"),

                         # 1st column
                         bslib::card(full_screen = FALSE,

                                     bslib::card_header(strong("Carte interactive des communes"), bslib::tooltip(
                                       id = "tooltip_map_card",
                                       placement = "right",
                                       options = list(customClass = "customTooltips"), # custom.scss
                                       trigger = phosphoricons::ph(title = NULL, "info"),

                                       "En cas de fusions communales, un décalage de quelques semaines peut être nécessaire
                                        pour mettre à jour les données avec les nouveaux périmètres communaux")
                                     ),

                                     bslib::card_body(class = "m-0 p-0",
                                                      leafletOutput("map"), fill = TRUE)
                         ),# End card() map

                         # 2nd column

                         tagList(

                           mod_table_content_ui("toc")

                         )
                       )# End layout_column_wrap

      ),# End nav_panel('Accueil')

      ## Energie ----
      bslib::nav_menu("Energie",

                      ### Electricite ----
                      bslib::nav_panel("Electricité",
                                       icon = phosphoricons::ph(title = NULL, "lightning",
                                                                weight = "fill",
                                                                fill = "gold"),

                                       bslib::navset_card_tab(id = "navset_elec",

                                                              #### cons elec ----
                                                              bslib::nav_panel(title = "Distribution d'électricité",
                                                                               icon = icon(NULL, class = "pulse"), # only when inactive, custom.scss

                                                                               # Module for producing cons elec charts

                                                                               mod_elec_charts_ui("consumption_charts",
                                                                                                  title = "Distribution d'électricité",
                                                                                                  title_complement = title_complement_elec_cons # utils_text_and_links.R
                                                                               )

                                                              ),# End nav_panel

                                                              #### prod elec ----
                                                              bslib::nav_panel("Production d'électricité",
                                                                               icon = icon(NULL, class = "pulse"), # only when inactive, custom.scss

                                                                               # Module for producing prod elec charts
                                                                               mod_elec_charts_ui("production_charts",
                                                                                                  title = "Production d'électricité",
                                                                                                  title_complement = title_complement_elec_prod # utils_text_and_links.R
                                                                               )

                                                              )# End nav_panel
                                       ),# End navset_card_pill
                      ),# End nav_panel 'Electricite'


                      ### Gaz naturel ----
                      bslib::nav_panel("Gaz naturel",
                                       icon = phosphoricons::ph(title = NULL, "flame",
                                                                weight = "fill",
                                                                fill = "#4A708B"),

                                       bslib::navset_card_tab(id = "navset_ng",

                                                              ### cons_ng ----
                                                              bslib::nav_panel(title = "Distribution de gaz naturel",

                                                                               mod_ng_charts_ui("ng_cons_charts",
                                                                                                title = "Distribution de gaz naturel",
                                                                                                title_complement = title_complement_ng_cons # utils_text_and_links.R
                                                                               )
                                                              )
                                       )
                      ),# End nav_panel 'Gaz naturel'

                      ### Chaleur batiments ----
                      bslib::nav_panel("Chaleur des bâtiments",
                                       icon = phosphoricons::ph(title = NULL, "building-apartment",
                                                                weight = "fill",
                                                                fill = "black"),

                                       bslib::navset_card_tab(id = "navset_regener",

                                                              #### besoins ----
                                                              bslib::nav_panel("Besoins des bâtiments",
                                                                               icon = icon(NULL, class = "pulse"), # only when inactive, custom.scss

                                                                               mod_regener_needs_charts_ui("regener_needs",
                                                                                                           title = "Besoins théoriques des bâtiments",
                                                                                                           title_complement = title_complement_regener_needs # utils_text_and_links.R
                                                                               )

                                                              ),
                                                              #### consommation ----
                                                              bslib::nav_panel("Consommation des bâtiments",
                                                                               icon = icon(NULL, class = "pulse"), # only when inactive, custom.scss

                                                                               mod_regener_cons_charts_ui("regener_cons",
                                                                                                          title = "Consommations théoriques des bâtiments",
                                                                                                          title_complement = title_complement_regener_cons # utils_text_and_links.R
                                                                               )
                                                              ),
                                                              #### misc ----
                                                              bslib::nav_panel("Informations bâtiments",
                                                                               icon = icon(NULL, class = "pulse"), # only when inactive, custom.scss

                                                                               mod_regener_misc_charts_ui("regener_misc",
                                                                                                          title = "Autres informations des bâtiments",
                                                                                                          title_complement = title_complement_regener_misc # utils_text_and_links.R
                                                                               )
                                                              )
                                       )# End navset_card_pill
                      ),# End nav_panel 'Chaleur bâtiments'


                      ### Subventions ----
                      bslib::nav_panel("Subventions bâtiments",
                                       icon = phosphoricons::ph(title = NULL, "files"),

                                       bslib::navset_card_tab(id = "navset_subsidies",

                                                              #### Par mesures ----
                                                              bslib::nav_panel("Vue par travaux subventionnés",
                                                                               icon = icon(NULL, class = "pulse"), # only when inactive, custom.scss

                                                                               mod_subsidies_measure_charts_ui("subsidies_measure",
                                                                                                               title = HTML("Subventions Programme bâtiments (vue par subventions)"),
                                                                                                               title_complement = title_complement_subsidies_measure # utils_text_and_links.R
                                                                               )
                                                              ),

                                                              #### Par batiments ----
                                                              bslib::nav_panel("Vue cumulée du parc subventionné",
                                                                               icon = icon(NULL, class = "pulse"), # only when inactive, custom.scss

                                                                               mod_subsidies_building_charts_ui("subsidies_building",
                                                                                                                title = HTML("Subventions Programme bâtiments (vue du parc)"),
                                                                                                                title_complement = title_complement_subsidies_building # utils_text_and_links.R
                                                                               )
                                                              )

                                       )# End navset_card_pill
                      )# End nav_panel 'Subventions bâtiments'
      ),# End nav_menu Energie

      ## Mobilité ----
      bslib::nav_menu("Mobilité",

                      # Véhicules électriques
                      bslib::nav_panel("Véhicules électriques",
                                       icon = phosphoricons::ph(title = NULL, "car", weight = "fill", fill = "#EEB422"),
                                       # Nested navset_card_tab()
                                       bslib::navset_card_tab(id = "navset_vehicules",
                                                              bslib::nav_panel(title = "Part des voitures électriques",

                                                                               mod_generic_charts_ui("part_voit_elec",
                                                                                                     title = "Part des voitures électriques",
                                                                                                     title_complement = title_complement_part_voit_elec
                                                                                                     )
                                                              )# end firstnav_panel
                                       )# end navset_card_tab
                      ),# end main nav_panel

                      # Taux de motorisation ----
                      bslib::nav_panel("Taux de motorisation",
                                       icon =  phosphoricons::ph(title = NULL, "car-profile", weight = "fill", fill = "black"),
                                       # Nested navset_card_tab()
                                       bslib::navset_card_tab(id = "navset_taux_motorisation",
                                                              bslib::nav_panel(title = "Taux de motorisation",

                                                                               mod_generic_charts_ui("taux_motorisation",
                                                                                                     title = "Taux de motorisation",
                                                                                                     title_complement = title_complement_taux_motorisation
                                                                               )
                                                              )# end firstnav_panel
                                       )# end navset_card_tab
                      ),# end main nav_panel

                      # Transports publics
                      bslib::nav_panel("Transports publics",
                                       icon = phosphoricons::ph(title = NULL, "tram"),
                                       # Nested navset_card_tab()
                                       bslib::navset_card_tab(id = "navset_qualite_desserte",
                                                              bslib::nav_panel(title = "Qualité de desserte des transports publics",

                                                                               mod_generic_charts_ui("qualite_desserte",
                                                                                                     title = "Qualité de desserte des transports publics",
                                                                                                     title_complement = title_complement_qualite_desserte
                                                                               )
                                                              )# end firstnav_panel
                                       )# end navset_card_tab
                      )# end main nav_panel
      ),# End nav_menu() 'Mobilite'


      ## Adaptation climat ----
      bslib::nav_menu("Adaptation climat",
                      # Canopée ----
                      bslib::nav_panel("Surface de canopée urbaine",
                                       icon = phosphoricons::ph(title = NULL, "tree",
                                                                weight = "fill",
                                                                fill = "#3A862D"),
                                       # Nested navset_card_tab()
                                       bslib::navset_card_tab(id = "navset_canopy",
                                                              bslib::nav_panel(title = "Surface de canopée urbaine",

                                                                               mod_generic_charts_ui("adaptation_canopy",
                                                                                                     title = "Surface de canopée urbaine",
                                                                                                     title_complement = title_complement_surface_canopee
                                                                               )
                                                              )# end nested nav_panel
                                       )# end navset_card_pill
                      ),# end first main nav_panel

                      # Dangers naturels (bâtiments) ----
                      bslib::nav_panel("Exposition aux dangers naturels",
                                       icon = phosphoricons::ph(title = NULL, "warning-diamond",
                                                                fill = "#EE7600"),
                                       #Nested navaset_card_tab()
                                       bslib::navset_card_tab(id = "navset_natural_hazards",
                                                              bslib::nav_panel(title = "Bâtiments exposés à des dangers naturels",

                                                                               mod_generic_charts_ui("buildings_exposure_hazards",
                                                                                                     title = "Bâtiments exposés à des dangers naturels",
                                                                                                     title_complement = title_complement_batiment_danger
                                                                               )
                                                              )# end nested nav_panel
                                       )
                      )# end second main nav_panel
      ), # End nav_menu() 'Adaptation'

      ## Misc ----
      bslib::nav_menu("Divers",

                      ### About ----
                      bslib::nav_panel("À propos",
                                       icon = phosphoricons::ph(title = NULL, "question-mark"),

                                       mod_about_the_app_ui("about")

                      ),
                      ### News ----
                      bslib::nav_panel("Nouveautés",
                                       icon = phosphoricons::ph(title = NULL, "star"),

                                       mod_news_ui("news")
                      ),
                      # ### Chiffres-clés ---- DISABLED FOR NOW
                      # bslib::nav_panel("Chiffres-clés", icon = phosphoricons::ph(title = NULL, "chart-bar"),
                      #
                      #                  bslib::layout_columns(col_widths = c(-1, 9, -2),
                      #
                      #                                        mod_stats_box_ui("vd_box"),
                      #                                        mod_stats_box_ui("communes_box")
                      #                  )
                      # ),
                      ### Contact ----
                      bslib::nav_item(
                        tags$a(
                          class = "dropdown-item",
                          phosphoricons::ph(title = NULL, "envelope"),
                          "Contact",
                          href = paste0("mailto:", mail_address, "?subject=Question profil climatique"), target = "_blank"
                        )
                      )
      ),#End nav_menu() 'Divers'


      #### (spacer) ----
      bslib::nav_spacer(),

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
    favicon(rel = "icon"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Profil climatique vaudois"
    )
  )
}


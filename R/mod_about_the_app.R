#' about_the_app UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_the_app_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::navset_tab(id = ns("tabset_main"),

                      ## |---------------------------------------------------------------|
                      ##          tabPanel 'Général' ----
                      ## |---------------------------------------------------------------|
                      bslib::nav_panel(title = "Général",
                                       bslib::layout_columns(col_widths = c(-1, 8, -3),
                                                             # breathing
                                                             br(),
                                                             h4(strong("Pourquoi cette application ?")),
                                                             tags$p("Le but de ce profil climatique est de faciliter la planification énergie et climat des territoires en diffusant des informations disponibles au niveau communal qui ne sont pas facilement accessibles autrement.
           Bien que dédié principalement aux communes, cet outil se veut accessible pour tout le monde.
           Les différentes visualisations, les options de téléchargement ainsi que la possibilité d'exporter un rapport automatisé permettent d'explorer les données et de les exporter facilement."),
                                                             # breathing
                                                             tags$br(),
                                                             "Vos suggestions, retours ou critiques sont précieux et nous permettent d'améliorer cette application.",
                                                             tags$span(
                                                               "Contact : ",
                                                               tags$a(href = paste0("mailto:", mail_address), # utils_helpers.R
                                                                      mail_address,
                                                                      target = "_blank")
                                                             ),
                                                             br(),
                                                             h4(strong("Collaborations")),
                                                             tags$p("Le profil énergétique a été initialement développée par la DGE-DIREN en 2023.
                                                                        En 2024, l’OCDC a souhaité mettre à disposition des communes d’autres données et l’application est devenue le profil climatique dès janvier 2025.
                                                                        De ce fait, le point de contact pour la gestion de cette application est désormais l’OCDC qui collabore toujours étroitement avec la DIREN.
                                                                        D’autres services ont également participé à la valorisation des données comme DGE-Biodiv, DGE-UDN et la DGMR."

                                                             )


                                       )# End 1st layout_columns'
                      ),# End nav_panel' Général

                      ## |---------------------------------------------------------------|
                      ##          tabPanel 'Données' ----
                      ## |---------------------------------------------------------------|
                      bslib::nav_panel(title = "Données",

                                       bslib::layout_columns(col_widths = c(-1, 9, -2),
                                                             # breathing
                                                             br(),
                                                             h4(strong("Sources des données")),
                                                             tags$p("Chaque donnée est détaillée ci-dessous.
                                              Les différentes informations fournies par cette application reflètent au mieux la réalité avec les données disponibles.
                                                     L'exactitude de ces informations ne peut être garantie. En cas d'incohérence de données, merci de prendre contact."),


                                                             ## |---------------------------------------------------------------|
                                                             ##        Higher navigation level (Energie/Climat/Mobilité...)
                                                             ## |---------------------------------------------------------------|
                                                             bslib::navset_pill(id = ns("navset_donnees"),

                                                                                # breathing
                                                                                header = br(),

                                                                                ## nav_panel 'Energie' ----
                                                                                bslib::nav_panel("Energie",

                                                                                                 ### Données subtabs ----
                                                                                                 bslib::navset_tab(id = ns("navset_energie"),

                                                                                                                   ### 1. Cons elec ----
                                                                                                                   # -> if title changed (html tag too !) adapt in app_server.R's subpanels_tribble object
                                                                                                                   bslib::nav_panel("Distribution d'électricité",
                                                                                                                                    tags$div(
                                                                                                                                      br(),
                                                                                                                                      h4(strong("Détails méthodologiques : distribution d'électricité")),
                                                                                                                                      br(),
                                                                                                                                      bslib::accordion(!!!elec_cons_doc_panels, open = FALSE),
                                                                                                                                      br(),
                                                                                                                                      h5(strong("Descriptif des variables")),
                                                                                                                                      br(),
                                                                                                                                      DT::dataTableOutput(ns("elec_cons_doc"))

                                                                                                                                    )# End tags$div
                                                                                                                   ),# End nested tabPanel 1.


                                                                                                                   ### 2. Prod élec ----
                                                                                                                   # -> if title changed (html tag too !) adapt in app_server.R's subpanels_tribble object
                                                                                                                   bslib::nav_panel("Production d'électricité",
                                                                                                                                    # create div to apply class
                                                                                                                                    tags$div(
                                                                                                                                      br(),
                                                                                                                                      # Overview method
                                                                                                                                      h4(strong("Détails méthodologiques : production d'électricité")),
                                                                                                                                      br(),
                                                                                                                                      bslib::accordion(!!!elec_prod_doc_panels, open = FALSE),
                                                                                                                                      br(),
                                                                                                                                      h5(strong("Descriptif des variables")),
                                                                                                                                      br(),
                                                                                                                                      DT::dataTableOutput(ns("elec_prod_doc"))
                                                                                                                                    )# End tags$div

                                                                                                                   ),# End nested tabPanel 2.

                                                                                                                   ### 3. Regener ----
                                                                                                                   # -> if title changed (html tag too !) adapt in app_server.R's subpanels_tribble object
                                                                                                                   bslib::nav_panel("Chaleur bâtiments",
                                                                                                                                    # create div to apply class
                                                                                                                                    tags$div(
                                                                                                                                      br(),
                                                                                                                                      h4(strong("Détails méthodologiques : chaleur bâtiments")),
                                                                                                                                      br(),
                                                                                                                                      bslib::accordion(!!!regener_doc_panels, open = FALSE),
                                                                                                                                      br(),
                                                                                                                                      h5(strong("Descriptif des variables")),
                                                                                                                                      br(),
                                                                                                                                      DT::dataTableOutput(ns("regener_doc"))

                                                                                                                                    )# End tags$div

                                                                                                                   ),# End nested tabPanel 3.

                                                                                                                   ### 4. Subsidies ----
                                                                                                                   # -> if title changed (html tag too !) adapt in app_server.R's subpanels_tribble object
                                                                                                                   bslib::nav_panel("Subventions bâtiments",
                                                                                                                                    # create div to apply class
                                                                                                                                    tags$div(
                                                                                                                                      br(),
                                                                                                                                      h4(strong("Détails méthodologiques : subventions bâtiments")),
                                                                                                                                      br(),
                                                                                                                                      bslib::accordion(!!!subsidies_doc_panels, open = FALSE),
                                                                                                                                      br(),
                                                                                                                                      h5(strong("Descriptif des variables")),
                                                                                                                                      br(),
                                                                                                                                      # Documentation table for both subsidies datasets
                                                                                                                                      DT::dataTableOutput(ns("subsidies_doc"))

                                                                                                                                    )# End tags$div
                                                                                                                   ),# End tabPanel 4

                                                                                                                   ### 5. Conso gaz ----
                                                                                                                   bslib::nav_panel("Distribution de gaz naturel",
                                                                                                                                    # create div to apply class
                                                                                                                                    tags$div(
                                                                                                                                      br(),
                                                                                                                                      h4(strong("Détails méthodologiques : distribution de gaz naturel")),
                                                                                                                                      br(),
                                                                                                                                      bslib::accordion(!!!ng_cons_doc_panels, open = FALSE),
                                                                                                                                      br(),
                                                                                                                                      h5(strong("Descriptif des variables")),
                                                                                                                                      br(),
                                                                                                                                      # Documentation table for both subsidies datasets
                                                                                                                                      DT::dataTableOutput(ns("ng_cons_doc"))

                                                                                                                                    )# End tags$div
                                                                                                                   )# End tabPanel 5
                                                                                                 )# End nested tabsetPanel within 'Données'
                                                                                ),# End nav_panel Energie


                                                                                ## nav_panel 'Adaptation' ----
                                                                                bslib::nav_panel("Adaptation climat",
                                                                                                 bslib::navset_tab(id = ns("navset_climat"),

                                                                                                                   bslib::nav_panel("Surface de canopée",

                                                                                                                                    tags$div(
                                                                                                                                      br(),
                                                                                                                                      h4(strong("Détails méthodologiques : surface de canopée")),
                                                                                                                                      # br(),
                                                                                                                                      bslib::accordion(!!!taux_canopee_doc_panels, open = FALSE),
                                                                                                                                      br()
                                                                                                                                      # h5(strong("Descriptif des variables")),
                                                                                                                                      # br(),
                                                                                                                                      # # Documentation table for both subsidies datasets
                                                                                                                                      # DT::dataTableOutput(ns("subsidies_doc"))

                                                                                                                                    )# End div)
                                                                                                                   ),# End nav_panel

                                                                                                                   bslib::nav_panel("Bâtiments exposés à des dangers naturels",

                                                                                                                                    tags$div(
                                                                                                                                      br(),
                                                                                                                                      h4(strong("Détails méthodologiques : Bâtiments exposés à des dangers naturels")),
                                                                                                                                      # br(),
                                                                                                                                      bslib::accordion(!!!batiment_danger_doc_panels, open = FALSE),
                                                                                                                                      br()
                                                                                                                                      # h5(strong("Descriptif des variables")),
                                                                                                                                      # br(),
                                                                                                                                      # # Documentation table for both subsidies datasets
                                                                                                                                      # DT::dataTableOutput(ns("subsidies_doc"))

                                                                                                                                    )# End div)
                                                                                                                   )# End nav_panel
                                                                                                 )# End navset_tab 'Adaptation'
                                                                                ),# End nav_panel 'Adaptation'

                                                                                ## nav_panel 'Mobilité' ----
                                                                                bslib::nav_panel("Mobilité",

                                                                                                 bslib::navset_tab(id = ns("navset_mobilite"),

                                                                                                                   bslib::nav_panel("Part des voitures électriques",

                                                                                                                                    tags$div(
                                                                                                                                      br(),
                                                                                                                                      h4(strong("Détails méthodologiques : part des voitures électriques")),
                                                                                                                                      # br(),
                                                                                                                                      bslib::accordion(!!!part_voit_elec_doc_panels, open = FALSE),
                                                                                                                                      br()
                                                                                                                                      # h5(strong("Descriptif des variables")),
                                                                                                                                      # br(),
                                                                                                                                      # Documentation table ??
                                                                                                                                      # DT::dataTableOutput(ns("subsidies_doc"))

                                                                                                                                    )# End div
                                                                                                                   ),# End nav_panel

                                                                                                                   bslib::nav_panel("Taux de motorisation",

                                                                                                                                    tags$div(
                                                                                                                                      br(),
                                                                                                                                      h4(strong("Détails méthodologiques : taux de motorisation")),
                                                                                                                                      # br(),
                                                                                                                                      bslib::accordion(!!!taux_motorisation_doc_panels, open = FALSE),
                                                                                                                                      br()
                                                                                                                                      # h5(strong("Descriptif des variables")),
                                                                                                                                      # br(),
                                                                                                                                      # Documentation table ??
                                                                                                                                      # DT::dataTableOutput(ns("subsidies_doc"))

                                                                                                                                    )# End div
                                                                                                                   ),# End nav_panel

                                                                                                                   bslib::nav_panel("Qualité de desserte des transports publics",

                                                                                                                                    tags$div(
                                                                                                                                      br(),
                                                                                                                                      h4(strong("Détails méthodologiques : qualité de desserte des transports publics")),
                                                                                                                                      # br(),
                                                                                                                                      bslib::accordion(!!!qualite_desserte_doc_panels, open = FALSE),
                                                                                                                                      br()
                                                                                                                                      # h5(strong("Descriptif des variables")),
                                                                                                                                      # br(),
                                                                                                                                      # Documentation table ??
                                                                                                                                      # DT::dataTableOutput(ns("subsidies_doc"))

                                                                                                                                    )# End div
                                                                                                                   )# End nav_panel
                                                                                                 )# End navset 'Mobilite'
                                                                                ), # End nav_panel 'Mobilite'
                                                             )# End main navset_pill
                                       )# End layout_columns()
                      ),# End tabPanel 'Données'

                      ## |---------------------------------------------------------------|
                      ##          tabPanel 'Glossaire' ----
                      ## |---------------------------------------------------------------|

                      bslib::nav_panel(title = "Glossaire",
                                       bslib::layout_columns(col_widths = c(-1, 8, -3),
                                                             # breathing
                                                             br(),
                                                             tags$h4(strong("Glossaire")),
                                                             tags$p("Ci-dessous un lexique des principales abbréviations et termes techniques utilisés dans cette application."),
                                                             DT::dataTableOutput(ns("glossary_table"))

                                       )# End layout_columns
                      ),# End tabPanel 'Confidentialité'

                      ## |---------------------------------------------------------------|
                      ##          tabPanel 'Techno' ----
                      ## |---------------------------------------------------------------|
                      bslib::nav_panel(title = "Technologie",
                                       bslib::layout_columns(col_widths = c(-1, 9, -2),
                                                             # breathing
                                                             br(),
                                                             tags$h4(strong("Technologie")),
                                                             tags$p("Cette application est programmée avec le language",
                                                                    tags$b("R"), "et la", tags$a(href = "https://shiny.rstudio.com/", "librairie {shiny}", target = "_blank"),
                                                                    "qui permet la création d'applications web interactives. L'architecture de l'application est construite
            à l'aide des librairies ",
                                                                    tags$a(href = "https://github.com/ThinkR-open/golem", "{golem} (ThinkR)", target = "_blank"),
                                                                    "et",
                                                                    tags$a(href = "https://rstudio.github.io/bslib", "{bslib} (Posit PBC)", target = "_blank"),
                                                                    "et le contenu des visualisations par de multiples librairies issues notamment du",
                                                                    tags$a(href = "https://www.tidyverse.org/", "{tidyverse} (Posit PBC).", target = "_blank"),
                                                                    "De plus amples demandes d'informations peuvent être adressées directement à l'e-mail de contact",
                                                                    tags$a(href = paste0("mailto:", mail_address), mail_address,target = "_blank"),
                                                                    "."),
                                                             br(),
                                                             tags$p("Ce projet est sous licence open source GNU General Public Licence ",
                                                                    tags$a(href = "https://www.gnu.org/licenses/gpl-3.0.en.html", "GPLv3.", target = "_blank")),
                                                             br(),
                                                             br(),
                                                             # the fluidrow is here to force the horizontal alignment of github button + p()


                                                             tags$div(class = "d-flex align-items-center",

                                                                      tags$a(href = link_github,
                                                                             bsicons::bs_icon("github",
                                                                                              size = "3rem",
                                                                                              class = "text-secondary"),
                                                                             target = "_blank"),
                                                                      tags$a(class = "m-0 ps-3",
                                                                             href = link_github,
                                                                             "Visiter le repo GitHub",
                                                                             target = "_blank")

                                                             )

                                       )# End layout_columns
                      )# End tabPanel 'Technologie'
    )# End main navset_panel
  )# End tagList
}

#' about_the_app Server Functions
#'
#' @noRd
mod_about_the_app_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # 1. Cons élec ----

    # Table for electricity consumption documentation

    output$elec_cons_doc <- DT::renderDataTable({

      doc_datasets$elec_cons |>  # loaded in utils_helpers.R
        create_doc_table_dt(doc_prefix = "doc_elec_cons_") # fct_helpers.R

    })


    # 2. Prod élec ----
    # Table for electricity production documentation
    output$elec_prod_doc <- DT::renderDataTable({

      doc_datasets$elec_prod |>  # loaded in utils_helpers.R
        create_doc_table_dt(doc_prefix = "doc_elec_prod_") # fct_helpers.R

    })

    # 3. Regener ----

    output$regener_doc <- DT::renderDataTable({
      doc_datasets$regener |>
        create_doc_table_dt(doc_prefix = "doc_regener_") # fct_helpers.R
    })


    # 4. Subsidies ----

    output$subsidies_doc <- DT::renderDataTable({
      doc_datasets$subsidies |>
        create_doc_table_dt(doc_prefix = "doc_subventions_") # fct_helpers.R
    })

    # 5. Conso gaz ----

    output$ng_cons_doc <- DT::renderDataTable({
      doc_datasets$ng_cons |>
        create_doc_table_dt(doc_prefix = "doc_conso_gaz_") # fct_helpers.R
    })


    # Glossaire ----
    output$glossary_table <- DT::renderDataTable({
      glossary |>
        create_doc_table_dt(doc_prefix = "glossaire_")
    })

  })
}

# # testing module
# nameApp <- function() {
#     ui <- fluidPage(
#       mod_about_the_app_ui("id")
#     )
#     server <- function(input, output, session) {
#       mod_about_the_app_server("id")
#     }
#     shinyApp(ui, server)
# }
# nameApp()





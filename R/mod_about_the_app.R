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

    bslib::navset_tab(id = ns("tabset"),

                      # tabPanel 'Général' ----
                      bslib::nav_panel(title = "Général",
                                       bslib::layout_columns(col_widths = c(-1, 8, -3),
                                                                 # breathing
                                                                 br(),
                                                                 h4(strong("Pourquoi cette application ?")),
                                                                 tags$p("Le but de ce profil énergétique est de faciliter la planification énergétique des territoires en diffusant des informations disponibles au niveau communal qui ne sont pas facilement accessibles autrement.
           Bien que dédié principalement aux communes, cet outil se veut accessible pour tout le monde.
           Les différentes visualisations, les options de téléchargement ainsi que la possibilité d'exporter un rapport automatisé permettent d'explorer les données et de les exporter facilement."),
                                                                 # breathing
                                                                 br(),
                                                                 h4(strong("Contact")),
                                                                 tags$p("Vos suggestions, retours ou critiques sont précieux et nous permettent d'améliorer cette application. N'hésitez pas à les envoyer via ",
                                                                        tags$a (href = paste0("mailto:", mail_address),mail_address,target = "_blank"), # open in new tab. address defined in utils_helpers.R
                                                                        " à l'unité chargée des données énergétiques (unité données, indicateurs et politique UDIP) de la",
                                                                        tags$a (href = link_diren, # utils_helpers.R
                                                                                "Direction de l'énergie du Canton de Vaud (DGE-DIREN).",
                                                                                target = "_blank"))

                                       )# End 1st layout_columns'
                                       ),# End nav_panel' Général

                      # tabPanel 'Données' ----
                      bslib::nav_panel(title = "Données",

                                       bslib::layout_columns(col_widths = c(-1, 9, -2),
                                              # breathing
                                              br(),
                                              h4(strong("Sources des données")),
                                              tags$p("Les différentes informations fournies par cette application reflètent au mieux la réalité en fonction des données disponibles à la Direction de l'énergie.
                                    L'exactitude de ces informations ne peut être garantie. En cas d'incohérence de données, prière d'en informer la Direction de l'énergie.
                                    Lorsque disponibles, des documentations plus détaillées sont annexées en-dessous de chaque onglet."),
                                              br(),

                                              ## Données subtabs ----
                                              bslib::navset_tab(id = ns("nested_tabset"),

                                                                   ### 1. Cons elec ----
                                                                   # -> if title changed (html tag too !) adapt in app_server.R's subpanels_tribble object
                                                                   bslib::nav_panel(h6("Distribution d'électricité"),
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
                                                                bslib::nav_panel(h6("Production d'électricité"),
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
                                                                   bslib::nav_panel(h6("Chaleur bâtiments"),
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
                                                                   bslib::nav_panel(h6("Subventions bâtiments"),
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
                                                                   )# End tabPanel 4/4
                                              )# End nested tabsetPanel within 'Données'
                                       )# End layout_columns()
                      ),# End tabPanel 'Données'

                      # Glossaire ----

                      bslib::nav_panel(title = "Glossaire",
                                       bslib::layout_columns(col_widths = c(-1, 8, -3),
                                              # breathing
                                              br(),
                                              tags$h4(strong("Glossaire")),
                                              tags$p("Ci-dessous un lexique des principales abbréviations et termes techniques utilisés dans cette application."),
                                              DT::dataTableOutput(ns("glossary_table"))

                                       )# End layout_columns
                      ),# End tabPanel 'Confidentialité'

                      # Techno ----
                      bslib::nav_panel(title = "Technologie",
                                       bslib::layout_columns(col_widths = c(-1, 9, -2),
                                              # breathing
                                              br(),
                                              tags$h4(strong("Technologie")),
                                              tags$p("Cette application est programmée avec le language",
                                                     tags$b("R"), "et la", tags$a(href = "https://shiny.rstudio.com/", "librairie shiny", target = "_blank"),
                                                     "qui permet la création d'applications web interactives. L'architecture de l'application est construite
            à l'aide des librairies ",
                                                     tags$a(href = "https://github.com/ThinkR-open/golem", "golem (ThinkR)", target = "_blank"),
                                                     "et",
                                                     tags$a(href = "https://rstudio.github.io/bslib", "bslib (Posit PBC)", target = "_blank"),
                                                     "et le contenu des visualisations par de multiples librairies issues notamment du",
                                                     tags$a(href = "https://www.tidyverse.org/", "tidyverse.", target = "_blank"),
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

      elec_cons_doc |>  # loaded in utils_helpers.R
        create_doc_table_dt(doc_prefix = "doc_elec_cons_") # fct_helpers.R

      })


    # 2. Prod élec ----
    # Table for electricity production documentation
    output$elec_prod_doc <- DT::renderDataTable({

      elec_prod_doc |>  # loaded in utils_helpers.R
        create_doc_table_dt(doc_prefix = "doc_elec_prod_") # fct_helpers.R

    })

    # 3. Regener ----

    output$regener_doc <- DT::renderDataTable({
      regener_doc |>
        create_doc_table_dt(doc_prefix = "doc_regener_") # fct_helpers.R
    })


    # 4. Subsidies ----

    output$subsidies_doc <- DT::renderDataTable({
      subsidies_doc |>
        create_doc_table_dt(doc_prefix = "doc_subventions_") # fct_helpers.R
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





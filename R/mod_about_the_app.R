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
           Les différentes visualisations, les options de téléchargement ainsi que la possibilité d'exporter un rapport automatisé permettent d'explorer les données et de les exporter facilement"),
                                                                 # breathing
                                                                 br(),
                                                                 h4(strong("Contact")),
                                                                 tags$p("Vos suggestions, retours ou critiques sont précieux et nous permettent d'améliorer cette application. N'hésitez pas à les envoyer à l'unité données et indicateurs via ",
                                                                        tags$a (href = paste0("mailto:", mail_address),mail_address,target = "_blank"), # open in new tab. address defined in utils_helpers.R
                                                                        " qui est l'unité transversale de la",
                                                                        tags$a (href = link_diren, "Direction de l'énergie de l'Etat de Vaud (DGE-DIREN)",
                                                                                target = "_blank"),
                                                                        "chargée des données énergétiques.")

                                       )# End 1st layout_columns'
                                       ),# End nav_panel' Général

                      # tabPanel 'Données' ----
                      bslib::nav_panel(title = "Données",

                                       bslib::layout_columns(col_widths = c(-1, 9, -2),
                                              # breathing
                                              br(),
                                              h4(strong("Sources des données")),
                                              tags$p("Les différentes informations fournies par cette application reflètent au mieux la réalité en fonction des données disponibles à la Direction de l'énergie.
                                    Néanmoins, l'exactitude de ces informations ne peut être garantie. En cas d'incohérence, n'hésitez pas à prendre contact afin d'en clarifier les causes probables.
                                    Lorsque disponibles, des documentations plus détaillées sont annexées en-dessous de chaque onglet."),
                                              br(),
                                              tags$p("Les sources de données suivantes sont utilisées : "),


                                              ## Données subtabs ----
                                              bslib::navset_tab(id = ns("nested_tabset"),

                                                                   ### 1. Cons elec ----
                                                                   bslib::nav_panel("Consommation d'électricité",
                                                                   tags$div(
                                                                            br(),
                                                                   h5(strong("Consommation d'électricité : Enquête DGE-DIREN auprès des GRD")),
                                                                             tags$p("En 2022, la DGE-DIREN a procédé à sa première enquête auprès des gestionnaires de réseau de distribution (GRD) du canton. La distribution d'électricité à chaque
                                                                   point de mesure du territoire vaudois a pu être récoltée et ce sont ces données agrégées par commune qui alimentent cette application.",
                                                                   "La mise à jour est faite annuellement avec une année de retard, le relevé se faisant par exemple en fin d'année 2021 pour obtenir les données 2020 afin de garantir qu'un maximum de
                                                                   compteurs aient pu être relevés durant 2021 pour l'année 2020. Selon l'importance du traitement nécessaire,
                                                                   les données peuvent prendre quelques mois à être disponibles dans l'application.",
                                                                   tags$strong("Les répartitions sectorielles (ménages, services, etc.) ne sont pas encore disponibles, la nomenclature diffère selon chaque gestionnaire de réseau.
                                                                               Des travaux d'harmonisation sont en cours.")
                                                                   ),# End tags$p()
                                                                   br(),

                                                                   # Documentation table for electricity production
                                                                   DT::dataTableOutput(ns("elec_cons_doc")),
                                                                   br()
                                                                   )# End tags$div
                                                                   ),# End nested tabPanel 1.


                                                                ### 2. Prod élec ----
                                                                bslib::nav_panel("Production d'électricité",
                                                                                 # create div to apply class
                                                                                 tags$div(
                                                                                   br(),
                                                                                   # Overview method
                                                                                   h4(strong("Synthèse de la méthode")),

                                                                                   p("La grande majorité des installations de production d'électricité sont répertoriées par l'organisme de certification accrédité
                pour la saisie de garanties d'origine (GO) et le traitement des programmes d'encouragement de la Confédération concernant les énergies renouvelables.
                Les données pour le canton de Vaud sont transmises annuellement à la DGE-DIREN, qui après plusieurs traitements (harmonisation des extractions annuelles,
                nettoyage des communes, estimation de l'autoconsommation photovoltaïque, etc.) permet de créer une table de données pour chaque commune vaudoise.",
                                                                                     br(),
                                                                                     tags$a(href = "https://pronovo.ch/fr/", "Plus d'informations sur Pronovo AG", target = "_blank")),# open in new tab
                                                                                   p("La mise à jour est faite annuellement après réception et traitement des données, en général vers juin, par exemple juin 2022 pour les données 2021."),
                                                                                   DT::dataTableOutput(ns("elec_prod_doc")),
                                                                                   br(),

                                                                                   # Detailed method
                                                                                   h4(strong("Méthode détaillée")),
                                                                                   br(),
                                                                                   # Add download link
                                                                                   tags$a(href= "www/Pronovo_synthese_traitement_sans_recommendations.html",
                                                                                          target="_blank", "Télécharger la documentation",
                                                                                          download = "Pronovo_synthese_traitement_sans_recommendations.html"),
                                                                                   br(),

                                                                                   tags$iframe(src = "www/Pronovo_synthese_traitement_sans_recommendations.html",
                                                                                               target = "_self",
                                                                                               height = "800px", width = "100%")
                                                                                 )# End tags$div

                                                                ),# End nested tabPanel 2.

                                                                   ### 3. Regener ----
                                                                   bslib::nav_panel("Chaleur bâtiments",
                                                                                    # create div to apply class
                                                                                    tags$div(
                                                                                             br(),
                                                                                             h4(strong("Synthèse de la méthode")),
                                                                                             h5("Chaleur des bâtiments : exploitation du registre énergétique des bâtiments vaudois (RegEner)"),
                                                                                             tags$p("En 2022, la DGE-DIREN a procédé à une refonte du cadastre des énergies (CadEner, 2017) qui se base sur l'exploitation
                                 des données du registre cantonal des bâtiments (RCB) et de nombreuses autres données énergétiques du bâtiment
                                 (subventions, CECB, données empiriques de consommation, etc.). Une nouvelle méthode, plus détaillée et mieux coordonnée avec le
                                 registre fédéral des bâtiments (RegBL) a été élaborée en 2022 sous le nom de RegEner. Pour chaque bâtiment (EGID), jusqu'à 4 producteurs
                                 de chaleur peuvent être renseignés, les besoins et la consommation sont estimés selon la surface de référence énergétique, l'année et l'affectation du bâtiment
                                 ainsi que la présence d'une ou de plusieurs rénovations. Ces besoins et consommations sont théoriques mais fondés sur des données de consommation empiriques.
                                 Les données qui alimentent cette application sont le résultat d'agrégations du RegEner. De nouveaux indices seront prochainement calculés suite à la récolte importante
                                 de données de consommation et à l'ajout d'une correction climatique sur les besoins de chauffage.",
                                                                                                    br(),
                                                                                                    "Les", strong("besoins optimisés"),"traduisent les besoins théoriques si tous les bâtiments construits avant 2001 ou qui n'ont pas été rénovés lourdement
                                     après 2001 étaient assainis énergétiquement. Uniquement les besoins de chauffage sont concernés. Ces valeurs sont indicatives et ne reflètent pas un objectif politique.",
                                                                                                    tags$br(), tags$br(),
                                                                                                    "Davantage de détails peuvent être fournis sur demande à",
                                                                                                    tags$a(href = paste0("mailto:", mail_address, "."), mail_address,target = "_blank"),
                                                                                                    "Un document de synthèse méthodologique est prévu prochainement afin de présenter plus en détail la méthode appliquée.",
                                                                                                    tags$br(),
                                                                                                    "Les communes étant responsables de ce qui figure dans le registre cantonal des bâtiments (et par extension dans le registre fédéral des bâtiments),
                                     la qualité des données relatives aux agents énergétique est donc directement liée à l'état de mise à jour de ces données par la commune.
                                     La DGE-DIREN se tient à disposition des communes qui souhaiteraient améliorer ces données par la mise à jour des agents énergétiques dans ces registres."
                                                                                             ),# End tags$p()
                                                                                             br(),

                                                                                             # Documentation table for electricity production
                                                                                             DT::dataTableOutput(ns("regener_doc")),
                                                                                             br(),

                                                                                             h5(strong("Méthode détaillée à venir...")) #,
                                                                                             # br(),
                                                                                             # # Add download link when doc available
                                                                                             # tags$a(href= "www/synthese_regener_energyvd.html",
                                                                                             #        target="_blank", "Télécharger la documentation",
                                                                                             #        download = "profil-energie_doc_regener_v[???].html"),
                                                                                             # br(),

                                                                                    )# End tags$div

                                                                   ),# End nested tabPanel 3.

                                                                   ### 4. Subsidies ----
                                                                   bslib::nav_panel("Subventions bâtiments",
                                                                                    # create div to apply class
                                                                                    tags$div(
                                                                                             br(),
                                                                                             h4(strong("Synthèse de la méthode")),
                                                                                             h5("Subventions bâtiments : exploitation des données du Programme Bâtiments vaudois"),
                                                                                             br(),
                                                                                             tags$p("Ces données sont utilisées afin de fournir deux perspectives : une selon le nombre
                                         de bâtiments subventionnés, et une sur le nombre de subventions émises. Uniquement les
                                         subventions de travaux achevés sont présentés, les promesses de subventions ne sont pas intégrées."),
                                                                                             tags$a(href = "https://www.vd.ch/themes/environnement/energie/subventions-programme-batiments",
                                                                                                    target = "_blank", "Voir ce lien pour le détail des subventions"),
                                                                                             br(), br(),
                                                                                             h6(strong("Vue par bâtiments subventionnés")),
                                                                                             tags$p("Ces données de subventions sont croisées avec les données du registre énergétique des bâtiments (RegEner)
                                         afin de fournir une vision par bâtiments. Uniquement les mesures directes du Programme Bâtiments
                                          sont inclues dans ces statistiques. La catégorie 'Autres' regroupe les mesures M09 à M11 et M-16 à M-18.
                                         Afin de simplifier la représentation des données, aucune indication n'est faite lorsqu'une mesure de la
                                         catégorie 'Autres' co-existe avec d'autres mesures, ce qui est peu probable."),
                                                                                             br(),
                                                                                             h6(strong("Vue par subventions octroyées")),
                                                                                             tags$p("Ces données reflètent l'ensemble des mesures directes du Programme Bâtiments (M01 à M16)."),
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
                                                     tags$b("R"), "et la", tags$a(href = "https://shiny.rstudio.com/", "librairie Shiny", target = "_blank"),
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





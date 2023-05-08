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

    bs4Dash::tabsetPanel(id = ns("tabset"),

                         # tabPanel 'Général' ----
                         shiny::tabPanel(title = "Général",
                                         column(width = 8,
                                                # breathing
                                                br(),
                                                h4(strong("Pourquoi cette application ?")),
                                                tags$p("Le but de ce profil énergétique est de faciliter la planification énergétique des territoires en diffusant des informations disponibles au niveau communal qui ne sont pas facilement accessibles autrement.
           Bien que dédié principalement aux communes, cet outil se veut accessible pour tout le monde.
           Les différentes visualisations, les options de téléchargement ainsi que la possibilité d'exporter un rapport automatisé permettent d'explorer les données et de les exporter simplement."),
           # breathing
           br(),
           h4(strong("Contact")),
           tags$p("Vos suggestions, retours ou critiques sont précieux et nous permettent d'améliorer cette application. N'hésitez pas à les envoyer à l'unité données et indicateurs via ",
                  tags$a (href = paste0("mailto:", mail_address),mail_address,target = "_blank"), # open in new tab. address defined in utils_helpers.R
                  ", qui est l'unité transversale de la",
                  tags$a (href = link_diren, "direction de l'énergie de l'Etat de Vaud (DGE-DIREN)",
                          target = "_blank"),
                  "chargée des données énergétiques.")
                                         )),
           # tabPanel 'Données' ----
           shiny::tabPanel(title = "Données",

                           column(width = 11,
                                  # breathing
                                  br(),
                                  h4(strong("Sources des données")),
                                  tags$p("Les différentes informations fournies par cette application reflètent au mieux la réalité en fonction des données disponibles à la Direction de l'énergie.
                                    Néanmoins, l'exactitude de ces informations ne peut être garantie. En cas d'incohérence, n'hésitez pas à prendre contact afin d'en clarifier les causes probables."),
                                  br(),
                                  tags$p("Les sources de données suivantes sont utilisées : "),

                                  ## tabsetPanel (nested) ----
                                  bs4Dash::tabsetPanel(id = ns("nested_tabset"),type = "pills",



                ### Prod élec ----
                tabPanel("Production d'électricité",
                br(),
                tags$ul(
                  tags$li(h5(strong("Production d'électricité : PRONOVO")),
                          p("L'immense majorité des installations de production d'électricité sont répertoriées par l'organisme de certification accrédité
                pour la saisie de garanties d'origine (GO) et le traitement des programmes d'encouragement de la Confédération concernant les énergies renouvelables.
                Les données pour le canton de Vaud sont transmises annuellement à la DGE-DIREN, qui après plusieurs traitements (harmonisation des extractions annuelles,
                nettoyage des communes, ajout de l'autoconsommation PV, etc.) permet de créer une base de données pour chaque commune qui alimente cette application.",
                tags$a(href = "https://pronovo.ch/fr/", "Plus d'informations sur PRONOVO AG", target = "_blank")),# open in new tab
                p("La mise à jour est faite annuellement après réception et traitement des données, en général vers juin, par exemple juin 2022 pour les données 2021."))
                                                                ),# End tags$ul 1/3
                br(),

                # Documentation table for electricity production
                DT::dataTableOutput(ns("elec_prod_doc")),
                br()

                                                       ),# End nested tabPanel 1/3

                ### Cons elec ----
                # !!CONS_ELEC removed!! # tabPanel("Consommation d'électricité",
                # !!CONS_ELEC removed!! #          br(),
                # !!CONS_ELEC removed!! #          tags$ul(
                # !!CONS_ELEC removed!! #            tags$li(h5(strong("Consommation d'électricité : Enquête DGE-DIREN auprès des GRD")),
                # !!CONS_ELEC removed!! #                    p("En 2022, la DGE-DIREN a procédé à la première enquête auprès des gestionnaires de réseau de distribution (GRD) du Canton. L'injection d'électricité à chaque
                # !!CONS_ELEC removed!! # point de mesure du territoire vaudois a pu être récoltée, le type de client associé à chaque point de mesure a été catégorisé en secteurs,
                # !!CONS_ELEC removed!! # et ce sont ces données agrégées par commune qui alimentent cette application."),
                # !!CONS_ELEC removed!! # p("La mise à jour est faite annuellement avec une année de retard, le relevé se fait par exemple en fin d'année 2021 pour obtenir les données 2020 afin de garantir qu'un maximum de
                # !!CONS_ELEC removed!! # compteurs aient pu être relevés durant 2021 pour l'année 2020. Selon l'importance du traitement nécessaire,
                # !!CONS_ELEC removed!! # les données peuvent prendre quelques mois à être disponibles dans l'application.",
                # !!CONS_ELEC removed!! # tags$strong("Les répartitions sectorielles sont encore hautement incertaines, la méthode diffère notamment selon chaque gestionnaire de réseau.
                # !!CONS_ELEC removed!! #             Une meilleure catégorisation et davantage d'harmonisation sont souhaitées à terme mais c'est un long processus.")))
                # !!CONS_ELEC removed!! #          ),# End tags$ul 2/3
                # !!CONS_ELEC removed!! # br(),
                # !!CONS_ELEC removed!! #
                # !!CONS_ELEC removed!! # # Documentation table for electricity production
                # !!CONS_ELEC removed!! # DT::dataTableOutput(ns("elec_cons_doc")),
                # !!CONS_ELEC removed!! # br()
                # !!CONS_ELEC removed!! #
                # !!CONS_ELEC removed!! # ),# End nested tabPanel 2/3

                ### Regener ----
                tabPanel("Chaleur bâtiments",
                         br(),
                         tags$ul(
                           tags$li(h5(strong("Chaleur des bâtiments : exploitation du registre énergétique des bâtiments vaudois (RegEner)")),
                                   tags$p("En 2022, la DGE-DIREN a procédé à une refonte du cadastre des énergies (CadEner, 2017) qui se base sur l'exploitation
                                     des données du registre cantonal des bâtiments (RCB) et de nombreuses autres données énergétiques du bâtiment
                                     (subventions, CECB, données empiriques de consommation, etc.). Une nouvelle méthode, plus détaillée et mieux coordonnée avec le
                                     registre fédéral des bâtiments (RegBL) a été élaborée en 2022 sous le nom de RegEner. Pour chaque bâtiment (EGID), jusqu'à 4 producteurs
                                     de chaleur peuvent être renseignés, les besoins et la consommation sont estimés selon la surface de référence énergétique, l'année et l'affectation du bâtiment
                                     ainsi que la présence d'une ou de plusieurs rénovations. Ces besoins et consommations sont théoriques mais fondés sur des données de consommation empiriques.
                                     Les données qui alimentent cette application sont le résultat d'agrégations du RegEner. De nouveaux indices seront prochainement calculés suite à la récolte de
                                     davantage de données de consommation. L'élaboration d'une méthode de correction climatique est également en cours.",
                                     "Les", strong("besoins optimisés"), "traduisent les besoins théoriques si tous les bâtiments construits avant 2001 ou qui n'ont pas été rénovés lourdement
                                     après 2001 étaient assainis énergétiquement. Uniquement les besoins de chauffage sont concernés. Ces valeurs sont indicatives et ne reflètent pas un objectif politique.",
                                     br(),
                                     "Davantage de détails peuvent être fournis sur demande à",
                                     tags$a(href = paste0("mailto:", mail_address, "."), mail_address,target = "_blank"),
                                     "Un document de synthèse méthodologique est prévu prochainement afin de présenter la méthode appliquée."))
                         ),# End tags$ul 2/3
                         br(),

                         # Documentation table for electricity production
                         DT::dataTableOutput(ns("regener_doc")),
                         br()

                )# End nested tabPanel 3/3
                                  )# End nested tabsetPanel within 'Données'
                           )# End column
           ),# End tabPanel 'Données'

           # Glossaire ----

           shiny::tabPanel(title = "Glossaire",
                           column(width = 8,
                                  # breathing
                                  br(),
                                  tags$h4(strong("Glossaire")),
                                  tags$p("Ci-dessous un lexique des principales abbréviations et termes techniques utilisés dans cette application."),
                                  DT::dataTableOutput(ns("glossary_table")),

                           )# End column
           ),# End tabPanel 'Confidentialité'

           # Techno ----
           shiny::tabPanel(title = "Technologie",


                           column(width = 8,
                                  # breathing
                                  br(),
                                  tags$h4(strong("Technologie")),
                                  tags$p("Cette application est programmée avec le language",
                                         tags$b("R"), "et la", tags$a(href = "https://shiny.rstudio.com/", "librairie Shiny", target = "_blank"),
                                         "qui permet la création d'applications web interactives. L'architecture de l'application est construite
            à l'aide de la",
            tags$a(href = "https://github.com/ThinkR-open/golem", "librairie golem (ThinkR)", target = "_blank"),
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
            fluidRow(
              column(1, div(style = "margin-top: -10px;", # inline css for nice alignment with p()
                            shinydashboardPlus::socialButton(
                              href = "https://github.com/mick-weber/eneRgyVD",
                              icon = icon("github", "fa-2x") # 2x bigger
                            ),# End socialbutton
              )# End div
              ),# End column
              column(3, p("Code complet sur GitHub"))
            )# End fluidRow
                           )# End column
           )# End tabPanel 'Technologie'


    )# End main tabsetPanel


  )# End tagList
}

#' about_the_app Server Functions
#'
#' @noRd
mod_about_the_app_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Table for electricity production documentation
    output$elec_prod_doc <- DT::renderDataTable({

      elec_prod_doc %>%  # loaded in utils_helpers.R
        create_doc_table_dt(doc_prefix = "doc_elec_prod_") # fct_helpers.R

    })

    # Table for electricity consumption documentation

    # !!CONS_ELEC removed!! # output$elec_cons_doc <- DT::renderDataTable({
    # !!CONS_ELEC removed!! #
    # !!CONS_ELEC removed!! #   elec_cons_doc %>%  # loaded in utils_helpers.R
    # !!CONS_ELEC removed!! #     create_doc_table_dt(doc_prefix = "doc_elec_cons_") # fct_helpers.R
    # !!CONS_ELEC removed!! #
    # !!CONS_ELEC removed!! # })


    output$regener_doc <- DT::renderDataTable({
      regener_doc %>%
        create_doc_table_dt(doc_prefix = "doc_regener_") # fct_helpers.R
    })



    output$glossary_table <- DT::renderDataTable({
      glossary %>%
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





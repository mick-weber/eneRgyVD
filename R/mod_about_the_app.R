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

    bs4Dash::tabsetPanel(

      shiny::tabPanel(title = "Général",
                      column(width = 8,
                             # breathing
                             br(),
                             h4(strong("Contact")),
                             tags$p("Vos suggestions, retours ou critiques sont précieux et nous permettent d'améliorer cette application. N'hésitez pas à les envoyer à l'Unité Données et Indicateurs via ",
                                    tags$a (href = paste0("mailto:", mail_address),mail_address,target = "_blank"), # open in new tab. address defined in utils_helpers.R
                                    ", qui est l'unité transversale de la",
                                    tags$a (href = link_diren, "direction de l'énergie de l'Etat de Vaud (DGE-DIREN)",
                                            target = "_blank"),
                                    "chargée des données énergétiques."),
                             # breathing
                             br(),
                             h4(strong("Pourquoi eneRgyVD ?")),
                             tags$p("Le but de cette application est de faciliter la planification énergétique des territoires en diffusant des informations disponibles au niveau communal qui ne sont pas facilement accessibles autrement.
           Bien que dédié principalement aux communes, cet outil se veut accessible pour tout le monde.
           Les différentes visualisations, les options de téléchargement ainsi que la possibilité d'exporter un rapport automatisé permettent d'explorer les données et de les exporter simplement.")
                      )),
      shiny::tabPanel(title = "Données",


                      column(width = 11,
                             # breathing
                             br(),
                             h4(strong("Sources des données")),
                             tags$p("Les sources de données suivantes sont utilisées : "),

                             bs4Dash::tabsetPanel(
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
                                        ),# End tags$ul 1/2
                br(),

                # Documentation table for electricity production
                DT::dataTableOutput(ns("elec_prod_doc")),
                br()

                               ),# End nested tabPanel 1/2

                tabPanel("Consommation d'électricité",
                         br(),
                         tags$ul(
                           tags$li(h5(strong("Consommation d'électricité : Enquête DGE-DIREN auprès des GRD")),
                                   p("En 2022, la DGE-DIREN a procédé à la première enquête auprès des gestionnaires de réseau de distribution (GRD) du Canton. L'injection d'électricité à chaque
                point de mesure du territoire vaudois a pu être récoltée, le type de client associé à chaque point de mesure a été catégorisé en secteurs,
                et ce sont ces données agrégées par commune qui alimentent cette application."),
                p("La mise à jour est faite annuellement avec une année de retard, le relevé se fait par exemple en fin d'année 2021 pour obtenir les données 2020 afin de garantir qu'un maximum de
                compteurs aient pu être relevés durant 2021 pour l'année 2020. Selon l'importance du traitement nécessaire,
                les données peuvent prendre quelques mois à être disponibles dans l'application."))
                         ),# End tags$ul 2/2
                br(),

                # Documentation table for electricity production
                DT::dataTableOutput(ns("elec_cons_doc")),
                br()

                )# End nested tabPanel 2/2
                             )# End nested tabsetPanel
                      )# End column
      ),# End tabPanel 'Données'


      shiny::tabPanel(title = "Confidentialité",
                      column(width = 8,
                             # breathing
                             br(),
                             tags$h4(strong("Protection des données")),
                             tags$p("Attente de confirmation de l'appui juridique DGE-DIREN que les données par communes présentent dans l'application
           ne sont pas sujettes à restrictions juridiques. Pour cette raison, des données aléatoires sont temporairement diffusées.")
                      )# End column
      ),# End tabPanel 'Confidentialité'


      shiny::tabPanel(title = "Technologie",


                      column(width = 8,
                             # breathing
                             br(),
                             tags$h4(strong("Technologie")),
                             tags$p("Cette application est programmée avec le language",
                                    tags$b("R"), "et la", tags$a(href = "https://shiny.rstudio.com/", "librairie Shiny", target = "_blank"),
                                    "qui permet la création simple d'applications web interactives. L'architecture de l'application est construite
            à l'aide de la",
            tags$a(href = "https://github.com/ThinkR-open/golem", "librairie golem", target = "_blank"),
            "et le contenu des visualisations par de multiples librairies issues notamment du",
            tags$a(href = "https://www.tidyverse.org/", "tidyverse.", target = "_blank"),
            "De plus amples demandes d'informations peuvent être adressées directement à l'e-mail de contact",
            tags$a (href = paste0("mailto:", mail_address),mail_address,target = "_blank"),
            "."),
            br(),
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
    output$elec_cons_doc <- DT::renderDataTable({

      elec_cons_doc %>%  # loaded in utils_helpers.R
        create_doc_table_dt(doc_prefix = "doc_elec_cons_") # fct_helpers.R

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





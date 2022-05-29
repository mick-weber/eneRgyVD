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
    column(width = 12,
    h4(strong("Contact")),
    tags$p("Vos suggestions, retours ou critiques sont précieux et nous permettent d'améliorer cette application. N'hésitez pas à les envoyer à l'Unité Données et Indicateurs via ",
           tags$a (href = "mailto:stats.energie@vd.ch", "stats.energie@vd.ch", target = "_blank"), # open in new tab
           ", qui est l'unité transversale de la Direction de l'énergie (DGE-DIREN) chargée des données énergétiques."),
    h4(strong("Pourquoi eneRgyVD ?")),
    tags$p("Le but de cette application est de faciliter la planification énergétique des territoires en diffusant des informations disponibles au niveau communal qui ne sont pas facilement accessibles autrement.
           Bien que dédié principalement aux communes, cet outil se veut accessible pour tout le monde.
           Les différentes visualisations, les options de téléchargement ainsi que la possibilité d'exporter un rapport automatisé permettent une interprétation facilitée de ces données."),
    h4(strong("Sources des données")),
    tags$p("Les sources de données suivantes sont utilisées : "),
    tags$ul(
      tags$li(h5(strong("Production d'électricité : PRONOVO")),
              p("L'immense majorité des installations de production d'électricité sont répertoriées par l'organisme de certification accrédité
                pour la saisie de garanties d'origine (GO) et le traitement des programmes d'encouragement de la Confédération concernant les énergies renouvelables.
                Les données pour le canton de Vaud sont transmises annuellement à la DGE-DIREN, qui après plusieurs traitements (harmonisation des extractions annuelles,
                nettoyage des communes, ajout de l'autoconsommation PV, etc.) permet de créer une base de données pour chaque commune qui alimente cette application.",
                tags$a(href = "https://pronovo.ch/fr/", "Plus d'informations sur PRONOVO AG", target = "_blank")),# open in new tab
              p("La mise à jour est faite annuellement après réception et traitement des données, en général vers juin, par exemple juin 2022 pour les données 2021.")),
      tags$li(h5(strong("Consommation d'électricité : Enquête DGE-DIREN auprès des GRD")),
              p("En 2022, la DGE-DIREN a procédé à la première enquête auprès des gestionnaires de réseau de distribution (GRD) du Canton. L'injection d'électricité à chaque
                point de mesure du territoire vaudois a pu être récoltée, le type de client associé à chaque point de mesure a été catégorisé en secteurs,
                et ce sont ces données agrégées par commune qui alimentent cette application."),
              p("La mise à jour est faite annuellement, le relevé se fait par exemple en fin d'année 2021 pour obtenir les données 2020 afin de garantir qu'un maximum de
                compteurs aient été relevés durant 2021 pour l'année 2020. Selon la traitement nécessaire, les données peuvent prendre quelques mois à être disponibles dans l'application."))
    ),# End tags$ul
    tags$h4(strong("Protection des données")),
    tags$p("Attente de confirmation de l'appui juridique DGE-DIREN que les données par communes présentent dans l'application ne sont pas sujettes à restrictions juridiques."),
    tags$h4(strong("Technologie")),
    tags$p("Cette application est programmée avec le language", tags$b("R"), "et la", tags$a(href = "https://shiny.rstudio.com/", "librairie Shiny", target = "_blank"),
           "qui permet la création simple d'applications web interactives.")
    )# End column
  )# End tagList
}

#' about_the_app Server Functions
#'
#' @noRd
mod_about_the_app_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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





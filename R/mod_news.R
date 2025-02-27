#' news UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_news_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::layout_columns(col_widths = c(-1, 8, -3), fill = FALSE,

                          h4(strong("Nouveautés")),

                          bslib::accordion(open = c("tocome", "feb25"), # refers to 'value' of accordion_panel. !!update when adding entry!!!!
                                           multiple = TRUE,

                                           # Next updates ----
                                           bslib::accordion_panel(title = HTML("<strong>À venir</strong>"),
                                                                  value = "tocome",
                                                                  icon = phosphoricons::ph(title = NULL, "question", height = "1.5em"),
                                                                  tagList(
                                                                    phosphoricons::ph(title = NULL, "dots-three", height = "1.5em"),
                                                                    "Mise à jour des données selon disponibilité propre à chaque thématique")
                                           ),

                                           # Février 2025

                                           bslib::accordion_panel(title = HTML("<strong>Février 2025</strong>"),
                                                                  value = "feb25",
                                                                  icon = phosphoricons::ph(title = NULL, "calendar-check", height = "1.5em"),
                                                                  tagList(phosphoricons::ph(title = NULL, "star", height = "1.5em"),
                                                                       "Migration profil climatique"),
                                                                  tags$li(phosphoricons::ph(title = NULL, "arrows-clockwise", height = "1.5em"),
                                                                          "Mise à jour des données de subventions Programme bâtiments 2024"),
                                                                  tags$li(phosphoricons::ph(title = NULL, "arrows-clockwise", height = "1.5em"),
                                                                          "Mise à jour des données de chaleur des bâtiments 2024"),
                                                                  tags$li(phosphoricons::ph(title = NULL, "folder-plus", height = "1.5em"),
                                                                       "Ajout des données de distribution de gaz 2018-2022"),
                                                                  tags$li(phosphoricons::ph(title = NULL, "folder-plus", height = "1.5em"),
                                                                       "Ajout des données de surface de canopée en zone urbaine"),
                                                                  tags$li(phosphoricons::ph(title = NULL, "folder-plus", height = "1.5em"),
                                                                       "Ajout des données des bâtiments exposés à des dangers naturels"),
                                                                  tags$li(phosphoricons::ph(title = NULL, "folder-plus", height = "1.5em"),
                                                                       "Ajout des données de qualité de zone de desserte des transports publics"),
                                                                  tags$li(phosphoricons::ph(title = NULL, "folder-plus", height = "1.5em"),
                                                                       "Ajout des données de part de véhicules électriques"),
                                                                  tags$li(phosphoricons::ph(title = NULL, "folder-plus", height = "1.5em"),
                                                                       "Ajout des données de taux de motorisation")
                                           ),

                                           # Octobre 2024

                                           bslib::accordion_panel(title = HTML("<strong>Octobre 2024</strong>"),
                                                                  value = "oct24",
                                                                  icon = phosphoricons::ph(title = NULL, "calendar-check", height = "1.5em"),
                                                                  tagList(phosphoricons::ph(title = NULL, "arrows-clockwise", height = "1.5rem"),
                                                                       "Mise à jour des données de distribution d'électricité 2022")
                                           ),

                                           # Juillet 2024 ----

                                           bslib::accordion_panel(title = HTML("<strong>Juillet  2024</strong>"),
                                                                  value = "july24",
                                                                  icon = phosphoricons::ph(title = NULL, "calendar-check", height = "1.5em"),
                                                                  tagList(phosphoricons::ph(title = NULL, "arrows-clockwise", height = "1.5rem"),
                                                                       "Mise à jour des données de production d'électricité 2023 et révision des années 2015-2022")
                                           ),

                                           # April 2024 ----
                                           bslib::accordion_panel(title = HTML("<strong>Avril 2024</strong>"),
                                                                  value = "april24",
                                                                  icon = phosphoricons::ph(title = NULL, "calendar-check", height = "1.5em"),

                                                                  tagList(phosphoricons::ph(title = NULL, "folder-plus", height = "1.5em"),
                                                                       "Ajout des données de subventions Programme Bâtiments"),
                                                                  br(),br(),
                                                                  tagList(phosphoricons::ph(title = NULL, "folder-plus", height = "1.5em"),
                                                                       "Ajout des données de distribution d'électricité 2018-2021 (2022 à venir)"),
                                                                  br(),br(),
                                                                  tagList(phosphoricons::ph(title = NULL, "folder-plus", height = "1.5em"),
                                                                          HTML(
                                                                            "Ajout des données chaleur bâtiments 2023 (méthode 2024)
                                                                       <br>
                                                                       <li>   Correctif 30.04.2024 avec pour effet +2.4% de besoins et de consommation (correctif IBC)"
                                                                          )),
                                                                  br(),br(),
                                                                  tagList(phosphoricons::ph(title = NULL, "arrows-clockwise", height = "1.5rem"),
                                                                       "Mise à jour rétroactive des données chaleur bâtiments 2022 (méthode 2024)")
                                           ),

                                           # June 2023 ----
                                           bslib::accordion_panel(title = HTML("<strong>Juin 2023</strong>"),
                                                                  value = "june23",
                                                                  icon = phosphoricons::ph(title = NULL, "calendar-check", height = "1.5em"),

                                                                  tagList(phosphoricons::ph(title = NULL, "star", height = "1.5em"),
                                                                       "Mise en ligne du profil énergétique"),
                                                                  br(),br(),
                                                                  tagList(phosphoricons::ph(title = NULL, "folder-plus", height = "1.5em"),
                                                                       "Ajout des données chaleur bâtiments 2022 (méthode 2023)"),
                                                                  br(),br(),
                                                                  tagList(phosphoricons::ph(title = NULL, "folder-plus", height = "1.5em"),
                                                                       "Ajout des données de production d'électricité 2015-2022")
                                           )
                          )# End accordion
    )# End layout_columns
  )
}

#' news Server Functions
#'
#' @noRd
mod_news_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_news_ui("news_1")

## To be copied in the server
# mod_news_server("news_1")

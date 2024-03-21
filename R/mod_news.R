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

                          br(),
                          h4(strong("Historique des versions")),
                          br(),

                          bslib::accordion(open = HTML("<strong>Avril 2024</strong>"), # Open only the latest update (item 2)
                                           multiple = TRUE,

                                           # Next updates ----
                                           bslib::accordion_panel(title = HTML("<strong>À venir</strong>"),
                                                                  icon = bsicons::bs_icon("question-circle", size = "1.5rem"),

                                                                  HTML(bsicons::bs_icon("database-fill-add", size = "1.5rem"),
                                                                       "Ajout des données de distribution de gaz 2018-2022"),
                                                                  br(),br(),
                                                                  HTML(bsicons::bs_icon("arrow-clockwise", size = "1.5rem"),
                                                                       "Mise à jour des données de production d'électricité 2023"),
                                                                  br(),br(),
                                                                  HTML(bsicons::bs_icon("arrow-clockwise", size = "1.5rem"),
                                                                       "Mise à jour des données de distribution d'électricité 2022")
                                           ),
                                           # April 2024 ----
                                           bslib::accordion_panel(title = HTML("<strong>Avril 2024</strong>"),
                                                                  icon = bsicons::bs_icon("calendar-check", size = "1.5rem"),

                                                                  HTML(bsicons::bs_icon("database-fill-add", size = "1.5rem"),
                                                                       "Ajout des données de subventions Programme Bâtiments"),
                                                                  br(),br(),
                                                                  HTML(bsicons::bs_icon("database-fill-add", size = "1.5rem"),
                                                                       "Ajout des données de distribution d'électricité 2018-2021 (2022 à venir)"),
                                                                  br(),br(),
                                                                  HTML(bsicons::bs_icon("database-fill-add", size = "1.5rem"),
                                                                       "Ajout des données chaleur bâtiments 2023 (méthode 2024)"),
                                                                  br(),br(),
                                                                  HTML(bsicons::bs_icon("arrow-clockwise", size = "1.5rem"),
                                                                       "Mise à jour rétroactive des données chaleur bâtiments 2022 (méthode 2024)")
                                           ),

                                           # June 2023 ----
                                           bslib::accordion_panel(title = HTML("<strong>Juin 2023</strong>"),
                                                                  icon = bsicons::bs_icon("calendar-check", size = "1.5rem"),

                                                                  HTML(bsicons::bs_icon("database-fill-add", size = "1.5rem"),
                                                                       "Ajout des données chaleur bâtiments 2022 (méthode 2023)"),
                                                                  br(),br(),
                                                                  HTML(bsicons::bs_icon("database-fill-add", size = "1.5rem"),
                                                                       "Ajout des données de production d'électricité 2015-2022"),
                                                                  br(),br(),
                                                                  HTML(bsicons::bs_icon("database-fill-add", size = "1.5rem"),
                                                                       "Mise en ligne du profil énergétique")
                                           )
                          )
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

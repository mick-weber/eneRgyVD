#' collapse_stats_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_collapse_stats_box_ui <- function(id){

  ns <- NS(id)

  tagList(
    bslib::as_fill_carrier(uiOutput(ns("statbox"))) # https://rstudio.github.io/bslib/reference/as_fill_carrier.html
  )

}

#' collapse_stats_box Server Functions
#'
#' @noRd
mod_collapse_stats_box_server <- function(id,
                                          title,
                                          selectedUnit,
                                          prod_elec_value,
                                          cons_elec_value,
                                          cons_rg_value,
                                          year){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    output$statbox <- renderUI({

      library(bslib)
      library(bsicons)

        bslib::card(fill = TRUE,
                    bslib::card_header(title,
                                       class = "bg-secondary"),




                    bslib::layout_column_wrap(width = 1/3,heights_equal = "all",


                                              tags$div(class = "text-center padding-top-1 rounded",

                                                       bsicons::bs_icon("lightning-charge-fill", size = "2rem", class = "text-warning"),
                                                       p(HTML("Production<br>électrique"), class = "p-0 m-0", style = "font-size:1.1rem;"),
                                                       tags$div(
                                                       strong("200 GWh", class = "fs-5"),
                                                       p("2022")
                                                       )

                                              ),

                                              tags$div(class = "text-center padding-top-1",

                                                       bsicons::bs_icon("fire", size = "2rem", class = "text-danger"),
                                                       p(HTML("Chaleur<br>bâtiments"), class = "p-0 m-0", style = "font-size:1.1rem;"),

                                                       tags$div(
                                                       strong("1'200 GWh", class = "fs-5"),
                                                       p("2022")
                                                       )

                                              ),

                                              tags$div(class = "text-center padding-top-1 rounded",

                                                       bsicons::bs_icon("house-check-fill", size = "2rem", class = "text-success"),
                                                       p(HTML("Subventions<br>bâtiments"), class = "p-0 m-0", style = "font-size:1.1rem;"),

                                                       tags$div(
                                                       strong("1'100", class = "fs-5"),
                                                       p("2022")

                                                       )

                                              ),

                                              tags$div(class = "text-center padding-top-1 rounded",

                                                       bsicons::bs_icon("activity", size = "2rem", class = "text-info"),
                                                       p(HTML("Morts<br>cardiaques"), class = "p-0 m-0", style = "font-size:1.1rem;"),

                                                       tags$div(
                                                         strong("1'400", class = "fs-5"),
                                                         p("2022")

                                                       )

                                              ),

                                              tags$div(class = "text-center padding-top-1 rounded",

                                                       bsicons::bs_icon("apple", size = "2rem", class = "text-dark"),
                                                       p(HTML("Pommes<br>mangées"), class = "p-0 m-0", style = "font-size:1.1rem;"),

                                                       tags$div(
                                                         strong("2", class = "fs-5"),
                                                         p("2022")

                                                       )

                                              )

                    )



        )# End card
    })# End RenderUI
  })# End moduleServer
}




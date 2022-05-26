#' unit_converter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_unit_converter_ui <- function(id){
  ns <- NS(id)
  tagList(

    shinydashboardPlus::dropdownBlock(
      id = ns("unit_dropdown"),
      title = "Conversion d'unités",
      badgeStatus = NULL, # avoids the annoying notif number counting the widgets in the dropdownBlock
      icon = icon("refresh", lib = "glyphicon"),

      numericInput(inputId = ns("value"), label = "Saisir une valeur", value = 0),

    fluidRow(column(6,
      prettyRadioButtons(inputId = ns("in_unit"), label = "Convertir de...",
                   choices = c("kWh", "MWh", "GWh", "TJ"),
                   inline = FALSE, width = "150px")),
      column(6,
      prettyRadioButtons(inputId = ns("out_unit"), label = "À...",
                         choices = c("kWh", "MWh", "GWh", "TJ"),
                         inline = FALSE, width = "50px"))),

    shinyWidgets::actionBttn(
      inputId = ns("convert"),
      label = "Convertir",
      style = "material-flat", size = "xs", icon = icon("refresh", lib = "glyphicon"),
      color = "success"
    ),

    fluidRow(br()),

    textOutput(ns("value_callback")),

      div(verbatimTextOutput(ns("result"))) # The output style is modified in custom.css, 'pre' selector


)
    )
}

#' unit_converter Server Functions
#'
#' @noRd
mod_unit_converter_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    value_callback <- eventReactive(input$convert, {

      return(
        paste(
          format(input$value, big.mark = "'", scientific = FALSE), input$in_unit, "équivalent à ..."
        )
      )
    })

    result <- eventReactive(input$convert, {

      # the conversion table is "hand" computed in utils_helpers.R
      factor <- conv_table %>%
        dplyr::filter(from == input$in_unit,
                      to == input$out_unit) %>%
        pull(factor)

      # We format the result in this way :  100 kWh = 0.1 MWh
      result <- paste(

        format(input$value*factor, big.mark = "'", scientific = FALSE), input$out_unit

        )

      return(result)

    })

output$value_callback <- renderText({

  value_callback()

})

output$result <- renderText({

  result()

  })


  })
}



  # testing module
  # nameApp <- function() {
  #     ui <- fluidPage(
  #       mod_unit_converter_ui("id")
  #     )
  #     server <- function(input, output, session) {
  #       mod_unit_converter_server("id")
  #     }
  #     shinyApp(ui, server)
  # }
  # nameApp()

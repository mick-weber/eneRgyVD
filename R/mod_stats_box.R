#' collapse_stats_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_stats_box_ui <- function(id){

  ns <- NS(id)

  tagList(

    bslib::as_fill_carrier(uiOutput(ns("statbox"))) # REQUIRED : https://rstudio.github.io/bslib/reference/as_fill_carrier.html
  )

}

#' collapse_stats_box Server Functions
#'
#' @noRd
mod_stats_box_server <- function(id,
                                 parent,
                                 title,
                                 energyUnit,
                                 elec_prod_value,
                                 elec_cons_value,
                                 cons_rg_value,
                                 subsidies_value,
                                 year_elec_cons,
                                 year_elec_prod,
                                 year_rgr,
                                 year_subsidies){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    ## |---------------------------------------------------------------|
    ##          Render statboxes dynamically
    ## |---------------------------------------------------------------|

    output$statbox <- renderUI({

      bslib::as_fill_carrier() # REQUIRED : https://rstudio.github.io/bslib/reference/as_fill_carrier.html

        bslib::card(fill = TRUE,
                    #max_height = "50vh", # Limit the extension when VD box is displayed alone

                    bslib::card_header(title, bslib::tooltip(
                      id = ns("tooltip_card"),
                      placement = "right",
                      options = list(customClass = "customTooltips"), # custom.scss
                      trigger = bsicons::bs_icon("info-circle"),
                      "Les années de référence correspondent aux dernières années pour lesquelles des données sont disponibles"
                    ),
                                       class = "bg-secondary"),

                    bslib::card_body(

                      class = "align-middle",
                      style = "flex:none;",

                    bslib::layout_column_wrap(width = "140px",
                                              fixed_width = TRUE,
                                              heights_equal = "all",
                                              fill = FALSE,
                                              fillable = TRUE,
                                              class = "justify-content-center",
                                              # fct_helpers.R

           # 1. Cons elec
           make_statbox_item(iconBgClass = "iconBgElecCons",
                            title = "Distribution<br>électricité", value = elec_cons_value, unit = energyUnit, year = year_elec_cons) |>
             # We assign an input on click, which is used below to redirect to the correct nav_panel
             tags$a(style = "cursor:zoom-in;", onclick = glue::glue(.open = '{{',.close = '}}',
                                                                    # reload the input to trigger observeEvent below
                                         "Shiny.setInputValue(id = '{{id}}-elec_cons_statbox_click', value = '{{id}}-elec_cons_statbox_click', {priority : 'event'})")
                    ),


           # 2. RG cons
           make_statbox_item(iconBgClass = "iconBgRgr",
                             title = "Consommation<br>chaleur bâtiment", value = cons_rg_value, unit = energyUnit, year = year_rgr) |>
             # We assign an input on click, which is used below to redirect to the correct nav_panel
             tags$a(style = "cursor:zoom-in;", onclick = glue::glue(.open = '{{',.close = '}}',
                                                                    # reload the input to trigger observeEvent below
                                                                    "Shiny.setInputValue(id = '{{id}}-rgr_cons_statbox_click', value = '{{id}}-rgr_cons_statbox_click', {priority : 'event'})")
             ),

           # 3. Subsidies M01
           make_statbox_item(iconBgClass = "iconBgSubs",
                             title = "Subventions<br>rénovation M01", value = subsidies_value, unit = "dossiers", year = year_subsidies) |>
             # We assign an input on click, which is used below to redirect to the correct nav_panel
             tags$a(style = "cursor:zoom-in;", onclick = glue::glue(.open = '{{',.close = '}}',
                                                                    # reload the input to trigger observeEvent below
                                                                    "Shiny.setInputValue(id = '{{id}}-subsidies_meausre_statbox_click', value = '{{id}}-subsidies_meausre_statbox_click', {priority : 'event'})")
             ),

           # 4. Prod elec
           make_statbox_item(iconBgClass = "iconBgElecProd",
                             title = "Production<br>électricité", value = elec_prod_value, unit = energyUnit, year = year_elec_prod) |>
             # We assign an input on click, which is used below to redirect to the correct nav_panel
             tags$a(style = "cursor:zoom-in;", onclick = glue::glue(.open = '{{',.close = '}}',
                                                                    # reload the input to trigger observeEvent below
                                                                    "Shiny.setInputValue(id = '{{id}}-elec_prod_statbox_click', value = '{{id}}-elec_prod_statbox_click', {priority : 'event'})")
             )
                    )# End layout_column_wrap
        )# End card body
        )# End card
    })# End RenderUI


    ## |---------------------------------------------------------------|
    ##          Logic to redirect statbox clicks with nav_select()
    ## |---------------------------------------------------------------|
    # We use parent argument that inherits from app_server.R's main session so we can navigate as if we were from there
    # alternative would be to assign each input to reactives, to retrieve them in app_server and to redirect from there.


    observeEvent(input$elec_cons_statbox_click, {
      bslib::nav_select(id = "nav", selected = "Electricité", session = parent)
      bslib::nav_select(id = "navset_elec", selected = "Distribution d'électricité", session = parent)
    })

    observeEvent(input$elec_prod_statbox_click, {
      bslib::nav_select(id = "nav", selected = "Electricité", session = parent)
      bslib::nav_select(id = "navset_elec", selected = "Production d'électricité", session = parent)
    })

    observeEvent(input$subsidies_meausre_statbox_click, {
      bslib::nav_select(id = "nav", selected = "Subventions bâtiments", session = parent)
      bslib::nav_select(id = "navset_subsidies", selected = "Vue par subventions", session = parent)
    })

    observeEvent(input$rgr_cons_statbox_click, {
      bslib::nav_select(id = "nav", selected = "Chaleur des bâtiments", session = parent)
      bslib::nav_select(id = "navset_regener", selected = "Consommation des bâtiments", session = parent)
    })




  })# End moduleServer
}




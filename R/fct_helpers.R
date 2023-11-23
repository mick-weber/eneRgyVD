# Info fn ----

#' info_dev_message()
#' shinyalert popup message from the dev to explain what is the app made for
#' @importFrom shinyalert shinyalert
#' @return A shinyalert object when opening the app

info_dev_message <- function(){

  shinyalert::shinyalert(
    inputId = "welcome-msg",
    title = "Bienvenue sur le profil énergétique des communes !",
                         text = paste0("Cette application est mise à disposition par la ",
                                       tags$a(href = link_diren, target = "_blank", "Direction de l'énergie du Canton de Vaud (DGE-DIREN)"),
                                       " afin de diffuser des données énergétiques à l'échelle des communes vaudoises.",
                                       tags$br(),
                                       "Cette démarche s'inscrit notamment dans l'accompagnement du Canton afin de faciliter l'élaboration des ",
                                       tags$a(href = link_pter, target = "_blank", # utils_helpers.R
                                              "planifications énergétiques territoriales."),
                                       tags$br(), tags$br(),
                                       "Attention : cette application contient des données résultant de méthodologies complexes vouées à améliorations.
                                       Pour cette raison, des valeurs peuvent changer de manière rétroactive.
                                       Il est donc important d'interpréter ces données avec précaution et d'anticiper le fait que celles-ci puissent changer au gré des prochaines mises à jour."
                                       ),
                         html = TRUE,
                         size = "m",
                         closeOnEsc = TRUE,
                         closeOnClickOutside = TRUE,
                         type = "info",
                         showConfirmButton = TRUE,
                         showCancelButton = FALSE,
                         confirmButtonText = "OK",
                         timer = 0,
                         animation = "pop"
  )

}

# Graph fns ----

#' create_select_leaflet()
#'
#' @description Creates the non-reactive part of the home leaflet map to select municipalities and interact with selectInputs.
#' @import leaflet
#' @return A leaflet base map without reactivity.
#'
#' @noRd

create_select_leaflet <- function(sf_districts,
                                  sf_lacs,
                                  sf_communes){

  leaflet::leaflet(options = leafletOptions(
    zoomControl = TRUE,
    zoomSnap = 0.1, # small scroll zoom
    zoomDelta = 0.2, # bigger button zoom
    minZoom = 9, # lock the back zoom range
    maxZoom = 12, # limit zoom max
    attributionControl = F # remove leaflet url
    )) |>
    # Couche de base des districts si un district est sélectionné
    leaflet::addPolygons(data = sf_districts,
                         fillColor = NULL,
                         fillOpacity = 0,
                         color = "black",
                         label = ~NOM_MAJ,
                         weight = 2,
                         # group is then used in leafem::addHomeButton()
                         group = "Vue cantonale") |>
    # Couche des lacs
    leaflet::addPolygons(data = sf_lacs,
                         fillColor = "lightblue",
                         color = "grey",
                         weight = 1,
                         # Si clickable : l'app crash car lac =/= commune !
                         options = leaflet::pathOptions(clickable = FALSE)) |>
    # Première couche des communes (en blanc, état non-sélectionné)
    leaflet::addPolygons(data = sf_communes,
                         fillColor = "white",
                         color = "grey",
                         weight = 1,
                         # layerID is needed to throw back the right item when clicked
                         layerId = ~NOM_MIN,
                         label = ~NOM_MIN,
                         group = "regions",
                         highlightOptions = leaflet::highlightOptions(
                           weight = 5,
                           fillColor = NULL,
                           fillOpacity = NULL,
                           color = "#FFB90F",
                           bringToFront = FALSE)) |>
    # Seconde couche des communes (en rouge, état sélectionné)
    leaflet::addPolygons(data = sf_communes,
                         fillColor = "#FFB90F",
                         fillOpacity = 1,
                         weight = 1,
                         color = "black",
                         stroke = TRUE,
                         layerId = ~NO_COM_FED, # any random layerId other than NOM_MIN (required)
                         label = ~NOM_MIN,
                         group = ~NOM_MIN,
                         highlightOptions = leaflet::highlightOptions(
                           weight = 5,
                           fillColor = NULL,
                           fillOpacity = NULL,
                           color = "#CFCFCF",
                           bringToFront = FALSE)) %>%
    # This will be switched on/off through the code below using click events
    leaflet::hideGroup(group = sf_communes$NOM_MIN)   |>
    # Set the background to white
    leaflet.extras::setMapWidgetStyle(list(background= "white")) |>
    # fitbounds with coordinates. ! tweak along with zoomSnap/zoomDelta
    # leaflet::setView(lng = 6.617,
    #                  lat = 46.63,
    #                  zoom = 9.3) |> # or fitBounds(lng1 = 6.50, lat1 = 46.18, lng2 = 6.54, lat2 = 47.15
    # Set max limits to avoid panning away from the map
    leaflet::setMaxBounds(lng1 = 5.7,
                          lat1 = 45.9,
                          lng2 = 7.8,
                          lat2 = 47.4)

}


#' create_bar_plotly()
#'
#'@description Creates a plotly object from a facetted ggplot bar plot for use in renderPlotly
#'
#' @param data the data to provide
#'
#'
#' @import ggplot2
#' @importFrom plotly ggplotly layout config
#' @return an interactive plotly object
#' @export

create_bar_plotly <- function(data,
                              n_communes,
                              var_year,
                              var_commune,
                              unit, # input$selected_unit value retrieved in app_server
                              var_rank_2, # one of secteur, categorie...
                              var_values, # one of consommation, production_totale...
                              color_palette, # 'colors_categories',
                              dodge = FALSE, # stacked by default
                              free_y = FALSE,
                              legend_title = NULL,
                              web_width = 1500, # set default when shinybrowser not used
                              web_height = 800, # set default when shinybrowser not used
                              ... # free
                              ){

  # First create ggplot graph
  # We turn to MWh to save space, especially when free_y is activated...
  ggplot <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = as.factor(.data[[var_year]]),
                        y = .data[[var_values]],
                        fill = .data[[var_rank_2]],
                        # Text is reused in ggplotly(tooltip = 'text')
                        text = paste0(.data[[var_rank_2]], "\n",
                                      format(round(.data[[var_values]], digits = 0), big.mark = "'"),
                                      paste("", unit, "en "), .data[[var_year]])))+
    ggplot2::geom_col(position = dplyr::if_else(condition = dodge, # arg
                                         true = "dodge",
                                         false = "stack"))+
    ggplot2::scale_y_continuous(labels = ifelse(unit == "kWh",
                                                scales::label_number(big.mark = "'", suffix = "K", scale = 1e-3),
                                                scales::label_number(big.mark = "'", accuracy = 1))
                                )+
    ggplot2::scale_fill_manual(name = legend_title, # passed from arg
                               values = color_palette)+ # palette defined in utils_helpers.R
    ggplot2::labs(x = "", y = unit)+
    ggplot2::facet_wrap(facets = ggplot2::vars(.data[[var_commune]]),
                        ncol = 2,
                        # if the toggle linked to the free_y argument is TRUE, then free y axis
                        scales = ifelse(free_y, "free_y", "fixed"))+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = "top",
                   # change the labels of facet wrap. main_color defined in utils_helpers.R
                   strip.background = ggplot2::element_rect(
                     color="black", fill=main_color, linewidth = 1, linetype="solid"
                   ),
                   strip.text = ggplot2::element_text(
                     size =10, color = "white"),
                   legend.text = ggplot2::element_text(size = 12),
                   legend.title = ggplot2::element_text(size = 12),
                   legend.background = ggplot2::element_rect(fill = NA), # transparent
                   legend.key.size = ggplot2::unit(2, "cm"),
                   panel.spacing.x = ggplot2::unit(.05, "cm"),
                   panel.spacing.y = ggplot2::unit(0.5, "cm"),
                   axis.text.x = ggplot2::element_text(size = 10))

  # Access how many facets there are for height management
  n_facets <- n_communes

  # Turn to plotly object and deal with plotting sizes
  ggplot %>%
    plotly::ggplotly(tooltip = "text", # refers to aes(text) defined in ggplot2

                     height = return_dynamic_size(which = 'height',
                                                  web_size = web_height,
                                                  n_facets = n_facets),
                     width = return_dynamic_size(which = 'width',
                                                 web_size = web_width,
                                                 n_facets = n_facets)
  ) %>%
    plotly::layout(
      legend = list(
        # font = list(size = 15),
        traceorder = "reversed",
        orientation = "h", # puts the legend in the middle instead of default right
        y = 1.35 # elevates the legend so its above the plot, not below
      )) %>%
    plotly::config(modeBarButtons = list(list("toImage")),
                   locale = "fr")
}

#' create_sunburst_plotly()
#'
#' @description Creates a sunburst, inspired from https://stackoverflow.com/questions/57395424/how-to-format-data-for-plotly-sunburst-diagram
#'
#' @param data the data to provide
#'
#' @import dplyr
#' @importFrom plotly plot_ly config
#' @importFrom tidyr pivot_longer
#' @return an interactive plot

create_sunburst_plotly <- function(data_sunburst,
                                   unit, # input$selected_unit value retrieved in app_server
                                   var_year,
                                   var_values,
                                   var_commune,
                                   var_rank_2,
                                   third_rank,
                                   var_rank_3_1,
                                   var_rank_3_2){

  # store the year for the center of sunburst plot label.
  # For the mod_regener_needs_charts.R specificities, we directly pass the 2022 value
  label_year <- if(is.numeric(data_sunburst[[var_year]])){
    max(data_sunburst[[var_year]])
  }else{
    regener_current_year
  }

  # overall total (layer 0)
  total_row <- data_sunburst %>%
    summarise(values = sum(.data[[var_values]])) %>%
    dplyr::mutate(labels = as.character(label_year), # center of sunburst label
           parents = NA,
           ids = "Total")

  # total per rank_1 (commune expected)
  subtotal_row <- data_sunburst %>%
    dplyr::group_by(labels = .data[[var_commune]]) %>%
    dplyr::summarise(values = sum(.data[[var_values]], na.rm = T)) %>%
    dplyr::mutate(labels = labels,
           parents = "Total",
           ids = paste0("Total - ",labels), .keep = "unused")

  # total per rank_2 (either categorie or secteur)
  subsubtotal_row <- data_sunburst %>%
    dplyr::mutate(labels = .data[[var_rank_2]],
           values = .data[[var_values]],
           parents = paste0("Total - ", .data[[var_commune]]),
           ids = paste0(parents, " - ", .data[[var_rank_2]]),
           .keep = "unused")

  # specificity : We have two cols that we pivot_long, hence the rank_3_1 and rank_3-2
  # if these were originally one column instead of two, we could simplify the code with rank_3 and remove pivot_longer()
  # It is conditionnal for flexibility to have the same function with only 2 ranks instead of 3.

  if(third_rank == TRUE){

    lastsubtotal_row <- subsubtotal_row %>%
      dplyr::select(-values, -labels) %>%
      dplyr::mutate(Injection = .data[[var_rank_3_1]],
                    Autonconsommation = .data[[var_rank_3_2]],
                    .keep = "unused") %>%
      tidyr::pivot_longer(cols = c(Injection, Autonconsommation),
                   names_to = "labels",
                   values_to = "values") %>%
      dplyr::mutate(labels = labels,
             parents = ids,
             ids = paste0(parents, " - ", labels),
             values = values)
    # assemble everything
    sunburst_df <- dplyr::bind_rows(list(total_row, subtotal_row, subsubtotal_row, lastsubtotal_row))

  }else{
    # assemble everything
    sunburst_df <- dplyr::bind_rows(list(total_row, subtotal_row, subsubtotal_row))
  }

  # We hadd the hover_values for the tooltip in plotly, in MWh for readability
  sunburst_df <- sunburst_df %>%
    dplyr::mutate(values_hover = paste(format(values,
                                              big.mark = "'",
                                              digits = 3,
                                              drop0trailing = T,
                                              scientific = F),
                                        unit))

  # plot & enjoy
  plotly::plot_ly(data = sunburst_df,
                  ids = ~ids,
                  labels= ~labels,
                  parents = ~parents,
                  values= ~values,
                  hoverinfo = "text",
                  hovertext = sunburst_df$values_hover,
                  type='sunburst',
                  branchvalues = 'total',
                  height = 600, width = 600 # decent size
                  ) %>%
    # change to fr
    plotly::config(modeBarButtons = list(list("toImage")),
                   locale = "fr")


} # end function


#' create_alluvial_chart()
#' creates a ggplot2 alluvial plot using ggalluvial library and uses labels and variable
#' names as arguments for a flexible data input
#'
#' @param data the dataset used to create the ggalluvial plot
#' @param var_from the variable for the left stratum
#' @param var_flow the variable that quantifies the flows from `var_from` to `var_to`
#' @param var_to the variable for the right stratum
#' creates a ggplot2 alluvial chart using the ggalluvial library and heat building
#' consumption data from an aggregated RegEner dataset
#' @return a ggplot2 object for regener alluvial visualisations
#' @export

create_alluvial_chart <- function(data,
                                  var_commune,
                                  var_flow,
                                  var_from,
                                  label_from,
                                  var_to,
                                  label_to){

  # Following https://stackoverflow.com/questions/67142718/embracing-operator-inside-mutate-function
  # Very tough subject, no idea why this ' := ' or {{ }} are required


  # data plotting
  data %>%
    ggplot2::ggplot(ggplot2::aes(axis1 = .data[[var_from]],
                                 axis2 = .data[[var_to]],
                                 y = .data[[var_flow]],
                                 label = .data[[var_flow]]))+
    ggalluvial::geom_alluvium(ggplot2::aes(fill = .data[[var_from]]),
                              width = 3/8, reverse = FALSE) +
    ggalluvial::geom_stratum(alpha = .25, width = 3/8, reverse = FALSE) +
    ggplot2::geom_label(stat = ggalluvial::StatStratum,
                        alpha = .65, size = 4.5,
                        ggplot2::aes(label = paste0(after_stat(stratum),
                                                    " ",
                                                    scales::percent(after_stat(prop), accuracy = 0.1))),
                        reverse = FALSE) +
    ggplot2::scale_x_continuous(breaks = 1:2, labels = c(label_from, label_to)) +
    ggplot2::scale_fill_manual(values =  return_palette_regener())+ # fcts_helpers.R
    ggplot2::facet_wrap(facets = var_commune,
                        scales = "free",
                        ncol = 2)+
    ggplot2::theme_void()+
    ggplot2::theme(legend.position = "none",
                   strip.text = element_text(size =16),
                   axis.text.x = element_text(size = 14)) %>%
    suppressWarnings() # avoid annoying warning due to 'Autres' <fct> in both strata

}


#' lump_alluvial_factors()
#' takes a dataframe structured for ggalluvial and lumps the factor variables (var_from, var_to)
#' according to two {forcats} functions which arguments should be modified in the code
#'
#' @param data the dataset used to create the ggalluvial plot
#' @param var_from the variable for the left stratum
#' @param var_flow the variable that quantifies the flows from `var_from` to `var_to`
#' @param var_to the variable for the right stratum
#'
#' @return a ggplot object
#' @export

lump_alluvial_factors <- function(data,
                                  var_commune,
                                  var_from,
                                  var_flow,
                                  var_to){

  # lumping factors both left and right of alluvia to 4 max (for readability)
  # fct_lump_prop won't lump a factor ALONE, there needs to be 2 factors to meet the prop criteria

  data %>%
    dplyr::group_by(.data[[var_commune]]) %>% # !! must group_by commune
    dplyr::mutate({{var_from}} := forcats::fct_lump_n(f = .data[[var_from]],
                                                      n = 3,
                                                      w = .data[[var_flow]],
                                                      other_level = "Autres sources")) %>%
    dplyr::mutate({{var_to}} := forcats::fct_lump_prop(f = .data[[var_to]],
                                                       prop = 0.1, # 10% min to appear individually
                                                       w = .data[[var_flow]],
                                                       other_level = "Autres"))

}


#' return_dynamic_size()
#' Returns a px value used for dynamic facet plots based on web display size and number of facets.
#' A facet row is typically well displayed at around 1/6 of the screen's height
#' @param which either 'width' or 'height'
#' @param web_size px size of either width or height, typically obtained with {shinybrowser}
#'
#' @return a numeric value corresponding to px
#' @export

return_dynamic_size <- function(which,
                                web_size,
                                n_facets){

  if(which == "height"){

    # This returns the correct number of facetted rows
    # Note : valid only when there are 2 facets per row
    n_facet_rows <- (n_facets+ (n_facets %% 2))/2

    # Empirically found web-relative height per facetted row
    px_per_row <- web_size/6

    # Empirically found absolute legend height
    legend_height <- 250 # px

    # Plot height acc. to number of rows and legend height

    plot_height <- legend_height + n_facet_rows * px_per_row # px/row

    return(plot_height)


  }else if(which == "width"){
    # For width, we return 75% of useful space if one facet, 90% if more than 1

    plot_width <- ifelse(
      # test :
      test = n_facets == 1,
      # Yes (1 facet) : web width - sidebar (300) * 75%
      yes = (web_size-300)*0.75,
      # No (More than 1 facet) : web width - sidebar (300) * 95%
      no = (web_size-300)*0.95)

    # That's an empirical minimal value for 'watchable' display
    #  For some reason on mobile, updating the app messes up with the shinybrowser width record
    #  So this ensures that a minimum width is set
    plot_width_ceiling <- 800


    return(
      max(plot_width_ceiling, plot_width)
           )

  }else{
    stop("'which' arg can only be of type 'height' or 'width'.")
  }

}

# Table fns ----

#' create_table_dt()
#'
#' @param data Specific electricity production, DGE-DIREN data to transform to datatable
#' Must follow Pronovo's outputs and utils_helpers.R format
#' @param unit Unit currently selected inside the app
#' @param DT_dom datatable 'dom' Option, see datatable documentation. Likely Bfrtip or frtip
#'
#' @import dplyr
#' @importFrom stringr str_replace_all str_to_title
#' @return A DT table with export functionalities
#' @export

create_prod_table_dt <- function(data,
                                 unit,
                                 DT_dom = "Bfrtip" # we set default with Buttons
                                 ){

  data %>%
    # Basic clean up for table output
    dplyr::mutate(
      # change year to factor %>%
      annee = as.factor(annee),
      # format numeric cols
      #  because of the NA->"Confidentiel" JS code in DT options (see below) we need
      #  to keep NAs alive with an if_else statement (only needed for this fn)
      across(where(is.numeric), ~ if_else(condition = !is.na(.x),
                                          true = format(.x,
                                                        big.mark = "'",
                                                        digits = 3,
                                                        drop0trailing = TRUE,
                                                        scientific = FALSE),
                                          false = NA_character_ )
             )) %>%
    dplyr::select(-c(numero_de_la_commune)) %>%
    dplyr::relocate(commune, annee, categorie,
                    production, injection, autoconsommation,
                    puissance_electrique_installee) %>%
    # add icons HTML tags from utils_helpers.R
    dplyr::left_join(prod_icons, by = "categorie") %>%
    dplyr::relocate(icon, .before = categorie) %>%
    dplyr::rename(" " = "icon") %>% # empty colname for icons
    rename_fr_colnames() %>%  # fct_helpers.R
    add_colname_units(unit = unit) %>%  # fct_helpers.R
    #turn to DT
    DT::datatable(escape = F, # rendering the icons instead of text
                  extensions = 'Buttons',
                  options = list(paging = TRUE,    # paginate the output
                                 pageLength = 15,  # number of rows to output for each page
                                 scrollY = TRUE,   # enable scrolling on Y axis
                                 autoWidth = TRUE, # use smart column width handling
                                 server = FALSE,   # use server-side processing
                                 dom = DT_dom,
                                 buttons = list(
                                   list(extend = 'copy', text = "Copier"),
                                   list(extend = 'excel', filename = paste0("prod_elec_vd_", Sys.Date()))
                                 ),
                                 columnDefs = list(list(
                                   targets = "_all",
                                   className = 'dt-center',
                                   # NA to custom string : see https://github.com/rstudio/DT/issues/322
                                   render = DT::JS(
                                     "function(data, type, row, meta) {",
                                     "return data === null ? '(Confidentiel)' : data;",
                                     "}")
                                 )),


                                 # https://rstudio.github.io/DT/004-i18n.html   for languages
                                 language = DT_fr_language # from utils_helpers.R !
                  ),
                  selection = 'single', # enable selection of a single row
                  rownames = FALSE      # don't show row numbers/names
    ) # End DT
}


#' create_cons_table_dt()
#'
#' @param data Specific electricity consumption, DGE-DIREN data to transform to datatable
#' Must follow specific data format which can be found in /data
#' @param unit Unit currently selected inside the app
#' @param DT_dom datatable 'dom' Option, see datatable documentation. Likely Bfrtip or frtip
#'
#' @import dplyr
#' @return A DT table with export functionalities
#' @export

create_cons_table_dt <- function(data,
                                 unit,
                                 DT_dom = "Bfrtip" # we set default with Buttons
                                 ){

  data %>%
    # Basic clean up for table output
    dplyr::mutate(
      # change year to factor
      annee = as.factor(annee),
      # format numeric cols
      dplyr::across(where(is.numeric), ~format(.x,
                                               big.mark = "'",
                                               digits = 3,
                                               drop0trailing = TRUE,
                                               scientific = FALSE))) %>%
    # clear out useless vars
    select(-code_secteur) %>%
    # put installed power in the last position
    dplyr::relocate(commune, annee, secteur, consommation) %>%
    # add icons HTML tags from utils_helpers.R
    dplyr::left_join(cons_icons, by = "secteur") %>%
    dplyr::relocate(icon, .before = secteur) %>% #
    dplyr::rename(" " = "icon") %>% # empty colname for icons
    rename_fr_colnames() %>% # fct_helpers.R
    add_colname_units(unit = unit) %>%  # fct_helpers.R
    #turn to DT
    DT::datatable(escape = F, # rendering the icons instead of text
                  extensions = 'Buttons',
      options = list(paging = TRUE,    # paginate the output
                     pageLength = 15,  # number of rows to output for each page
                     scrollY = TRUE,   # enable scrolling on Y axis
                     autoWidth = TRUE, # use smart column width handling
                     server = FALSE,   # use server-side processing
                     dom = DT_dom, # dynamic according to needs
                     buttons = list(
                       list(extend = 'copy', text = "Copier"),
                       list(extend = 'excel', filename = paste0("prod_elec_vd_", Sys.Date()))
                     ),
                     columnDefs = list(list(targets = "_all", className = 'dt-center')),
                     # https://rstudio.github.io/DT/004-i18n.html   for languages
                     language = DT_fr_language # from utils_helpers.R !
      ),
    selection = 'single', ## enable selection of a single row
    rownames = FALSE               ## don't show row numbers/names
    ) # End DT
}

#' create_rg_needs_table_dt()
#' @param data Specific regener needs dataset to transform to datatable
#' Must follow specific data format which can be found in /data
#' @param unit Unit currently selected inside the app
#' @param DT_dom datatable 'dom' Option, see datatable documentation. Likely Bfrtip or frtip
#'
#' @return a DT table with export functionnalities
#' @export

create_rg_needs_table_dt <- function(data,
                                     unit,
                                     DT_dom = "Bfrtip" # we set default with Buttons
                                     ){

  data %>%
    # Basic clean up for table output
    dplyr::mutate(
      # change year (etat for rg dataset) to factor
      etat = as.factor(etat),
      # format numeric cols
      dplyr::across(where(is.numeric), ~format(.x,
                                               big.mark = "'",
                                               digits = 3,
                                               drop0trailing = TRUE,
                                               scientific = FALSE))) %>%
    # add icons HTML tags from utils_helpers.R
    dplyr::left_join(regener_icons_type, by = "type") %>%
    # relocate call
    dplyr::relocate(commune, etat, icon, type) %>%
    dplyr::rename(" " = "icon") %>% # empty colname for icons
    rename_fr_colnames() %>% # fct_helpers.R
    add_colname_units(unit = unit) %>%  # fct_helpers.R
    #turn to DT
    DT::datatable(escape = F, # rendering the icons instead of text
                  extensions = 'Buttons',
                  options = list(paging = TRUE,    # paginate the output
                                 pageLength = 15,  # number of rows to output for each page
                                 scrollY = TRUE,   # enable scrolling on Y axis
                                 autoWidth = TRUE, # use smart column width handling
                                 server = FALSE,   # use server-side processing
                                 dom = DT_dom, # dynamic according to needs
                                 buttons = list(
                                   list(extend = 'copy', text = "Copier"),
                                   list(extend = 'excel', filename = paste0("rgr_needs_vd_", Sys.Date()))
                                 ),
                                 columnDefs = list(list(targets = "_all", className = 'dt-center')),
                                 # https://rstudio.github.io/DT/004-i18n.html   for languages
                                 language = DT_fr_language # from utils_helpers.R !
                  ),
                  selection = 'single', ## enable selection of a single row
                  rownames = FALSE               ## don't show row numbers/names
    )# End DT

}


#' create_regener_table_dt()
#' creates a DT table with custom formatting and html icons
#' @param data Specific regener consumption table, DGE-DIREN data to transform to datatable
#' @param unit Unit currently selected inside the app
#' @param DT_dom datatable 'dom' Option, see datatable documentation. Likely Bfrtip or frtip
#'
#' @return a datatable object for regener datasets
#' @export

create_regener_table_dt <- function(data,
                                    unit,
                                    DT_dom = "Bfrtip" # we set default with Buttons
){

  data %>%
    # Set pct_commune to % display (output for dl is in numeric)
    dplyr::mutate(pct_commune = scales::percent(
      pct_commune, accuracy = 0.01)) %>%
    dplyr::mutate(
      # change year to factor
      etat = as.factor(etat), # if needed later
      # format numeric cols
      dplyr::across(where(is.numeric), ~format(.x,
                                          big.mark = "'",
                                          digits = 3,
                                          drop0trailing = TRUE,
                                          scientific = FALSE))) %>%
    # add icons HTML tags from utils_helpers.R
    dplyr::left_join(regener_icons, by = "ae") %>%
    # relocate call
    dplyr::relocate(commune, etat, icon, ae,
                    tidyselect::any_of(c("usage", "affectation")),
                    consommation, pct_commune) %>%
    dplyr::rename(" " = "icon") %>% # empty colname for icons
    #turn to DT
    rename_fr_colnames() %>% # fct_helpers.R
    add_colname_units(unit = unit) %>%  # fct_helpers.R
    DT::datatable(escape = F, # rendering the icons instead of text
                  extensions = 'Buttons',
                  options = list(paging = TRUE,    # paginate the output
                                 pageLength = 15,  # number of rows to output for each page
                                 scrollY = TRUE,   # enable scrolling on Y axis
                                 autoWidth = TRUE, # use smart column width handling
                                 server = FALSE,   # use server-side processing
                                 dom = DT_dom, # dynamic according to needs
                                 buttons = list(
                                   list(extend = 'copy', text = "Copier"),
                                   list(extend = 'excel', filename = paste0("rgr_table_vd_", Sys.Date()))
                                 ),
                                 columnDefs = list(list(targets = "_all", className = 'dt-center')),
                                 # https://rstudio.github.io/DT/004-i18n.html   for languages
                                 language = DT_fr_language # from utils_helpers.R !
                  ),
                  selection = 'single', ## enable selection of a single row
                  rownames = FALSE               ## don't show row numbers/names
    ) # End DT

}


#' create_rg_misc_table_dt()
#' Creates datatable for regener_misc dataset
#' @param data the dataset containing variables and descriptions
#' @param unit Unit currently selected inside the app
#' @param DT_dom datatable 'dom' Option, see datatable documentation. Likely Bfrtip or frtip
#'
#' @return a DT object
#' @export

create_rg_misc_table_dt <- function(data,
                                    # unit arg not needed for misc data
                                    DT_dom = "Bfrtip" # we set default with Buttons
){

  data %>%
    # Basic clean up for table output
    dplyr::mutate(
      # change year (etat for rg dataset) to factor
      etat = as.factor(etat),
      # format numeric cols
      dplyr::across(where(is.numeric), ~format(.x,
                                               big.mark = "'",
                                               digits = 3,
                                               drop0trailing = TRUE,
                                               scientific = FALSE))) %>%
    dplyr::relocate(commune, etat) %>%
    rename_misc_colnames() |>
    # add_colname_units(unit = unit) %>%  # fct_helpers.R
    #turn to DT
    DT::datatable(escape = F, # rendering the icons instead of text
                  extensions = 'Buttons',
                  options = list(paging = TRUE,    # paginate the output
                                 pageLength = 15,  # number of rows to output for each page
                                 scrollY = TRUE,   # enable scrolling on Y axis
                                 autoWidth = TRUE, # use smart column width handling
                                 server = FALSE,   # use server-side processing
                                 dom = DT_dom, # dynamic according to needs
                                 buttons = list(
                                   list(extend = 'copy', text = "Copier"),
                                   list(extend = 'excel', filename = paste0("rgr_misc_vd_", Sys.Date()))
                                 ),
                                 columnDefs = list(list(targets = "_all", className = 'dt-center')),
                                 # https://rstudio.github.io/DT/004-i18n.html   for languages
                                 language = DT_fr_language # from utils_helpers.R !
                  ),

                  selection = 'single', ## enable selection of a single row
                  rownames = FALSE               ## don't show row numbers/names
    )# End DT

}

#' create_doc_table_dt
#' Creates minimalistic documentation table with download feature
#' @param data the dataset containing variables and descriptions
#'
#' @return A DT object

create_doc_table_dt <- function(data,
                                doc_prefix){

  data %>%
    DT::datatable(rownames = FALSE, # no index col
                  extensions = "Buttons",
                  options = list(
      dom = "Bfti", # Button ; filter; table ; information summary
      buttons = list(
        list(extend = 'csv', filename = paste0(doc_prefix, Sys.Date())),
        list(extend = 'excel', filename = paste0(doc_prefix, Sys.Date()))),
      columnDefs = list(list(targets = 0, className = 'dt-center')), # or "_all"
      paging = TRUE,
      pageLength = 20,
      scrollY = FALSE,
      autoWidth = TRUE,
      language = DT_fr_language # utils_helpers.R
    ))

}





#' create_subsidies_table_dt()
#' Creates datatable for subsidies dataset. Flexible enough to handle both datasets
#' `subsidies_by_building` and `subsidies_by_measure` hence its arguments for variables.
#' @param data the subsidies dataset
#' @param var_year the time variable
#' @param DT_dom datatable 'dom' Option, see datatable documentation. Likely Bfrtip or frtip
#'
#' @return a DT object
#' @export

create_subsidies_table_dt <- function(data,
                                      var_year,
                                      var_rank_2,
                                      icon_list,
                                      DT_dom = "Bfrtip" # we set default with Buttons
){

  data |>
    # Clear undesired vars
    dplyr::select(-any_of("mesure_simplifiee")) |>
    # Basic clean up for table output
    dplyr::mutate({{var_year}} := as.factor(.data[[var_year]])) |>
    dplyr::mutate(
      # format numeric cols
      across(where(is.numeric), ~ format(.x,
                                         big.mark = "'",
                                         digits = 3,
                                         drop0trailing = TRUE,
                                         scientific = FALSE))
      )%>%
    # add icons HTML tags from utils_helpers.R
    dplyr::left_join(icon_list, by = var_rank_2) %>%
    dplyr::relocate(icon, .before = any_of(var_rank_2)) %>%
    dplyr::rename(" " = "icon") %>% # empty colname for icons
    rename_misc_colnames() |> # fct_helpers.R
    rename_fr_colnames() |>  # fct_helpers.R
    #turn to DT
    DT::datatable(escape = F, # rendering the icons instead of text
                  extensions = 'Buttons',
                  options = list(paging = TRUE,    # paginate the output
                                 pageLength = 15,  # number of rows to output for each page
                                 scrollY = TRUE,   # enable scrolling on Y axis
                                 autoWidth = TRUE, # use smart column width handling
                                 server = FALSE,   # use server-side processing
                                 dom = DT_dom,
                                 buttons = list(
                                   list(extend = 'copy', text = "Copier"),
                                   list(extend = 'excel', filename = paste0("prod_elec_vd_", Sys.Date()))
                                 ),
                                 columnDefs = list(list(
                                   targets = "_all",
                                   className = 'dt-center'
                                 )),


                                 # https://rstudio.github.io/DT/004-i18n.html   for languages
                                 language = DT_fr_language # from utils_helpers.R !
                  ),
                  selection = 'single', # enable selection of a single row
                  rownames = FALSE      # don't show row numbers/names
    ) # End DT
}



# Palette fns ----

#' return_palette_prod_elec
#' Returns the color palette for categories in the electricity production dataset
#' @return a vector with categorical data and hex color strings
#' @export

return_palette_prod_elec <- function(){

  return(colors_categories) # in utils_helpers.R

}

#' return_palette_cons_elec
#' Returns the color palette for sectors in the electricity consumption dataset
#' @return a vector with categorical data and hex color strings
#' @export

return_palette_cons_elec <- function(){

  return(colors_sectors) # in utils_helpers.R

}



#' return_palette_regener
#' returns the color palette for energy sources in the regener dataset
#' @return a vector with categorical data and hex color strings
#' @export

return_palette_regener <- function(){

  # We take the initial AE colors and we add a 'Autres sources' which is
  # the 'other_level' from fct_lump_n() used inside the 'create_alluvial_chart()' fun

  colors_ae_others <- c(colors_ae,
                        "Autres sources" = "#6CFF6C")

  return(colors_ae_others)

}

# Icons fns ----

# This one is required because create_subsidies_table_dt is flexible enough to host
# two palettes (building+measure) but we need a function to return the palette
# if we want to be using the rmarkdown feature ! Otherwise not needed.

#' return_icons_subsidies
#' returns one of two icons palette for subsidies data (either building or measure icons)
#' @param which either `building` to return the building palette,
#' or `measure` to return the measure palette
#' @return a tibble with icon as html and a categorical variable to match icons
#' @export

return_icons_subsidies <- function(which){

  # According to which palette is asked, we return one of the objects from utils_helpers.R
  icons_subsidies <- switch(which,
         "building" = subsidies_building_icons,
         "measure" = subsidies_measure_icons)

  return(icons_subsidies)

}


# Unit fns ----

#' convert_units()
#' Converts units either from dataframe (in target columns) or directly from a numeric value
#' according to which current unit is selected in the application
#' @param data the dataframe containing the columns where to convert units
#' @param colnames the colnames where to convert units
#' @param unit_from the unit to convert from
#' @param unit_to the unit to convert to. Choice between "kWh", "MWh", "GWh", "TJ"
#'
#' @return the same dataframe with updated units on target colnames
#' @export

convert_units <- function(data,
                          colnames, # to be defined how it should be passed
                          unit_from = "kWh",
                          unit_to){ # check if it's worth saving the widget selected unit in a specific variable before (in mod_inputs.R)

  # Retrieves the right conversion factor unit_table in utils_helpers.R according to unit_to
  conversion_factor <- units_table %>%
    dplyr::filter(unit == unit_to) %>%
    dplyr::pull(kWh)

  # If we have a dataframe we call mutate
  if(is.data.frame(data)){
  # Applies the conversion factor to the target colnames of the dataframe (division)
  data %>%
    dplyr::mutate(dplyr::across(colnames, ~.x / conversion_factor))
  }else{
    # if numeric value (not df) we directly apply the conversion factor
    data/conversion_factor

  }
}

#' add_colnames_units()
#' returns unit extension in target columns according to the currently selected unit
#' of the app for power and energy related colnames. Should be called before making
#' nicely formatted columns with rename_fr_colnames()
#'
#' @param data the dataframe
#' @param unit the unit selected in the app
#'
#' @return dataframe with renamed columns
#' @export

add_colname_units <- function(data, unit){

  # Important : the code is not elegant but using if(){data <- data |> (...)} is the only way
  #  I found to work. Using only rename_with(.cols = any_of(...)) doesnt work when no match inside any_of is found !

  # Step 1 : rename nrg vars if contains energy related keywords and add the power unit in brackets
  if(any(stringr::str_detect(string = colnames(data),
                             pattern = stringr::regex(paste0(energy_col_keywords, collapse = "|"), ignore_case = TRUE)))){

    data <- data |>
      # For all energy-related units
      dplyr::rename_with(.cols = contains(energy_col_keywords, # utils_helpers.R
                                          ignore.case = TRUE),
                         ~paste0(.x, " [", unit, "]"))

  }else data


  # Step 2 : rename power vars if contains power related keywords and add the power unit in brackets
  # This step works if related after Step 1
  if(any(stringr::str_detect(string = colnames(data),
                             pattern = stringr::regex(paste0(power_col_keywords,
                                                             collapse = "|"),
                                                      ignore_case = TRUE)))){

    data %>%
      # For all power-related units
      dplyr::rename_with(.cols = contains(power_col_keywords, # utils_helpers.R
                                          ignore.case = TRUE),
                         ~paste0(.x, " [", # The colnames + the [unit] extension according to ifelse() below
                                 ifelse(
                                   test = stringr::str_detect(string = unit, pattern = "Wh"), # search for *[Wh] in unit
                                   yes = stringr::str_remove(string = unit, pattern = "h"), # (k)Wh -> (k)W in cols
                                   no = paste0(unit, "/h") # TJ -> TJ/h, and all other non-Wh units
                                 ), "]") # closing bracket for unit
      )

  }else data

  # Step 3 : rename CO2 vars if contains co2 related keywords and add the unit in brackets
  if(any(stringr::str_detect(string = colnames(data),
                             pattern = stringr::regex(paste0(co2_keywords, collapse = "|"), ignore_case = TRUE)))){

    data <- data |>
      # For all energy-related units
      dplyr::rename_with(.cols = contains(co2_keywords, # utils_helpers.R
                                          ignore.case = TRUE),
                         ~paste0(.x, " [", "tCO2", "]"))

  }else data

}

# Colnames fns ----

#' rename_fr_colnames()
#' A generic function that aims at adding correct french accents inside the create_x_table_dt functions
#' @return a dataframe with modified colnames, title case, accents where needed and space instead of underscore.
#' @export

rename_fr_colnames <- function(data){

  data %>%
    # Standard renaming
    rename_with(.cols = dplyr::everything(), .fn = stringr::str_to_sentence) %>%
    rename_with(.cols = dplyr::everything(), .fn = stringr::str_replace_all,
                pattern = "_", replacement = " ") %>%
    rename_with(.cols = everything(),
                .fn = stringr::str_replace_all, replace_fr_accents) # utils_helpers.R


}


#' rename_misc_columns()
#' Specific function to rename specific columns that can't be fixed with `rename_fr_colnames()`
#' It relies on a named vector of specific columns to rename adequately.
#' @return a renamed dataframe
#' @export

rename_misc_colnames <- function(data){

  data |>
    dplyr::rename(dplyr::any_of(cols_renaming_vector)) # utils_helpers.R

}


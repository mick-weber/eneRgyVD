#' info_dev_message
#' shinyalert popup which informs that this is a development version. Called in app_server.R
#' @importFrom shinyalert shinyalert
#' @return A shinyalert object when opening the app

info_dev_message <- function(){

  shinyalert::shinyalert(title = "Bienvenue sur eneRgyVD !",
                         text = paste0("Cette application est en cours de développement.",
                                       tags$br(),
                                       "Pour des raisons juridiques, les données communales ont été temporairement
                                       remplacées par des valeurs aléatoires, celles-ci ne reflètent donc pas la réalité.
                                       Plus d'informations sur cette application en cliquant sur 'À propos`
                                       dans la barre latérale."),
                         html = TRUE,
                         size = "s",
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


dl_requires_data_message <-  function(){

  shinyalert::shinyalert(title = "Duh.",
                         text = "DUH DUH DUH",
                         html = TRUE,
                         size = "xs",
                         closeOnEsc = TRUE,
                         closeOnClickOutside = TRUE,
                         type = "warning",
                         showConfirmButton = TRUE,
                         showCancelButton = FALSE,
                         confirmButtonText = "OK",
                         timer = 0,
                         animation = "pop"
  )

}

#' create_select_leaflet
#'
#' @description Creates the non-reactive part of the home leaflet map to select municipalities and interact with selectInputs.
#' @import leaflet
#' @return A leaflet base map without reactivity.
#'
#' @noRd

create_select_leaflet <- function(sf_districts, sf_lacs, sf_communes){

  leaflet::leaflet(options = leafletOptions(
    zoomControl = TRUE,
    zoomSnap = .1, # improve zoom increments
    zoomDelta = 1,
    minZoom = 7.9, # lock the back zoom range
    attributionControl = F # remove leaflet url
    )) %>%
    # Couche de base des districts si un district est sélectionné
    leaflet::addPolygons(data = sf_districts,
                         fillColor = NULL,
                         fillOpacity = 0,
                         color = "black",
                         label = ~NOM_MAJ,
                         weight = 2,
                         # group is then used in leafem::addHomeButton()
                         group = "Vue cantonale") %>%
    # Ajout de labels pour les districts : résultat pas convainquant
    # addLabelOnlyMarkers(data = sf_districts_labels, ~lng, ~lat,  label =~NOM_MIN,
    #                     labelOptions = labelOptions(noHide = T, direction = 'top',
    #                                                 textOnly = T, textsize = "20px",
    #                                                 style = list("color" = "grey"))) %>%
    # Couche des lacs
    leaflet::addPolygons(data = sf_lacs,
                         fillColor = "lightblue",
                         color = "grey",
                         weight = 1,
                         # Si clickable : l'app crash car lac =/= commune !
                         options = pathOptions(clickable = FALSE)) %>%
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
                           bringToFront = FALSE)) %>%
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
    leaflet::hideGroup(group = sf_communes$NOM_MIN)   %>%
    #  !! DOESNT WORKAdapt original view by capping the max bounds
    # leaflet::setMaxBounds(lng1 = 6.047974, lat1 = 46.126556, lng2 = 7.135620, lat2 = 46.949325) %>%
    # Set the background to white
    leaflet.extras::setMapWidgetStyle(list(background= "white")) %>%
    # Add reset button to zoom back to original view
    #leaflet.extras::addResetMapButton() %>%
    # fitbounds with coordinates. ! tweak along with zoomSnap/zoomDelta
    leaflet::setView(lng = 6.617, lat = 46.63, zoom = 7.8) %>% # or fitBounds(lng1 = 6.50, lat1 = 46.18, lng2 = 6.54, lat2 = 47.15
    # Set max limits to avoid panning away from the map
    leaflet::setMaxBounds(lng1 = 5.4, lat1 = 45.98, lng2 = 7.7, lat2 = 47.4)

}

# create_select_leaflet(sf_districts, sf_lacs, sf_communes)

#' create_bar_plotly
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
                              inputVals,
                              var_year,
                              var_commune,
                              unit, # input$selected_unit value retrieved in app_server
                              var_rank_2, # one of secteur, categorie_diren...
                              var_values, # one of consommation, production_totale...
                              color_palette, # 'colors_categories',
                              dodge = FALSE, # stacked by default
                              free_y = FALSE,
                              legend_title){

  # First create ggplot graph
  # We turn to MWh to save space, especially when free_y is activated...
  ggplot <- data %>%
    ggplot2::ggplot(aes(x = as.factor(.data[[var_year]]),
                        y = .data[[var_values]],
                        fill = .data[[var_rank_2]],
                        # Text is reused in ggplotly(tooltip = 'text')
                        text = paste0(.data[[var_rank_2]], "\n",
                                      format(round(.data[[var_values]], digits = 0), big.mark = "'"),
                                      paste("", unit, "en "), .data[[var_year]])))+
    ggplot2::geom_col(position = if_else(condition = dodge, # arg
                                         true = "dodge",
                                         false = "stack"))+
    ggplot2::scale_y_continuous(labels = ifelse(unit == "kWh",
                                                scales::label_number(big.mark = "'", suffix = "K", scale = 1e-3),
                                                scales::label_number(big.mark = "'", accuracy = 1))
                                )+
    ggplot2::scale_fill_manual(name = legend_title, # passed from arg
                               values = color_palette)+ # palette defined in utils_helpers.R
    ggplot2::labs( x = "", y = unit)+
    ggplot2::facet_wrap(facets = vars(.data[[var_commune]]),
                        ncol = 2,
                        # if the toggle linked to the free_y argument is TRUE, then free y axis
                        scales = ifelse(free_y, "free_y", "fixed"))+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = "top",
                   # change the labels of facet wrap. main_color defined in utils_helpers.R
                   strip.background = element_rect(
                     color="black", fill=main_color, size=1, linetype="solid"
                   ),
                   strip.text = element_text(
                     size =10, color = "white"),
                   legend.text = element_text(size = 12),
                   legend.title = element_text(size = 12),
                   legend.key.size = unit(2, "cm"),
                   panel.spacing.x = unit(.05, "cm"),
                   panel.spacing.y = unit(0.5, "cm"),
                   axis.text.x = element_text(size = 12))

  # Access how many facets there are for height management
  n_facets <- length(inputVals$selectedCommunes)

  # Turn to plotly object
  ggplot %>% plotly::ggplotly(tooltip = "text", # refers to aes(text) defined in ggplot2
                              height = ifelse(n_facets>n_facets_limit, # utils_helpers.R
                                              height_facet_above_limit,
                                              height_facet_under_limit)) %>%
    plotly::layout(
      legend = list(
      orientation = "h", # puts the legend in the middle instead of default right
      y = 1.25 # elevates the legend so its above the plot, not below
    )) %>%
    config(modeBarButtons = list(list("toImage")),
           locale = "fr")
}

#' create_sunburst_plotly
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
  label_year <- max(data_sunburst[[var_year]])

  # overall total (layer 0)
  total_row <- data_sunburst %>% summarise(values = sum(.data[[var_values]])) %>%
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

  # total per rank_2 (either categorie_diren or secteur)
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


#' create_table_dt
#'
#' @param data Specific electricity production, DGE-DIREN data to transform to datatable.
#' Must follow Pronovo's outputs and utils_helpers.R format.
#'
#' @import dplyr
#' @importFrom stringr str_replace_all str_to_title
#' @return A DT table with export functionalities
#' @export

create_prod_table_dt <- function(data, unit){

  data %>%
    # Basic clean up for table output
    dplyr::mutate(
      # change year to factor %>%
      annee = as.factor(annee),
      # format numeric cols
      across(where(is.numeric), ~format(.x,
                                        big.mark = "'",
                                        digits = 3,
                                        drop0trailing = TRUE,
                                        scientific = FALSE))) %>%
    dplyr::select(-c(numero_de_la_commune,
                     nombre_installations_total,
                     district)) %>%
    # put installed power in the last position
    dplyr::relocate(puissance_electrique_installee, .after = dplyr::last_col()) %>%
    # add icons HTML tags from utils_helpers.R
    dplyr::left_join(prod_icons, by = "categorie_diren") %>%
    dplyr::relocate(icon, .before = categorie_diren) %>% #
    dplyr::rename(" " = "icon") %>% # empty colname for icons
    rename_fr_colnames() %>%  # fct_helpers.R
    add_colname_units(unit = unit) %>%  # fct_helpers.R
    #turn to DT
    DT::datatable(escape = F, # rendering the icons instead of text
                  options = list(paging = TRUE,    ## paginate the output
                                 pageLength = 15,  ## number of rows to output for each page
                                 scrollY = TRUE,   ## enable scrolling on Y axis
                                 autoWidth = TRUE, ## use smart column width handling
                                 server = FALSE,   ## use server-side processing
                                 dom = 'Bfrtip',
                            # Buttons not needed anymore since mod_download_data.R is spot on
                                 # buttons = list(
                                 #   list(extend = 'csv', filename = paste0("prod_elec_vd_", Sys.Date())),
                                 #   list(extend = 'excel', filename = paste0("prod_elec_vd_", Sys.Date()))),
                                 columnDefs = list(list(targets = "_all", className = 'dt-center')),
                                 # https://rstudio.github.io/DT/004-i18n.html   for languages
                                  language = DT_fr_language # from utils_helpers.R !
    ),
    #extensions = 'Buttons',
    selection = 'single', ## enable selection of a single row
    #filter = 'bottom',              ## include column filters at the bottom
    rownames = FALSE               ## don't show row numbers/names
    ) # End DT
}


#' create_cons_table_dt
#'
#' @param data Specific electricity consumption, DGE-DIREN data to transform to datatable.
#' Must follow specific data format which can be found in /data
#'
#' @import dplyr
#' @return A DT table with export functionalities
#' @export

create_cons_table_dt <- function(data, unit){

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
      options = list(paging = TRUE,    ## paginate the output
                                 pageLength = 15,  ## number of rows to output for each page
                                 scrollY = TRUE,   ## enable scrolling on Y axis
                                 autoWidth = TRUE, ## use smart column width handling
                                 server = FALSE,   ## use server-side processing
                                 dom = 'Bfrtip',
                                 # Buttons not needed anymore since mod_download_data.R is spot on
                                 # buttons = list(
                                 #   list(extend = 'csv', filename = paste0("prod_elec_vd_", Sys.Date())),
                                 #   list(extend = 'excel', filename = paste0("prod_elec_vd_", Sys.Date()))),
                                 columnDefs = list(list(targets = "_all", className = 'dt-center')),
                                 # https://rstudio.github.io/DT/004-i18n.html   for languages
                                 language = DT_fr_language # from utils_helpers.R
    ),
    #extensions = 'Buttons',
    selection = 'single', ## enable selection of a single row
    #filter = 'bottom',              ## include column filters at the bottom
    rownames = FALSE               ## don't show row numbers/names
    ) # End DT
}


#' create_doc_table_dt
#' Creates minimalistic documentation table with download feature
#' @param data the dataset containing variables and descriptions
#'
#' @return A DT object
#' @export

create_doc_table_dt <- function(data, doc_prefix){

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
      language = DT_fr_language # from utils_helpers.R
    ))

}


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



#' return_palette_regener()
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

  # Check if this is correct form or if I need to assign the new dataframe (and return() it)

}


#' add_colnames_units
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

  data %>%
    # For all energy-related units
    dplyr::rename_with(.cols = any_of(contains(energy_col_keywords)), # utils_helpers.R
                       ~paste0(.x, " [", unit, "]")) %>%
    # For all power-related units
    dplyr::rename_with(.cols = any_of(contains(power_col_keywords)), # utils_helpers.R
                       ~paste0(.x, " [", # The colnames + the [unit] extension according to ifelse() below
                               ifelse(
                                 test = stringr::str_detect(string = unit, pattern = "Wh"), # search for *[Wh] in unit
                                 yes = stringr::str_remove(string = unit, pattern = "h"), # (k)Wh -> (k)W in cols
                                 no = paste0(unit, "/h") # TJ -> TJ/h, and all other non-Wh units
                               ), "]") # closing bracket for unit
    )

}


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
              aes(label = paste0(after_stat(stratum),
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

lump_alluvial_factors <- function(data, var_commune, var_from, var_flow, var_to){

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

#' create_regener_table_dt()
#' creates a DT table with custom formatting and html icons
#'
#' @return a datatable object for regener datasets
#' @export

create_regener_table_dt <- function(data, unit){

  data %>%
    # Set pct_commune to % display (output for dl is in numeric)
    dplyr::mutate(pct_commune = scales::percent(
      pct_commune, accuracy = 0.01)) %>%
    dplyr::mutate(
      # change year to factor
      # Annee = as.factor(Annee), # if needed later
      # format numeric cols
      dplyr::across(consommation, ~format(.x,
                                               big.mark = "'",
                                               digits = 3,
                                               drop0trailing = TRUE,
                                               scientific = FALSE))) %>%

    # add icons HTML tags from utils_helpers.R
    dplyr::left_join(regener_icons, by = "ae") %>%
    dplyr::relocate(pct_commune, .after = "consommation") %>%
    dplyr::relocate(icon, .before = "ae") %>%
    dplyr::rename(" " = "icon") %>% # empty colname for icons
    #turn to DT
    rename_fr_colnames() %>% # fct_helpers.R
    add_colname_units(unit = unit) %>%  # fct_helpers.R
    DT::datatable(escape = F, # rendering the icons instead of text
                  options = list(paging = TRUE,    ## paginate the output
                                 pageLength = 15,  ## number of rows to output for each page
                                 scrollY = TRUE,   ## enable scrolling on Y axis
                                 autoWidth = TRUE, ## use smart column width handling
                                 server = FALSE,   ## use server-side processing
                                 dom = 'Bfrtip',
                                 # Buttons not needed anymore since mod_download_data.R is spot on
                                 # buttons = list(
                                 #   list(extend = 'csv', filename = paste0("prod_elec_vd_", Sys.Date())),
                                 #   list(extend = 'excel', filename = paste0("prod_elec_vd_", Sys.Date()))),
                                 columnDefs = list(list(targets = "_all", className = 'dt-center')),
                                 # https://rstudio.github.io/DT/004-i18n.html   for languages
                                 language = DT_fr_language # from utils_helpers.R
                  ),
                  #extensions = 'Buttons',
                  selection = 'single', ## enable selection of a single row
                  #filter = 'bottom',              ## include column filters at the bottom
                  rownames = FALSE               ## don't show row numbers/names
    ) # End DT

}

#' rename_fr_colnames
#' A generic function that aims at adding correct french accents inside the create_x_table_dt functions
#' @return a dataframe with modified colnames, title case, accents where needed and space instead of underscore.
#' @export

rename_fr_colnames <- function(data){

  data %>%
    rename_with(.cols = dplyr::everything(), .fn = stringr::str_to_sentence) %>%
    rename_with(.cols = dplyr::everything(), .fn = stringr::str_replace_all,
                pattern = "_", replacement = " ") %>%
    rename_with(.cols = everything(), .fn = stringr::str_replace_all, replace_fr_accents) # utils_helpers.R


}










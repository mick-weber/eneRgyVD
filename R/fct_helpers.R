
# Librairies ----
library(tidyverse)
library(leaflet)
library(plotly)

#' create_select_leaflet
#'
#' @description Creates the non-reactive part of the home leaflet map to select municipalities and interact with selectInputs.
#'
#' @return A leaflet base map without reactivity.
#'
#' @noRd

create_select_leaflet <- function(sf_districts, sf_lacs, sf_communes){

  leaflet::leaflet(options = leafletOptions(
    # set initial zoom (found empirically)
    minZoom = 9,
    # remove leaflet url bottom of the map
    attributionControl = F)) %>%
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
                         weight = 1) %>%
    # Première couche des communes (en blanc, état non-sélectionné)
    leaflet::addPolygons(data = sf_communes,
                         fillColor = "white",
                         color = "grey",
                         weight = 1,
                         # layerID is needed to throw back the right item when clicked
                         layerId = ~NOM_MIN,
                         label = ~NOM_MIN,
                         group = "regions",
                         highlightOptions = highlightOptions(
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
                         highlightOptions = highlightOptions(
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
    # Add home button to zoom back to original view
    leaflet.extras::addResetMapButton() %>%
    leaflet.extras::addFullscreenControl(position = "topleft",
                                         pseudoFullscreen = TRUE) %>%
    # Set max limits where the users cannot pan further (approximate VD coords with padding)
    leaflet::setMaxBounds(lng1 = 5.9, lat1 = 46.1, lng2 = 7.3, lat2 = 47.1)

}

# create_select_leaflet(sf_districts = sf_districts,
#                       sf_lacs = sf_lacs,
#                       sf_communes = sf_communes)

#' create_bar_plotly
#'
#'@description Creates a plotly object from a facetted ggplot bar plot for use in renderPlotly
#'
#' @param data the data to provide
#'
#' @return an interactive plotly object

create_bar_plotly <- function(data){

  # First create ggplot graph
  ggplot <- data %>%
    ggplot2::ggplot()+
    ggplot2::geom_col(aes(x = as.factor(annee),
                          y = production_totale,
                          fill = categorie_diren,
                          # Text is reused in ggplotly(tooltip = 'text')
                          text = paste0(categorie_diren, "\n",
                                        format(round(production_totale/1e3, digits = 0), big.mark = "'"),
                                        " MWh en ", annee)),
                      position = "dodge")+
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = "'", accuracy = 1))+
    ggplot2::scale_fill_manual(name = "Technologies",
                               values = colors_categories)+ # palette defined in utils_helpers.R
    ggplot2::labs( x = "", y = "kWh")+
    ggplot2::facet_wrap(facets = vars(commune), ncol = 3)+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = "top",
                   # change the labels of facet wrap. main_color defined in utils_helpers.R
                   strip.background = element_rect(
                     color="black", fill=main_color, size=1.5, linetype="solid"
                   ),
                   strip.text = element_text(
                     size = 14, color = "white"
                   ),
                   legend.text = element_text(size = 14),
                   legend.title = element_text(size = 14),
                   legend.key.size = unit(2, "cm")
    )


  # Turn to plotly object
  ggplot %>% plotly::ggplotly(tooltip = "text") %>% # refers to aes(text) defined in ggplot2
    plotly::layout(legend = list(
      orientation = "h", # puts the legend in the middle instead of default right
      y = 1.15 # elevates the legend so its above the plot, not below
    )) %>%
    config(locale = "fr")

  # code for testing quickly. remove in prod.
# elec_prod_communes %>%
#   filter(commune %in% c("Morges", "Lausanne")) %>%
#   create_bar_plotly()

}


#' create_sunburst_plotly
#'
#' @description Creates a sunburst, inspired from https://stackoverflow.com/questions/57395424/how-to-format-data-for-plotly-sunburst-diagram
#'
#' @param data the data to provide
#'
#'
#' @return an interactive plot



subset <- elec_prod_communes %>% filter(commune %in% c("Lausanne", "Aigle"))

create_sunburst_plotly <- function(data, year_var, year,
                                    values_tot, rank_1, rank_2, rank_3_1, rank_3_2){

  # filter data so make sure there's only one year, otherwise the plot won't work (3 levels of labels allowed here)
  data <- data %>% filter(.data[[year_var]] == year)

  # total overall
  total_row <- data %>% summarise(values = sum(.data[[values_tot]])) %>%
    mutate(labels = as.character(year),
           parents = NA,
           ids = "Total")

  # total per commune
  subtotal_row <- data %>%
    group_by(labels = .data[[rank_1]]) %>%
    summarise(values = sum(.data[[values_tot]])) %>%
    mutate(labels = labels,
      parents = "Total",
      ids = paste0("Total - ",labels), .keep = "unused")

  # total per tech
  subsubtotal_row <- data %>%
    mutate(labels = .data[[rank_2]],
           values = .data[[values_tot]],
           parents = paste0("Total - ", .data[[rank_1]]),
           ids = paste0(parents, " - ", .data[[rank_2]]),
           .keep = "unused")

  # total per usage
  # specificity : we have two cols that we pivot_long, hence the rank_3_1 and rank_3-2
  # if these were originally one column instead of two, we could simplify the code with rank_3 and remove pivot_longer()
  lastsubtotal_row <- subsubtotal_row %>%
    select(-values, -labels) %>%
    mutate(Injection = .data[[rank_3_1]], Autonconsommation = .data[[rank_3_2]], .keep = "unused") %>%
    pivot_longer(cols = c(Injection, Autonconsommation),
                 names_to = "labels",
                 values_to = "values") %>%
    mutate(labels = labels,
           parents = ids,
           ids = paste0(parents, " - ", labels),
           values = values,
           values_hover = paste0(format(values/1e3, big.mark = "'", digits = 0, scientific = F), " MWh"),
           .keep = "unused")

  # assemble everything
  sunburst_df <- bind_rows(list(total_row, subtotal_row, subsubtotal_row, lastsubtotal_row))

  # plot & enjoy
  plotly::plot_ly(data = sunburst_df,
                  ids = ~ids,
                  labels= ~labels,
                  parents = ~parents,
                  values= ~values,
                  hoverinfo = "text", hovertext = sunburst_df$values_hover,
          type='sunburst', branchvalues = 'total') %>%
    # change to fr
    config(locale = "fr")
}


#  COMMENTED CODE BELOW IS FOR TESTING PURPOSES, SHOULD BE REMOVED LATER IN PROD
create_sunburst_plotly(data = subset, year_var = "annee", year = 2020,
                       values_tot = "production_totale",
                       rank_1 = "commune",
                       rank_2 = "categorie_diren",
                       rank_3_1 = "injection_totale", rank_3_2 = "autoconso_totale")


#' create_table_dt
#'
#' @param data Specific DGE-DIREN data to transform to datatable. Must follow Pronovo's outputs and utils_helpers.R format.
#'
#' @return A DT table with export functionalities
#'

create_table_dt <- function(data){

  data %>%
    dplyr::select(-numero_de_la_commune) %>%
    # put installed power in the last position
    dplyr::relocate(puissance_electrique_installee, .after = dplyr::last_col()) %>%
    # rename columns to title case, replace "_" and trim blank spaces
    dplyr::rename_with(.cols = dplyr::everything(), ~stringr::str_trim(
      stringr::str_replace_all(string = str_to_title(.x),
                               # replacements pairs below
                               c("_" = " ",
                                 "totale" = "")))) %>%
    # add units
    dplyr::rename_with(.cols = 4:6, ~paste0(.x, " [kWh]")) %>%
    dplyr::rename_with(.cols = 7, ~paste0(.x, " [kW]")) %>%
    dplyr::mutate(
      # change year to factor %>%
      Annee = as.factor(Annee),
      # format numeric cols
      across(where(is.numeric), ~format(.x, big.mark = "'", digits = 0, scientific = FALSE))) %>%
    # turn to DT
    DT::datatable(options = list(paging = TRUE,    ## paginate the output
                                 pageLength = 15,  ## number of rows to output for each page
                                 scrollX = TRUE,   ## enable scrolling on X axis
                                 scrollY = TRUE,   ## enable scrolling on Y axis
                                 autoWidth = TRUE, ## use smart column width handling
                                 server = FALSE,   ## use client-side processing
                                 dom = 'Bfrtip',
                                 buttons = list(
                                   list(extend = 'csv', filename = paste0("prod_elec_vd_", Sys.Date())),
                                   list(extend = 'excel', filename = paste0("prod_elec_vd_", Sys.Date()))),
                                 columnDefs = list(list(targets = c(0,1), className = 'dt-center')),
                                 # https://rstudio.github.io/DT/004-i18n.html   for languages
                                 language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
    ),
    extensions = 'Buttons',
    selection = 'single', ## enable selection of a single row
    #filter = 'bottom',              ## include column filters at the bottom
    rownames = FALSE               ## don't show row numbers/names
    ) # End DT
}



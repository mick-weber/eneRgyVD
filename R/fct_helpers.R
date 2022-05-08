
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

  ggplot <- data %>%
    ggplot2::ggplot()+
    ggplot2::geom_col(aes(x = as.factor(annee), y = production_totale, fill = categorie_diren),
                      position = "dodge")+
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = "'", accuracy = 1))+
    ggplot2::scale_fill_manual(name = "Technologies",
                               values = colors_categories)+ # palette defined in utils_helpers.R
    ggplot2::labs( x = "", y = "kWh")+
    ggplot2::facet_wrap(facets = vars(commune), ncol = 2)+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = "top")


  # turn to plotly object
  ggplot %>% plotly::ggplotly() %>%
    plotly::layout(legend = list(
      orientation = "h", # puts the legend in the middle instead of default right
      y = 1.25 # elevates the legend so its above the plot, not below
    ))

  # test :  elec_prod_communes %>% filter(commune %in% c("Morges", "Lausanne")) %>% create_bar_plotly()
}


#' create_treemap_plotly
#'
#' @description Creates a treemap
#'
#' @param data the data to provide
#'
#' @return an interactive plot
#'

create_treemap_plotly <- function(data){

# library(treemapify)
#
# df <- tribble(~categorie_diren, ~parents, ~n,
#                   "Hydro", "Renouvelable", 120,
#                   "Solaire", "Renouvelable", 90,
#                   "STEP", "Non-Renouvelable", 80,
#                   "Thermique fossile", "Non-Renouvelable", 100
#                   )
# ggplot <- df %>%
#   ggplot(aes(area = n, subgroup = parents, fill = categorie_diren, label = categorie_diren))+
#   geom_treemap()+
#   geom_treemap_subgroup_border()+
#   geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
#                                "black", fontface = "italic", min.size = 0)+
#   geom_treemap_text(colour = "white", place = "topleft", reflow = T)

}



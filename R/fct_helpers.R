#' create_select_leaflet
#'
#' @description Creates the non-reactive part of the home leaflet map to select municipalities and interact with selectInputs.
#'
#' @return A leaflet map without reactivity.
#'
#' @noRd

library(tidyverse)
library(leaflet)

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



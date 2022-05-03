
# Loading libraries ----

library(sf)
library(leaflet)
library(tidyverse)
library(shiny)
library(leaflet.extras)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(shinycssloaders)
library(shinyWidgets)

# Loading .rda objects ----

## sf data ----

load("./data/sf_communes.rda")
load("./data/sf_districts.rda")
load("./data/sf_lacs.rda")

## electricity_production data ----

load("./data/elec_prod.rda")
load("./data/elec_prod_doc.rda")

## electricity_consumption data ----

# TO BE ADDED

# Modified datasets ----

## elec_prod_communes ----


# Generic utils ----

## Color used for multiple esthetic elements, for instance shinycssloaders()

main_color <- "#3A862D"

## Colors for categorie_diren

categories_diren <- elec_prod %>%
  distinct(categorie_diren) %>%
  arrange(categorie_diren) %>%
  pull()

# We directly make a named vector since it's easier to spot what is what and we don't screw the order

colors_categories <- c("Biomasse agricole" = "#48A649", # biomasse agricole
                       "Bois-énergie" = "#CC9E62", # bois-énergie
                       "Déchets incinérables" = "#BFDB86", # déchets méthanisables
                       "Déchets méthanisables" = "#747D82", # déchets incinérables
                       "Eolien" = "#6DD5E3", # éolien
                       "Hydroélectricité" = "#6495ED", # hydroélectricité
                       "Solaire" = "#FFB90F", # solaire
                       "STEP" = "#A58DE6", # step
                       "Thermique fossile" = "#E67A78") # thermique fossile


# création du dataset par communes pour éviter de le calculer à chaque fois.
# le dataset non agrégé servira à quelques graphiques (p.ex. courbes des puissances installées)

elec_prod_communes <- elec_prod %>%
  dplyr::group_by(commune, annee, categorie_diren) %>%
  dplyr::summarise(dplyr::across(
    .cols = c(puissance_electrique_installee,
              production_totale,
              injection_totale,
              autoconso_totale), ~sum(.x, na.rm = T)),
    numero_de_la_commune = dplyr::first(numero_de_la_commune)) %>%
  ungroup()


# Non-reactive objects for input widgets ----

## Generic objects (across all tabs) ----

# list of communes names for select inputs

communes_names <- sf_communes %>%
  dplyr::distinct(NOM_MIN) %>%
  dplyr::arrange(NOM_MIN) %>%
  dplyr::rename(Commune = NOM_MIN)

# list of districts ids ; we add one row for the cantonal view

districts_names <- sf_districts %>%
  dplyr::distinct(NOM_MIN) %>%
  dplyr::arrange(NOM_MIN) %>%
  # add one row manually for the Canton, it should be placed first
  dplyr::add_row(NOM_MIN = "Canton", .before = 1) %>%
  dplyr::pull(NOM_MIN)

# list of district centroids for labels
# for use in leaflet::addLabelOnlyMarkers() of the create_select_leaflet() fct in fct_helpers.R

# Not used atm because not so convincing
# sf_districts_labels <- sf_districts %>%
#   dplyr::distinct(NOM_MIN, .keep_all = T) %>%
#   mutate(centroid = sf::st_centroid(geometry)) %>%
#   mutate(lng = sf::st_coordinates(centroid)[,1],
#          lat = sf::st_coordinates(centroid)[,2])

## Objects specific to the tabMap  ----

# We retrieve the coordinates
# With a nested list ; each district name has 4 coordinates (xmin,ymin,xmax,ymax)
# These coordinates represent the boundaries for the leaflet map zoom adjustments (through widget)

bboxes <- districts_names %>%
  purrr::map(.f = ~ sf::st_bbox(sf_districts %>% dplyr::filter(NOM_MIN == .x))) %>%
  purrr::set_names(districts_names)

## Add & fill the Canton bbox in our bboxes using the whole districts borders
bboxes$Canton <- sf_districts %>% sf::st_bbox()

## Objects specific to the tabProd  ----

# I think these should be reactive to the filtered dataset (by municipalities)

## Objects specific to the tabConso  ----

# I think these should be reactive to the filtered dataset (by municipalities)

































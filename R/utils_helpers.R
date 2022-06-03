
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
library(DT)
library(rjson)

# Loading .rda objects ----

## sf data ----

load("./data/sf_communes.rda")
load("./data/sf_districts.rda")
load("./data/sf_lacs.rda")

## electricity_production data ----

load("./data/elec_prod.rda")
load("./data/elec_prod_doc.rda")

## electricity_consumption data ----

load("./data/elec_cons.rda")

# Generic utils ----

## E-mail address ----
# Used for contact notificationMenu in ui.R & mod_about_the_app.R

mail_address <- "stat.energie@vd.ch"

## DT language file ----
### Run ONCE : Store JSON french language items file for DT library

# rjson::fromJSON(
#   file = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/French.json') %>%
#   toJSON() %>%
# write(file = "./data/DT_fr_language.json")

### Load json french language file for DT library

DT_fr_language <- fromJSON(file = "./data/DT_fr_language.json")


## Color used for multiple ui items ----

main_color <- "#3A862D"


# Non-reactive objects for input widgets ----

## Generic objects (across all tabs) ----

### Conversion units table ----

# Directly linked to mod_unit_converter.R module

# From... vector. Supplies
first_units <- c("kWh", "MWh", "GWh", "TJ")
# To... vector.
second_units <- c("kWh", "MWh", "GWh", "TJ")
# Create table of conversion with all possible combinaisons
conv_table <- expand.grid(from = first_units, to = second_units) %>%
  arrange(from) %>%
  mutate(factor = c(1, 1/1e3, 1/1e6, 3.6/1e6, # kWh to...
                    1e3, 1, 1/1e3, 1/3.6/1e3, # MWh to...
                    1e6, 1e3, 1, 1/3.6, # GWh to...
                    1/3.6*1e6, 1/3.6*1e3, 1/3.6*1, 1)) # TJ to...


### Available communes ----
# With more datasets (i.e. elec consumption), checking that the names are OK
# throughout all datasets will be a crucial issue.

communes_names <- sf_communes %>%
  dplyr::distinct(NOM_MIN) %>%
  dplyr::arrange(NOM_MIN) %>%
  dplyr::rename(Commune = NOM_MIN)

### Available districts ----
# For the zooming feature. We add one row for the cantonal view

districts_names <- sf_districts %>%
  dplyr::distinct(NOM_MIN) %>%
  dplyr::arrange(NOM_MIN) %>%
  # add one row manually for the Canton, it should be placed first
  dplyr::add_row(NOM_MIN = "Canton", .before = 1) %>%
  dplyr::pull(NOM_MIN)

## Objects specific to the tabProd  ----

### Colors for categorie_diren ----

categories_diren <- elec_prod %>%
  distinct(categorie_diren) %>%
  arrange(categorie_diren) %>%
  pull()

# We directly make a named vector since it's easier to spot what is what and we don't screw the order
# Eolien was added as anticipated

colors_categories <- c("Biomasse agricole" = "#48A649", # biomasse agricole
                       "Bois-énergie" = "#CC9E62", # bois-énergie
                       "Déchets incinérables" = "#BFDB86", # déchets méthanisables
                       "Déchets méthanisables" = "#747D82", # déchets incinérables
                       "Eolien" = "#6DD5E3", # éolien
                       "Hydroélectricité" = "#6495ED", # hydroélectricité
                       "Solaire" = "#FFB90F", # solaire
                       "STEP" = "#A58DE6", # step
                       "Thermique fossile" = "#E67A78") # thermique fossile


### elec_prod_communes ----
# From installation-specific to communes-specific (faster calculation)

elec_prod_communes <- elec_prod %>%
  dplyr::group_by(commune, annee, categorie_diren) %>%
  dplyr::summarise(dplyr::across(
    .cols = c(puissance_electrique_installee,
              production_totale,
              injection_totale,
              autoconso_totale), ~sum(.x, na.rm = T)),
    numero_de_la_commune = dplyr::first(numero_de_la_commune)) %>%
  dplyr::ungroup()

## Objects specific to the tabConso  ----
# to be populated

## Objects specific to the tabMap  ----

### Fixed statistics for boxes ----

### Retrieve last available year
# WHEN CONSO AVAILABLE: UPDATE programatically
# and find the year common to the TWO datasets (!)

vd_last_year <- 2020

#### VD electricity production

prod_elec_vd_last_year <- elec_prod_communes %>%
  filter(annee == vd_last_year) %>%
  summarise(production_totale = sum(production_totale, na.rm = TRUE)) %>%
  pull()

#### VD electricity consumption

cons_elec_vd_last_year <- 4051844000 # from statvd; update dynamically when dataset is here

#### VD electricity coverage

coverage_elec_vd_last_year <- prod_elec_vd_last_year/cons_elec_vd_last_year

### Map-specific ----
# We retrieve the coordinates
# With a nested list ; each district name has 4 coordinates (xmin,ymin,xmax,ymax)
# These coordinates represent the boundaries for the leaflet map zoom adjustments (through widget)

bboxes <- districts_names %>%
  purrr::map(.f = ~ sf::st_bbox(sf_districts %>% dplyr::filter(NOM_MIN == .x))) %>%
  purrr::set_names(districts_names)

## Add & fill the Canton bbox in our bboxes using the whole districts borders
bboxes$Canton <- sf_districts %>% sf::st_bbox()

































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

# Loading .rda objects ----

## sf data

load("./data/sf_communes.rda")
load("./data/sf_districts.rda")
load("./data/sf_lacs.rda")

## electricity_production data

load("./data/elec_prod.rda")
load("./data/elec_prod_doc.rda")

## electricity_consumption data

# to be added

# Non-reactive objects for input widgets ----

## list of communes names for select inputs ----

communes_names <- sf_communes %>%
  dplyr::distinct(NOM_MIN) %>%
  dplyr::arrange(NOM_MIN) %>%
  dplyr::rename(Commune = NOM_MIN)

## list of districts ids ----
## we add one row for the cantonal view

districts_names <- sf_districts %>%
  dplyr::distinct(NOM_MIN) %>%
  dplyr::arrange(NOM_MIN) %>%
  # add one row manually for the Canton, it should be placed first
  dplyr::add_row(NOM_MIN = "Canton", .before = 1) %>%
  dplyr::pull(NOM_MIN)

## Bboxes for districts ----
## With a nested list ; each district name has 4 coordinates (xmin,ymin,xmax,ymax)
## These coordinates represent the boundaries for the leaflet map zoom adjustments (through widget)

bboxes <- districts_names %>%
  purrr::map(.f = ~ sf::st_bbox(sf_districts %>% dplyr::filter(NOM_MIN == .x))) %>%
  purrr::set_names(districts_names)

## Add & fill the Canton bbox in our bboxes using the whole districts borders
bboxes$Canton <- sf_districts %>% sf::st_bbox()




































# This file contains most non-shiny elements
# Loading .rda objects ----
## sf data ----

load("./data/sf_communes.rda")
load("./data/sf_districts.rda")
load("./data/sf_lacs.rda")

## electricity_production data ----

load("./data/elec_prod.rda")
load("./data/elec_prod_doc.rda")

## electricity_consumption data ----

load("./data/elec_cons_communes.rda")
load("./data/elec_cons_doc.rda")

# Store .Rmd in temp dir ----
# https://mastering-shiny.org/action-transfer.html#downloading-reports
# Files others than .Rd are in ./inst/extdata/


# ETAIT JUSTE A LA BASE ??

# report_path <- tempfile(fileext = ".Rmd")
# file.copy("./inst/extdata/downloadable_report.Rmd", report_path, overwrite = TRUE)


# Generic utils ----

## E-mail address and links ----
# Used for mod_about_the_app.R and/or contact notificationMenu in ui.R

mail_address <- "stat.energie@vd.ch"
link_diren <- "https://www.vd.ch/toutes-les-autorites/departements/departement-de-lenvironnement-et-de-la-securite-des/direction-generale-de-lenvironnement-dge/diren-energie/"

## DT language file ----
### Run ONCE : Store JSON french language items file for DT library

# rjson::fromJSON(
#   file = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/French.json') %>%
#   toJSON() %>%
# write(file = "./data/DT_fr_language.json")

### Load json french language file for DT library
# Files others than .Rd are in ./inst/extdata/

DT_fr_language <- rjson::fromJSON(file = "./inst/extdata/DT_fr_language.json")


# Theme ----

## Color used for multiple ui items ----

main_color <- "#3A862D"

## {fresh} theme for shinydashboard ----
# Actuellement en test

energyvd_theme <- fresh::create_theme(theme = "default",
  output_file = "./inst/app/www/custom.css", # location of css (optionnal)
  include_assets = TRUE, # copy fonts file used in BS themes ?
  fresh::adminlte_color(
    green = "#3A862D",
     blue = "#3A862D",
     red = "#3A862D",
     yellow = "#3A862D",
     fuchsia = "#3A862D",
     navy = "#3A862D",
     purple = "#3A862D",
     maroon = "#3A862D",
     light_blue = "#3A862D"
  ),
  fresh::adminlte_sidebar(
    dark_bg = "#3A862D",
    dark_hover_bg = "#3A862D",
    dark_color = "#3A862D"
  ),
  fresh::adminlte_global(
    content_bg = "#3A862D"
  ))



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
  dplyr::arrange(from) %>%
  dplyr::mutate(factor = c(1, 1/1e3, 1/1e6, 3.6/1e6, # kWh to...
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
  dplyr::distinct(categorie_diren) %>%
  dplyr::arrange(categorie_diren) %>%
  dplyr::pull()

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

# We also make a named vector for sectors of elec consumption data
# Be careful if sectors change name ! (SDN wrote them inconsistently here...)
colors_sectors <- c("Industrie/Services" = "#00CED1", # blue shade
                    "Ménage" = "#FF6A6A", # red shade
                    "Transports" = "#EEB422", # orange shade
                    "Inconnu" = "#CCCCCC") # grey shade

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

## Objects specific to the tabCons  ----
# to be populated

## Objects specific to the tabMap  ----

### Common electricity year ----
# We seek for the year that is common to both electricity and production datasets
# This is to create the statistic boxes (tabMap) and compare similar years.

last_common_elec_year <- dplyr::intersect(
  elec_cons_communes %>% dplyr::distinct(annee),
  elec_prod_communes %>% dplyr::distinct(annee)) %>%
  dplyr::slice_max(annee) %>%
  dplyr::pull(annee)


### Fixed statistics for boxes ----

#### VD electricity production for last common year

prod_elec_vd_last_year <- elec_prod_communes %>%
  dplyr::filter(annee == last_common_elec_year) %>%
  dplyr::summarise(production_totale = sum(production_totale, na.rm = TRUE)) %>%
  dplyr::pull()

#### VD electricity consumption for last common year

cons_elec_vd_last_year <- elec_cons_communes %>%
  dplyr::filter(annee == last_common_elec_year) %>%
  dplyr::summarise(consommation_kwh = sum(consommation_kwh, na.rm = TRUE)) %>%
  dplyr::pull()


### Map-specific ----
# We retrieve the coordinates
# With a nested list ; each district name has 4 coordinates (xmin,ymin,xmax,ymax)
# These coordinates represent the boundaries for the leaflet map zoom adjustments (through widget)

bboxes <- districts_names %>%
  purrr::map(.f = ~ sf::st_bbox(sf_districts %>% dplyr::filter(NOM_MIN == .x))) %>%
  purrr::set_names(districts_names)

## Add & fill the Canton bbox in our bboxes using the whole districts borders

bboxes$Canton <- sf_districts %>% sf::st_bbox()
































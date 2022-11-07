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

## regener_communes data ----

load("./data/regener_communes.rda")

# Store .Rmd in temp dir ----
# https://mastering-shiny.org/action-transfer.html#downloading-reports
# Files others than .Rd are in ./inst/extdata/

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

## Units conversion table ----
# Must match the choices in header's widget
# Linked to fct_helpers.R convert_units()'s function

units_table <- dplyr::tribble(
  ~unit, ~kWh,
  "kWh", 1,
  "MWh", 1e3,
  "GWh", 1e6,
  "TJ", 1/3.6*1e6
)

## Column keywords ----
## These are used to dynamically target columns renaming in fct_helpers.R and mod_elec_charts.R
energy_col_keywords <- c("Consommation", "Production", "Injection", "Autoconsommation")
power_col_keywords <- c("Puissance")

## Prod colors and icons (prod) ----
# Base tribble with categorie, icon and color
prod_colors <- dplyr::tribble(~icon, ~`Catégorie DIREN`, ~color,
                             as.character(icon("droplet")),  "Hydroélectricité","#6495ED",
                             as.character(icon("sun")),  "Solaire", "#FFB90F",
                             as.character(icon("apple")),  "Déchets méthanisables","#BFDB86",
                             as.character(icon("leaf")),  "Biomasse agricole", "#48A649",
                             as.character(icon("tree")),  "Bois-énergie", "#CC9E62",
                             as.character(icon("industry")), "STEP", "#A58DE6",
                             as.character(icon("fire")), "Déchets incinérables","#E67A78",
                             as.character(icon("oil-can")), "Thermique fossile", "#747D82",
                             as.character(icon("wind")), "Eolien", "#2596BE")


# Used for table icons
# Adding the color style in the html tag for the icon
prod_icons <- prod_colors %>%
  dplyr::rowwise() %>%
  dplyr::mutate(icon = stringr::str_replace(string = icon, pattern = "></i>",
                                     replacement = paste0(" style=\"color:", color, '\"></i>'))) %>%
  dplyr::select(-color)

# Used for plots
colors_categories <- prod_colors$color %>% setNames(nm = prod_colors$`Catégorie DIREN`)

## Cons colors and icons (cons) ----
# Base tribble with sector, icon and color
cons_colors <- dplyr::tribble(~icon, ~`Secteur`, ~color,
                             as.character(icon("industry")), "Industrie/Services","#6495ED",
                             as.character(icon("house")), "Ménages", "#FFB90F",
                             as.character(icon("car")),  "Transports", "#BFDB86",
                             as.character(icon("question")),  "Inconnu", "#BFDB86")
# Used for table icons
# Adding the color style in the html tag for the icon
cons_icons <- cons_colors %>%
  dplyr::rowwise() %>%
  dplyr::mutate(icon = stringr::str_replace(string = icon, pattern = "></i>",
                                            replacement = paste0(" style=\"color:", color, '\"></i>'))) %>%
  dplyr::select(-color)


# Used for plots
# Be careful if sectors change name ! (SDN wrote them inconsistently here...)
colors_sectors <- cons_colors$color %>% setNames(nm = cons_colors$Secteur)





# Theme ----

## Color used for multiple ui items ----

main_color <- "#3A862D"
main_color_active <- "#26C026"

## Custom {fresh} theme passed to bs4Dash in app_ui.R
# Example from https://dreamrs.github.io/fresh/

# Search bs4dash vars
# fresh::search_vars_bs4dash(pattern = "navbar") %>%
#   View()

eneRgy_theme <- fresh::create_theme(

  fresh::bs4dash_status(
    primary = main_color
  ),

  fresh::bs4dash_vars(
    navbar_dark_color = "white",
    navbar_dark_active_color = main_color_active,
    navbar_dark_hover_color = main_color_active
  ),
  fresh::bs4dash_yiq(
    contrasted_threshold = 10,
    text_dark = "#FFF",
    text_light = "#272c30"
  ),
  fresh::bs4dash_layout(
    main_bg = "white"
  ),
  fresh::bs4dash_sidebar_light(
    bg = "#272c30",
    color = "#bec5cb",
    hover_color = "#FFF",
    submenu_bg = "#272c30",
    submenu_color = "#bec5cb",
    submenu_hover_color = "#FFF"
  )

)


# Non-reactive objects for input widgets ----

## Generic objects (across all tabs) ----

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
  dplyr::distinct(`Catégorie DIREN`) %>%
  dplyr::arrange(`Catégorie DIREN`) %>%
  dplyr::pull()

### elec_prod_communes ----
# From installation-specific to communes-specific (faster calculation)

elec_prod_communes <- elec_prod %>%
  dplyr::group_by(Commune, Année, `Catégorie DIREN`) %>%
  dplyr::summarise(dplyr::across(
    .cols = c(`Puissance électrique installée`,
              Production,
              Injection,
              Autoconsommation), ~sum(.x, na.rm = T)),
    `N° OFS` = dplyr::first(`N° OFS`)) %>%
  dplyr::ungroup()

## Objects specific to the tabCons  ----
# to be populated

## Objects specific to the tabMap  ----

### Common electricity year ----
# We seek for the year that is common to both electricity and production datasets
# This is to create the statistic boxes (tabMap) and compare similar years.

last_common_elec_year <- dplyr::intersect(
  elec_cons_communes %>% dplyr::distinct(Année),
  elec_prod_communes %>% dplyr::distinct(Année)) %>%
  dplyr::slice_max(Année) %>%
  dplyr::pull(Année)


### Fixed statistics for boxes ----

#### VD electricity production for last common year

prod_elec_vd_last_year <- elec_prod_communes %>%
  dplyr::filter(Année == last_common_elec_year) %>%
  dplyr::summarise(Production = sum(Production, na.rm = TRUE)) %>%
  dplyr::pull()

#### VD electricity consumption for last common year

cons_elec_vd_last_year <- elec_cons_communes %>%
  dplyr::filter(Année == last_common_elec_year) %>%
  dplyr::summarise(Consommation = sum(Consommation, na.rm = TRUE)) %>%
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
































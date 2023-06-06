# This script loads and stores all 'static' or initial objects fed to the app

# Loading .rda objects ----
## sf data ----

load("./data/sf_canton.rda")
load("./data/sf_communes.rda")
load("./data/sf_districts.rda")
load("./data/sf_lacs.rda")


## electricity_production data ----

load("./data/elec_prod_communes.rda")
load("./data/elec_prod_doc.rda")

## electricity_consumption data ----

# !!CONS_ELEC removed!! load("./data/elec_cons_communes.rda")
# !!CONS_ELEC removed!! load("./data/elec_cons_doc.rda")

## regener_communes data ----

load("./data/regener_cons_ae_use.rda")
load("./data/regener_cons_ae_aff.rda")
load("./data/regener_needs.rda")
load("./data/regener_misc.rda")
load("./data/regener_doc.rda")

## glossary ----
load("./data/glossary.rda")

# Store .Rmd in temp dir ----
# https://mastering-shiny.org/action-transfer.html#downloading-reports

report_path <- tempfile(fileext = ".Rmd")
file.copy("./data/downloadable_report.Rmd", report_path, overwrite = TRUE)

# Files others than .Rd are in ./inst/extdata/

# Generic utils ----

## Sentence for required communes ----
# To avoid multiple repetitions troughout the app we store it once here

req_communes_phrase <- "Sélectionner au moins une commune pour générer un résultat."

## Sentence methodological changes ----
# Used in ui.r to avoid repetition

generic_method_warning <- "La méthode utilisée pour obtenir ces résultats est sujette à amélioration au fil des années. Pour cette raison, il est possible que les données changent légèrement de manière rétroactive.
                                                       Les grands principes de chaque méthode ainsi que les principales modifications sont documentés dans l'onglet 'À propos'."

specific_prod_elec_warning <- "Si des données sont visiblement manquantes ou erronées, merci de nous contacter afin que nous puissions améliorer les résultats."

specific_rgr_warning <- "Ces données dépendent notamment de la qualité de l'information qui figure dans les registres cantonal et fédéral des bâtiments, en particulier pour les agents énergétiques.
La DGE-DIREN se rend disponible pour accompagner des communes qui souhaiteraient procéder à une amélioration des données énergétiques figurant dans le registre."


## User notifications  ----
# These are served to bs4DropdownMenu in app_ui.R

notif_msg <- tidyr::tribble(~icon, ~status, ~text,
                    "cloud-arrow-up", "info", "06.23: Ajout données consommation th. bâtiments 2022",
                     "cloud-arrow-up", "info", "06.23: Ajout données production électricité 2015-2022",
                     "star", "info", "06.23: Mise en ligne du profil"
)


## Sidebar width ----

sidebar_width <- 300 # px ; used in custom.css & plotting fns with inputVals$web_width/height

## Facetted plot's height ----

# Number of facets starting from which a height increase is necessary
n_facets_limit <- 4

# Height of facet plot's below limit
height_facet_under_limit <- 400

# Height of facet plot's above limit
height_facet_above_limit <- 700


## Links and mail ----
# Used for mod_about_the_app.R and/or contact notificationMenu in ui.R

mail_address <- "stat.energie@vd.ch"
link_diren <- "https://www.vd.ch/toutes-les-autorites/departements/departement-de-lenvironnement-et-de-la-securite-des/direction-generale-de-lenvironnement-dge/diren-energie/"
link_dge <- "https://www.vd.ch/toutes-les-autorites/departements/departement-de-la-jeunesse-de-lenvironnement-et-de-la-securite-djes/direction-generale-de-lenvironnement-dge/"
link_pter <- "https://www.vd.ch/themes/etat-droit-finances/communes/energie-environnement-agriculture/energie/planification-energetique-territoriale"


## DT language file ----
### Run ONCE : Store JSON french language items file for DT library

# rjson::fromJSON(
#   file = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/French.json') %>%
#   toJSON() %>%
# write(file = "./data/DT_fr_language.json")

### Load json french language file for DT library
# Files others than .Rd are in ./inst/extdata/

DT_fr_language <- rjson::fromJSON(file = "./data/DT_fr_language.json")

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

## List of non-ASCII words that should replace internal colnames
# Note that colnames are already in a 'sentence' format (i.e. Ae and not ae)
replace_fr_accents <- c("electricite" = "électricité",
                        "electrique" = "électrique",
                        "energetique" = "énergétique",
                        "Categorie" = "Catégorie",
                        "installee" = "installée",
                        "Annee" = "Année",
                        "Ae" = "Agent énergétique",
                        "optimises" = "optimisés")

## These are used to dynamically target columns renaming in fct_helpers.R and mod_elec_charts.R
energy_col_keywords <- c("Consommation", "Production", "Injection", "Autoconsommation", "Besoins")
power_col_keywords <- c("Puissance")



## Prod colors and icons (prod) ----
# Base tribble with categorie, icon and color
prod_colors <- dplyr::tribble(~icon, ~categorie, ~color,
                             as.character(shiny::icon("droplet")),  "Hydroélectricité","#6495ED",
                             as.character(shiny::icon("sun")),  "Solaire", "#FFB90F",
                             as.character(shiny::icon("apple")),  "Déchets méthanisables","#BFDB86",
                             as.character(shiny::icon("leaf")),  "Biomasse agricole", "#48A649",
                             as.character(shiny::icon("tree")),  "Bois-énergie", "#CC9E62",
                             as.character(shiny::icon("industry")), "STEP", "#A58DE6",
                             as.character(shiny::icon("fire")), "Déchets incinérables","#E67A78",
                             as.character(shiny::icon("oil-can")), "Thermique fossile", "#747D82",
                             as.character(shiny::icon("wind")), "Eolien", "#2596BE")


# Used for table icons
# Adding the color style in the html tag for the icon
prod_icons <- prod_colors %>%
  dplyr::rowwise() %>%
  dplyr::mutate(icon = stringr::str_replace(string = icon, pattern = "></i>",
                                     replacement = paste0(" style=\"color:", color, '\"></i>'))) %>%
  dplyr::select(-color) %>%
  dplyr::ungroup()

# Palette for plots: named vector with level + associated color
colors_categories <- prod_colors$color %>% setNames(nm = prod_colors$categorie)

## Cons colors and icons (cons) ----
# Base tribble with sector, icon and color
cons_colors <- dplyr::tribble(~icon, ~secteur, ~color,
                             as.character(shiny::icon("industry")), "Industrie/Services","#6495ED",
                             as.character(shiny::icon("house")), "Ménages", "#FFB90F",
                             as.character(shiny::icon("car")),  "Transports", "#BFDB86",
                             as.character(shiny::icon("question")),  "Inconnu", "#BFDB86")
# Used for table icons
# Adding the color style in the html tag for the icon
cons_icons <- cons_colors %>%
  dplyr::rowwise() %>%
  dplyr::mutate(icon = stringr::str_replace(string = icon, pattern = "></i>",
                                            replacement = paste0(" style=\"color:", color, '\"></i>'))) %>%
  dplyr::select(-color) %>%
  dplyr::ungroup()


# Palette for plots: named vector with level + associated color
# Be careful if sectors change name ! (SDN wrote them inconsistently here...)
colors_sectors <- cons_colors$color %>% setNames(nm = cons_colors$secteur)


## Regener AE icons (rg) ----
# Base tribble for AE, color and icon

regener_colors <- dplyr::tribble(~icon, ~ae, ~color,
                              as.character(shiny::icon("tree")), "Bois", "#CC9E62",
                              as.character(shiny::icon("industry")), "CAD", "#A58DE6",
                              as.character(shiny::icon("bolt")), "Electricité (direct)", "#FFEF0F",
                              as.character(shiny::icon("bolt-lightning")), "Electricité (PAC)", "#FF870F",
                              as.character(shiny::icon("temperature-half")), "Chaleur ambiante (PAC)", "#FF870F", # 2023-05-10
                              as.character(shiny::icon("fire-flame-simple")), "Gaz", "#477fc1", #gaznat
                              as.character(shiny::icon("fire-flame-simple")), "Mazout", "#5d5f63",
                              as.character(shiny::icon("sun")), "Solaire thermique", "#FFB90F"
                              )

# Used for table icons
# Adding the color style in the html tag for the icon
regener_icons <- regener_colors %>%
  dplyr::rowwise() %>%
  dplyr::mutate(icon = stringr::str_replace(string = icon, pattern = "></i>",
                                            replacement = paste0(" style=\"color:", color, '\"></i>'))) %>%
  dplyr::select(-color) %>%
  dplyr::ungroup()

# Used for plots: named vector with level + associated color
colors_ae <- regener_colors$color %>% setNames(nm = regener_colors$ae)

## Regener type icons (rg) ----

# Base tribble for AE, color and icon

regener_colors_type <- dplyr::tribble(~icon, ~type, ~color,
                                      as.character(shiny::icon("fire")), "Chauffage", "#DC143C",
                                      as.character(shiny::icon("droplet")), "ECS", "#6495ED"
)

# Used for table icons
# Adding the color style in the html tag for the icon
regener_icons_type <- regener_colors_type %>%
  dplyr::rowwise() %>%
  dplyr::mutate(icon = stringr::str_replace(string = icon, pattern = "></i>",
                                            replacement = paste0(" style=\"color:", color, '\"></i>'))) %>%
  dplyr::select(-color) %>%
  dplyr::ungroup()

# Used for plots: named vector with level + associated color
colors_rg_type <- regener_colors_type$color %>%setNames(nm = regener_colors_type$type)

# Theme ----

## Color used for multiple ui items ----

main_color <- "#3A862D"
main_color_active <- "black"

## Custom {fresh} theme passed to bs4Dash in app_ui.R
# Example from https://dreamrs.github.io/fresh/

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
# Used exclusively for selectizeInput in mod_inputs.R and its update counterpart in app_server.R
# The map is thus the reference for the commune names !
communes_names <- sf_communes %>%
  dplyr::distinct(NOM_MIN) %>%
  dplyr::arrange(NOM_MIN) %>%
  dplyr::pull(NOM_MIN)

### Choices selectizeInput ----
# Since we want to add the canton de Vaud as an input choice, we make a list for selectizeInput()

choices_canton_communes <- list(
  Canton = list("Canton de Vaud"),
  Communes = communes_names
)

### Available districts ----
# For the zooming feature. We add one row for the cantonal view

districts_names <- sf_districts %>%
  dplyr::distinct(NOM_MIN) %>%
  dplyr::arrange(NOM_MIN) %>%
  # add one row manually for the Canton, it should be placed first
  dplyr::add_row(NOM_MIN = "Canton", .before = 1) %>%
  dplyr::pull(NOM_MIN)

## Objects specific to the tabProd  ----

### Colors for categorie ----

categories_diren <- elec_prod_communes %>%
  dplyr::distinct(categorie) %>%
  dplyr::arrange(categorie) %>%
  dplyr::pull()

## Objects specific to the tabCons  ----

# to be populated

## Objects specific to the tabRegener  ----

regener_current_year <- 2022  # used for create_sunburst_plotly() specific to regener data...

## Objects specific to the tabMap  ----

### Common electricity year ----
# We seek for the year that is common to both electricity consumption and production datasets
# This is to create the statistic boxes (tabMap) and compare similar years.

last_common_elec_year <- max(elec_prod_communes$annee) # When prod elec alone

  # !!CONS_ELEC removed!! # dplyr::intersect(
  # !!CONS_ELEC removed!! # elec_cons_communes %>% dplyr::distinct(annee),
  # !!CONS_ELEC removed!! # elec_prod_communes %>% dplyr::distinct(annee)) %>%
  # !!CONS_ELEC removed!! # dplyr::slice_max(annee) %>%
  # !!CONS_ELEC removed!! # dplyr::pull(annee)


### Cantonal statbox values ----

#### VD electricity production for last common year

prod_elec_vd_last_year <- elec_prod_communes %>%
  dplyr::filter(commune == "Canton de Vaud") %>%
  dplyr::filter(annee == last_common_elec_year) %>%
  dplyr::summarise(production = sum(production, na.rm = TRUE)) %>%
  dplyr::pull()

#### VD electricity consumption for last common year

cons_elec_vd_last_year <- NULL   # so we keep most code unchanged in mod_collapse_stats_box.R !!

# !!CONS_ELEC removed!! #  cons_elec_vd_last_year <- elec_cons_communes %>%
# !!CONS_ELEC removed!! #   dplyr::filter(annee == last_common_elec_year) %>%
# !!CONS_ELEC removed!! #   dplyr::filter(commune == "Canton de Vaud") %>%
# !!CONS_ELEC removed!! #   dplyr::summarise(consommation = sum(consommation, na.rm = TRUE)) %>%
# !!CONS_ELEC removed!! #   dplyr::pull()

#### VD heat consumption for last common year

cons_rg_vd_last_year <- regener_cons_ae_aff %>%
  # dplyr::filter(annee == last_common_elec_year) %>% when year is here
  dplyr::filter(commune == "Canton de Vaud") %>%
  dplyr::summarise(consommation = sum(consommation, na.rm = TRUE)) %>%
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



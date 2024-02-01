# This script loads and stores all 'static' or initial objects fed to the app
library(magrittr)
# Loading .rda objects ----
## sf data ----

load("./data/sf_canton.rda")
load("./data/sf_communes.rda")
load("./data/sf_districts.rda")
load("./data/sf_lacs.rda")

## electricity_production data ----

load("./data/elec_prod.rda")
load("./data/elec_prod_doc.rda")

## electricity_consumption data ----

load("./data/elec_cons.rda")
load("./data/elec_cons_doc.rda")

## regener_communes data ----

load("./data/regener_cons_ae_use.rda")
load("./data/regener_cons_ae_aff.rda")
load("./data/regener_needs.rda")
load("./data/regener_misc.rda")
load("./data/regener_doc.rda")

## subsidies data ----

load("./data/subsidies_by_building.rda")
load("./data/subsidies_by_measure.rda")
load("./data/subsidies_doc.rda")

## glossary ----

load("./data/glossary.rda")

# Store .Rmd in temp dir ----
# https://mastering-shiny.org/action-transfer.html#downloading-reports

report_path <- tempfile(fileext = ".Rmd")
file.copy("./data/downloadable_report.Rmd", report_path, overwrite = TRUE)

# Files others than .Rd are in ./inst/extdata/

# Generic utils ----
# Tab-specific items at the end (see outline : `Objects specific to...`) !

## Debounce time ----
# Usef in app_server.R + plotting modules to avoid loop errors and flickering

debounce_plot_time <- 400 # ms

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

specific_subsidies_warning <- "Ces données sont issues d'un traitement des données du Programme bâtiments et concernent exclusivement les subventions versées (avec achèvement de travaux).
Les promesses (subventions promises mais pas encore versées) sont exclues."



## NEWS notifications  ----
# These are served to bs4DropdownMenu in app_ui.R

notif_msg <- tidyr::tribble(~icon, ~status, ~text,
                    "cloud-arrow-up", "info", "01.24: Ajout données subventions Programme Bâtiments",
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
link_github <- "https://github.com/mick-weber/eneRgyVD"

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

## Column replacement ----

cols_renaming_vector <- c(
  # regener_misc
  "Commune" = "commune",  # applies to `subsidies` too
  "Etat" = "etat",        # applies to `subsidies` too
  "Surface de référence énergétique (m2)" = "SRE",  # applies to `subsidies` too
  "Bâtiments chauffés" = "N_EGID",
  "Bâtiments neufs (2001+)" = "N_NEW_POST_2000",
  "Bâtiments rénovés légèrement (2001+)" = "N_RENOV_L_POST_2000",
  "Bâtiments rénovés lourdement (2001+)" = "N_RENOV_H_POST_2000",
  "Bâtiments sans rénovation récente" = "N_NO_RENOV",
  "Bâtiments sans année de construction" = "N_NO_GBAUJ",
  # subsidies
  "Type de subvention" = "subv_type"
)


## List of non-ASCII words that should replace internal colnames
# Note that colnames are already in a 'sentence' format (i.e. Ae and not ae)
replace_fr_accents <- c("electricite" = "électricité",
                        "electrique" = "électrique",
                        "energetique" = "énergétique",
                        "Categorie" = "Catégorie",
                        "installee" = "installée",
                        "Annee" = "Année",
                        "Ae" = "Agent énergétique",
                        "optimises" = "optimisés",
                        "Detail" = "Détail")

## These are used to dynamically target columns renaming in fct_helpers.R and mod_elec_charts.R
energy_col_keywords <- c("Consommation", "Production", "Injection", "Autoconsommation", "Besoins")
power_col_keywords <- c("Puissance", "installé")
co2_keywords <- c("CO2")


# Colors and icons ----

## Default ggplot2 colors ----

# Avoids darkgrey for geom_cols when 'var_rank_2' is NULL
ggplot2::update_geom_defaults("col", aes(fill = "#90b0ee"))

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

colors_categories <- prod_colors$color %>%
  setNames(nm = prod_colors$categorie)

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

colors_sectors <- cons_colors$color %>%
  setNames(nm = cons_colors$secteur)

## Regener AE colors and icons (rg) ----
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

colors_ae <- regener_colors$color %>%
  setNames(nm = regener_colors$ae)

## Regener type colors and icons  (rg) ----

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

colors_rg_type <- regener_colors_type$color %>%
  setNames(nm = regener_colors_type$type)

## Subsidies colors and icons (subs) ----

### Subsidies building ----
# if needed : subsidies_building |> distinct(subv_type)

subsidies_building_palette <- dplyr::tribble(~icon, ~subv_type, ~color,
                                         as.character(shiny::icon("house")), "Isolation partielle", "#FFEF0F",
                                         as.character(shiny::icon("house")), "Isolation globale", "#FF870F",
                                         as.character(shiny::icon("house-fire")), "Isolation partielle + chauffage renouvelable", "#b5dbb6",
                                         as.character(shiny::icon("house-fire")), "Isolation globale + chauffage renouvelable", "#48A649",
                                         as.character(shiny::icon("fire")), "Chauffage renouvelable", "#6cb76d",
                                         as.character(shiny::icon("asterisk")), "Autres", "#cccccc",

)

# Used for table icons

subsidies_building_icons <- subsidies_building_palette |>
  dplyr::rowwise() %>%
  dplyr::mutate(icon = stringr::str_replace(string = icon, pattern = "></i>",
                                            replacement = paste0(" style=\"color:", color, '\"></i>'))) %>%
  dplyr::select(-color) %>%
  dplyr::ungroup()

# Used for plots: named vector with level + associated color

subsidies_building_colors <- subsidies_building_palette$color %>%
  setNames(nm = subsidies_building_palette$subv_type)

### Subsidies measure ----
# Slight different approach than others to simplify the work

# We generate a custom palette (available palettes are less than xy colors...)
# It's flexible enough if new/removed subsidies appear
# Thanks https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r

# Extract number of categories for mesure_simplifiee (supplied to brewer.pal below)
n_max_mesure_simplifiee <- dplyr::n_distinct(subsidies_by_measure$mesure_simplifiee)

# This palette is only used for the plot, based on `mesure_simplifiee`
subsidies_measure_palette_plot <- subsidies_by_measure |>
  dplyr::distinct(mesure_simplifiee) |>
  dplyr::mutate(color = RColorBrewer::brewer.pal(
    n = n_max_mesure_simplifiee,
    name = "Pastel1"
  ))
# Prepare palette for create_bar_plotly()
subsidies_measure_simplifiee_colors <- subsidies_measure_palette_plot$color %>%
  setNames(nm = subsidies_measure_palette_plot$mesure_simplifiee)


# This palette is only supplying colors for the table. The p
subsidies_measure_palette_table <- subsidies_by_measure |>
  dplyr::distinct(mesure) |>
  dplyr::mutate(icon = dplyr::case_when(
    mesure %in% c("M01", "M12", "M13", "M14") ~ as.character(shiny::icon("house")),
    .default = as.character(shiny::icon("fire")))) |>
  # allow for lumped factor generated in mod_subsidies_measure_charts.R
  dplyr::mutate(color = "#cccccc") # grey for all


# Used for table icons

subsidies_measure_icons <- subsidies_measure_palette_table |>
  dplyr::rowwise() %>%
  dplyr::mutate(icon = stringr::str_replace(string = icon, pattern = "></i>",
                                            replacement = paste0(" style=\"color:", color, '\"></i>'))) %>%
  dplyr::select(-color) %>%
  dplyr::ungroup()

# Unused for plots: we only take the simplified version above

# subsidies_measure_detail_colors <- subsidies_measure_palette_table$color %>%
#   setNames(nm = subsidies_measure_palette_table$mesure)


# Theme ----

## Color used for multiple ui items ----

main_color <- "#3A862D"
main_color_active <- "#343A40"

## Custom {fresh} theme passed to bs4Dash in app_ui.R
# Example from https://dreamrs.github.io/fresh/

profil_theme <- bslib::bs_theme(version = 5,
                                preset = "bootstrap",
                                font_scale = 1.2,
                                "navbar-bg" = "#3A862D",
                                "modal-footer-margin-between" = "0rem",
                                primary = "#3A862D",
                                secondary = "#343A40"
) |>
  # add some variables
  bslib::bs_add_variables(

    "dropdown-link-active-bg" = "white",
    "dropdown-link-active-color" = "$secondary",
    "accordion-button-active-bg" = "$secondary", # bg color when activated
    "accordion-button-active-color" = "white",   # text color when activated
    "accordion-button-focus-box-shadow" = "0 0 0 $btn-focus-width rgba($secondary, 0.25)", # width + color of shadow when active

    .where = "declarations") |>
    bslib::bs_add_rules(sass::sass_file("./inst/app/www/custom_bs5.scss")) # add complementary sass file


# Non-reactive objects for input widgets ----

## Generic objects (across all tabs) ----

### Available communes ----
# Used exclusively for selectizeInput in mod_inputs.R and its update counterpart in app_server.R
# The map is thus the reference for the commune names !
communes_names <- sf_communes %>%
  dplyr::distinct(NOM_MIN) %>%
  dplyr::arrange(NOM_MIN) %>%
  dplyr::pull(NOM_MIN)

# Create named vector of communes + OFS number, used for the upload file feature
#  which requires OFS numbers from the user (less error prone !)
communes_names_id <- sf_communes |>
  dplyr::distinct(NOM_MIN, NO_COM_FED) |>
  dplyr::arrange(NOM_MIN) |>
  dplyr::pull(NOM_MIN,NO_COM_FED)

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

categories_diren <- elec_prod %>%
  dplyr::distinct(categorie) %>%
  dplyr::arrange(categorie) %>%
  dplyr::pull()

## Objects specific to the tabCons  ----

# to be populated

## Objects specific to the tabRegener  ----
# used for fct_helpers.R -> mod_regener_cons_charts.R + mod_collapse_stats_box.R

min_regener_year <- min(regener_cons_ae_use$etat)
max_regener_year <- max(regener_cons_ae_use$etat)

regener_current_year <- max_regener_year

## Objects specific to the tabSubsidies  ----

# mesures_pb |> dplyr::select(MESURE, MESURE_TRAD, MESURE_SIMPLIFIEE2)


## Objects specific to the tabMap  ----

### Common electricity year ----
# We seek for the year that is common to both electricity consumption and production datasets
# This is to create the statistic boxes (tabMap) and compare similar years.

last_common_elec_year <- max(elec_prod$annee) # When prod elec alone

  # !!CONS_ELEC removed!! # dplyr::intersect(
  # !!CONS_ELEC removed!! # elec_cons %>% dplyr::distinct(annee),
  # !!CONS_ELEC removed!! # elec_prod %>% dplyr::distinct(annee)) %>%
  # !!CONS_ELEC removed!! # dplyr::slice_max(annee) %>%
  # !!CONS_ELEC removed!! # dplyr::pull(annee)


### Cantonal statbox values ----

#### VD electricity production for last common year

prod_elec_vd_last_year <- elec_prod %>%
  dplyr::filter(commune == "Canton de Vaud") %>%
  dplyr::filter(annee == last_common_elec_year) %>%
  dplyr::summarise(production = sum(production, na.rm = TRUE)) %>%
  dplyr::pull()

#### VD electricity consumption for last common year

cons_elec_vd_last_year <- NULL   # so we keep most code unchanged in mod_collapse_stats_box.R !!

# !!CONS_ELEC removed!! #  cons_elec_vd_last_year <- elec_cons %>%
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

#### VD subsidies M01 for last common year
last_subsidies_year <- max(subsidies_by_measure$annee)

subsidies_m01_vd_last_year <- subsidies_by_measure |>
  dplyr::filter(annee == last_subsidies_year) |>
  dplyr::filter(commune == "Canton de Vaud") |>
  dplyr::filter(mesure == "M01") |>
  dplyr::summarise(nombre = sum(nombre, na.rm = TRUE)) |>
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



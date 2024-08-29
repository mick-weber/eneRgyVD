# This script loads and stores all 'static' or initial objects fed to the app
# Loading .rda objects ----
## sf data ----

load("./data/sf_canton.rda")
load("./data/sf_communes.rda")
load("./data/sf_districts.rda")
load("./data/sf_lacs.rda")

## elec_prod data ----

load("./data/elec_prod.rda")
load("./data/elec_prod_doc.rda")

## elec_cons data ----

load("./data/elec_cons.rda")
load("./data/elec_cons_doc.rda")

## regener data ----

load("./data/regener_cons_ae_use.rda")
load("./data/regener_cons_ae_aff.rda")
load("./data/regener_needs.rda")
load("./data/regener_misc.rda")
load("./data/regener_doc.rda")

## subsidies data ----

load("./data/subsidies_by_building.rda")
load("./data/subsidies_by_measure.rda")
load("./data/subsidies_doc.rda")

## profil-climat dummy data (generic_data)----

load("./data/dummy_climat.rda")

## |------------------------------------------------------------------|
##          PICK one of <canop> or <part_ve> as generic_data prototype
## |------------------------------------------------------------------|
# generic_data <- canop |>
#   dplyr::rename("generic_value" = surface_canopee)
#
# generic_data_vd <- generic_data |>
#   dplyr::summarise(commune = "Canton de Vaud", generic_value = sum(generic_value, na.rm = T), annee = dplyr::first(annee))
#
# generic_data <- generic_data |>
#   dplyr::bind_rows(generic_data_vd)


## profil-climat dummy data 2 (generic data) ----
# note : data prep is done before as opposed to dummy_climat.rda

load("./data/dummy_mobilite.rda")

generic_data <- part_ve


## glossary ----

load("./data/glossary.rda")

## doc panels for accordions ----
# see fct_helpers.R

elec_prod_doc_panels <- generate_doc_accordion_panels(md_file = "./data-doc/elec_prod-doc.md")
elec_cons_doc_panels <- generate_doc_accordion_panels(md_file = "./data-doc/elec_cons-doc.md")
regener_doc_panels <- generate_doc_accordion_panels(md_file = "./data-doc/regener-doc.md")
subsidies_doc_panels <- generate_doc_accordion_panels(md_file = "./data-doc/subsidies-doc.md")

# DEV for now
generic_data_panels <- generate_doc_accordion_panels(md_file = "./data-doc/generic-doc.md")



# REDIRECTIONS ----

# This is used by app_server.R/ and methodological accordions in specific modules to redirect user in 'mod_about_the_app.R' module
# This is also ulsed by mod_table_content.R to redirect users towards tabpanels

subpanels_tribble <- dplyr::tribble(
  ~observe_input, ~about_nav_panel, ~navset_id,  ~about_tabpanel_name, ~data_id, ~nav_panel, ~navset_name, ~nav_name,
  "consumption_charts-elec_data_help","Energie","navset_energie", "Distribution d'électricité", "data_1", "Electricité", "navset_elec", "Distribution d'électricité",
  "production_charts-elec_data_help", "Energie","navset_energie", "Production d'électricité", "data_2", "Electricité", "navset_elec", "Production d'électricité",
  "regener_needs-rgr_needs_help", "Energie","navset_energie", "Chaleur bâtiments", "data_3", "Chaleur des bâtiments","navset_regener", "Besoins des bâtiments",
  "regener_cons-rgr_cons_help", "Energie", "navset_energie","Chaleur bâtiments", "data_4", "Chaleur des bâtiments","navset_regener", "Consommation des bâtiments",
  "regener_misc-rgr_misc_help", "Energie","navset_energie", "Chaleur bâtiments", "data_5", "Chaleur des bâtiments","navset_regener", "Informations bâtiments",
  "subsidies_building-subsidies_building_help", "Energie","navset_energie", "Subventions bâtiments", "data_6", "Subventions bâtiments","navset_subsidies", "Vue par bâtiments",
  "subsidies_measure-subsidies_measure_help", "Energie", "navset_energie", "Subventions bâtiments", "data_7", "Subventions bâtiments","navset_subsidies", "Vue par subventions",

  # COMPLETE THESE ONES (and more !) when real data is here
  "test_generic_climat-generic_data_help",  "Adaptation","navset_climat", "Donnée générique", "data_8", "Exemple générique 1", "navset_climat", "Première donnée",
  "test_generic_mob-generic_data_help",  "Mobilité","navset_mobilite", "Donnée générique", "data_9", "Exemple générique 2", "navset_mobilite","Première donnée"


)



# Store .Rmd in temp dir ----
# https://mastering-shiny.org/action-transfer.html#downloading-reports

report_path <- tempfile(fileext = ".Rmd")

# Files others than .Rd are in ./inst/extdata/
file.copy("./inst/extdata/downloadable_report.Rmd", report_path, overwrite = TRUE)

# Generic utils ----
# Tab-specific items at the end (see outline : `Objects specific to...`) !

## Debounce time ----
# Usef in app_server.R + plotting modules to avoid flickering and error loops with leaflet map

debounce_plot_time <- 400 # ms, empirically defined as ideal

## Sentence related to communes selection ----

# when 0 commune is selected for an output display request :
req_communes_phrase <- "Sélectionner au moins une commune pour générer un résultat"

# when 1+ commune is selected but not available in the dataset :
req_communes_not_available <- "Aucune donnée n'est disponible pour la sélection actuelle"

## Sentence methodological changes ----
# Used in ui.r to avoid repetition

generic_method_warning <- "La méthode utilisée pour obtenir ces résultats est sujette à amélioration au fil des années. Pour cette raison, il est possible que les données changent légèrement de manière rétroactive.
                                                       Les grands principes de chaque méthode ainsi que les principales modifications sont documentés dans l'onglet 'À propos'."

specific_elec_warning <- "Si des données sont visiblement manquantes ou erronées, merci de prendre contact afin qu'une évaluation du problème puisse être faite."

specific_rgr_warning <- "Ces données dépendent notamment de la qualité de l'information qui figure dans les registres cantonal et fédéral des bâtiments, en particulier pour les agents énergétiques.
La DGE-DIREN se rend disponible pour accompagner des communes qui souhaiteraient procéder à une amélioration des données énergétiques figurant dans le registre."

specific_subsidies_warning <- "Ces données sont issues d'un traitement des données du Programme bâtiments et concernent exclusivement les subventions versées (avec achèvement de travaux).
Les subventions promises pour lesquels les travaux n'ont pas été effectués ne sont donc pas comptabilisées."



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
link_pecc <- "https://www.vd.ch/pecc" # shortcut link redirecting to https://www.vd.ch/etat-droit-finances/communes/climat-et-durabilite/plan-energie-et-climat-communal-pecc
link_github <- "https://github.com/mick-weber/eneRgyVD"

link_dummy_generic_data <- 'https://www.geo.vd.ch/?mapresources=GEOVD_ENERGIE&visiblelayers={%22GEOVD_ENERGIE%22:[%22Eoliennes%20du%20site%22,%22Site%20%C3%A9olien%22]}'

### geoportail links ----

regener_geovd_link <- 'https://www.geo.vd.ch' # temporary


## DT language file ----
### RUN ONCE : Store JSON french language items file for DT library. May require rjson library (not in DESCRIPTION)

# rjson::fromJSON(
#   file = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/French.json') |>
#   toJSON() |>
# write(file = "./inst/extdata/DT_fr_language.json")

### Load json french language file for DT library
# Files others than .Rd are in ./inst/extdata/

DT_fr_language <- rjson::fromJSON(file = "./inst/extdata/DT_fr_language.json")

## Units conversion table ----
# Must match the choices in header's widget
# Linked to fct_helpers.R convert_units()'s function

# all datasets should come as 'kWh' values so we index based on kWh
energy_units_table <- dplyr::tribble(
  ~unit, ~factor,
  "kWh", 1,
  "MWh", 1e3,
  "GWh", 1e6,
  "TJ", 1/3.6*1e6
)

# all datasets come as 'tCO2' values so we index based on tCO2
co2_units_table <- dplyr::tribble(
  ~unit, ~factor,
  "kgCO2", 1e-3,
  "tCO2", 1,
  "ktCO2", 1e3
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
  "Type de subvention" = "subv_type",

  # part_ve
  "Part véhicules électriques (hybrides compris)" = "part_ve"
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

## These are used to dynamically target columns renaming in add_colname_units() and mod_elec_charts.R
energy_col_keywords <- c("Consommation", "Production", "Injection", "Autoconsommation", "Besoins")
power_col_keywords <- c("Puissance", "installé")
co2_keywords <- c("CO2")
percent_keywords <- c("Pct", "Part")


# Colors and icons ----

## Default ggplot2 colors ----

# Avoids darkgrey for geom_cols when 'var_cat' is NULL and if no palette/color is supplied
ggplot2::update_geom_defaults("col", ggplot2::aes(fill = "#90b0ee"))

## Default palette if not supplied
default_palette <- RColorBrewer::brewer.pal(name = "Set3", n = 12)

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

prod_icons <- prod_colors |>
  dplyr::rowwise() |>
  dplyr::mutate(icon = stringr::str_replace(string = icon, pattern = "></i>",
                                     replacement = paste0(" style=\"color:", color, '\"></i>'))) |>
  dplyr::select(-color) |>
  dplyr::ungroup()

# Palette for plots: named vector with level + associated color

colors_categories <- prod_colors$color |>
  setNames(nm = prod_colors$categorie)

## Cons colors and icons (cons) ----
# Base tribble with sector, icon and color

cons_colors <- dplyr::tribble(~icon, ~secteur, ~color,
                             as.character(shiny::icon("industry")), "Hors-ménages","#6495ED",
                             as.character(shiny::icon("house")), "Ménages", "#FFB90F",
                             as.character(shiny::icon("question")),  "Indéfini", "#BFDB86")
# Used for table icons
# Adding the color style in the html tag for the icon

cons_icons <- cons_colors |>
  dplyr::rowwise() |>
  dplyr::mutate(icon = stringr::str_replace(string = icon, pattern = "></i>",
                                            replacement = paste0(" style=\"color:", color, '\"></i>'))) |>
  dplyr::select(-color) |>
  dplyr::ungroup()

# Palette for plots: named vector with level + associated color
# Be careful if sectors change name ! (SDN wrote them inconsistently here...)

colors_sectors <- cons_colors$color |>
  setNames(nm = cons_colors$secteur)

## Regener AE colors and icons (rg) ----
# Base tribble for AE, color and icon

regener_colors <- dplyr::tribble(~icon, ~ae, ~color,
                              as.character(shiny::icon("tree")), "Bois", "#CC9E62",
                              as.character(shiny::icon("industry")), "CAD", "#A58DE6",
                              as.character(shiny::icon("bolt")), "Electricité (direct)", "#ffdb0f",
                              as.character(shiny::icon("bolt-lightning")), "Electricité (PAC)", "#FF870F",
                              as.character(shiny::icon("temperature-half")), "Chaleur ambiante (PAC)", "#FF870F", # 2023-05-10
                              as.character(shiny::icon("fire-flame-simple")), "Gaz", "#477fc1", #gaznat
                              as.character(shiny::icon("fire-flame-simple")), "Mazout", "#5d5f63",
                              as.character(shiny::icon("sun")), "Solaire thermique", "#FFB90F"
                              )

# Used for table icons
# Adding the color style in the html tag for the icon

regener_icons <- regener_colors |>
  dplyr::rowwise() |>
  dplyr::mutate(icon = stringr::str_replace(string = icon, pattern = "></i>",
                                            replacement = paste0(" style=\"color:", color, '\"></i>'))) |>
  dplyr::select(-color) |>
  dplyr::ungroup()

# Used for plots: named vector with level + associated color

colors_ae <- regener_colors$color |>
  setNames(nm = regener_colors$ae)

## Regener type colors and icons  (rg) ----

# Base tribble for AE, color and icon

regener_colors_type <- dplyr::tribble(~icon, ~type, ~color,
                                      as.character(shiny::icon("fire")), "Chauffage", "#DC143C",
                                      as.character(shiny::icon("droplet")), "ECS", "#6495ED"
)

# Used for table icons
# Adding the color style in the html tag for the icon

regener_icons_type <- regener_colors_type |>
  dplyr::rowwise() |>
  dplyr::mutate(icon = stringr::str_replace(string = icon, pattern = "></i>",
                                            replacement = paste0(" style=\"color:", color, '\"></i>'))) |>
  dplyr::select(-color) |>
  dplyr::ungroup()

# Used for plots: named vector with level + associated color

colors_rg_type <- regener_colors_type$color |>
  setNames(nm = regener_colors_type$type)

## Subsidies colors and icons (subs) ----

### Subsidies building ----
# if needed : subsidies_building |> distinct(subv_type)

subsidies_building_palette <- dplyr::tribble(~icon, ~subv_type, ~color,
                                         as.character(shiny::icon("house")), "Isolation partielle", "#ffdb0f",
                                         as.character(shiny::icon("house")), "Isolation globale", "#FF870F",
                                         as.character(shiny::icon("house-fire")), "Isolation partielle + chauffage renouvelable", "#66ccff",
                                         as.character(shiny::icon("house-fire")), "Isolation globale + chauffage renouvelable", "#0077b3",
                                         as.character(shiny::icon("fire")), "Chauffage renouvelable", "#6cb76d",
                                         as.character(shiny::icon("asterisk")), "Autres", "#cccccc"

)

# Used for table icons

subsidies_building_icons <- subsidies_building_palette |>
  dplyr::rowwise() |>
  dplyr::mutate(icon = stringr::str_replace(string = icon, pattern = "></i>",
                                            replacement = paste0(" style=\"color:", color, '\"></i>'))) |>
  dplyr::select(-color) |>
  dplyr::ungroup()

# Used for plots: named vector with level + associated color

subsidies_building_colors <- subsidies_building_palette$color |>
  setNames(nm = subsidies_building_palette$subv_type)

### Subsidies measure ----
# Slight different approach than others to simplify the work

# We generate a custom palette (available palettes are less than xy colors...)
# It's flexible enough if new/removed subsidies appear
# Thanks https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r

## |---------------------------------------------------------------|
##          REVIEW START
## |---------------------------------------------------------------|

# NEW CODE
subsidies_measure_palette_plot <- dplyr::tribble(~mesure_simplifiee, ~ color,
                                                 "Isolation partielle", "#ffdb0f",
                                                 "Isolation globale", "#FF870F",
                                                 "Chauffage PAC", "#c8e4c9",
                                                 "Chauffage solaire thermique", "#aed7ae",
                                                 "Chauffage bois", "#93ca94",
                                                 "Chauffage CAD", "#79bd7a",
                                                 "Autres", "#cccccc",
                                                 "Autres mesures (voir table)", "#cccccc"
                                                 )


## |---------------------------------------------------------------|
##          REVIEW END
## |---------------------------------------------------------------|

# Prepare palette for create_bar_plotly()
subsidies_measure_simplifiee_colors <- subsidies_measure_palette_plot$color |>
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
  dplyr::rowwise() |>
  dplyr::mutate(icon = stringr::str_replace(string = icon, pattern = "></i>",
                                            replacement = paste0(" style=\"color:", color, '\"></i>'))) |>
  dplyr::select(-color) |>
  dplyr::ungroup()

# Unused for plots: we only take the simplified version above

# subsidies_measure_detail_colors <- subsidies_measure_palette_table$color |>
#   setNames(nm = subsidies_measure_palette_table$mesure)


# Non-reactive objects for input widgets ----

## Generic objects (across all tabs) ----

### Available communes ----
# Used exclusively for selectizeInput in mod_inputs.R and its update counterpart in app_server.R
# The map is thus the reference for the commune names !
communes_names <- sf_communes |>
  dplyr::distinct(NOM_MIN) |>
  dplyr::arrange(NOM_MIN) |>
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

## Objects specific 'Production électricité'  ----

### Colors for categorie

categories_diren <- elec_prod |>
  dplyr::distinct(categorie) |>
  dplyr::arrange(categorie) |>
  dplyr::pull()

## Objects specific 'Consommation électricité'  ----

# to be populated

## Objects specific 'Chaleur bâtiments'  ----
# used for fct_helpers.R -> mod_regener_cons_charts.R + mod_stats_box.R

min_regener_year <- min(regener_cons_ae_use$etat)
max_regener_year <- max(regener_cons_ae_use$etat)

regener_current_year <- max_regener_year

## Objects specific 'Subventions'  ----

# mesures_pb |> dplyr::select(MESURE, MESURE_TRAD, MESURE_SIMPLIFIEE2)


## Objects specific 'Accueil' (Statbox)  ----

#last_common_elec_year <- max(elec_prod$annee) # When prod elec alone

### Cantonal statbox values ----

#### VD electricity production for last available year

last_year_elec_prod <- max(elec_prod$annee) # When prod elec alone

elec_prod_vd_last_year <- elec_prod |>
  dplyr::filter(commune == "Canton de Vaud") |>
  dplyr::filter(annee == last_year_elec_prod) |>
  dplyr::summarise(production = sum(production, na.rm = TRUE)) |>
  dplyr::pull()

#### VD electricity consumption for last available year

last_year_elec_cons <- max(elec_cons$annee) # When prod elec alone

elec_cons_vd_last_year <- elec_cons |>
  dplyr::filter(commune == "Canton de Vaud") |>
  dplyr::filter(annee == last_year_elec_cons) |>
  dplyr::summarise(consommation = sum(consommation, na.rm = TRUE)) |>
  dplyr::pull()

#### VD heat consumption for last common year
# !`annee` -> `etat`
last_year_rgr <- max(regener_needs$etat)

cons_rg_vd_last_year <- regener_cons_ae_aff |>
  dplyr::filter(commune == "Canton de Vaud") |>
  dplyr::filter(etat == last_year_rgr) |>
  dplyr::summarise(consommation = sum(consommation, na.rm = TRUE)) |>
  dplyr::pull()

#### VD subsidies M01 for last common year
last_year_subsidies <- max(subsidies_by_measure$annee)

subsidies_m01_vd_last_year <- subsidies_by_measure |>
  dplyr::filter(commune == "Canton de Vaud") |>
  dplyr::filter(annee == last_year_subsidies) |>
  dplyr::filter(mesure == "M01") |>
  dplyr::summarise(nombre = sum(nombre, na.rm = TRUE)) |>
  dplyr::pull()


### Map-specific ----
# We retrieve the coordinates
# With a nested list ; each district name has 4 coordinates (xmin,ymin,xmax,ymax)
# These coordinates represent the boundaries for the leaflet map zoom adjustments (through widget)

# bboxes <- districts_names |>
#   purrr::map(.f = ~ sf::st_bbox(sf_districts |> dplyr::filter(NOM_MIN == .x))) |>
#   purrr::set_names(districts_names)
#
# ## Add & fill the Canton bbox in our bboxes using the whole districts borders
#
# bboxes$Canton <- sf_districts |> sf::st_bbox()



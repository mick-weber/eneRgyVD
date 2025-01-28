# This script loads and stores all 'static' or initial objects fed to the app
# Loading .rda objects ----
## sf data ----

load("./data/sf_canton.rda")
load("./data/sf_communes.rda")
load("./data/sf_districts.rda")
load("./data/sf_lacs.rda")

## energy data ----
# elec_prod
load("./data/elec_prod.rda")
load("./data/elec_prod_doc.rda")
# elec_cons
load("./data/elec_cons.rda")
load("./data/elec_cons_doc.rda")
# ng_cons
load("./data/ng_cons.rda")
load("./data/ng_cons_doc.rda")

# regener
load("./data/regener_cons_ae_use.rda")
load("./data/regener_cons_ae_aff.rda")
load("./data/regener_needs.rda")
load("./data/regener_misc.rda")
load("./data/regener_doc.rda")
# subsidies
load("./data/subsidies_by_building.rda")
load("./data/subsidies_by_measure.rda")
load("./data/subsidies_doc.rda")

## mobility data ----
load("./data/part_voit_elec.rda")
load("./data/qualite_desserte.rda")
load("./data/taux_motorisation.rda")

## adaptation data ----
load("./data/surface_canopee.rda")
load("./data/batiment_danger.rda")

## glossary ----
load("data/glossary.rda")

# Group dataset objects ----

# energy
energy_datasets_objects <- c("elec_prod",
                             "elec_cons",
                             "ng_cons",
                             "regener_needs",
                             "regener_cons_ae_use",
                             "regener_cons_ae_aff",
                             "regener_misc",
                             "subsidies_by_measure",
                             "subsidies_by_building")
# adaptation
adaptation_datasets_objects <- c("surface_canopee",
                                 "batiment_danger")

# mobility
mobility_datasets_objects <- c("part_voit_elec",
                               "taux_motorisation",
                               "qualite_desserte")

# Create datasets groups

## energy
energy_datasets <- setNames(mget(energy_datasets_objects), energy_datasets_objects)

## mobility
mobility_datasets <- setNames(mget(mobility_datasets_objects), mobility_datasets_objects)

## adaptation
adaptation_datasets <- setNames(mget(adaptation_datasets_objects), adaptation_datasets_objects)

# Dictionnary tables if provided in .rda ! ----
# Reminder : this is not necessary but provides a dictionnary table if available to mod_about_the_app.R
doc_objects <- c("elec_prod_doc",
                 "elec_cons_doc",
                 "ng_cons_doc",
                 "regener_doc",
                 "subsidies_doc")

doc_datasets <- setNames(mget(doc_objects), doc_objects)

## doc panels for accordions ----
# see fct_helpers.R

# energy
elec_prod_doc_panels <- generate_doc_accordion_panels(md_file = "./data-doc/elec_prod-doc.md")
elec_cons_doc_panels <- generate_doc_accordion_panels(md_file = "./data-doc/elec_cons-doc.md")
regener_doc_panels <- generate_doc_accordion_panels(md_file = "./data-doc/regener-doc.md")
subsidies_doc_panels <- generate_doc_accordion_panels(md_file = "./data-doc/subsidies-doc.md")
ng_cons_doc_panels <- generate_doc_accordion_panels(md_file = "./data-doc/ng_cons-doc.md")

# mobility
part_voit_elec_doc_panels <- generate_doc_accordion_panels(md_file = "./data-doc/part_voit_elec-doc.md")
qualite_desserte_doc_panels <- generate_doc_accordion_panels(md_file = "./data-doc/qualite_desserte-doc.md")
taux_motorisation_doc_panels <-  generate_doc_accordion_panels(md_file = "./data-doc/taux_motorisation-doc.md")


# adaptation
surface_canopee_doc_panels <- generate_doc_accordion_panels(md_file = "./data-doc/surface_canopee-doc.md")
batiment_danger_doc_panels <- generate_doc_accordion_panels(md_file = "./data-doc/batiment_danger-doc.md")


# REDIRECTIONS ----

# This is used by app_server.R/ and methodological accordions in specific modules to redirect user in 'mod_about_the_app.R' module
# This is also ulsed by mod_table_content.R to redirect users towards tabpanels

subpanels_tribble <- dplyr::tribble(
  # <about_*> = item names in mod_about_the_app.R   //  <nav_*> = item names in app_ui.R
  ~observe_input, ~about_nav_panel, ~navset_id,  ~about_tabpanel_name, ~data_id, ~nav_panel, ~navset_name, ~nav_name,
  # energy (data_1-20)
  "consumption_charts-elec_data_help","Energie","navset_energie", "Distribution d'électricité", "data_1", "Electricité", "navset_elec", "Distribution d'électricité",
  "production_charts-elec_data_help", "Energie","navset_energie", "Production d'électricité", "data_2", "Electricité", "navset_elec", "Production d'électricité",
  "regener_needs-rgr_needs_help", "Energie","navset_energie", "Chaleur bâtiments", "data_3", "Chaleur des bâtiments","navset_regener", "Besoins des bâtiments",
  "regener_cons-rgr_cons_help", "Energie", "navset_energie","Chaleur bâtiments", "data_4", "Chaleur des bâtiments","navset_regener", "Consommation des bâtiments",
  "regener_misc-rgr_misc_help", "Energie","navset_energie", "Chaleur bâtiments", "data_5", "Chaleur des bâtiments","navset_regener", "Informations bâtiments",
  "subsidies_building-subsidies_building_help", "Energie","navset_energie", "Subventions bâtiments", "data_6", "Subventions bâtiments","navset_subsidies", "Vue du parc subventionné",
  "subsidies_measure-subsidies_measure_help", "Energie", "navset_energie", "Subventions bâtiments", "data_7", "Subventions bâtiments","navset_subsidies", "Vue par subventions",
  "ng_cons_charts-ng_cons_help", "Energie", "navset_energie", "Distribution de gaz naturel", "data_8", "Gaz naturel", "navset_ng", "Distribution de gaz naturel",

  # adaptation (data_20-29)
  "adaptation_canopy-generic_data_help", "Adaptation climat", "navset_climat", "Surface de canopée urbaine", "data_20", "Surface de canopée urbaine", "navset_canopy", "Surface de canopée urbaine",
  "buildings_exposure_hazards-generic_data_help", "Adaptation climat", "navset_climat", "Bâtiments exposés à des dangers naturels", "data_21", "Exposition aux dangers naturels", "navset_natural_hazards", "Bâtiments exposés à des dangers naturels",

  # mobility (data_30-39)
  "part_voit_elec-generic_data_help", "Mobilité", "navset_mobilite", "Part des voitures électriques", "data_30", "Véhicules électriques", "navset_vehicules", "Part des voitures électriques",
  "taux_motorisation-generic_data_help", "Mobilité", "navset_mobilite", "Taux de motorisation", "data_31", "Taux de motorisation", "navset_taux_motorisation", "Taux de motorisation",
  "qualite_desserte-generic_data_help", "Mobilité", "navset_mobilite", "Qualité de desserte des transports publics", "data_32", "Transports publics", "navset_qualite_desserte", "Qualité de desserte des transports publics"

  )


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

# all datasets must come as 'kWh' values so we index based on kWh
energy_units_table <- dplyr::tribble(
  ~unit, ~factor,
  "kWh", 1,
  "MWh", 1e3,
  "GWh", 1e6,
  "TJ", 1/3.6*1e6
)

# all datasets must come as 'tCO2' values so we index based on tCO2
co2_units_table <- dplyr::tribble(
  ~unit, ~factor,
  "kgCO2-eq", 1e-3,
  "tCO2-eq", 1,
  "ktCO2-eq", 1e3
)

## Column replacement ----
# load csv file of replacement columns for tables & downloads in ./inst/extdata/
# used by rename_columns_output()
colnames_replacement_display <- read.delim(file = "inst/extdata/colnames_replacement_display.csv",
                                           header = TRUE,
                                           sep = ";",
                                           encoding = "UTF-8") |>
  tidyr::as_tibble() |>
  dplyr::distinct(colname, replacement) |>
  dplyr::arrange(desc(nchar(colname))) # do this so that longer names match first, avoid partial undesired matches (e.g. nombre_batiments & nombre)


## List of non-ASCII words that should replace internal colnames
# Note that colnames are already in a 'sentence' format (i.e. Ae and not ae)
# They are used in `rename_fr_colnames()` for a nicer print/export of data tables
replace_fr_accents <- c("electricite" = "électricité",
                        "electrique" = "électrique",
                        "energetique" = "énergétique",
                        "Categorie" = "Catégorie",
                        "installee" = "installée",
                        "Annee" = "Année",
                        "Ae" = "Agent énergétique",
                        "optimises" = "optimisés",
                        "Detail" = "Détail",
                        "canopee" = "canopée")

## These are used to dynamically target columns renaming in add_colname_units() and mod_elec_charts.R
energy_col_keywords <- c("Consommation", "Production", "Injection", "Autoconsommation", "Besoins")

power_col_keywords <- c("Puissance", "installé")
power_col_keywords_dev <- c("puissance", "installe")

co2_keywords <- c("CO2")
percent_keywords <- c("Pct", "Part", "Taux")


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

# Prepare palette for create_bar_plotly()
subsidies_measure_simplifiee_colors <- subsidies_measure_palette_plot$color |>
  setNames(nm = subsidies_measure_palette_plot$mesure_simplifiee)


# This palette is only supplying colors for the table. The p
subsidies_measure_palette_table <- energy_datasets$subsidies_by_measure |>
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

### Canopee palette ----

surface_canopee_palette <- c("avec canopée >3m" = "limegreen",
                              "sans canopée >3m" = "grey75")

### Natural hazards palette ----

batiment_danger_palette <- c("danger moyen" = "#6f95ff",
                              "danger élevé" = "#ff4d4d")


### Qualite desserte palette ----

qualite_desserte_palette <- c("Population" = "#ab96c3",
                              "Emploi" = "#75b364")


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

categories_diren <- energy_datasets$elec_prod |>
  dplyr::distinct(categorie) |>
  dplyr::arrange(categorie) |>
  dplyr::pull()

## Objects specific 'Consommation électricité'  ----

# to be populated

## Objects specific 'Chaleur bâtiments'  ----
# RegEner specificity (see mod_inputs.R, app_server.R)

min_regener_year <- min(energy_datasets$regener_cons_ae_use$etat)
max_regener_year <- max(energy_datasets$regener_cons_ae_use$etat)

regener_current_year <- max_regener_year

# years for make_slider_input_years (fct_helpers.R + mod_inputs.R) ----

## Energy datasets
elec_cons_years <- c(
  min(energy_datasets$elec_cons$annee),
  max(energy_datasets$elec_cons$annee)
)

elec_prod_years <- c(
  min(energy_datasets$elec_prod$annee),
  max(energy_datasets$elec_prod$annee)
)

ng_cons_years <- c(
  min(energy_datasets$ng_cons$annee),
  max(energy_datasets$ng_cons$annee)
)

subsidies_years <- c(
  min(energy_datasets$subsidies_by_building$etat),
  max(energy_datasets$subsidies_by_building$etat)
)

## Mobility datasets ----
part_voit_elec_years <- c(
  min(mobility_datasets$part_voit_elec$annee),
  max(mobility_datasets$part_voit_elec$annee)
)

taux_motorisation_years <- c(
  min(mobility_datasets$taux_motorisation$annee),
  max(mobility_datasets$taux_motorisation$annee)
)

qualite_desserte_years <- c(
  min(mobility_datasets$qualite_desserte$annee),
  max(mobility_datasets$qualite_desserte$annee)
)

## Climate datasets ----
surface_canopee_years <- c(
  min(adaptation_datasets$surface_canopee$annee),
  max(adaptation_datasets$surface_canopee$annee)
)

batiment_danger_years <- c(
  min(adaptation_datasets$batiment_danger$annee),
  max(adaptation_datasets$batiment_danger$annee)
)








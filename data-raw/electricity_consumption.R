## code to prepare `electricity_consumption` dataset goes here

library(readxl)
library(tidyverse)
library(janitor)

## Preparing electricity production data, FIRST sheet ----

filepath <- "./data-raw/electricity_consumption_raw/31-Analyses complementaires_CLIENTS_communes.xlsx"

elec_cons <- readxl::read_excel(filepath,
                                sheet = 1,
                                guess_max = 1e5) %>%  # to avoid warnings if reading many NAs first
  janitor::clean_names(case = "snake")

# Clean up the names for a more generic use
elec_cons <- elec_cons %>%
  rename(commune = gwr_gdename,
         secteur = client_cat3_trad,
         code_secteur = client_type_cat3,
         consommation_kwh = gde_client_tot) %>%
# Add a year var ; this should be inherent to the raw data but is not yet
  mutate(annee = 2020)



## Saving in './data/' subfolder as '.rda' objects

usethis::use_data(elec_cons, overwrite = TRUE)

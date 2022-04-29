## code to prepare `electricity_production` dataset goes here

library(readxl)
library(tidyverse)
library(janitor)

## Preparing electricity production data, FIRST sheet ----

elec_prod <- readxl::read_excel("./data-raw/electricity_production_raw/anonymised_meta_pronovo.xlsx",
                           sheet = 1,       # 2nd sheet contains variable description
                           guess_max = 1e5) %>%  # to avoid warnings when reading many NAs first
  janitor::clean_names(case = "snake")

## Preparing variable documentation, SECOND sheet ----

elec_prod_doc <- read_excel("./data-raw/electricity_production_raw/anonymised_meta_pronovo.xlsx",
                          sheet = 2) %>%
  mutate(variable_snake = janitor::make_clean_names(Variable)) %>% # make_clean_names -> ok for vectors
  relocate(variable_snake, .after = Variable)

## Saving in './data/' subfolder as '.rda' objects

usethis::use_data(elec_prod, overwrite = TRUE)
usethis::use_data(elec_prod_doc, overwrite = TRUE)

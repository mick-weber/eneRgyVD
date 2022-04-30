# This script simply reads the anonymised pronovo data, cleans its variable names and outputs it in `/data` as .rda

## Librairies ----

library(readxl)
library(tidyverse)
library(snakecase)

## Preparing `pronovo_data` ----


pronovo_data <- read_excel("./data-raw/pronovo-vd/anonymised_meta_pronovo_2021.xlsx",
                      sheet = 1,       # 2nd sheet contains variable description
                      guess_max = 1e5) %>%  # to avoid warnings when reading many NAs first
  janitor::clean_names(case = "snake")
  # we simply add three sub-prod columns into one, we are not interested in the production type itself

## Preparing variable documentation ----

pronovo_doc <- read_excel("./data-raw/pronovo-vd/anonymised_meta_pronovo_2021.xlsx",
                           sheet = 2) %>% 
  mutate(variable_snake = make_clean_names(Variable)) %>% # make_clean_names -> ok for vectors
  relocate(variable_snake, .after = Variable)


# convert to separate .Rdata objects which will be saved in /data

pronovo_data %>%
  saveRDS(file = "./data/pronovo_data.rda")

pronovo_doc %>% 
  saveRDS(file = "./data/pronovo_doc.rda")

# in a golem repo we'll use :    usethis::use_data(x, overwrite = T)
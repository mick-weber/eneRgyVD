## code to prepare `limits_canton` dataset goes here

# read libraries

library(sf)

# read the data ----
# CRS is CH1903+ originally
sf_communes <- sf::read_sf(".\\data-raw\\limits_canton_raw\\MN95_OIT_TPR_LAD_GEN_COMMUNE.shp")
sf_districts <- sf::read_sf(".\\data-raw\\limits_canton_raw\\MN95_OIT_TPR_LAD_GEN_DISTRICT.shp")
sf_lacs <- sf::read_sf(".\\data-raw\\limits_canton_raw\\MN95_OIT_TPR_LAD_GEN_LAC.shp")

# reproject in WGS84 ('EPSG:4326') since it's best supported by leaflet
sf_communes <- sf_communes %>% sf::st_transform(crs = 'EPSG:4326')
sf_districts <- sf_districts %>% sf::st_transform(crs = 'EPSG:4326')
sf_lacs <- sf_lacs %>% sf::st_transform(crs = 'EPSG:4326')

# send data back as .Rda in ./data

usethis::use_data(sf_communes, overwrite = TRUE)
usethis::use_data(sf_districts, overwrite = TRUE)
usethis::use_data(sf_lacs, overwrite = TRUE)



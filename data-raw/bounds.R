# Description: Loads boundary data to be used for everything else
# Author:      Jonas Lieth
# R version:   R version 4.2.1 (2022-06-23 ucrt)
# OS:          Windows 10 x64 (build 22621)
# Requirements: 
#   - GRETA multinational survey results in ~/Datasets delivery
#   - Boundary data created by bounds.R
# Packages:
#   - giscoR 0.3.3
#   - sf 1.0-9
#   - dplyr 2.3.1
#   - httr2 0.2.2

library(dplyr)
library(giscoR)
library(sf)
library(httr2)

countries <- c(
  "AT", "BE", "CZ", "DK", "FI", "FR", "DE", "EL", "HU", "IE",
  "IT", "NL", "PL", "PT", "RO", "ES"
)

# EU regional boundaries ----
# NUTS, LAU and COM

# Retrieve NUTS boundaries from GISCO
nuts0 <- giscoR::gisco_get_nuts(
  year = "2021",
  nuts_level = "0",
  country = countries,
  epsg = "3035",
  resolution = "10",
  spatialtype = "RG",
  cache = TRUE
) %>%
  select(nid = NUTS_ID, name = NUTS_NAME) %>%
  st_transform(3035)

nuts1 <- giscoR::gisco_get_nuts(
  year = "2021",
  nuts_level = "1",
  country = countries,
  epsg = "3035",
  resolution = "10",
  spatialtype = "RG",
  cache = TRUE
) %>%
  select(nid = NUTS_ID, name = NUTS_NAME, code = CNTR_CODE) %>%
  st_transform(3035)

nuts2 <- giscoR::gisco_get_nuts(
  year = "2021",
  nuts_level = "2",
  country = countries,
  epsg = "3035",
  resolution = "03",
  spatialtype = "RG",
  cache = TRUE
) %>%
  select(nid = NUTS_ID, name = NUTS_NAME, code = CNTR_CODE) %>%
  st_transform(3035)

grid <- giscoR::gisco_get_grid(resolution = "100") %>%
  select(gid = GRD_ID) %>%
  st_transform(3035)

# Retrieve local regions corresponding to C3 level
lau <- sf::read_sf(
  "https://gisco-services.ec.europa.eu/distribution/v2/lau/geojson/LAU_RG_01M_2021_3035.geojson",
  quiet = TRUE
) %>%
  select(nid = LAU_ID, name = LAU_NAME, code = CNTR_CODE) %>%
  filter(code %in% countries) %>%
  st_transform(3035)

com <- sf::read_sf(
  "https://gisco-services.ec.europa.eu/distribution/v2/communes/geojson/COMM_RG_01M_2016_3035.geojson",
  quiet = TRUE
) %>%
  select(nid = COMM_ID, name = COMM_NAME, code = CNTR_CODE) %>%
  filter(code %in% countries) %>%
  st_transform(3035)


output <- list("nuts0", "nuts1", "nuts2", "grid")

for (x in c(output, "nuts3", "lau", "com")) {
  saveRDS(eval(as.symbol(x)), paste0("bounds/", x, ".rds"))
}

# st_write(nuts0, dsn = "bounds.sqlite", layer = "nuts0", append = FALSE)
# st_write(nuts1, dsn = "bounds.sqlite", layer = "nuts1", append = FALSE)
# st_write(nuts2, dsn = "bounds.sqlite", layer = "nuts2", append = FALSE)
# st_write(bgn_1, dsn = "bounds.sqlite", layer = "bgn_1", append = FALSE)
# st_write(bgn_2, dsn = "bounds.sqlite", layer = "bgn_2", append = FALSE)
# st_write(bgn_3, dsn = "bounds.sqlite", layer = "bgn_3", append = FALSE)
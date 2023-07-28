# Description: Loads boundary data to be used for everything else
# Author:      Jonas Lieth
# R version:   R version 4.2.1 (2022-06-23 ucrt)
# OS:          Windows 10 x64 (build 22621)
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
cat("Downloading NUTS0...\n")
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

cat("Downloading NUTS1...\n")
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

cat("Downloading NUTS2...\n")
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

cat("Downloading INSPIRE grid...\n")
grid <- giscoR::gisco_get_grid(resolution = "100") %>%
  select(gid = GRD_ID) %>%
  st_transform(3035)

# Retrieve local regions corresponding to C3 level
cat("Downloading LAU...\n")
lau <- giscoR::gisco_get_lau(
  year = "2020",
  epsg = "3035",
  cache = TRUE,
  country = countries
) %>%
  select(lid = LAU_ID, name = LAU_NAME, code = CNTR_CODE, geometry = `_ogr_geometry_`)

cat("Downloading communes...\n")
com <- giscoR::gisco_get_communes(
  year = "2016",
  epsg = "3035",
  cache = TRUE,
  country = countries
) %>%
  select(cid = COMM_ID, name = COMM_NAME, code = CNTR_CODE, geometry = `_ogr_geometry_`)


output <- list("nuts0", "nuts1", "nuts2", "grid")

if (!dir.exists("data-ext/bounds")) {
  dir.create("data-ext/bounds", recursive = TRUE)
}

cat("Saving output...\n")
for (x in c(output, "lau", "com")) {
  saveRDS(eval(as.symbol(x)), paste0("data-ext/bounds/", x, ".rds"))
}

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
  select(nid = NUTS_ID, name = NUTS_NAME)

nuts1 <- giscoR::gisco_get_nuts(
  year = "2021",
  nuts_level = "1",
  country = countries,
  epsg = "3035",
  resolution = "10",
  spatialtype = "RG",
  cache = TRUE
) %>%
  select(nid = NUTS_ID, name = NUTS_NAME, code = CNTR_CODE)

nuts2 <- giscoR::gisco_get_nuts(
  year = "2021",
  nuts_level = "2",
  country = countries,
  epsg = "3035",
  resolution = "03",
  spatialtype = "RG",
  cache = TRUE
) %>%
  select(nid = NUTS_ID, name = NUTS_NAME, code = CNTR_CODE)

nuts3 <- giscoR::gisco_get_nuts(
  year = "2021",
  nuts_level = "3",
  country = countries,
  epsg = "3035",
  resolution = "03",
  spatialtype = "RG",
  cache = TRUE
) %>%
  select(nid = NUTS_ID, name = NUTS_NAME, code = CNTR_CODE)

# Retrieve local regions corresponding to C3 level
lau <- sf::read_sf(
  "https://gisco-services.ec.europa.eu/distribution/v2/lau/geojson/LAU_RG_01M_2021_3035.geojson",
  quiet = TRUE
) %>%
  select(nid = LAU_ID, name = LAU_NAME, code = CNTR_CODE) %>%
  filter(code %in% countries)

com <- sf::read_sf(
  "https://gisco-services.ec.europa.eu/distribution/v2/communes/geojson/COMM_RG_01M_2016_3035.geojson",
  quiet = TRUE
) %>%
  select(nid = COMM_ID, name = COMM_NAME, code = CNTR_CODE) %>%
  filter(code %in% countries)


# Bologna boundaries ----
# Bologna administrative structure:
# 1. Quartieri - quarters
# 2. Zone - zones
# 3. Aree statistiche - statistical areas

bgn_query <- function(id) {
  blgn_url <- "https://opendata.comune.bologna.it/api/v2"
  httr2::request(blgn_url) %>%
    httr2::req_method("GET") %>%
    httr2::req_url_path_append("catalog/datasets") %>%
    httr2::req_url_path_append(id) %>%
    httr2::req_url_path_append("exports/geojson") %>%
    httr2::req_perform() %>%
    httr2::resp_body_string() %>%
    sf::read_sf(quiet = TRUE)
}

# Get quarters
bgn_1 <- bgn_query("quartieri-di-bologna") %>%
  select(code = cod_quar, name = quartiere)

# Get zones
bgn_2 <- bgn_query("zone-del-comune-di-bologna") %>%
  select(code = codzona, quarter = numquart, name = nomezona)

# Get statistical areas
bgn_3 <- bgn_query("aree-statistiche") %>%
  select(
    code = codice_area_statistica,
    name = area_statistica,
    zone = cod_zona,
    quarter = cod_quar
  )


# Donostia boundaries ----
# Donostia administrative structure:
# 1. Barrios - Neighborhoods
# 2. Unidades menores - Minor units
# 3. Secciones - Census sections

# Get neighborhoods
don_1 <- sf::read_sf(
  "https://www.donostia.eus/datosabiertos/recursos/mapa_auzoak/auzoak.json",
  quiet = TRUE
) %>%
  select(code = KodAuzo, name)
  

# Get minor units
don_2 <- sf::read_sf(
  "https://www.donostia.eus/datosabiertos/recursos/mapa_unidades_menores/unitatetxikiak.json",
  quiet = TRUE
) %>%
  select(code = KodUTxiki, name = IzenUTxiki, neighborhood = KodAuzo)


# Get census sections
download.file(
  "https://www.donostia.eus/ide/ADMINISTRAZIO_MUGAK-LIMITES_ADMINISTRATIVOS/shp/Sekzio.zip",
  .temp <- tempfile(fileext = ".zip"),
  quiet = TRUE
)
unzip(.temp, exdir = tempdir())
don_3 <- sf::read_sf(file.path(tempdir(), "Sekzio.shp"), quiet = TRUE)




# Darmstadt boundaries ----
# No open data portal yet
# OSM might work as an alternative if CS3 is going to be implemented


output <- list(
  "nuts0", "nuts1", "nuts2", #"nuts3",
  "bgn_1", "bgn_2", "bgn_3"#,
  #"don_1", "don_2", "don_3"
)

st_write(nuts0, dsn = "bounds.sqlite", layer = "nuts0", append = FALSE)
st_write(nuts1, dsn = "bounds.sqlite", layer = "nuts1", append = FALSE)
st_write(nuts2, dsn = "bounds.sqlite", layer = "nuts2", append = FALSE)
st_write(bgn_1, dsn = "bounds.sqlite", layer = "bgn_1", append = FALSE)
st_write(bgn_2, dsn = "bounds.sqlite", layer = "bgn_2", append = FALSE)
st_write(bgn_3, dsn = "bounds.sqlite", layer = "bgn_3", append = FALSE)
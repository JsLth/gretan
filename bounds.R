library(sf)
library(httr2)
library(dplyr)
library(giscoR)

countries <- c(
  "Austria", "Belgium", "Czechia", "Denmark", "Finland", "France", "Germany",
  "Greece", "Hungary", "Ireland", "Italy", "Netherlands", "Poland", "Portugal",
  "Romania", "Spain"
) %>%
  countrycode(origin = "country.name", destination = "eurostat")

dir.create("data/bounds", recursive = TRUE, showWarnings = FALSE)

# EU regional boundaries ----
# NUTS, LAU and COM

# Retrieve NUTS boundaries from GISCO
gisco_get_nuts(
  year = "2021",
  nuts_level = "0",
  country = countries,
  epsg = "3035",
  resolution = "10",
  spatialtype = "RG",
  cache = TRUE
) %>%
  select(nid = NUTS_ID, name = NUTS_NAME) %>%
  saveRDS(file = "data/bounds/nuts0.rds")
gisco_get_nuts(
  year = "2021",
  nuts_level = "1",
  country = countries,
  epsg = "3035",
  resolution = "10",
  spatialtype = "RG",
  cache = TRUE
) %>%
  select(nid = NUTS_ID, name = NUTS_NAME, code = CNTR_CODE) %>%
  saveRDS(file = "data/bounds/nuts1.rds")
gisco_get_nuts(
  year = "2021",
  nuts_level = "2",
  country = countries,
  epsg = "3035",
  resolution = "03",
  spatialtype = "RG",
  cache = TRUE
) %>%
  select(nid = NUTS_ID, name = NUTS_NAME, code = CNTR_CODE) %>%
  saveRDS(file = "data/bounds/nuts2.rds")
gisco_get_nuts(
  year = "2021",
  nuts_level = "3",
  country = countries,
  epsg = "3035",
  resolution = "03",
  spatialtype = "RG",
  cache = TRUE
) %>%
  select(nid = NUTS_ID, name = NUTS_NAME, code = CNTR_CODE) %>%
  saveRDS(file = "data/bounds/nuts3.rds")

# Retrieve local regions corresponding to C3 level
read_sf(
  "https://gisco-services.ec.europa.eu/distribution/v2/lau/geojson/LAU_RG_01M_2021_3035.geojson",
  quiet = TRUE
) %>%
  select(nid = LAU_ID, name = LAU_NAME, code = CNTR_CODE) %>%
  filter(code %in% countries) %>%
  saveRDS(file = "data/bounds/lau.rds")
read_sf(
  "https://gisco-services.ec.europa.eu/distribution/v2/communes/geojson/COMM_RG_01M_2016_3035.geojson",
  quiet = TRUE
) %>%
  select(nid = COMM_ID, name = COMM_NAME, code = CNTR_CODE) %>%
  filter(code %in% countries) %>%
  saveRDS(file = "data/bounds/com.rds")

# Bologna boundaries ----
# Bologna administrative structure:
# 1. Quartieri - quarters
# 2. Zone - zones
# 3. Aree statistiche - statistical areas

bgn_query <- function(id) {
  blgn_url <- "https://opendata.comune.bologna.it/api/v2"
  request(blgn_url) %>%
    req_method("GET") %>%
    req_url_path_append("catalog/datasets") %>%
    req_url_path_append(id) %>%
    req_url_path_append("exports/geojson") %>%
    req_perform() %>%
    resp_body_string() %>%
    read_sf(quiet = TRUE)
}

# Get quarters
bgn_query("quartieri-di-bologna") %>%
  select(code = cod_quar, name = quartiere) %>%
  saveRDS("data/bounds/bgn_1.rds")

# Get zones
bgn_query("zone-del-comune-di-bologna") %>%
  select(code = codzona, quarter = numquart, name = nomezona) %>%
  saveRDS("data/bounds/bgn_2.rds")

# Get statistical areas
bgn_query("aree-statistiche") %>%
  select(
    code = codice_area_statistica,
    name = area_statistica,
    zone = cod_zona,
    quarter = cod_quar
  ) %>%
  saveRDS("data/bounds/bgn_3.rds")


# Donostia boundaries ----
# Donostia administrative structure:
# 1. Barrios - Neighborhoods
# 2. Unidades menores - Minor units
# 3. Secciones - Census sections

# Get neighborhoods
read_sf(
  "https://www.donostia.eus/datosabiertos/recursos/mapa_auzoak/auzoak.json",
  quiet = TRUE
) %>%
  select(code = KodAuzo, name) %>%
  saveRDS("data/bounds/don_1.rds")
  

# Get minor units
read_sf(
  "https://www.donostia.eus/datosabiertos/recursos/mapa_unidades_menores/unitatetxikiak.json",
  quiet = TRUE
) %>%
  select(code = KodUTxiki, name = IzenUTxiki, neighborhood = KodAuzo) %>%
  saveRDS("data/bounds/don_2.rds")

# Get census sections
download.file(
  "https://www.donostia.eus/ide/ADMINISTRAZIO_MUGAK-LIMITES_ADMINISTRATIVOS/shp/Sekzio.zip",
  .temp <- tempfile(fileext = ".zip")
)
unzip(.temp, exdir = tempdir())
read_sf(file.path(tempdir(), "Sekzio.shp"), quiet = TRUE) %>%
  saveRDS("data/bounds/don_3.rds")



# Darmstadt boundaries ----
# No open data portal yet
# OSM might work as an alternative if CS3 is going to be implemented

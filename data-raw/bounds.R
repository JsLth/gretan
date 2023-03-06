library(dplyr)

countries <- c(
  "AT", "BE", "CZ", "DK", "FI", "FR", "DE", "EL", "HU", "IE",
  "IT", "NL", "PL", "PT", "RO", "ES"
)

dir.create("data/bounds", recursive = TRUE, showWarnings = FALSE)

# EU regional boundaries ----
# NUTS, LAU and COM

# Retrieve NUTS boundaries from GISCO
giscoR::gisco_get_nuts(
  year = "2021",
  nuts_level = "0",
  country = countries,
  epsg = "3035",
  resolution = "10",
  spatialtype = "RG",
  cache = TRUE
) %>%
  select(nid = NUTS_ID, name = NUTS_NAME) %>%
  saveRDS(file = "data/nuts0.rds")
giscoR::gisco_get_nuts(
  year = "2021",
  nuts_level = "1",
  country = countries,
  epsg = "3035",
  resolution = "10",
  spatialtype = "RG",
  cache = TRUE
) %>%
  select(nid = NUTS_ID, name = NUTS_NAME, code = CNTR_CODE) %>%
  saveRDS(file = "data/nuts1.rds")
giscoR::gisco_get_nuts(
  year = "2021",
  nuts_level = "2",
  country = countries,
  epsg = "3035",
  resolution = "03",
  spatialtype = "RG",
  cache = TRUE
) %>%
  select(nid = NUTS_ID, name = NUTS_NAME, code = CNTR_CODE) %>%
  saveRDS(file = "data/nuts2.rds")
giscoR::gisco_get_nuts(
  year = "2021",
  nuts_level = "3",
  country = countries,
  epsg = "3035",
  resolution = "03",
  spatialtype = "RG",
  cache = TRUE
) %>%
  select(nid = NUTS_ID, name = NUTS_NAME, code = CNTR_CODE) %>%
  saveRDS(file = "data/nuts3.rds")

# Retrieve local regions corresponding to C3 level
sf::read_sf(
  "https://gisco-services.ec.europa.eu/distribution/v2/lau/geojson/LAU_RG_01M_2021_3035.geojson",
  quiet = TRUE
) %>%
  select(nid = LAU_ID, name = LAU_NAME, code = CNTR_CODE) %>%
  filter(code %in% countries) %>%
  saveRDS(file = "data/lau.rds")
sf::read_sf(
  "https://gisco-services.ec.europa.eu/distribution/v2/communes/geojson/COMM_RG_01M_2016_3035.geojson",
  quiet = TRUE
) %>%
  select(nid = COMM_ID, name = COMM_NAME, code = CNTR_CODE) %>%
  filter(code %in% countries) %>%
  saveRDS(file = "data/com.rds")

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
bgn_query("quartieri-di-bologna") %>%
  select(code = cod_quar, name = quartiere) %>%
  saveRDS("data/bgn_1.rds")

# Get zones
bgn_query("zone-del-comune-di-bologna") %>%
  select(code = codzona, quarter = numquart, name = nomezona) %>%
  saveRDS("data/bgn_2.rds")

# Get statistical areas
bgn_query("aree-statistiche") %>%
  select(
    code = codice_area_statistica,
    name = area_statistica,
    zone = cod_zona,
    quarter = cod_quar
  ) %>%
  saveRDS("data/bgn_3.rds")


# Donostia boundaries ----
# Donostia administrative structure:
# 1. Barrios - Neighborhoods
# 2. Unidades menores - Minor units
# 3. Secciones - Census sections

# Get neighborhoods
sf::read_sf(
  "https://www.donostia.eus/datosabiertos/recursos/mapa_auzoak/auzoak.json",
  quiet = TRUE
) %>%
  select(code = KodAuzo, name) %>%
  saveRDS("data/don_1.rds")
  

# Get minor units
sf::read_sf(
  "https://www.donostia.eus/datosabiertos/recursos/mapa_unidades_menores/unitatetxikiak.json",
  quiet = TRUE
) %>%
  select(code = KodUTxiki, name = IzenUTxiki, neighborhood = KodAuzo) %>%
  saveRDS("data/don_2.rds")

# Get census sections
download.file(
  "https://www.donostia.eus/ide/ADMINISTRAZIO_MUGAK-LIMITES_ADMINISTRATIVOS/shp/Sekzio.zip",
  .temp <- tempfile(fileext = ".zip"),
  quiet = TRUE
)
unzip(.temp, exdir = tempdir())
sf::read_sf(file.path(tempdir(), "Sekzio.shp"), quiet = TRUE) %>%
  saveRDS("data/don_3.rds")



# Darmstadt boundaries ----
# No open data portal yet
# OSM might work as an alternative if CS3 is going to be implemented

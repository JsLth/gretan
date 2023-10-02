library(magrittr)
library(janitor)
library(dplyr)
library(sf)

db <- "inst/db/cs5spain.gpkg"

buildings <- read_sf("data-ext/ur_beroa/ResultLayer.gpkg") %>%
  clean_names() %>%
  select(building_id, substation, year_constr, number_of_dw, a_heat_dem_m2)

st_delete(dsn = db)
st_write(buildings, dsn = db, layer = "buildings")

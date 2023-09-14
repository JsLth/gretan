library(magrittr)
library(janitor)
library(dplyr)
library(stringr)
library(sf)

db <- "inst/db/cs1italy.gpkg"

fragility <- read_sf("data-ext/pilastro_roveri/fragility in Pilastro-Roveri.shp") %>%
  mutate(nomezona = str_to_title(nomezona)) %>%
  mutate(across(all_of(
    c(
      "pop_res", "rmpe_fam", "frag_demo",
      "frag_soc", "frag_econ", "frag_compl"
    )
  ), as.numeric))
buildings <- read_sf("data-ext/pilastro_roveri/Use.gpkg") %>%
  clean_names() %>%
  mutate(across(.cols = where(is.double), ~ round(.x, 2))) %>%
  mutate(property = if_else(is.na(property), "Private", property)) %>%
  select(
    building_id, use, year_constr, property, electricity_demand_m2,
    heating_demand_m2, installed_pv_capacity_k_w
  )

st_delete(dsn = db)
st_write(buildings, dsn = db, layer = "buildings")
st_write(fragility, dsn = db, layer = "fragility")

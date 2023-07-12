library(magrittr)

db <- "inst/db/cs1italy.gpkg"

fragility <- sf::read_sf("data-ext/pilastro_roveri/fragility in Pilastro-Roveri.shp") %>%
  dplyr::mutate(nomezona = stringr::str_to_title(nomezona)) %>%
  dplyr::mutate(across(all_of(
    c("pop_res", "rmpe_fam", "frag_demo",
      "frag_soc", "frag_econ", "frag_compl")
  ), as.numeric))
buildings <- sf::read_sf("data-ext/pilastro_roveri/Use.gpkg") %>%
  janitor::clean_names() %>%
  dplyr::mutate(dplyr::across(.cols = dplyr::where(is.double), ~round(.x, 2))) %>%
  dplyr::mutate(property = dplyr::if_else(is.na(property), "Private", property)) %>%
  dplyr::select(building_id, use, year_constr, property, electricity_demand_m2,
                heating_demand_m2, installed_pv_capacity_k_w)

sf::st_delete(dsn = db)
sf::st_write(buildings, dsn = db, layer = "buildings")
sf::st_write(fragility, dsn = db, layer = "fragility")
library(magrittr)

db <- "inst/db/cs1italy.gpkg"

fragility <- sf::read_sf("data-ext/pilastro_roveri/fragility in Pilastro-Roveri.shp") %>%
  dplyr::mutate(nomezona = stringr::str_to_title(nomezona))
buildings <- sf::read_sf("data-ext/pilastro_roveri/Use.gpkg") %>%
  janitor::clean_names() %>%
  dplyr::mutate(dplyr::across(.cols = dplyr::where(is.double), ~round(.x, 2))) %>%
  dplyr::mutate(property = dplyr::if_else(is.na(property), "Private", property))

sf::st_delete(dsn = db)
sf::st_write(buildings, dsn = db, layer = "buildings")
sf::st_write(fragility, dsn = db, layer = "fragility")
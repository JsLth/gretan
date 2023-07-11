cs_data <- sf::read_sf("data-ext/pilastro_roveri/Use.gpkg") %>%
  janitor::clean_names()

sf::st_write(cs_data, dsn = "inst/db/cs1italy.gpkg", layer = "buildings")
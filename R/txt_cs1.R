txts$cs1$title <- "Pilastro-Roveri, Italy"
txts$cs1$icon <- icon("map-pin")
txts$cs1$tags <- c(
  "case", "study", "studies", "bologna", "pilastro", "roveri", "dummy",
  "example"
)

cs1coords <- sf::st_sf(
  name = c("a"),
  geometry = sf::st_sfc(
    sf::st_point(c(11.399926, 44.507145))
  ), crs = 4326
)

txts$cs1$poi <- list(
  a = HTML(paste(
    p("You selected a marker on the map. This marker could be a point of
      interest relating to the case study and this text could be a text that
      explains or is otherwise relevant to that point of interest.")
  )),
  none = HTML(paste(
    p("Click on a map marker to learn more about the places of this case study.")
  ))
)
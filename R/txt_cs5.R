txts$cs5$shortitle <- "UR Beroa, Spain"
txts$cs5$title <- "Case study: UR BEROA \u2013 energy efficiency-driven cooperative"
txts$cs5$date <- "2023-mm-dd"
txts$cs5$icon <- icon("map-pin")
txts$cs5$tags <- c(
  "case", "study", "studies", "UR Beroa", "San Sebastian", "Basque", "Spain",
  "cooperative", "model", "substation", "demand", "building"
)
txts$cs5$affil <- list(
  "Nekane Hermoso Martinez" = "Tecnalia Research & Innovation",
  "Izaskun Jimenez Iturriza" = "Tecnalia Research & Innovation"
)

txts$cs5$dict <- list(
  buildings = list(
    substation = list(title = "Substation", lab = ""),
    year_constr = list(title = "Construction year", lab = ""),
    number_of_dw = list(title = "Number of dwellings", lab = ""),
    a_heat_dem_m2 = list(title = "Heating demand", lab = " kWh/m\u00b2")
  )
)

txts$cs5$buildings_info <- paste(
  "This map presents the energy models for buildings in",
  "Bera Bera. Hover over individual buildings to learn more",
  "about their energy-related properties. Using the controls on",
  "the right side of the map you can also switch between layers"
)
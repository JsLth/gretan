txts$main$cs5 <- list(
  shortitle = "UR Beroa, Spain",
  title = "Case study: UR BEROA \u2013 energy efficiency-driven cooperative",
  date = "2023-mm-dd",
  icon = "map-pin",
  tags = c(
    "case", "study", "studies", "UR Beroa", "San Sebastian", "Basque", "Spain",
    "cooperative", "model", "substation", "demand", "building"
  ),
  affil = list(
    "Nekane Hermoso Martinez" = "Tecnalia Research & Innovation",
    "Izaskun Jimenez Iturriza" = "Tecnalia Research & Innovation"
  ),
  dict = list(
    buildings = list(
      substation = list(title = "Substation", lab = ""),
      year_constr = list(title = "Construction year", lab = ""),
      number_of_dw = list(title = "Number of dwellings", lab = ""),
      a_heat_dem_m2 = list(title = "Heating demand", lab = " kWh/m\u00b2")
    )
  ),
  energy_model = list(
    title = "Energy modelling",
    content = shinipsum::random_text(nwords = 250)
  ),
  case_study = list(
    title = "Case study",
    content = shinipsum::random_text(nwords = 250)
  ),
  buildings_info = "This map presents the energy models for buildings in
    Bera Bera. Hover over individual buildings to learn more
    about their energy-related properties. Using the controls on
    the right side of the map you can also switch between layers"
)

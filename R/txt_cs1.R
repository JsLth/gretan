txts$cs1$title <- "Pilastro-Roveri, Italy"
txts$cs1$icon <- icon("map-pin")
txts$cs1$tags <- c(
  "case", "study", "studies", "bologna", "pilastro", "roveri", "dummy",
  "example"
)

txts$cs1$dict <- list(
  buildings = list(
    use = list(title = "Use", lab = ""),
    year_constr = list(title = "Construction year", lab = ""),
    property = list(title = "Property", lab = ""),
    electricity_demand_m2 = list(title = "Electricity demand", lab = " kWh/m\u00b2"),
    heating_demand_m2 = list(title = "Heating demand", lab = " kWh/m\u00b2"),
    installed_pv_capacity_k_w = list(title = "Installed PV capacity", lab = " kW")
  ),
  fragility = list(
    pop_res = list(title = "Population", lab = ""),
    var_5y_pop = list(title = "Population change (5yr)", lab = ""),
    saldonat = list(title = "\u00d8 Population growth", lab = " %"),
    ultra80 = list(title = "% over 80", lab = " %"),
    soli_65 = list(title = "% over 65 living alone", lab = " %"),
    imem_itali = list(title = "% Italian 20 – 64", lab = " %"),
    imem_stra = list(title = "% foreign 20 – 64", lab = " %"),
    stra_0_19 = list(title = "% foreign 0 – 19", lab = " %"),
    minori1gen = list(title = "% minors in single-parent families", lab = " %"),
    perc_laur = list(title = "% graduates 25 – 44", lab = " %"),
    perc_ab_no = list(title = "% unoccupied dwellings", lab = " %"),
    perfragsan = list(title = "% elderly in high health fragility", lab = " %"),
    peraffit = list(title = "% rented occupied dwellings", lab = " %"),
    rmpe_fam = list(title = "Median household income per capita", lab = " €"),
    perfam_r60 = list(title = "% family members with income <13k €/yr", lab = " %"),
    frag_demo = list(title = "Potential demographic fragility", lab = ""),
    frag_soc = list(title = "Potential social fragility", lab = ""),
    frag_econ = list(title = "Potential economic fragility", lab = ""),
    frag_compl = list(title = "Potential fragility index", lab = "")
  )
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
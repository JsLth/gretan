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
    imem_itali = list(title = "% Italian 20 \u2013 64", lab = " %"),
    imem_stra = list(title = "% foreign 20 \u2013 64", lab = " %"),
    stra_0_19 = list(title = "% foreign 0 \u2013 19", lab = " %"),
    minori1gen = list(title = "% minors in single-parent families", lab = " %"),
    perc_laur = list(title = "% graduates 25 \u2013 44", lab = " %"),
    perc_ab_no = list(title = "% unoccupied dwellings", lab = " %"),
    perfragsan = list(title = "% elderly in high health fragility", lab = " %"),
    peraffit = list(title = "% rented occupied dwellings", lab = " %"),
    rmpe_fam = list(title = "Median household income per capita", lab = " \u20ac"),
    perfam_r60 = list(title = "% family members with income <13k \u20ac/yr", lab = " %"),
    frag_demo = list(title = "Potential demographic fragility", lab = ""),
    frag_soc = list(title = "Potential social fragility", lab = ""),
    frag_econ = list(title = "Potential economic fragility", lab = ""),
    frag_compl = list(title = "Potential fragility index", lab = "")
  )
)

txts$cs1$energy_model <- tagList(
  p2("Energy modelling of cities is necessary in order to obtain an estimation of
  energy demand. This information is needed especially when actual energy
  consumption data is not available. By understanding the energy needs of a
  city, decision makers can develop strategies to reduce energy consumption
  and develop energy-efficient buildings and infrastructure. Energy modelling
  can be used to identify energy demand in different scenarios and develop
  energy-efficient strategies for reducing energy consumption, assess the
  potential for energy efficiency improvements and renewable energy sources
  and to analyse the impact of changes in energy demand on the environment
  and the economy. This will help to identify areas where energy savings can
  be made or may be more favourable to implement certain actions or strategies.
  In this way, energy modelling can also be used to identify the suitability of
  implementing energy policies and planning decisions in different locations."),
  p2("In the GRETA project, the ENERKAD\u00ae[1] tool has been used to generate the models.
  ENERKAD\u00ae is a plugin for QGIS which evaluates urban energy scenarios at
  building, district and city scale and calculates the thermal energy demand
  and consumption per hour for each building in a district, using cadastral
  data, basic cartography and climatic information of the study area."),
  p2("ENERKAD\u00ae foregoes the use of representative buildings and uses individual
  building information for its input data. It takes the basic mandatory
  parameters from the cadastre, which can be considered as its greatest
  advantage, since in many cases, detailed information of the buildings is not
  available and some of the information comes from different sources like energy
  performance certificates or municipal datasets."),
  p2("The use of GIS software facilitates the representation of results, so that it
  is possible to analyse the actual state of energy demand in the city in a
  visual way and identify the areas with the greatest potential for savings or
  implementation of planned interventions, based on the energy demand but also
  in other non-energy related parameters such as socio-economic data or
  behavioural aspects directly related to the citizens.")
)

txts$cs1$case_study <- tagList(
  p2("The case study of Pilastro-Roveri is oriented towards the analysis of PV
  generation as it has one of the largest rooftop PV plants in Europe in the
  industrial area. According to the information on existing PV installations
  reported in the GIS layers, there are currently 19,600 kWp installed on the
  roofs of buildings in the area, resulting in an estimated production of
  approximately 26 GWh/year."),
  p2("As the image shows, most of the installations are located in tertiary
  buildings, more specifically in industrial buildings. However, the largest
  installations of all are on the roof of a commercial and office buildings
  located next to eachother, which represents more than 50% of the installed
  capacity of the entire case study area. This building has practically its
  entire roof covered with PV panels, with an installed capacity of almost
  10 MW."),
  p2("The following graph shows electricity demand and production by type of
  building use. At this point it is important to note that industrial buildings
  have not been included in the analysis, so no results are available, nor real
  consumption data as these are private companies."),
  p2("The highest electricity demand comes from residential buildings, and that
  they produce the least energy, accounting for only 0.44% of the total
  installed power. Residential buildings account for 46% of the total floor
  area and only 9% of the roof area (excluding industrial buildings in both
  cases)."),
  tags$img(src = "www/Figure2.png", width = "100%"),
  p(
    tags$b("Figure 2"),
    "Electricity consumption vs. PV production by building use",
    style = "align: center;"
  ),
  p2("Based on the results of the energy model, only 60% of the current PV
  production would fully cover the electricity needs of residential buildings.
  However, as mentioned above, the consumption data of industrial buildings and
  the self-consumption that may take place in them are not available. Therefore,
  it has been estimated how much installed capacity would be needed in
  residential buildings to cover the electricity demand in these buildings.")
)

txts$cs1$desc <- list(
  use = HTML("The <b>Use</b>layer shows the main use of each building. 17 uses are displayed
    on the map, but only 10 of them are considered in the energy demand model:
    residential and commercial areas, education, hospital, hotel, single family
    house, office, public admin, restaurant, and sport. The rest of the uses are
    not related to energy demand."),
  year_constr = HTML("The <b>construction year</b> layer shows the construction year of each building. In
    this way it is possible to see the development of the city by zones in the
    different periods."),
  property = HTML("The <>property</b> layer shows the ownership type of the buildings."),
  electricity_demand_m2 = HTML("The <b>electricity demand</b> layer shows the electricity demand of the
    buildings calculated in the Enerkad Tool simulation. Some of the building
    uses (See \u201cUse\u201d layer description) are excluded from the
    simulation and show a 0 value."),
  heat_demand_m2 = HTML("The <b>heating demand</b> layer shows the heat demand of the buildings, which
    includes the needs for heating and domestic hot water, calculated in the
    Enerkad Tool simulation. Some of the building uses (See \u201cUse\u201d
    layer description) are excluded from the simulation and show a 0 value."),
  installed_pv_capacity_k_w = HTML("The <b>PV capacity</b> layer shows the installed PV capacity in
    each building if existing. Most of the existing installations are located
    in tertiary buildings")
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
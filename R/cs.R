txts$csdesc <- list(
  Italy = HTML(paste(
    h2("Renewable energy district Pilastro-Roveri"),
    p(HTML(paste(
      "The case study takes place in the Pilastro-Roveri district in",
      "the northeast of Bologna, where a financed Green Energy",
      "Community project (GECO) has been running since 2019 to",
      "support communities in the process of designing and creating",
      "a green energy community. Pilastro-Roveri is a",
      "socio-economically stratified district composed of two",
      "areas:"
    )), style = "margin-bottom: 0.5cm;"),
    p(HTML(paste(
      "<b>Pilastro “Rione”</b> (neighbourhood): a residential neighborhood",
      "with a long history of activism but also of socio-economic issues."
    )), style = "margin-bottom: 0.5cm;"),
    p(HTML(paste(
      "<b>Roveri</b> area: distinctly separated from Pilastro by a former",
      "railway terminal, is an industrial and productive neighborhood",
      "hosting a variety of companies in sectors such as packaging,",
      "mechanics, and electric vehicles. The area also hosts the",
      "Agriculture and Food Center of Bologna’s (CAAB) food and",
      "agriculture theme park (FICO) with its industrial partners, which",
      "has the largest solar power plant on industrial roofs within the EU."
    )), style = "margin-bottom: 0.5cm;")
  )),
  Portugal = HTML(paste(
    h2("Coopérnico – renewable energy-driven cooperative"),
    p(paste(
      "The case study examines Coopérnico, Portugal’s first renewable",
      "energy cooperative, founded in 2013. Coopérnico has more than",
      "1,700 members, including citizens, small and medium-sized",
      "enterprises, and municipalities all over Portugal. Its mission is",
      "to involve its members in reshaping the energy sector to be more",
      "renewable, socially just and collaborative."
    ))
  )),
  Germany = HTML(paste(
    h2("The Earnest App – a virtual community for sustainable mobility in Darmstadt"),
    p(paste(
      "The case study explores how the regular use of a sustainability",
      "app can foster energy citizenship among members of a virtual",
      "community. The case study is conducted in Darmstadt – a city with",
      "160.000 inhabitants located in the state of Hesse in Germany. In",
      "cooperation with students from the University for Applied Science",
      "in Darmstadt (h_da), the case study explores how a virtual energy",
      "community – connected by the shared experience of using an app –",
      "affects citizens’ awareness and behaviour in regard to their",
      "mobility and energy consumption choices in everyday life."
    ))
  )),
  `The Netherlands` = HTML(paste(
    h2("Natural gas-free neighbourhoods"),
    p(paste(
      "Most Dutch households currently use natural gas to, for example,",
      "heat their homes. The gas has been mainly produced in the Groningen",
      "gas field, in the northeastern part of the Netherlands. However,",
      "the exploitation of Groningen has caused increasing earthquakes and",
      "damage to the city and nearby areas since the late 1980s. Because",
      "of this, the Netherlands has decided that all its neighbourhoods",
      "will become natural gas-free by 2050. The case study examines the",
      "transition towards natural gas-free homes in the Netherlands."
    ))
  )),
  Spain = HTML(paste(
    h2("UR BEROA – energy efficiency-driven cooperative"),
    p(paste(
      "The case study examines UR BEROA, a cooperative providing energy to",
      "the Bera Bera neighbourhood in San Sebastian, Spain. The",
      "cooperative was founded in 1985 to provide hot water and community",
      "heating to the residents and improve the energy efficiency of the",
      "neighbourhood. Since its establishment, the cooperative has",
      "successfully introduced cleaner energy sources and ways to measure",
      "the energy consumption of each household. Now, the cooperative is",
      "slowly making its way toward decarbonisation."
    ))
  )),
  none = HTML(paste(
    p("Click on a map marker to learn more about the GRETA case studies.")
  ))
)



####################
# Case study 1 ----
####################

cs1coords <- sf::st_sf(
  name = c("a"),
  geometry = sf::st_sfc(
    sf::st_point(c(11.399926, 44.507145))
  ), crs = 4326
)

txts$cs1poi <- list(
  a = HTML(paste(
    p("You selected a marker on the map. This marker could be a point of
      interest relating to the case study and this text could be a text that
      explains or is otherwise relevant to that point of interest.")
  )),
  none = HTML(paste(
    p("Click on a map marker to learn more about the places of this case study.")
  ))
)
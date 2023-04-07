txts$home$title <- "Home"
txts$home$icon <- icon("house")
txts$home$tags <- c(
  "home", "welcome", "overview", "introduction", "case", "study", "studies",
  "map", "marker", "survey", "country", "countries"
)

txts$home$welcome <- tagList(
  p2("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy
     eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam
     voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet
     clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit
     amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam
     nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat,
     sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum.
     Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor
     sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam
     nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed
     diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum.
     Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor
     sit amet."),
  p2("Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse
     molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero
     eros et accumsan et iusto odio dignissim qui blandit praesent luptatum
     zzril delenit augue duis dolore te feugait nulla facilisi. Lorem ipsum
     dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod
     tincidunt ut laoreet dolore magna aliquam erat volutpat."),
  p2("Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper
     suscipit lobortis nisl ut aliquip ex ea commodo consequat. Duis autem vel
     eum iriure dolor in hendrerit in vulputate velit esse molestie consequat,
     vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et
     iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis
     dolore te feugait nulla facilisi."),
  h2("Subtitle"),
  p2("Nam liber tempor cum soluta nobis eleifend option congue nihil imperdiet
     doming id quod mazim placerat facer possim assum. Lorem ipsum dolor sit
     amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt
     ut laoreet dolore magna aliquam erat volutpat. Ut wisi enim ad minim
     veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut
     aliquip ex ea commodo consequat."),
  p2("Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse
     molestie consequat, vel illum dolore eu feugiat nulla facilisis."),
  p2("At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd
     gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem
     ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod
     tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.
     At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd
     gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem
     ipsum dolor sit amet, consetetur sadipscing elitr, At accusam aliquyam diam
     diam dolore dolores duo eirmod eos erat, et nonumy sed tempor et et
     invidunt justo labore Stet clita ea et gubergren, kasd magna no rebum.
     sanctus sea sed takimata ut vero voluptua. est Lorem ipsum dolor sit amet.
     Lorem ipsum dolor sit amet, consetetur")
)

txts$home$csdesc <- list(
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
      "<b>Pilastro \u201cRione\u201d</b> (neighbourhood): a residential neighborhood",
      "with a long history of activism but also of socio-economic issues."
    )), style = "margin-bottom: 0.5cm;"),
    p(HTML(paste(
      "<b>Roveri</b> area: distinctly separated from Pilastro by a former",
      "railway terminal, is an industrial and productive neighborhood",
      "hosting a variety of companies in sectors such as packaging,",
      "mechanics, and electric vehicles. The area also hosts the",
      "Agriculture and Food Center of Bologna\u2019s (CAAB) food and",
      "agriculture theme park (FICO) with its industrial partners, which",
      "has the largest solar power plant on industrial roofs within the EU."
    )), style = "margin-bottom: 0.5cm;")
  )),
  Portugal = HTML(paste(
    h2("Coop\u00e9rnico \u2013 renewable energy-driven cooperative"),
    p(paste(
      "The case study examines Coop\u00e9rnico, Portugal\u2019s first renewable",
      "energy cooperative, founded in 2013. Coop\u00e9rnico has more than",
      "1,700 members, including citizens, small and medium-sized",
      "enterprises, and municipalities all over Portugal. Its mission is",
      "to involve its members in reshaping the energy sector to be more",
      "renewable, socially just and collaborative."
    ))
  )),
  Germany = HTML(paste(
    h2("The Earnest App \u2013 a virtual community for sustainable mobility in Darmstadt"),
    p(paste(
      "The case study explores how the regular use of a sustainability",
      "app can foster energy citizenship among members of a virtual",
      "community. The case study is conducted in Darmstadt \u2013 a city with",
      "160.000 inhabitants located in the state of Hesse in Germany. In",
      "cooperation with students from the University for Applied Science",
      "in Darmstadt (h_da), the case study explores how a virtual energy",
      "community \u2013 connected by the shared experience of using an app \u2013",
      "affects citizens\u2019 awareness and behaviour in regard to their",
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
    h2("UR BEROA \u2013 energy efficiency-driven cooperative"),
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

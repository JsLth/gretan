txts$home$title <- "Home"
txts$home$icon <- icon("house")
txts$home$tags <- c(
  "home", "welcome", "overview", "introduction", "case", "study", "studies",
  "map", "marker", "survey", "country", "countries"
)

txts$home$welcome <- tagList(
  p2(
    HTML("GRETA Analytics is a <b>GIS-based analytical tool</b> to study the emergence of
     energy citizenship. The tool allows you to freely and interactively explore
     the output produced by the GRETA project with a geospatial focus. It
     answers the the need to create a bridge between data collected from case
     studies, their re-use in scenario predictions, and their framing for the
     purpose of <b>Community Transition Pathways (CTPs)</b>. GRETA Analytics accounts
     for variations within and between geographical levels by georeferencing and
     spatializing the data from the GRETA multinational survey and by studying
     their spillover, crossover, and cross-level interaction effects on the
     local, regional, national, and supranational levels. The tool facilitates
     the geographic assessment of energy citizenship emergence."),
    p2("What can you find on GRETA Analytics?")
  ),
  tags$ul(
    tags$li(
      HTML("Quickly and comfortably <b>explore, compare, inspect</b> and
                 download the data produced in GRETA's multinational survey.\u2800"),
      actionLink("welcome-li-exp", label = "", icon = icon("arrow-right"))
    ),
    tags$li(
      HTML("Understand how GRETA taxonomises the geographical composition
                 of energy citizenship across levels and domains.\u2800"),
      actionLink("welcome-li-taxonomy", label = "", icon = icon("arrow-right"))
    ),
    tags$li(
      HTML("Explore the energy landscapes and energy fragility in
                 the Pilastro-Roveri district of Bologna, Italy.\u2800"),
      actionLink("welcome-li-cs1italy", label = "", icon = icon("arrow-right"))
    ),
    tags$li(
      HTML("Comprehend the interactions between citizens, policymakers,
                 and businesses in a cross-country comparison.\u2800"),
      actionLink("welcome-li-stakeholder", label = "", icon = icon("arrow-right"))
    ),
    tags$li(
      HTML("Learn about your own role as a citizen in the energy
                 transition and how geography can impact it.\u2800"),
      actionLink("welcome-li-persona", label = "", icon = icon("arrow-right"))
    ),
    tags$li(
      HTML("Understand the multi-dimensional vulnerability to energy
                 poverty and how it compares across the EU.\u2800"),
      actionLink("welcome-li-enpov", label = "", icon = icon("arrow-right"))
    ),
    tags$li(
      HTML("Find out how living environments steer the public social media
                 discourse on energy transition and environmental change.\u2800"),
      actionLink("welcome-li-attitudes", label = "", icon = icon("arrow-right"))
    ),
    tags$li(
      HTML("Learn about the uneven geographies of energy research and
                 its thematic versatility.\u2800"),
      actionLink("welcome-li-research", label = "", icon = icon("arrow-right"))
    )
  ),
  p2(
    "You can explore the contents by clicking on the triple-bar",
    icon("bars", style = "vertical-align: center"),
    "and browsing through the data explorer or one of the many studies done by
     GRETA partners."
  )
)
txts$home$about <- tagList(
  p2(
    "This website is part of the",
    a("GRETA project", href = "https://projectgreta.eu/", class = "intext"),
    "(GReen Energy Transition Actions). GRETA studies solutions and provides
     recommendations on how to achieve civic energy empowerment through energy
     citizenship. The project has five objectives:"
  ),
  tags$ol(
    tags$li("To understand who energy citizens are"),
    tags$li("To understand how energy citizens act and interact."),
    tags$li("To develop and test behavioural strategies, approaches and models
            for facilitating energy citizenship."),
    tags$li("To realize impact by scaling approaches from local to regional,
            national, and supranational levels."),
    tags$li("To improve the policymaking process.")
  ),
  p2(
    HTML("The project creates roadmaps for decarbonisation that enhance energy
     citizenship. The findings will help policymakers encourage active citizen
     participation in the energy transition. From 2021 to 2023, GRETA partners
     <b>worked with energy communities</b> in Germany, Italy, the Netherlands,
     Portugal, and Spain. Citizens in these communities will adopt renewable
     energy, use electric vehicles, monitor their energy consumption and
     participate in the sustainable planning of mobility both within their
     cities and internationally. You can consult the case studies by clicking
     on one of the pins"),
    icon("map-pin", style = "vertical-align: center"),
    "on the map to the right."
  ),
  p2(
    HTML("GRETA also launched a first-of-its kind <b>EU-wide empirical citizen
     consultation</b>. The survey was sent to energy customers, suppliers,
     intermediaries, retailers and policymakers in 25 countries, driving
     unique insights about the emergence of energy citizenship. On the
     map to the right you can examine which countries were included in the
     multinational citizen survey. For more insights into the research conducted
     in the GRETA project, you can consult the"),
    a(
      "deliverables",
      href = "https://projectgreta.eu/resources/",
      class = "intext"
    ),
    "published on the official website."
  ),
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

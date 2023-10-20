txts$main$cs5 <- list(
  shortitle = "UR Beroa, Spain",
  title = "Case study: UR BEROA \u2013 energy efficiency-driven cooperative",
  date = "2023-10-04",
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
  introduction = list(
    title = "Introduction",
    content = p2(HTML(
      "The case study analyses UR BEROA, an energy cooperative located in
      Spain, focusing on the different factors affecting citizens'
      preferences and engagement to join an energy cooperative. More
      precisely, the case study analyses how different factors and the
      interactions among actors affect the emergence of energy citizenship
      and the level of engagement to energy citizenship in the case of
      UR BEROA.<br><br>
      UR BEROA is an energy cooperative providing energy to its 550
      members living at Bera Bera neighbourhood in Donostia-San Sebastian,
      Spain. The cooperative was established in 1985 for the purpose of
      providing district heating and hot water for the residents of the
      neighbourhood.<br><br>
      Over the years, UR BEROA has successfully implemented solutions to
      produce cleaner energy and currently, the facilities consist of
      three natural gas boilers, a cogeneration engine, a biomass boiler,
      and solar panels. UR BEROA is currently taking another step towards
      decarbonisation with the installation of photovoltaic (PV) system
      providing electricity to around 100 households.<br><br>
      Overall, the goal of the cooperative is to drive a shift towards
      higher level of decarbonisation and energy efficiency. The
      cooperative aims to significantly grow its member base, increase
      the energy services it provides, and implement collective renewable
      energy-based self-consumption."
    ))
  ),
  how = list(
    title = "How was the study conducted?",
    content = tagList(
      p2(HTML("
        To analyse the factors affecting individual citizens' preferences, the
        case study used secondary data consisting of a background study
        looking at existing information of the UR BEROA energy cooperative,
        its actors, and policy framework. The background study was
        complemented by primary data collection through interviews to UR
        BEROA members, policymakers at different governance levels (local,
        regional, and national).
      ")),
      p2(
        "Energy modelling of cities is necessary in order to obtain an estimation of
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
        implementing energy policies and planning decisions in different locations."
      ),
      p2("In the GRETA project, the ENERKAD\u00ae{@2} tool has been used to generate the models.
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
  ),
  analysis = list(
    title = "Summary of the findings and analysis",
    content = tagList(
      p2(HTML(
        "The UR BEROA case study shows that <b>individuals can play a
        proactive role in engaging with the energy transition when
        certain context conditions are met</b>. The case shows how
        individuals can be drivers for the energy transition when
        they take collective action and set up their own energy
        cooperative together with their peers. It also shows that
        this initiative, although supported by different policies,
        can arise and be driven by citizens. However, for this
        initiative to happen, some baseline conditions are needed:<br>",
      )),
      list_to_li(list(
        HTML(
          "a <b>set of values</b> that motivates individuals to take
          collective action towards more sustainable behaviors."
        ),
        HTML(
          "a <b>certain level of education and knowledge</b> regarding
          the energy system and the role of citizens in it."
        ),
        HTML(
          "<b>financial and other types of resources</b> that allow
          citizens to invest and set up collective energy initiatives"
        )
      )),
      p2(HTML(
        "In addition, the UR BEROA case study draws a very comprehensive
        policy landscape in terms of fostering the participation of
        citizens in the energy transition at both individual and
        collective level.<br>
        However, the lack of a more detailed regulation of energy
        communities and previous regulatory shifts and instability
        at national level, hinder these organisations to spread wider.
        Citizens are concerned about the possible future shifts of the
        national policies in relation with energy communities and ask
        for stability in terms of regulation and the national energy
        model. The case study highlights the <b>important role
        policymakers play in promoting energy citizenship</b>. Policymakers
        at regional and local level have actively supported UR BEROA in
        terms of financial support in different technological projects
        towards a higher degree of decarbonisation of its system.<br>
        In addition, the UR BEROA case illustrates the importance of
        having access to technical and technological knowledge in the
        development of energy citizenship, and particularly for the
        creation and evolution of collective energy projects. Policymakers
        have a say on facilitating the access of citizens to this
        technological knowledge and creating brokerage spaces for those
        actors to collaborate."
      ))
    )
  ),
  findings = list(
    title = "Main conclusions and recommendations",
    content = tagList(
      p2(HTML(
        "The results show some relevant asymmetries on several of the
        aspects that can influence the level of engagement of the
        different actors (citizens, policymakers and energy companies)
        involved in the case.<br>",
        "The members of UR BEROA demonstrate a high level of self-motivation
        when engaging in collective energy action. They mention their
        values and the influence of their peers as one of the reasons
        for their active participation in the energy system, and do not
        mention policymakers nor companies as critical actors influencing
        their choices. Although they acknowledge the need of citizens to
        take an active role, they still expect policymakers to take a
        leadership role and deepen their engagement, both as being drivers
        of the green energy transition and as supporters of the active
        participation of citizen in the transition. However, this push
        from citizens towards a higher leadership is not perceived by
        policymakers. On the contrary, they feel the opposition of some
        citizens and traditional energy companies when they act towards
        a greener and more participative energy transition.<br>
        These asymmetries on expectations and influences among actors
        are confirmed by the differences on perceived and preferred
        relationships among them. The current situation shows that
        relationships among the three actors are not based on equality
        and collaboration, but rather top-down authoritative approaches.
        However, when asked about the preferred relationships, most of the
        actors´ state that <b>more collaborative and balanced relationships are
        convenient to increase energy citizenship</b>. In this case, the main
        discrepancy is shown by policymakers that do not consider that a
        more collaborative relation with citizens is needed. <br>
        With these insights from the case study, several policy measures
        and tools could be utilised to increase energy citizenship:"
      )),
      list_to_li(list(
        HTML("<b>A clearer leadership role of policymakers is needed</b>,
          with not only financial support to energy communities,
          but with other support instruments, and with a more
          explicit prioritisation of collective energy initiatives."),
        HTML("<b>The awareness and knowledge of the relevance of
          renewable energies in the energy transition needs
          to be increased in the society</b>, emphasising on the
          role of energy communities as a means of gaining
          local ownership and social acceptance of large RES
          projects and installations. Moreover, the role of
          energy communities in decreasing energy poverty and
          contributing to a fairer energy transition should
          be highlighted."),
        HTML("<b>Collaboration among the actors involved should be
          improved</b> to foster more balanced power-relations
          among them. It is of foremost importance to increase
          collaboration and trust among citizens and policymakers
          to reach the objective of enhancing energy citizenship.")
      ))
    )
  ),
  buildings_info = "This map presents the energy models for buildings in
    Bera Bera. Hover over individual buildings to learn more
    about their energy-related properties. Using the controls on
    the right side of the map you can also switch between layers",
  desc = list(
    substation = HTML(
      "The <b>substation</b> layer shows which substation a building belongs to
      and is provided with electricity."
    ),
    year_constr = HTML(
      "The <b>construction year</b> layer shows the construction year of each building. In
      this way it is possible to see the development of the city by zones in the
      different periods."
    ),
    number_of_dw = HTML(
      "The <b>Number of dwellings</b> layer shows the number of dwellings
      inside a building."
    ),
    a_heat_dem_m2 = HTML(
      "The <b>heating demand</b> layer shows the heat demand of the buildings, which
      includes the needs for heating and domestic hot water, calculated in the
      ENERKAD\u00ae Tool simulation. Some of the building uses
      are excluded from the simulation and show a 0 value."
    )
  )
)
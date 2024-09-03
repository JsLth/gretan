var_names <- list(
  pay_worries = "Payment worries",
  supp_threat = "Supplier threat",
  summer_haz = "Summer hazard",
  winter_haz = "Winter hazard",
  detached = "House detached",
  house_area = "House area",
  is_tenant = "Tenancy",
  urbanity = "Urbanity",
  cond_trans = "Condition transport",
  cond_heat = "Condition heating",
  cond_air = "Condition air",
  behav_unplug = "Unplug devices",
  behav_products = "Efficient products",
  behav_lights = "Turn lights off",
  behav_share = "Encourage others",
  lit_energy = "Literacy consumption",
  lit_heating = "Literacy heating/cooling",
  lit_costs = "Literacy energy bill",
  lit_share = "Literacy renewable energy",
  lit_solutions = "Literacy implement solutions",
  hdd = "Heating degree days",
  cdd = "Cooling degree days"
)

txts$main$enpov <- list(
  shortitle = "Energy vulnerability",
  title = "Analyzing the spatially varying dimensions of energy vulnerability across the EU",
  icon = "plug-circle-exclamation",
  tags = c(
    "poverty", "vulnerability", "index", "multivariate", "principal",
    "component", "gwpca", "pca"
  ),
  authors = c("Jonas Lieth", "Dennis Abel", "Stefan J\u00fcnger"),
  affil = "GESIS - Leibniz Institute for the Social Sciences",
  date = "2024-09-03",
  introduction = list(
    title = "Energy poverty in the EU",
    content = p2(HTML("Energy poverty is a profound issue across the European
    Union. According to the EU, energy poverty describes a situation
    in which households are <b>unable to access essential energy services</b>
    and products<sup>1</sup>. This relates to a lack of heating and the usage
    of household appliances, but also to other domains such as the use
    of transport or the internet. While energy poverty is recognized
    and actively tackled across member states, recent events such as
    the COVID-19 crisis, the Russian invasion of the Ukraine had highly
    adverse effects on energy prices and households living in energy
    poverty. According to Eurostat<sup>2</sup>, the share of people not able to
    keep their home adequately warm <b>rose from 6.9 % to 9.3%</b> between 2021
    and 2022 in the EU-27 after years of almost constant decline. Some
    studies also predict a further increase of energy poverty in the EU
    as climate change grows more severe<sup>3</sup>. Simultaneously, inequalities
    in energy access make it significantly harder for the energy poor to
    actively participate in and shape the energy transition<sup>4</sup>. It is
    therefore important to <b>comprehensively measure the vulnerability to
    energy poverty</b> and highlight <b>geographical inequalities</b> across the
    European Union in order to enable place-based action against energy
    poverty. This module presents the results of a research study in the
    context of GRETA that focuses on measuring energy vulnerability and
    determining its link to energy citizenship."))
  ),
  how = list(
    approach = list(
      title = "How to measure energy vulnerability?",
      content = p2(HTML("Measuring energy vulnerability is challenging.
        However, significant effort has been invested in developing reliable
        methods for deriving relevant indicators. Energy vulnerability is
        <b>inherently multi-dimensional</b> and subject to behavioural intricacies
        such as hidden energy poverty. It is therefore impossible to describe
        this phenomenon with a single indicator. The GRETA multinational
        survey, which involved close to 10,000 EU citizens, gathered countless
        indicators revolving around <b>energy affordability, housing conditions,
        health, literacy, and behaviour</b>.<br><br>We performed a multivariate analysis
        using <b>Principal Component Analysis (PCA)</b> on 22 selected indicators from
        the survey. You can view each indicator that was used for this study in
        the second tab of this box. PCA is a method that is generally used to
        reduce the dimensionality of a set of indicators. Our aim is to create
        multiple indices that <b>describe different aspects of energy
        vulnerability</b>. The large box below shows how these different aspects
        of energy vulnerability manifest across the EU."))
    ),
    variables = list(
      title = "Variables",
      content = data.frame(
        Subindex = c(
          rep("Energy affordability", 4),
          rep("Housing precarity", 4),
          rep("Energy-related health", 3),
          rep("Energy behavior", 4),
          rep("Energy literacy", 5),
          rep("Climate", 2)
        ),
        Indicator = unlist(var_names, use.names = FALSE),
        Description = c(
          "How often did you worry that you wouldnt be able to pay your home energy bill?",
          "How often did you have a supplier threaten you to disconnect your electricity or home heating fuel service, or discontinue making fuel deliveries?",
          "During the winter months, how often did you keep your home at a temperature that you felt was unsafe or unhealthy?",
          "During the summer months, how often did you keep your home at a temperature that you felt was unsafe or unhealthy?",
          "Binary indicator measuring whether a person lives in a detached or semi-detached house.",
          "What is the housing area of your home?",
          "Binary indicator whether respondent is a tenant or not.",
          "Where do you live?",
          "Chronical disease or disability that requires special care for transport.",
          "Chronical disease or disability that requires more heating.",
          "Chronical disease or disability that requires more air",
          "How often do you perform the following activities? - Unplug electronic devices that are not being used",
          "How often do you perform the following activities? - Actively search for products that are more energy efficient",
          "How often do you perform the following activities? - Turn off all lights before leaving a room",
          "How often do you perform the following activities? - Encourage friends or family to be more energy-efficient",
          "Perceived complexity of electricity consumption",
          "Perceived complexity of heating/cooling consumption",
          "Perceived complexity of energy spending",
          "Perceived complexity of the share of renewables",
          "Perceived complexity of the implementation of green energy solutions",
          "Mean monthly cooling degree days from 1979 to 2022, disaggregated from NUTS-3 to household level",
          "Mean monthly heating degree days from 1979 to 2022, disaggregated from NUTS-3 to household level"
        )
      )
    )
  ),
  index_map = list(
    title = "Energy vulnerability in the EU",
    content = p2(HTML(
      "The map below shows the different facets of energy vulnerability in
      the EU. Each point on the map represents a household, smoothed across
      the nearest 100 households. In other words, the energy vulnerability
      index for each household is the mean of its nearest neighbors. One
      apparent take-away is the clear north-south descent. Countries in the
      South of Europe are visibly more vulnerable to energy poverty than
      those in the North."
    ))
  ),
  references = list(
    title = "References",
    content = tags$ul(
      class = "list-style: none",
      style = "margin-left: -30px",
      pbib(HTML("[1] European Commission (2023). Energy poverty in the EU.
        <a>Retrieved from https://energy.ec.europa.eu/topics/
        markets-and-consumers/energy-consumer-rights/energy-poverty-eu_en</a>")),
      pbib(HTML("[2] Eurostat. (2023). Inability to keep home adequately
        warm - EU-SILC survey. Retrieved from <a>https://ec.europa.eu/
        eurostat/databrowser/view/ILC_MDES01/default/table?lang=en</a>")),
      pbib("[3] Awaworyi Churchill, S., Smyth, R., & Trinh, T.-A. (2022).
        Energy poverty, temperature and climate change. Energy Economics,
        114, 106306. doi: 10.1016/j.eneco.2022.106306"),
      pbib("[4] DellaValle, N., & Czako, V. (2022). Empowering energy
        citizenship among the energy poor. Energy Research & Social
        Science, 89, 102654. doi: 10.1016/j.erss.2022.102654")
    )
  )
)

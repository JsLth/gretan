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
  date = "2023-10-12",
  introduction = list(
    title = "Energy poverty in the EU",
    content = p2(HTML("Energy poverty is a profound issue across the European
    Union. According to the EU, energy poverty describes a situation
    in which households are unable to access essential energy services
    and products<sup>1</sup>. This relates to a lack of heating and the usage
    of household appliances, but also to other domains such as the use
    of transport or the internet. While energy poverty is recognized
    and actively tackled across member states, recent events such as
    the COVID-19 crisis, the Russian invasion of the Ukraine had highly
    adverse effects on energy prices and households living in energy
    poverty. According to Eurostat<sup>2</sup>, the share of people not able to
    keep their home adequately warm rose from 6.9 % to 9.3% between 2021
    and 2022 in the EU-27 after years of almost constant decline. Some
    studies also predict a further increase of energy poverty in the EU
    as climate change grows more severe<sup>3</sup>. Simultaneously, inequalities
    in energy access make it significantly harder for the energy poor to
    actively participate in and shape the energy transition<sup>4</sup>. It is
    therefore important to comprehensively measure the vulnerability to
    energy poverty and highlight geographical inequalities across the
    European Union in order to enable place-based action against energy
    poverty. This module presents the preliminary results of an ongoing
    research study in the context of GRETA that focuses on measuring
    energy vulnerability and determining its link to energy citizenship."))
  ),
  how = list(
    approach = list(
      title = "How to measure energy vulnerability?",
      content = p2(HTML("Measuring energy vulnerability is not easy and much work
        has been dedicated to finding reasonable approaches to derive relevant
        indicators. As energy vulnerability is inherently multi-dimensional
        and subject to behavioral intricacies such as ‘hidden energy poverty’,
        there is no single indicator that can reasonably describe this
        phenomenon. As part of the GRETA multinational survey involving close
        to 10,000 citizens of the EU, countless indicators were gathered
        revolving around energy security, literacy, behavior and efficiency.
        Using 32 selected indicators from the survey, we performed a
        multivariate analysis using Principal Component Analysis (PCA) You
        can view each indicator that was used for this study in the second
        tab of this box. PCA is a method that is generally used to reduce the
        dimensionality of a set of indicators. Here, we aim to create multiple
        indicators describing different aspects of energy vulnerability and
        thus reduce the variables of sub-indices to their first component
        (or one dimension). In total, we derive seven sub-indices, each
        describing a key component of energy vulnerability: energy security,
        energy exclusion, housing precarity, disempowerment, special
        conditions, energy behavior, and energy literacy. We extend this
        analysis by applying a geographically weighted variant of PCA to
        clearly uncover spatial inequalities. To understand the links to
        energy citizenship, we perform linear regression with variables from
        the multinational survey related to institutional trust and social
        cohesion."))
    ),
    variables = list(
      title = "Variables",
      content = data.frame(
        Subindex = c(
          rep("Energy insecurity", 5),
          rep("Energy exclusion", 3),
          rep("Housing precarity", 5),
          rep("Disempowerment", 4),
          rep("Disability", 4),
          rep("Energy behavior", 6),
          rep("Energy literacy", 5)
        ),
        Indicator = c(
          "Inability to pay for energy",
          "Threats by energy supplier",
          "Safety in winter",
          "Safety in summer",
          "Income",
          "Lacks cooling system",
          "Lacks heating system",
          "Lacks central heating",
          "Lives in a detached house",
          "House area",
          "House age",
          "Is tenant",
          "Energy cost",
          "Is not male",
          "Is unemployed",
          "Lacks education",
          "Household size",
          "Suffers from condition requiring more air",
          "Suffers from condition requiring more heat",
          "Suffers from condition requiring more transport",
          "Receives public financial support",
          "Unplugs unused electronic devices",
          "Searches for energy-efficient products",
          "Turns off lights before leaving a room",
          "Encourages friends and family to be more energy-efficient",
          "Participates in carpooling",
          "Chooses to travel without a car",
          "Perceived complexity of electricity consumption",
          "Perceived complexity of heating/cooling consumption",
          "Perceived complexity of energy spending",
          "Perceived complexity of the share of renewables",
          "Perceived complexity of the implementation of green energy solutions"
        ),
        Type = c(
          "Ordinal", "Ordinal", "Ordinal", "Ordinal", "Ordinal", "Binary", "Binary", 
          "Binary", "Binary", "Ordinal", "Ordinal", "Binary", "Ordinal", 
          "Binary", "Binary", "Binary", "Count", "Binary", "Binary", "Binary", 
          "Binary", "Binary", "Binary", "Binary", "Binary", "Binary", "Binary", 
          "Ordinal", "Ordinal", "Ordinal", "Ordinal", "Ordinal"),
        Missing = c(
          "0 %", "0 %", "0 %", "0 %", "0 %", "0 %", "0 %", "0 %", "0 %", "0 %", 
          "7.9 %", "0 %", "12.9 %", "0 %", "0 %", "0 %", "0 %", "0 %", 
          "0 %", "0 %", "0 %", "1.7 %", "1.8 %", "1.6 %", "2.9 %", "4.9 %", 
          "2.6 %", "0 %", "0 %", "0 %", "0 %", "0 %"
        ),
        Mean = c(
          "1.83", "1.30", "1.60", "1.52", "0.79", "0.512", "0.0291", "0.595", 
          "0.39", "2.54", "4.03", "0.342", "4.09", "0.493", "0.433", "0.459", 
          "2.11", "0.107", "0.1", "0.07", "0.148", "3.73", "3.69", "4.44", 
          "3.52", "2.60", "3.37", "4.03", "4.19", "3.95", "4.92", "4.99"
        ),
        Median = c(
          "1", "1", "1", "1", "1", "1", "0", "1", "0", "2", "4", "0", "4", 
          "0", "0", "0", "2", "0", "0", "0", "0", "4", "4", "5", "4", "3", 
          "4", "4", "4", "4", "5", "5"
        ),
        SD = c(
          "1.04", "0.711", "0.916", "0.878", "0.407", "0.5", "0.168", "0.491", 
          "0.49", "0.973", "1.94", "0.474", "1.77", "0.5", "0.5", "0.21", 
          "1.17", "0.309", "0.299", "0.255", "0.355", "1.17", "1.08", "0.888", 
          "1.24", "1.39", "1.27", "2.14", "2.10", "2.13", "2.03", "2.01"
        )
      )
    )
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

txts$main$stakeholder <- list(
  shortitle = "Stakeholder interactions",
  title = "Stakeholder interaction modelling",
  icon = "people-arrows",
  tags = c(
    "stakeholder", "model", "interaction", "lotka", "volterra", "equation",
    "equilibrium", "waves"
  ),
  authors = c("Omar Usmani", "Carlos Montalvo Corral"),
  affil = "TNO - Netherlands Organisation for Applied Scientific Research",
  date = "13-10-2023",
  what = list(
    title = "What are stakeholder interactions?",
    content = HTML("This model (pLAtYpus) looks at the <b>engagement levels</b> of three
      types of stakeholders (citizens, business, governments) for three
      sustainable initiatives (autonomous cars, sustainable transport,
      cooperative self-generation). The engagement levels cover the
      various ways the stakeholders engage with the sustainable
      initiatives, including purchasing the product/service,
      producing it, or promoting/subsidizing it. The engagement
      levels of the various stakeholders are not independent of each
      other. Rather, there are interactions between them. The engagement
      level of a given stakeholder will be influenced by the engagement
      levels of all three stakeholders (i.e. including themselves). For
      example, citizens will be more engaged if the government promotes
      autonomous cars, if businesses produce them, and if other citizens
      purchase them. The degree to which these engagement levels vary depends
      on the situation and on the relations between the stakeholders
      (including trust or knowledge of each other). In some cases
      (strong distrust of government or given companies, e.g.), this
      interaction could even lead to negative feedback.<br><br>
    
      This section of GRETA Analytics demonstrates how engagement levels of
      stakeholders develop over time and space. We have diagrams both on a country
      basis, showing the engagement levels of the three types of stakeholders,
      and maps that compare the long-term engagement levels of stakeholders
      across countries. The users of this tool can look at the impact of
      the various input parameters by changing the slider values below.")
  ),
  how = list(
    title = "How can we model them?",
    content = HTML("We compute the engagement levels by balancing to rates,
      namely the rate at which a stakeholder adopts/engages into a given
      sustainable initiative, and the rate at which they leave/disengage
      from the sustainable initiative. Generally, these rates are computed
      looking at three phases. The first phase is the <b>Attention phase</b>, which
      takes into account inertia/habit effects. This includes both the fact
      that stakeholder make such decisions on a cyclical basis (average
      vehicle ownership time, factory retooling, legislative cycles, etc)
      and the fact that stakeholders will stick to their habitual behaviour,
      unless something compels them to look at alternatives. The second
      phase is the <b>Enable phase</b>, which says that the stakeholders may face
      obstacles that would prevent them from considering a switch (for
      example not having the means or knowledge to change, or because the
      alternative would prevent them from performing their desired
      activities). The third phase in the <b>Intention phase</b>, where the
      stakeholders balance out the pros and cons of a switch. Examples of
      such elements include (but are not limited to) extra costs and
      complexity versus social desirability/need to conform and benefits
      to the environment. For the GRETA project, only the third phase was
      implemented, due to data availability issues. Note that the first
      two phases mostly impact the ramp-up of engagement (as all
      stakeholders will have been reached and most practical obstacles
      will disappear with time and technology improvements). As such,
      the focus of the plots shown here should be the long-term values
      (which are shown in the maps).")
  ),
  params = list(
    countries = c(
      "Austria", "Belgium", "Denmark", "Germany", "Netherlands",
      "Poland", "Portugal", "Spain", "Romania", "Czechia", "Finland",
      "Hungary", "Greece", "France", "Ireland", "Italy"
    ),
    product = list(
      "Autonomous cars" = "autonomous_cars",
      "Sustainable transport" = "sustainable_transport",
      "Cooperative self-generation" = "cooperative_self_generation"
    ),
    stakeholder = list(
      "Citizens" = "citizens",
      "Business" = "business",
      "Government" = "government"
    ),
    survey_topic = list(
      survey_topic = list(
        "Agency to engage" = "agency_to_engage",
        "Emotions" = "emotions",
        "Environmental outcomes" = "environmental_outcomes",
        "Outcomes to the individual and household" = "outcomes_to_the_individual_and_household",
        "Relation score citizens" = "relation_score_citizens",
        "Relation score business" = "relation_score_business",
        "Relation score government" = "relation_score_government"
      ),
      intention_weight = list(
        "Agency to engage" = "agency_to_engage",
        "Environmental outcomes" = "environmental_outcomes",
        "Outcomes to the individual and household" = "outcomes_to_the_individual_and_household",
        "Social norm" = "social_norm",
        "Enagement emotion" = "engagement_emotion",
        "Dominant relational model" = "dominant_relational_model"
      )
    ),
    adopt_leave = c("Adopt", "Leave")
  )
)

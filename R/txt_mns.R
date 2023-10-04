txts$main$exp <- list(
  shortitle = "Explore data",
  icon = "map",
  tags = c("explore", "data", "multinational", "survey", "map"),
  help = list(
    databox = "This box helps you to select a variable from the GRETA
      multinational survey.
      <br>
      <b>Topic:</b> Choose a topic to display in the
      explorer. Each topic corresponds to a question in the multinational survey.
      <br>
      <b>Subitem:</b> If applicable, choose a subgroup of the selected topic.
      <br>
      <b>Option:</b> In case of categorical questions, choose a response option.",
    confbox = "This box helps you to configure the data explorer.
      <br>
      <b>Aggregation level:</b> Allows you to switch between geographical levels,
      i.e. local, regional and national.
      <br>
      <b>Color palette:</b> Changes the appearance of the data explorer.
      <br>
      <b>Legend values:</b> Changes the calibration of the legend. To compare
      variables, choose \u201cFull range\u201d, to compare regions, choose
      \u201cFull contrast\u201d.
      <br>
      <b>Aggregate options:</b> Whether to show options seperately as percentages
      or show the most common response option for a topic."
  ),
  about = "This visualization is powered by GRETA's multinational
    survey (MNS). The MNS is a social survey covering citizens, policymakers,
    and businesses in 16 EU countries. Over 10,000 people responded to a variety
    of questions related to energy, citizenship, and sustainability. The GRETA
    Analytics data explorer allows you to explore what citizens responded to the
    various questions in the survey."
)


txts$main$cmp <- list(
  shortitle = "Compare data",
  tags = c("compare", "data", "multinational", "survey", "map"),
  left = list(
    help = list(
      databox = HTML(
        "This box helps you to select a variable from the GRETA
        multinational survey. Choose one of the topics corresponding to a survey
        question to update the <b>left</b> map. If applicable, you can also specify
        subitems and options."
      ),
      confbox = HTML(
        "This box helps you to configure the data explorer. Choose an aggregation
        level, a palette, the legend configuration, or whether to aggregate
        options to update the <b>left</b> map."
      )
    )
  ),
  right = list(
    help = list(
      databox = HTML(
        "This box helps you to select a variable from the GRETA
        multinational survey. Choose one of the topics corresponding to a survey
        question to update the <b>left</b> map. If applicable, you can also specify
        subitems and options."
      ),
      confbox = HTML(
        "This box helps you to configure the data explorer. Choose an aggregation
        level, a palette, the legend configuration, or whether to aggregate
        options to update the <b>left</b> map."
      )
    )
  )
)


txts$main$insp <- list(
  shortitle = "Inspect data",
  tags = c(
    "inspect", "data", "multinational", "survey",
    "table", "transpose", "download"
  )
)

txts$main$persona <- list(
  shortitle = "Energy personas",
  title = "Persona-based clustering of energy citizens",
  icon = "user-gear",
  tags = c("persona", "group", "cluster", "move", "multivariate"),
  authors = c("Ajesh Kumar", "Toni Kuronen", "Annika Wolff"),
  affil = "Lappeenranta-Lahti University of Technology LUT",
  date = "DD-MM-YYYY",
  steps = list(
    list(
      title = "Welcome to the persona quiz!",
      desc = HTML(paste(
        "Take this questionnaire and learn about your role in the energy transition.",
        br(),
        "Discover how you compare to other people in the EU.",
        br(),
        "Find out how moving to a different country can change your outlook."
      )),
      consent = "I give consent to the GRETA research team to analyze the data
        that I provide in this survey."
    ),
    list(
      question = "How would you assess your knowledge and financial resources to
        participate in cooperative self-generation of renewable energy?",
      choices = list(
        "None selected" = NA,
        "Very low" = 1,
        "Low" = 2,
        "Somewhat low" = 3,
        "Neutral" = 4,
        "Somewhat high" = 5,
        "High" = 6,
        "Very high" = 7,
        "I do not know" = -1
      )
    ),
    list(
      question = "Do you believe that environmental problems caused by
        fossil-based electricity are likely to affect future generations?",
      choices = list(
        "None selected" = NA,
        "Strongly disagree" = 1,
        "Disagree" = 2,
        "Somewhat disagree" = 3,
        "Neutral" = 4,
        "Somewhat agree" = 5,
        "Agree" = 6,
        "Strongly agree" = 7
      )
    ),
    list(
      question = HTML(
        "To what degree do you agree with the following statement:<br>
        \u201cI think of myself as someone who is very concerned with
        environmental issues.\u201d"
      ),
      choices = list(
        "None selected" = NA,
        "Strongly disagree" = 1,
        "Disagree" = 2,
        "Somewhat disagree" = 3,
        "Neutral" = 4,
        "Somewhat agree" = 5,
        "Agree" = 6,
        "Strongly agree" = 7
      )
    ),
    list(
      question = "Ten years from now, how do you think your country will
        have changed in the consumption of renewable energy sources (e.g.,
        wind, solar, etc.)?",
      choices = list(
        "None selected" = NA,
        "Prefer not to say" = -2,
        "Significantly decrease" = 1,
        "Decrease" = 2,
        "Same as today" = 3,
        "Increase" = 4,
        "Significantly increase" = 5
      )
    ),
    list(
      question = HTML(
        "To what degree do you agree with the following
        statement?<br>\u201cI feel proud about the idea of
        cooperative self-generation of renewable energy for
        my household use.\u201d"
      ),
      choices = list(
        "None selected" = NA,
        "Strongly disagree" = 1,
        "Disagree" = 2,
        "Somewhat disagree" = 3,
        "Neutral" = 4,
        "Somewhat agree" = 5,
        "Agree" = 6,
        "Strongly agree" = 7,
        "I do not know" = -1
      )
    ),
    list(
      thanks = "Thank you!",
      submit = "You have finished the survey. Click \u201cSubmit\u201d
        now to have your information analyzed."
    )
  ),
  results = list(
    map_info = list(
      title = "How do you compare?",
      content = "This map shows you how you compare to other regions in the EU.
        Where do other energy citizens live? Which regions perform
        better than others? What changes can you expect when moving to
        a different region?"
    ),
    map_move = list(
      title = "Effects of moving",
      content = HTML("What happens if you travel between places? What effects does
        moving have on energy citizenship in your area? Are people more
        or less engaged with energy transition at your new home?<hr/>Find out
        how energy citizenship compares between two places! Select a start
        and a destination and see how both places compare:")
    ),
    personas = list(
      list(
        name = "Indifferent",
        desc = paste("This persona is characterized by", shinipsum::random_text(nchars = 250)),
        src = ""
      ),
      list(
        name = "Information sceptic",
        desc = paste("This persona is characterized by", shinipsum::random_text(nchars = 250)),
        src = ""
      ),
      list(
        name = "Resource Constrained",
        desc = paste("This persona is characterized by", shinipsum::random_text(nchars = 250)),
        src = ""
      ),
      list(
        name = "Young Mindful",
        desc = paste("This persona is characterized by", shinipsum::random_text(nchars = 250)),
        src = ""
      ),
      list(
        name = "Socially Motivated",
        desc = paste("This persona is characterized by", shinipsum::random_text(nchars = 250)),
        src = ""
      ),
      list(
        name = "High Income Investor",
        desc = paste("This persona is characterized by", shinipsum::random_text(nchars = 250)),
        src = ""
      ),
      list(
        name = "Practical Advocate",
        desc = paste("This persona is characterized by", shinipsum::random_text(nchars = 250)),
        src = ""
      ),
      list(
        name = "Tech-savvy advocate",
        desc = paste("This persona is characterized by", shinipsum::random_text(nchars = 250)),
        src = ""
      )
    )
  )
)

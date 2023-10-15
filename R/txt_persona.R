txts$main$persona <- list(
  shortitle = "Energy personas",
  title = "Persona-based clustering of energy citizens",
  icon = "user-gear",
  tags = c("persona", "group", "cluster", "move", "multivariate"),
  authors = c("Ajesh Kumar", "Toni Kuronen", "Annika Wolff", "Jonas Lieth"),
  affil = list(
    "Ajesh Kumar" = "Lappeenranta-Lahti University of Technology LUT",
    "Toni Kuronen" = "Lappeenranta-Lahti University of Technology LUT",
    "Annika Wolff" = "Lappeenranta-Lahti University of Technology LUT",
    "Jonas Lieth" = "GESIS - Leibniz Institute for the Social Sciences"
  ),
  date = "2023-09-22",
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
    campaign = tags$a(
      "Visit our campaign site and read more about energy communities.",
      href = "https://projectgreta.eu/"
    ),
    personas = list(
      list(
        name = "Indifferent",
        desc = HTML("You are aware of climate issues in general, but you don\u2019t pay
          that much attention to them. That\u2019s because you don\u2019t think your own
          actions make a difference. For you, saving energy doesn\u2019t seem
          worthwhile, and you don\u2019t have great concerns for the environment
          either. You might be open to considering adopting green energy \u2013
          but only if it doesn\u2019t affect your lifestyle.<br>
          Could you do more? Try these:"),
        tips = tags$ul(
          tags$li(
            "Try energy activities that you can easily do as part of
            your current lifestyle. For example, switch your electricity
            provider to a renewable one."
          ),
          tags$li("Visit existing energy parks and communities."),
          tags$li(
            "Watch videos and documentaries about energy history and
            its impact on the world."
          )
        ),
        src = "www/personas/1_indifferent.png"
      ),
      list(
        name = "Information sceptic",
        desc = HTML("You\u2019re very sceptical of nearly all sources of
          information \u2013 hence your name. You\u2019re aware of the climate
          debate but don\u2019t believe your own actions have an impact.
          Your lack of motivation also stems from the fact that your energy
          costs are not very high. Thus, you don\u2019t see yourself investing
          in green technologies or joining energy co-operatives.<br>
          Could you do more? Try these:"),
        tips = tags$ul(
          tags$li(
            "Install an app that calculates or contextualises your energy
            saving actions and shows its benefits."
          ),
          tags$li(
            "Ask other people about their experiences: visit existing
            energy communities or talk to someone, who has made energy
            saving actions or invested in green energy."
          ),
          tags$li("Test drive an electric vehicle.")
        ),
        src = "www/personas/2_information-sceptic.png"
      ),
      list(
        name = "Practical Advocate",
        desc = HTML("Practical Advocates are likely to be employed or
          retired, but still seek additional sources of income.
          You recognize the importance of taking green actions and
          believe that we all need to do our part. You\u2019re likely
          to discuss environmental issues with your friends and
          family and trust the information you get from them. Simple
          and easy energy saving actions interest you, whereas you\u2019re
          much more cautious about significant energy investments or
          actions that make you depend on others.<br>
          Could you do more? Try these:"),
        tips = tags$ul(
          tags$li(
            "Learn about the financial benefits of
            investing in green energy."
          ),
          tags$li(
            "Register as an early adapter and
            test new energy technology."
          ),
          tags$li(
            "Visit an energy conference, volunteer for
            energy research workshops or write energy-related blogs."
          ),
          tags$li(
            "Teach kids you know why saving and
            using green energy is important."
          )
        ),
        src = "www/personas/7_practical_advocates.png"
      ),
      list(
        name = "Resource Constrained",
        desc = HTML("Resource Constrained are likely to be older and live in
          apartment buildings. You\u2019re aware of climate issues but rely
          on science to solve the problem. You don\u2019t believe energy
          companies or governments have a big impact on the situation.
          You have limited options for bigger energy actions, as you
          don\u2019t likely have much disposable income. However, you\u2019re up
          to saving energy in your daily life, especially if it saves
          you money. You might actually already use energy quite sustainably,
          for example, by using public transport.<br>
          Could you do more? Try these:"),
        tips = tags$ul(
          tags$li("Share your own energy tips with someone you know."),
          tags$li("Visit existing energy communities."),
          tags$li(
            "Find out which energy subsidies from your local
            or regional government you could be eligible for."
          ),
          tags$li(
            "Talk with local politicians about establishing
            green energy communities."
          )
        ),
        src = "www/personas/3_resource-constrained.png"
      ),
      list(
        name = "Socially Motivated",
        desc = HTML("You\u2019re not very concerned with environmental issues
          and don\u2019t have much faith in national institutions,
          industry, scientists or individual actions either. However,
          you\u2019re strongly driven by social factors and curious about
          social energy activities, such as participating in an energy
          community. In fact, you might already be doing so. For you,
          it\u2019s all about finding new opportunities to cooperate with
          other like-minded people.<br>
          Could you do more? Try these:"),
        tips = tags$ul(
          tags$li(
            "Look for energy-related initiatives
            in your community that you would join."
          ),
          tags$li("Visit existing energy communities."),
          tags$li(
            "Watch videos and documentaries about energy
            history and its impact on the world."
          )
        ),
        src = "www/personas/5_socially-motivated.png"
      ),
      list(
        name = "Tech-savvy advocate",
        desc = HTML("You\u2019re well acquainted with environmental topics and
          understand that everyone needs to do their part to tackle
          climate change. You trust the information provided by
          governments, scientists and various online sources. You try
          to save energy and will happily use apps to help achieve
          this goal. You\u2019re also sensitive to issues of energy justice
          and equal access to energy, and might be interested in getting
          involved in advocating a just energy transition in society.<br>
          Could you do more? Try these:"),
        tips = tags$ul(
          tags$li(
            "Try out new technologies that you could use
            to support sustainable living."
          ),
          tags$li("Use energy saving apps or games."),
          tags$li(
            "Discuss energy related issues on social
            media and share positive energy stories."
          ),
          tags$li("Invest in green energy stocks."),
          tags$li("Discuss policy initiatives with policy makers.")
        ),
        src = "www/personas/8_tech-savvy-advocate.png"
      ),
      list(
        name = "High Income Investor",
        desc = HTML("As your name suggests, you probably have a high income
          and value time over money. You\u2019re very concerned about the
          environment and happy to take positive action in the form
          of renewable energy investments, such as solar panels,
          electric vehicles and other forms of sustainable transport.
          You understand that communal actions may be useful for
          achieving impact. However, given your busy lifestyle, you
          don\u2019t want to invest too much of your time.<br>
          Could you do more? Try these:"),
        tips = tags$ul(
          tags$li(
            "Look for a less time consuming role in an energy
            community. For example, offer specialist technical knowledge."
          ),
          tags$li("Invest in green energy companies and technologies."),
          tags$li(
            "Visit an energy conference, volunteer for
            energy research workshops or write energy-related blogs."
          ),
          tags$li(
            "Teach kids you know why saving and using
            green energy is important."
          )
        ),
        src = "www/personas/6_high-income-investors.png"
      ),
      list(
        name = "Young Mindful",
        desc = HTML("Young Mindfuls are likely to be young, live in apartment
          buildings and lack disposable income. You\u2019re concerned about
          the environment and believe in the need for increased greener
          transport solutions and renewable energy. However, due to your
          living situation, you\u2019re unlikely to take actions that require
          investments. You like to discuss climate issues with your friends
          and family and trust them in energy topics. Thus, you have
          potential to become an advocate.<br>
          Could you do more? Try these:"),
        tips = tags$ul(
          tags$li(
            "Look for energy saving activities that suit
            your current lifestyle and don\u2019t cost money."
          ),
          tags$li(
            "Discuss climate issues with someone
            to provoke their thinking."
          ),
          tags$li("Use energy saving apps or games."),
          tags$li(
            "Discuss energy related issues on social
            media and share positive energy stories."
          ),
          tags$li("Invest in green energy stocks.")
        ),
        src = "www/personas/4_young-mindfuls.png"
      )
    )
  )
)

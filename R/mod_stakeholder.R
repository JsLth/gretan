mod_stakeholder_ui <- function(id) {
  ns <- NS(id)

  get_text <- dispatch_to_txt(id)

  bs4Dash::tabItem(
    "stakeholder",
    make_header(
      title = get_text("title"),
      authors = get_text("authors"),
      affil = get_text("affil"),
      date = get_text("date")
    ),
    fluidRow(
      col_6(
        bs4Dash::box(
          title = with_literata(get_text("what", "title")),
          status = "primary",
          width = 12,
          p2(get_text("what", "content"))
        )
      ),
      col_6(
        bs4Dash::box(
          title = with_literata(get_text("how", "title")),
          status = "primary",
          width = 12,
          p2(get_text("how", "content"))
        )
      )
    ),
    fluidRow(
      bs4Dash::tabBox(
        title = "Modelling results",
        status = "primary",
        width = 12,
        type = "tabs",
        side = "right",
        tabPanel(
          title = "Cycle dynamics",
          plotOutput(ns("plot"), height = "600px")
        ),
        tabPanel(
          title = "Stability maps",
          leaflet::leafletOutput(ns("map"))
        )
      )
    ),
    fluidRow(
      bs4Dash::tabBox(
        width = 12,
        type = "tabs",
        side = "right",
        title = "Control parameters",
        solidHeader = FALSE,
        status = "primary",
        tabPanel(
          title = "Initial Yes values",
          bs4Dash::tabsetPanel(
            vertical = TRUE,
            .list = lapply(
              get_text("params", "countries"),
              function(country) {
                tabPanel(
                  title = country,
                  div(
                    style = "margin: 20px;",
                    h4(tags$b("Country:"), country)
                  ),
                  fluidRow(
                    col_1(),
                    col_10(
                      h5("Product: Autonomous cars"),
                      fluidRow(
                        col_4(
                          sliderInput(
                            ns(paste0("initial_yes__autonomous_cars__citizens__", country)),
                            label = "Citizens", min = 0, max = 1, value = 0.5
                          )
                        ),
                        col_4(
                          sliderInput(
                            ns(paste0("initial_yes__autonomous_cars__business__", country)),
                            label = "Business", min = 0, max = 1, value = 0.5
                          )
                        ),
                        col_4(
                          sliderInput(
                            ns(paste0("initial_yes__autonomous_cars__government__", country)),
                            label = "Government", min = 0, max = 1, value = 0.5
                          )
                        )
                      ),
                      hr(),
                      h5("Product: Sustainable transport"),
                      fluidRow(
                        col_4( # autonomous_cars, sustainable_transport, cooperative_self_generation
                          sliderInput(
                            ns(paste0("initial_yes__sustainable_transport__citizens__", country)),
                            label = "Citizens", min = 0, max = 1, value = 0.5
                          )
                        ),
                        col_4(
                          sliderInput(
                            ns(paste0("initial_yes__sustainable_transport__business__", country)),
                            label = "Business", min = 0, max = 1, value = 0.5
                          )
                        ),
                        col_4(
                          sliderInput(
                            ns(paste0("initial_yes__sustainable_transport__government__", country)),
                            label = "Government", min = 0, max = 1, value = 0.5
                          )
                        )
                      ),
                      hr(),
                      h5("Product: Cooperative self-generation"),
                      fluidRow(
                        col_4(
                          sliderInput(
                            ns(paste0("initial_yes__cooperative_self_generation__citizens__", country)),
                            label = "Citizens", min = 0, max = 1, value = 0.5
                          )
                        ),
                        col_4(
                          sliderInput(
                            ns(paste0("initial_yes__cooperative_self_generation__business__", country)),
                            label = "Business", min = 0, max = 1, value = 0.5
                          )
                        ),
                        col_4(
                          sliderInput(
                            ns(paste0("initial_yes__cooperative_self_generation__government__", country)),
                            label = "Government", min = 0, max = 1, value = 0.5
                          )
                        )
                      )
                    ),
                    col_1()
                  )
                )
              }
            )
          )
        ),
        tabPanel(
          title = "Intention weights",
          bs4Dash::tabsetPanel(
            vertical = TRUE,
            .list = lapply(
              names(get_text("params", "survey_topic", "intention_weight")),
              function(category) {
                lst <- get_text("params", "survey_topic", "intention_weight")
                name <- category
                category <- lst[name]
                tabPanel(
                  title = name,
                  div(
                    style = "margin: 20px;",
                    h4(tags$b("Topic:"), name)
                  ),
                  fluidRow(
                    col_1(),
                    col_10(
                      h5("Product: Autonomous cars"),
                      fluidRow(
                        col_4(
                          sliderInput(
                            ns(paste0("intention_weight__autonomous_cars__citizens__EU__", category)),
                            label = "Citizens", min = 0, max = 1, value = 0.5
                          )
                        ),
                        col_4(
                          sliderInput(
                            ns(paste0("intention_weight__autonomous_cars__business__EU__", category)),
                            label = "Business", min = 0, max = 1, value = 0.5
                          )
                        ),
                        col_4(
                          sliderInput(
                            ns(paste0("intention_weight__autonomous_cars__government__EU__", category)),
                            label = "Government", min = 0, max = 1, value = 0.5
                          )
                        )
                      ),
                      hr(),
                      h5("Product: Sustainable transport"),
                      fluidRow(
                        col_4( # autonomous_cars, sustainable_transport, cooperative_self_generation
                          sliderInput(
                            ns(paste0("intention_weight__sustainable_transport__citizens__EU__", category)),
                            label = "Citizens", min = 0, max = 1, value = 0.5
                          )
                        ),
                        col_4(
                          sliderInput(
                            ns(paste0("intention_weight__sustainable_transport__business__EU__", category)),
                            label = "Business", min = 0, max = 1, value = 0.5
                          )
                        ),
                        col_4(
                          sliderInput(
                            ns(paste0("intention_weight__sustainable_transport__government__EU__", category)),
                            label = "Government", min = 0, max = 1, value = 0.5
                          )
                        )
                      ),
                      hr(),
                      h5("Product: Cooperative self-generation"),
                      fluidRow(
                        col_4(
                          sliderInput(
                            ns(paste0("intention_weight__cooperative_self_generation__citizens__EU__", category)),
                            label = "Citizens", min = 0, max = 1, value = 0.5
                          )
                        ),
                        col_4(
                          sliderInput(
                            ns(paste0("intention_weight__cooperative_self_generation__business__EU__", category)),
                            label = "Business", min = 0, max = 1, value = 0.5
                          )
                        ),
                        col_4(
                          sliderInput(
                            ns(paste0("intention_weight__cooperative_self_generation__government__EU__", category)),
                            label = "Government", min = 0, max = 1, value = 0.5
                          )
                        )
                      )
                    ),
                    col_1()
                  )
                )
              }
            )
          )
        ),
        tabPanel(
          title = "Survey topics",
          bs4Dash::tabsetPanel(
            vertical = TRUE,
            .list = lapply(
              get_text("params", "countries"),
              function(country) {
                tabPanel(
                  title = country,
                  bs4Dash::tabsetPanel(
                    .list = lapply(
                      names(get_text("params", "survey_topic", "survey_topic")),
                      function(category) {
                        lst <- get_text("params", "survey_topic", "survey_topic")
                        name <- category
                        category <- lst[name]
                        tabPanel(
                          title = name,
                          div(
                            style = "margin: 20px;",
                            tags$table(
                              tags$tr(
                                tags$td(h4(tags$b("Country:"))),
                                tags$td(" "),
                                tags$td(h4(country))
                              ),
                              tags$tr(
                                tags$td(h4(tags$b("Topic:"))),
                                tags$td(" "),
                                tags$td(h4(name))
                              )
                            )
                          ),
                          bs4Dash::accordion(
                            id = ns(sprintf("survey_topic_adoptleave_%s_%s", country, category)),
                            bs4Dash::accordionItem(
                              collapsed = FALSE,
                              title = "Product decision: Adopt",
                              fluidRow(
                                col_1(),
                                col_10(
                                  h5("Product: Autonomous cars"),
                                  fluidRow(
                                    col_4(
                                      sliderInput(
                                        ns(sprintf("survey_topic__autonomous_cars__citizens__%s__%s__adopt", country, category)),
                                        label = "Citizens", min = 0, max = 1, value = 0.5
                                      )
                                    ),
                                    col_4(
                                      sliderInput(
                                        ns(sprintf("survey_topic__autonomous_cars__business__%s__%s__adopt", country, category)),
                                        label = "Business", min = 0, max = 1, value = 0.5
                                      )
                                    ),
                                    col_4(
                                      sliderInput(
                                        ns(sprintf("survey_topic__autonomous_cars__government__%s__%s__adopt", country, category)),
                                        label = "Government", min = 0, max = 1, value = 0.5
                                      )
                                    )
                                  ),
                                  hr(),
                                  h5("Product: Sustainable transport"),
                                  fluidRow(
                                    col_4( # autonomous_cars, sustainable_transport, cooperative_self_generation
                                      sliderInput(
                                        ns(sprintf("survey_topic__sustainable_transport__citizens__%s__%s__adopt", country, category)),
                                        label = "Citizens", min = 0, max = 1, value = 0.5
                                      )
                                    ),
                                    col_4(
                                      sliderInput(
                                        ns(sprintf("survey_topic__sustainable_transport__business__%s__%s__adopt", country, category)),
                                        label = "Business", min = 0, max = 1, value = 0.5
                                      )
                                    ),
                                    col_4(
                                      sliderInput(
                                        ns(sprintf("survey_topic__sustainable_transport__government__%s__%s__adopt", country, category)),
                                        label = "Government", min = 0, max = 1, value = 0.5
                                      )
                                    )
                                  ),
                                  hr(),
                                  h5("Product: Cooperative self-generation"),
                                  fluidRow(
                                    col_4( # autonomous_cars, sustainable_transport, cooperative_self_generation
                                      sliderInput(
                                        ns(sprintf("survey_topic__cooperative_self_generation__citizens__%s__%s__adopt", country, category)),
                                        label = "Citizens", min = 0, max = 1, value = 0.5
                                      )
                                    ),
                                    col_4(
                                      sliderInput(
                                        ns(sprintf("survey_topic__cooperative_self_generation__business__%s__%s__adopt", country, category)),
                                        label = "Business", min = 0, max = 1, value = 0.5
                                      )
                                    ),
                                    col_4(
                                      sliderInput(
                                        ns(sprintf("survey_topic__cooperative_self_generation__government__%s__%s__adopt", country, category)),
                                        label = "Government", min = 0, max = 1, value = 0.5
                                      )
                                    )
                                  )
                                ),
                                col_1()
                              )
                            ),
                            bs4Dash::accordionItem(
                              title = "Product decision: Leave",
                              fluidRow(
                                col_1(),
                                col_10(
                                  h5("Product: Autonomous cars"),
                                  fluidRow(
                                    col_4(
                                      sliderInput(
                                        ns(sprintf("survey_topic__autonomous_cars__citizens__%s__%s__leave", country, category)),
                                        label = "Citizens", min = 0, max = 1, value = 0.5
                                      )
                                    ),
                                    col_4(
                                      sliderInput(
                                        ns(sprintf("survey_topic__autonomous_cars__business__%s__%s__leave", country, category)),
                                        label = "Business", min = 0, max = 1, value = 0.5
                                      )
                                    ),
                                    col_4(
                                      sliderInput(
                                        ns(sprintf("survey_topic__autonomous_cars__government__%s__%s__leave", country, category)),
                                        label = "Government", min = 0, max = 1, value = 0.5
                                      )
                                    )
                                  ),
                                  hr(),
                                  h5("Product: Sustainable transport"),
                                  fluidRow(
                                    col_4( # autonomous_cars, sustainable_transport, cooperative_self_generation
                                      sliderInput(
                                        ns(sprintf("survey_topic__sustainable_transport__citizens__%s__%s__leave", country, category)),
                                        label = "Citizens", min = 0, max = 1, value = 0.5
                                      )
                                    ),
                                    col_4(
                                      sliderInput(
                                        ns(sprintf("survey_topic__sustainable_transport__business__%s__%s__leave", country, category)),
                                        label = "Business", min = 0, max = 1, value = 0.5
                                      )
                                    ),
                                    col_4(
                                      sliderInput(
                                        ns(sprintf("survey_topic__sustainable_transport__government__%s__%s__leave", country, category)),
                                        label = "Government", min = 0, max = 1, value = 0.5
                                      )
                                    )
                                  ),
                                  hr(),
                                  h5("Product: Cooperative self-generation"),
                                  fluidRow(
                                    col_4( # autonomous_cars, sustainable_transport, cooperative_self_generation
                                      sliderInput(
                                        ns(sprintf("survey_topic__cooperative_self_generation__citizens__%s__%s__leave", country, category)),
                                        label = "Citizens", min = 0, max = 1, value = 0.5
                                      )
                                    ),
                                    col_4(
                                      sliderInput(
                                        ns(sprintf("survey_topic__cooperative_self_generation__business__%s__%s__leave", country, category)),
                                        label = "Business", min = 0, max = 1, value = 0.5
                                      )
                                    ),
                                    col_4(
                                      sliderInput(
                                        ns(sprintf("survey_topic__cooperative_self_generation__government__%s__%s__leave", country, category)),
                                        label = "Government", min = 0, max = 1, value = 0.5
                                      )
                                    )
                                  )
                                ),
                                col_1()
                              )
                            )
                          )
                        )
                      }
                    )
                  )
                )
              }
            )
          )
        )
      )
    )
  )
}


mod_stakeholder_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    browser()
    observe({
    })
  })
}

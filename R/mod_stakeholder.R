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
        id = ns("control"),
        width = 12,
        type = "tabs",
        side = "right",
        title = "Control parameters",
        solidHeader = FALSE,
        status = "primary",
        tabPanel(
          title = "Initial yes",
          mod_stakeholder_initialyes_ui(ns("initialyes"), get_text)
        ),
        tabPanel(
          title = "Intention weights",
          mod_stakeholder_intentionweight_ui(ns("intentionweight"), get_text)
        ),
        tabPanel(
          title = "Survey topic",
          mod_stakeholder_surveytopic_ui(ns("surveytopic"), get_text)
        )
      )
    )
  )
}


mod_stakeholder_server <- function(id, changed) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    get_text <- dispatch_to_txt(ns(NULL))

    observe({
      if (identical(input$control, "Initial yes")) {
        mod_stakeholder_initialyes_server("initialyes", get_text, changed)
      } else if (identical(input$control, "Intention weights")) {
        mod_stakeholder_intentionweight_server("intentionweight", get_text)
      } else if (identical(input$control, "Survey topic")) {
        mod_stakeholder_surveytopic_server("surveytopic", get_text)
      }
    }) %>%
      bindEvent(input$control)
  })
}



mod_stakeholder_initialyes_ui <- function(id, get_text) {
  ns <- NS(id)

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
          div(
            style = "min-height: 600px;",
            htmlOutput(ns(sprintf(
              "control__%s",
              country
            )))
          )
        )
      }
    )
  )
}


mod_stakeholder_intentionweight_ui <- function(id, get_text) {
  ns <- NS(id)

  bs4Dash::tabsetPanel(
    vertical = TRUE,
    .list = lapply(
      names(get_text("params", "survey_topic", "intention_weight")),
      function(category) {
        lst <- get_text("params", "survey_topic", "intention_weight")
        tabPanel(
          title = category,
          div(
            style = "margin: 20px;",
            tags$table(
              tags$tr(
                tags$td(h4(tags$b("Country:"))),
                tags$td(" "),
                tags$td(h4("EU"))
              ),
              tags$tr(
                tags$td(h4(tags$b("Topic:"))),
                tags$td(" "),
                tags$td(h4(category))
              )
            )
          ),
          div(
            style = "min-height: 600px;",
            htmlOutput(ns(sprintf(
              "control__%s",
              lst[[category]]
            )))
          )
        )
      }
    )
  )
}


mod_stakeholder_surveytopic_ui <- function(id, get_text) {
  ns <- NS(id)

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
                tabPanel(
                  title = category,
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
                        tags$td(h4(category))
                      )
                    )
                  ),
                  bs4Dash::accordion(
                    id = ns(sprintf(
                      "adoptleave_%s_%s",
                      country, lst[[category]]
                    )),
                    bs4Dash::accordionItem(
                      title = "Product decision: Adopt",
                      collapsed = FALSE,
                      htmlOutput(ns(sprintf(
                        "control__%s__%s__adopt",
                        country, lst[[category]]
                      )))
                    ),
                    bs4Dash::accordionItem(
                      title = "Product decision: Leave",
                      collapsed = TRUE,
                      htmlOutput(ns(sprintf(
                        "control__%s__%s__leave",
                        country, lst[[category]]
                      )))
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
}



mod_stakeholder_initialyes_server <- function(id, get_text, changed) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    lapply(get_text("params", "countries"), function(country) {
      out_id <- paste0("control__", country)
      output[[out_id]] <- renderUI({
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
            h5("Product: Sustainable transport"),
            fluidRow(
              col_4(
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
      })
    })

    platypus <- reactive({
      req(identical(isolate(get_tab()), "stakeholder"))
      if (FALSE) {
        reticulate::import("pLAtYpus_TNO")$GRETA_tool
      } else {
        reticulate::import_from_path("GRETA_tool", app_sys("extdata/pLAtYpus_TNO"))
      }
    })

    observe({
      changed <- changed()
      req(startsWith(changed, ns("initial_yes")))
      plat <- platypus()
      changed <- substr(changed, nchar(ns(NULL)) + 2, nchar(changed))
      product <- strsplit(changed, "__")[[1]][2]
      parameters <- plat$cook$parameters_from_TOML(
        app_sys("extdata/pLAtYpus_TNO/pLAtYpus.toml")
      )
      browser()
      # plat$update_from_slider(changed, input[[changed]], parameters)
      # plat$get_output_tables(product, parameters)
    }) %>%
      bindEvent(changed(), ignoreInit = TRUE)
  })
}


mod_stakeholder_intentionweight_server <- function(id, get_text) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    iw_topics <- get_text("params", "survey_topic", "intention_weight")
    lapply(iw_topics, function(category) {
      out_id <- paste0("control__", category)
      output[[out_id]] <- renderUI(with_eval_args({
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
              col_4(
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
      }))
    })
  })
}


mod_stakeholder_surveytopic_server <- function(id, get_text) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    st_topics <- get_text("params", "survey_topic", "survey_topic")
    countries <- get_text("params", "countries")
    decisions <- c("adopt", "leave")
    lapply(countries, function(country) {
      lapply(st_topics, function(topic) {
        lapply(decisions, function(decision) {
          out_id <- sprintf("control__%s__%s__%s", country, topic, decision)
          output[[out_id]] <- renderUI(with_eval_args({
            fluidRow(
              col_1(),
              col_10(
                h5("Product: Autonomous cars"),
                fluidRow(
                  col_4(
                    sliderInput(
                      ns(sprintf("survey_topic__autonomous_cars__citizens__%s__%s__adopt", country, topic)),
                      label = "Citizens", min = 0, max = 1, value = 0.5
                    )
                  ),
                  col_4(
                    sliderInput(
                      ns(sprintf("survey_topic__autonomous_cars__business__%s__%s__adopt", country, topic)),
                      label = "Business", min = 0, max = 1, value = 0.5
                    )
                  ),
                  col_4(
                    sliderInput(
                      ns(sprintf("survey_topic__autonomous_cars__government__%s__%s__adopt", country, topic)),
                      label = "Government", min = 0, max = 1, value = 0.5
                    )
                  )
                ),
                hr(),
                h5("Product: Sustainable transport"),
                fluidRow(
                  col_4(
                    sliderInput(
                      ns(sprintf("survey_topic__sustainable_transport__citizens__%s__%s__adopt", country, topic)),
                      label = "Citizens", min = 0, max = 1, value = 0.5
                    )
                  ),
                  col_4(
                    sliderInput(
                      ns(sprintf("survey_topic__sustainable_transport__business__%s__%s__adopt", country, topic)),
                      label = "Business", min = 0, max = 1, value = 0.5
                    )
                  ),
                  col_4(
                    sliderInput(
                      ns(sprintf("survey_topic__sustainable_transport__government__%s__%s__adopt", country, topic)),
                      label = "Government", min = 0, max = 1, value = 0.5
                    )
                  )
                ),
                hr(),
                h5("Product: Cooperative self-generation"),
                fluidRow(
                  col_4(
                    sliderInput(
                      ns(sprintf("survey_topic__cooperative_self_generation__citizens__%s__%s__adopt", country, topic)),
                      label = "Citizens", min = 0, max = 1, value = 0.5
                    )
                  ),
                  col_4(
                    sliderInput(
                      ns(sprintf("survey_topic__cooperative_self_generation__business__%s__%s__adopt", country, topic)),
                      label = "Business", min = 0, max = 1, value = 0.5
                    )
                  ),
                  col_4(
                    sliderInput(
                      ns(sprintf("survey_topic__cooperative_self_generation__government__%s__%s__adopt", country, topic)),
                      label = "Government", min = 0, max = 1, value = 0.5
                    )
                  )
                )
              ),
              col_1()
            )
          }))
        })
      })
    })
  })
}

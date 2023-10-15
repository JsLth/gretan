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
    tags$style("
      #main-stakeholder-control .irs-min {
        visibility: hidden !important;
      }
      
      #main-stakeholder-control .js-irs-0 .irs .irs-min:after {
        content: '0' !important;
      }
      
      #main-stakeholder-control .irs-min:after {
        visibility: visibile !important;
        display: block;
        background: rgba(0, 0, 0, 0.1) none repeat scroll 0 0;
        border-radius: 3px;
        color: #333;
        font-size: 10px;
        line-height: 1.333;
        padding: 1px 3px;
        text-shadow: none;
        top: 0;
        cursor: default;
        display: block;
        left: 0;
        position: absolute;
      }
    "),
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
      bs4Dash::box(
        title = "Engagement over time",
        id = ns("results"),
        status = "primary",
        width = 6,
        class = "tight-map-box",
        style = "margin-bottom: -10px;",
        plotly::plotlyOutput(ns("plot"), height = 600, width = "100%"),
        waiter::waiter_hide_on_render(ns("plot")),
        leafletPanel(
          ns("plot-control"),
          title = "Control",
          position = "bottomright",
          width = "300px",
          right = "100px",
          top = "250px",
          shinyWidgets::pickerInput(
            ns("plot_control_product"),
            label = "Pick a product",
            choices = get_text("params", "product")
          ),
          shinyWidgets::pickerInput(
            ns("plot_control_country"),
            label = "Pick a country",
            choices = setdiff(get_text("params", "countries"), "EU")
          ),
          shinyWidgets::pickerInput(
            ns("plot_control_stakeholder"),
            label = "Pick a stakeholder",
            choices = get_text("params", "stakeholder")
          )
        )
      ),
      bs4Dash::box(
        title = "Engagement by country",
        id = ns("results"),
        status = "primary",
        width = 6,
        class = "tight-map-box",
        # style = "margin-bottom: -10px;",
        leaflet::leafletOutput(ns("map"), height = 600, width = "100%"),
        waiter::waiter_hide_on_render(ns("map"))
      )
    ),
    fluidRow(
      bs4Dash::tabBox(
        id = ns("control"),
        width = 12,
        type = "tabs",
        side = "right",
        title = "Control parameters",
        footer = bs4Dash::actionButton(
          ns("reset"),
          label = "Reset sliders",
          icon = icon("refresh"),
          style = "float: right;"
        ),
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


mod_stakeholder_server <- function(id, tab) {
  moduleServer(id, function(input, output, session) {
    # Server setup ----
    ns <- session$ns
    get_text <- dispatch_to_txt(ns(NULL))
    reset <- reactiveVal(FALSE)
    changed <- reactive(input$changed)
    init_ctrl <- rep(FALSE, 3)

    init_waiter <- waiter::Waiter$new(
      id = ns(c("plot", "map")),
      html = tagList(waiter::spin_pulse(), h4("Updating database...")),
      color = "rgba(179, 221, 254, 1)"
    )
    
    pwaiter <- waiter::Waiter$new(
      id = ns("plot"),
      html = tagList(waiter::spin_pulse(), h4("Loading figure...")),
      color = "rgba(179, 221, 254, 1)"
    )
    
    mwaiter <- waiter::Waiter$new(
      id = ns("map"),
      html = tagList(waiter::spin_pulse(), h4("Loading figure...")),
      color = "rgba(179, 221, 254, 1)"
    )

    # Import platypus ----
    platypus <- reactive({
      reticulate::import("pLAtYpus_TNO", convert = FALSE)$GRETA_tool
    })

    # Load parameters ----
    parameters <- reactive({
      plat <- platypus()
      params <- plat$cook$parameters_from_TOML(
        app_sys("extdata/stakeholder/pLAtYpus.toml")
      )
      input_dir <- app_sys("extdata/stakeholder/input")
      output_dir <- app_sys("extdata/stakeholder/output")
      params$files$output_folder <- output_dir
      params$survey$data$output$output_folder <- input_dir
      params$survey$data$source$source_folder <- input_dir
      if (!dir.exists(input_dir)) dir.create(input_dir)
      if (!dir.exists(output_dir)) dir.create(output_dir)
      params
    })

    # Handle reset ----
    observe({
      init_waiter$show()
      on.exit(init_waiter$hide())
      log_it("Resetting database to survey")
      reticulate::py_capture_output(platypus()$reset_to_survey(parameters()))
      init_ctrl <<- rep(FALSE, 3)
      reset(TRUE)
    }) %>%
      bindEvent(input$reset)
    
    observe({
      req(identical(tab(), "stakeholder"))
      shinyjs::click("reset")
    }) %>%
      bindEvent(tab())

    # Dispatch to submodules ----
    observe({
      if (identical(input$control, "Initial yes") && !init_ctrl[1]) {
        mod_stakeholder_initialyes_server(
          "initialyes",
          get_text = get_text,
          changed = changed,
          plat = platypus,
          params = parameters
        )
        init_ctrl[1] <<- TRUE
      } else if (identical(input$control, "Intention weights") && !init_ctrl[2]) {
        mod_stakeholder_intentionweight_server(
          "intentionweight",
          get_text,
          changed = changed,
          plat = platypus,
          params = parameters
        )
        init_ctrl[2] <<- TRUE
      } else if (identical(input$control, "Survey topic") && !init_ctrl[3]) {
        mod_stakeholder_surveytopic_server(
          "surveytopic",
          get_text,
          changed = changed,
          plat = platypus,
          params = parameters
        )
        init_ctrl[3] <<- TRUE
      }
    }) %>%
      bindEvent(input$control, reset())


    # Collect output tables ----
    tables <- reactive({
      plat <- isolate(platypus())
      reticulate::py_capture_output(tables <- plat$get_output_tables(
        input$plot_control_product,
        parameters()
      ))
      tables <- reticulate::py_to_r(tables)
      list(aggr = tables[[1]], by_country = tables[[2]])
    })


    # Render engagement plot ----
    init_plot <- FALSE
    output$plot <- plotly::renderPlotly({
      allowed <- c(
        "initialyes-initial_yes", "intentionweight-intention_weight",
        "surveytopic-survey_topic",
        "plot_control_country", "plot_control_product", "reset"
      )

      is_valid_input <- any(startsWith(changed(), ns(allowed)))
      is_init <- !identical(input[[rm_ns(changed(), ns)]], "N/A")
      is_reset <- isTRUE(reset())
      print(changed())
      req((is_valid_input && is_init) || is_reset, cancelOutput = TRUE)
      
      pwaiter$show()

      tables <- execute_safely(tables())
      country <- input$plot_control_country
      table <- reticulate::py_to_r(tables[[2]][[input$plot_control_country]])
      names(table) <- c("Time", "Citizens", "Business", "Government")
      table <- cbind(Time = table$Time, stack(table, select = names(table)[-1]))
      names(table) <- c("Time", "Engagement level", "Stakeholder")

      p <- ggplot2::ggplot(data = table) +
        ggplot2::aes(
          x = Time,
          y = `Engagement level`,
          group = Stakeholder,
          color = Stakeholder
        ) +
        ggplot2::geom_line(size = 2) +
        ggplot2::labs(x = "Time", y = "Engagement level") +
        ggplot2::scale_color_viridis_d(
          name = "Stakeholder",
          labels = c("Citizens", "Business", "Government"),
          option = "E",
          begin = 0.7
        ) +
        ggplot2::theme_minimal(base_size = 15) +
        ggplot2::theme(
          legend.background = ggplot2::element_rect(
            fill = "white",
            colour = "white"
          )
        )

      plotly::ggplotly(p, tooltip = c("x", "y", "group")) %>%
        plotly::config(displayModeBar = FALSE) %>%
        plotly::layout(
          legend = list(x = 0.15, y = 0.2),
          xaxis = list(fixedrange = TRUE),
          yaxis = list(fixedrange = TRUE)
        )
    }) %>%
      bindEvent(
        changed(),
        reset(),
        input$plot_control_country,
        input$plot_control_product,
        input$plot_control_stakeholder
      )


    
    # Render engagement map ----
    output$map <- leaflet::renderLeaflet({
      allowed <- c(
        "initialyes-initial_yes", "intentionweight-intention_weight",
        "surveytopic-survey_topic",
        "plot_control_stakeholder", "plot_control_product"
      )
      
      is_valid_input <- any(startsWith(changed(), ns(allowed)))
      is_init <- !identical(input[[rm_ns(changed(), ns)]], "N/A")
      is_reset <- isTRUE(reset())
      req((is_valid_input && is_init) || is_reset, cancelOutput = TRUE)
      
      reset(FALSE)
      mwaiter$show()

      tables <- execute_safely(tables())
      table <- tables[[1]]
      table <- merge(
        table,
        srv_nuts0["nuts0"],
        by.x = "Country",
        by.y = "nuts0"
      )
      table <- sf::st_transform(sf::st_as_sf(table), 4326)

      stakeholder <- input$plot_control_stakeholder
      pal <- leaflet::colorNumeric("Oranges", domain = table[[stakeholder]])
      leaflet::leaflet(
        data = table,
        options = leaflet::leafletOptions(zoomSnap = 0.25)
      ) %>%
        leaflet::addTiles() %>%
        leaflet::setView(lng = 8, lat = 55, zoom = 3.5) %>%
        leaflet::addPolygons(
          fillColor = ~ pal(table[[stakeholder]]),
          fillOpacity = 0.7,
          weight = 1,
          color = "black",
          opacity = 0.5,
          label = align_in_table(
            Country = table$Country,
            Engagement = round(table[[stakeholder]], 2)
          ),
          highlightOptions = highlight_opts
        ) %>%
        leaflet::addLegend(
          position = "bottomright",
          na.label = "No data",
          pal = pal,
          values = table[[stakeholder]],
          opacity = 0.9,
          title = "Long term<br>engagement"
        )
    }) %>%
      bindEvent(
        changed(),
        reset(),
        input$plot_control_country,
        input$plot_control_product,
        input$plot_control_stakeholder
      )

    outputOptions(output, "map", suspendWhenHidden = FALSE, priority = 0)
    outputOptions(output, "plot", suspendWhenHidden = FALSE, priority = 1)
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



mod_stakeholder_initialyes_server <- function(id,
                                              get_text,
                                              changed,
                                              plat,
                                              params) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    lapply(get_text("params", "countries"), function(country) {
      out_id <- paste0("control__", country)
      products <- get_text("params", "product")
      stakeholders <- get_text("params", "stakeholder")
      input_ids <- lapply(products, function(p) {
        lapply(stakeholders, function(s) {
          id <- ns(sprintf("initial_yes__%s__%s__%s", p, s, country))
          id
        })
      })
      output[[out_id]] <- renderUI({
        fluidRow(
          col_1(),
          col_10(
            h5("Product: Autonomous cars"),
            fluidRow(
              col_4(interactionSlider(input_ids[[1]][[1]])),
              col_4(interactionSlider(input_ids[[1]][[2]])),
              col_4(interactionSlider(input_ids[[1]][[3]]))
            ),
            h5("Product: Sustainable transport"),
            fluidRow(
              col_4(interactionSlider(input_ids[[2]][[1]])),
              col_4(interactionSlider(input_ids[[2]][[2]])),
              col_4(interactionSlider(input_ids[[2]][[3]]))
            ),
            hr(),
            h5("Product: Cooperative self-generation"),
            fluidRow(
              col_4(interactionSlider(input_ids[[3]][[1]])),
              col_4(interactionSlider(input_ids[[3]][[2]])),
              col_4(interactionSlider(input_ids[[3]][[3]]))
            )
          ),
          col_1()
        )
      })
    })


    observe({
      changed <- changed()
      req(startsWith(changed, ns("initial_yes")))
      is_init <- !"N/A" %in% input[[rm_ns(changed, ns)]]
      req(is_init)
      plat <- isolate(plat())
      params <- isolate(params())
      changed <- rm_ns(changed, ns)
      reticulate::py_capture_output(
        plat$update_from_slider(changed, input[[changed]], params())
      )
    }) %>%
      bindEvent(changed())
  })
}


mod_stakeholder_intentionweight_server <- function(id,
                                                   get_text,
                                                   changed,
                                                   plat,
                                                   params) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    iw_topics <- get_text("params", "survey_topic", "intention_weight")
    lapply(iw_topics, function(category) {
      out_id <- paste0("control__", category)
      products <- get_text("params", "product")
      stakeholders <- get_text("params", "stakeholder")
      input_ids <- lapply(products, function(p) {
        lapply(stakeholders, function(s) {
          id <- ns(sprintf("intention_weight__%s__%s__EU__%s", p, s, category))
          id
        })
      })
      output[[out_id]] <- renderUI({
        fluidRow(
          col_1(),
          col_10(
            h5("Product: Autonomous cars"),
            fluidRow(
              col_4(interactionSlider(input_ids[[1]][[1]])),
              col_4(interactionSlider(input_ids[[1]][[2]])),
              col_4(interactionSlider(input_ids[[1]][[3]]))
            ),
            hr(),
            h5("Product: Sustainable transport"),
            fluidRow(
              col_4(interactionSlider(input_ids[[1]][[1]])),
              col_4(interactionSlider(input_ids[[2]][[2]])),
              col_4(interactionSlider(input_ids[[2]][[3]]))
            ),
            hr(),
            h5("Product: Cooperative self-generation"),
            fluidRow(
              col_4(interactionSlider(input_ids[[3]][[1]])),
              col_4(interactionSlider(input_ids[[3]][[2]])),
              col_4(interactionSlider(input_ids[[3]][[3]]))
            )
          ),
          col_1()
        )
      })
    })


    observe({
      changed <- changed()
      req(startsWith(changed, ns("intention_weight")))
      is_init <- !"N/A" %in% input[[rm_ns(changed, ns)]]
      req(is_init)
      plat <- isolate(plat())
      params <- isolate(params())
      changed <- substr(changed, nchar(ns(NULL)) + 2, nchar(changed))
      reticulate::py_capture_output(
        plat$update_from_slider(changed, input[[changed]], params())
      )
    }) %>%
      bindEvent(changed())
  })
}


mod_stakeholder_surveytopic_server <- function(id,
                                               get_text,
                                               changed,
                                               plat,
                                               params) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    st_topics <- get_text("params", "survey_topic", "survey_topic")
    countries <- get_text("params", "countries")
    decisions <- c("adopt", "leave")
    lapply(countries, function(country) {
      lapply(st_topics, function(topic) {
        lapply(decisions, function(decision) {
          out_id <- sprintf("control__%s__%s__%s", country, topic, decision)
          products <- get_text("params", "product")
          stakeholders <- get_text("params", "stakeholder")
          input_ids <- lapply(products, function(p) {
            lapply(stakeholders, function(s) {
              id <- ns(sprintf(
                "survey_topic__%s__%s__%s__%s__%s",
                p, s, country, topic, decision
              ))
              id
            })
          })
          output[[out_id]] <- renderUI({
            fluidRow(
              col_1(),
              col_10(
                h5("Product: Autonomous cars"),
                fluidRow(
                  col_4(interactionSlider(input_ids[[1]][[1]])),
                  col_4(interactionSlider(input_ids[[1]][[2]])),
                  col_4(interactionSlider(input_ids[[1]][[3]])),
                ),
                hr(),
                h5("Product: Sustainable transport"),
                fluidRow(
                  col_4(interactionSlider(input_ids[[2]][[1]])),
                  col_4(interactionSlider(input_ids[[2]][[2]])),
                  col_4(interactionSlider(input_ids[[2]][[3]])),
                ),
                hr(),
                h5("Product: Cooperative self-generation"),
                fluidRow(
                  col_4(interactionSlider(input_ids[[3]][[1]])),
                  col_4(interactionSlider(input_ids[[3]][[2]])),
                  col_4(interactionSlider(input_ids[[3]][[3]])),
                )
              ),
              col_1()
            )
          })
        })
      })
    })


    observe({
      changed <- changed()
      req(startsWith(changed, ns("surveytopic")))
      is_init <- !"N/A" %in% input[[rm_ns(changed, ns)]]
      req(is_init)
      plat <- isolate(plat())
      params <- isolate(params())
      changed <- substr(changed, nchar(ns(NULL)) + 2, nchar(changed))
      reticulate::py_capture_output(
        plat$update_from_slider(changed, input[[changed]], params())
      )
    }) %>%
      bindEvent(changed())
  })
}

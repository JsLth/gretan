mod_persona_ui <- function(id) {
  ns <- NS(id)
  
  get_text <- dispatch_to_txt(id)
  
  persona_step <- function(..., step, format = TRUE) {
    shinyjs::hidden(div(
      id = ns(paste0("step", step)),
      class = if (format) "persona-page",
      ...
    ))
  }

  bs4Dash::tabItem(
    "persona",
    make_header(
      title = get_text("title"),
      authors = get_text("authors"),
      affil = get_text("affil"),
      date = get_text("date")
    ),
    fluidRow(
      col_1(),
      col_10(
        div(
          id = ns("results"),
          bs4Dash::box(
            id = ns("typebox"),
            status = "primary",
            width = 12,
            headerBorder = FALSE,
            solidHeader = TRUE,
            collapsible = FALSE,
            boxToolSize = "lg",
            background = "primary",
            bs4Dash::appButton(
              label = "Reset results",
              inputId = ns("reset"),
              icon = icon("refresh"),
              color = NULL,
              class = "persona-reset"
            ),
            uiOutput(ns("type"))
          ),
          bs4Dash::box(
            id = ns("mapbox"),
            status = "primary",
            width = 12,
            height = 800,
            headerBorder = FALSE,
            collapsible = FALSE,
            boxToolSize = "lg",
            leaflet::leafletOutput(ns("map"), height = "100%"),
            leafletPanel(
              ns("map-info"),
              title = with_literata(get_text("results", "map_info", "title")),
              position = "topleft",
              p(get_text("results", "map_info", "content"))
            )
          )
        ),
        bs4Dash::box(
          id = ns("surveybox"),
          status = "primary",
          width = 12,
          height = 800,
          headerBorder = FALSE,
          collapsible = FALSE,
          boxToolSize = "lg",
          background = "primary",
          solidHeader = TRUE,
          persona_step(
            step = 1,
            format = FALSE,
            class = "jumbotron bg-primary",
            tags$h1(class = "display-4", get_text("steps", 1, "title")),
            tags$p(class = "lead", get_text("steps", 1, "desc")),
            tags$hr(class = "my-4"),
            shinyWidgets::prettyToggle(
              inputId = ns("consent"),
              label_on = get_text("steps", 1, "consent"),
              label_off = get_text("steps", 1, "consent"),
              outline = TRUE,
              plain = TRUE,
              bigger = TRUE,
              animation = "smooth",
              status_on = "success",
              status_off = "info",
              icon_on = icon("square-check"),
              icon_off = icon("square")
            )
          ),
          persona_step(
            step = 2,
            h2("Question 1"),
            shinyWidgets::awesomeRadio(
              ns("question1"),
              label = get_text("steps", 2, "question"),
              choices = get_text("steps", 2, "choices"),
              status = "info"
            )
          ),
          persona_step(
            step = 3,
            h2("Question 2"),
            shinyWidgets::awesomeRadio(
              ns("question2"),
              label = get_text("steps", 3, "question"),
              choices = get_text("steps", 3, "choices"),
              status = "info"
            )
          ),
          persona_step(
            step = 4,
            h2("Question 3"),
            shinyWidgets::awesomeRadio(
              ns("question3"),
              label = get_text("steps", 4, "question"),
              choices = get_text("steps", 4, "choices"),
              status = "info"
            )
          ),
          persona_step(
            step = 5,
            h2("Question 4"),
            shinyWidgets::awesomeRadio(
              ns("question4"),
              label = get_text("steps", 5, "question"),
              choices = get_text("steps", 5, "choices"),
              status = "info"
            )
          ),
          persona_step(
            step = 6,
            h2("Question 5"),
            shinyWidgets::awesomeRadio(
              ns("question5"),
              label = get_text("steps", 6, "question"),
              choices = get_text("steps", 6, "choices"),
              status = "info"
            )
          ),
          persona_step(
            step = 7,
            h2(get_text("steps", 7, "thanks")),
            p(get_text("steps", 7, "submit")),
            loadingButton(
              inputId = ns("submit"),
              label = "Submit!",
              icon = icon("wand-magic-sparkles"),
              status = "success",
              size = "lg"
            ),
            br(), br(),
            bs4Dash::infoBoxOutput(ns("required"))
          ),
          div(
            bs4Dash::actionButton(
              ns("prevBtn"),
              label = div(icon("caret-left"), "Previous"),
              size = "lg"
            ),
            style = "position: absolute; bottom: 5%; left: 5%;"
          ),
          div(
            shinyjs::disabled(bs4Dash::actionButton(
              ns("nextBtn"),
              label = div("Next", icon("caret-right")),
              size = "lg"
            )),
            style = "position: absolute; bottom: 5%; right: 5%;"
          )
        )
      ),
      col_1()
    )
  )
}



mod_persona_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    get_text <- dispatch_to_txt(session$ns(NULL))
    
    page <- reactiveVal(1)
    prev_page <- reactiveVal(NULL)
    log_cache <- reactiveVal(list(from = NULL, to = 1))
    ready <- reactiveVal(0)

    waitress <- waiter::Waitress$new(
      selector = paste0("#", ns("submit")),
      infinite = TRUE,
      theme = "line"#,
      #color = "rgba(179, 221, 254, 1)"
    )

    observe({
      shinyjs::toggleState(
        id = "prevBtn",
        condition = page() > 1
      )
      shinyjs::toggleState(
        id = "nextBtn",
        condition = page() < 7 && input$consent
      )
      shinyjs::toggleState(
        id = "submit",
        condition = all(has_answered())
      )
      shinyjs::hide(id = paste0("step", prev_page()))
      shinyjs::show(paste0("step", page()))

      pgs <- list(from = prev_page(), to = page())
      if (!identical(pgs, log_cache())) {
        log_cache(pgs)
        log_it(sprintf(
          "Switching from %s to %s",
          paste("step", prev_page()),
          paste("step", page())
        ))
      }
    })
    
    observe(log_it(paste0(print(ready()))))
    
    observe({
      shinyjs::toggleElement("surveybox", condition = ready() < 2)
      shinyjs::toggleElement("results", condition = ready() >= 2)
    })

    observe({
      shinyWidgets::updatePrettyToggle(inputId = ns("consent"), value = FALSE)

      for (i in seq(5)) {
        with_eval_args(shinyWidgets::updateAwesomeRadio(
          inputId = ns(paste0("question", i)),
          selected = "None selected"
        ))
      }
      
      session$sendCustomMessage(
        "resetLoadingButton",
        message = list(inputId = session$ns("submit"))
      )
      
      shinyjs::hide("results")
      shinyjs::show("surveybox")
      prev_page(page())
      page(1)
      ready(0)
    }) %>%
      bindEvent(input$reset)

    observe({
      prev_page(page())
      page(page() + 1)
    }) %>%
      bindEvent(input$nextBtn)

    observe({
      prev_page(page())
      page(page() - 1)
    }) %>%
      bindEvent(input$prevBtn)

    has_answered <- reactive({
      vapply(
        FUN.VALUE = logical(1),
        seq(5),
        function(i) !is.na(input[[paste0("question", i)]])
      ) %>%
        stats::setNames(paste0("question", seq(5)))
    })


    output$required <- renderUI({
      idx <- which(!has_answered())
      if (length(idx)) {
        div(
          tagAppendChildren(
            div(
              style = "margin-left: -7.5px; width: 400px;",
              class = "info-box bg-danger"
            ),
            span(class = "info-box-icon", icon("circle-exclamation")),
            div(
              style = "margin-left: 20px;",
              span(
                HTML("The following question need to be answered before proceeding:"),
                p(list_to_li(as.list(paste("Question", idx))))
              )
            )
          )
        )
      } else {
        div()
      }
    })
    
    model <- reactive(reticulate::py_load_object(system.file(
      "python/persona_model.pkl",
      package = "gretan"
    )))
    
    responses <- reactive(as.integer(c(
      input$question1, input$question2, input$question3,
      input$question4, input$question5
    )))
    
    results <- reactive({
      waitress$start()
      waitress$inc(10)
      
      # Load model
      model <- isolate(model())
      
      waitress$inc(30)
      
      # Reshape input data
      features <- responses() %>%
        reticulate::np_array() %>%
        reticulate::array_reshape(c(1, -1))
      
      waitress$inc(30)
      
      # Predict persona from input
      pred <- c(model$predict(features))
      
      # Extract and order personas
      top_personas <- which(order(pred, decreasing = TRUE) <= 3)
      top_prob <- pred[top_personas]
      top_order <- order(top_prob, decreasing = TRUE)
      top_personas <- top_personas[top_order]
      top_prob <- top_prob[top_order]
      
      waitress$inc(30)
      waitress$close()
      
      personas <- get_text("results", "personas")
      lapply(seq(3), function(i) {
        pers <- top_personas[i]
        prob <- top_prob[i]
        list(
          name = personas[[pers]]$name,
          p = round(prob, 4),
          desc = personas[[pers]]$desc,
          src = "www/personas/prototype.png"
        )
      })
    }) %>%
      bindEvent(input$submit)
    
    
    output$type <- renderUI({
      items <- lapply(results(), function(x) {
        bs4Dash::carouselItem(
          fluidRow(
            col_2(),
            col_5(
              div(
                h4(
                  sprintf("With a chance of %s %% you are...", x$p * 100),
                  style = "text-align: center;"
                ),
                h1(x$name, style = "text-align: center;"),
                p(x$desc, style = "margin-bottom: 50px; text-align: justify;"),
                style = "margin-top: auto; margin-bottom: auto; display: block;"
              )

            ),
            col_3(tags$img(
              src = x$src,
              style = "margin-left: auto; margin-right: auto; display:block;"
            )),
            col_2()
          )
        )
      })
      ready(ready() + 1)
      bs4Dash::carousel(
        id = "type-carousel",
        .list = items
      )
    }) %>%
      bindEvent(input$submit)
    
    
    clusters <- reactive(readRDS(system.file(
      "extdata/persona.rds",
      package = "greta"
    )))
    
    
    output$map <- leaflet::renderLeaflet({
      res <- responses()
      clus <- clusters()
      p <- leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::setView(lng = 9, lat = 55, zoom = 4) %>%
        leaflet::addPolygons(
          fill = FALSE,
          weight = 1,
          color = "black",
          opacity = 0.5,
          # label = params$labels,
          # highlightOptions = highlight_opts,
          group = "NUTS-0",
          data = sf::st_transform(srv_nuts0, 4326)
        ) %>%
        leaflet::addPolygons(
          fill = FALSE,
          weight = 1,
          color = "black",
          opacity = 0.5,
          # label = params$labels,
          # highlightOptions = highlight_opts,
          group = "NUTS-1",
          data = sf::st_transform(srv_nuts1, 4326)
        ) %>%
        leaflet::addPolygons(
          fill = FALSE,
          weight = 1,
          color = "black",
          opacity = 0.5,
          # label = params$labels,
          # highlightOptions = highlight_opts,
          group = "NUTS-2",
          data = sf::st_transform(srv_nuts2, 4326)
        ) %>%
        leaflet::addLayersControl(baseGroups = c("NUTS-2", "NUTS-1", "NUTS-0"))
      ready(ready() + 1)
      p
    }) %>%
      bindEvent(input$submit)

    outputOptions(output, "type", suspendWhenHidden = FALSE, priority = 1)
    outputOptions(output, "map", suspendWhenHidden = FALSE, priority = 2)
  })
}

mod_persona_ui <- function(id) {
  # UI setup ----
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
    # Header ----
    make_header(
      title = get_text("title"),
      authors = get_text("authors"),
      affil = get_text("affil"),
      date = get_text("date")
    ),
    
    fluidRow(
      col_1(),
      col_10(
        # Results container ----
        # Hidden until specified otherwise
        div(
          id = ns("results"),
          ## Persona results ----
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
          ## Mapped results ----
          bs4Dash::box(
            id = ns("mapbox"),
            status = "primary",
            width = 12,
            height = 800,
            headerBorder = FALSE,
            collapsible = FALSE,
            boxToolSize = "lg",
            leaflet::leafletOutput(ns("map"), height = "100%", width = "100%"),
            ### Info panel ----
            leafletPanel(
              ns("map-info"),
              title = with_literata(get_text("results", "map_info", "title")),
              position = "topleft",
              p(get_text("results", "map_info", "content"))
            ),
            ### Move panel ----
            leafletPanel(
              ns("map-move"),
              title = with_literata(get_text("results", "map_move", "title")),
              position = "bottomleft",
              bottom = "20px",
              left = "10px",
              p(get_text("results", "map_move", "content")),
              tags$table(
                tags$tr(
                  tags$td("Start"),
                  tags$td(style = "padding-left: 10px;"),
                  tags$td(div("None selected"), id = ns("start-placeholder")),
                  tags$td(uiOutput(ns("move_from")))
                ),
                tags$tr(
                  tags$td("Destination"),
                  tags$td(style = "padding-left: 10px;"),
                  tags$td(div("None selected"), id = ns("dest-placeholder")),
                  tags$td(uiOutput(ns("move_to")))
                )
              ),
              br(),
              bs4Dash::actionButton(
                ns("move_go"),
                label = "Go!",
                status = "success"
              )
            ),
            ### Control panel ----
            leafletPanel(
              ns("map-control"),
              title = "Map control",
              position = "topright",
              top = "20px",
              right = "10px",
              shinyWidgets::pickerInput(
                ns("aggr"),
                label = "Aggregation level",
                choices = list(
                  "Countries" = "nuts0",
                  "Major regions" = "nuts1",
                  "Minor regions" = "nuts2"
                ),
                selected = "nuts0"
              ),
              shinyWidgets::pickerInput(
                ns("item"),
                label = "Item",
                choices = list(
                  "Question 1" = "q1",
                  "Question 2" = "q2",
                  "Question 3" = "q3",
                  "Question 4" = "q4",
                  "Question 5" = "q5",
                  "Persona" = "cluster"
                )
              ),
              uiOutput(ns("item-text")),
              br(),
              shinyWidgets::pickerInput(
                ns("option"),
                label = "Option",
                choices = as.list(setNames(
                  seq(8),
                  names(get_text("steps", 2, "choices"))[-1]
                ))
              ),
              shinyWidgets::prettyToggle(
                ns("mode"),
                label_on = "Show as most popular",
                label_off = "Show as percentage",
                icon_on = icon("fire"),
                icon_off = icon("percent"),
                status_on = "warning",
                status_off = "warning",
                bigger = TRUE,
                value = TRUE
              )
            )
          )
        ),
        # Questionnaire container ----
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
          ## Consent ----
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
          ## Question 1 ----
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
          ## Question 2 ----
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
          ## Question 3 ----
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
          ## Question 4 ----
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
          ## Question 5 ----
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
          ## Submit ----
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
    # Server setup ----
    ns <- session$ns
    get_text <- dispatch_to_txt(session$ns(NULL))
    
    itemToIdx <- function(x) as.integer(substr(x, 2, 2))
    
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

    
    
    # Questionnaire ----
    ## Manage page flips ----
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
    }, label = "Page manager")
    
    
    ## Toggle containers ----
    observe({
      shinyjs::toggleElement("surveybox", condition = ready() < 2)
      shinyjs::toggleElement("results", condition = ready() >= 2)
    }, label = "Toggle containers")

    
    ## Reset questionnaire ----
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
    }, label = "Reset") %>%
      bindEvent(input$reset)

    
    ## Go one page forward ----
    observe({
      prev_page(page())
      page(page() + 1)
    }, label = "Page forward") %>%
      bindEvent(input$nextBtn)

    
    ## Go one page backward ----
    observe({
      prev_page(page())
      page(page() - 1)
    }, label = "Page backward") %>%
      bindEvent(input$prevBtn)

    
    ## Determine missing answers ----
    has_answered <- reactive({
      vapply(
        FUN.VALUE = logical(1),
        seq(5),
        function(i) !is.na(input[[paste0("question", i)]])
      ) %>%
        stats::setNames(paste0("question", seq(5)))
    }, label = "Response fail check")
    
    
    ## Render missing answers ----
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
    
    
    
    # Predict persona ----
    ## Load model ----
    model <- reactive(reticulate::py_load_object(system.file(
      "extdata/persona_model.pkl",
      package = "gretan"
    )), label = "Load persona_model.pkl")
    
    
    ## Collect responses ----
    responses <- reactive(as.integer(c(
      input$question1, input$question2, input$question3,
      input$question4, input$question5
    )), label = "Collect responses")
    
    
    ## Predict top 3 personas ----
    results <- reactive({
      execute_safely({
        waitress$start()
        
        # Load model
        model <- isolate(model())
        
        # Reshape input data
        features <- responses() %>%
          reticulate::np_array() %>%
          reticulate::array_reshape(c(1, -1))
        
        # Predict persona from input
        pred <- c(model$predict(features))
        
        # Extract and order personas
        top_personas <- which(order(pred, decreasing = TRUE) <= 3)
        top_prob <- pred[top_personas]
        top_order <- order(top_prob, decreasing = TRUE)
        top_personas <- top_personas[top_order]
        top_prob <- top_prob[top_order]
        
        waitress$close()
        Sys.sleep(0.2)
        
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
      })
    }, label = "Compute personas") %>%
      bindEvent(input$submit)
    
    
    ## Render persona carousel ----
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
    
    
    
    # Map personas ----
    ## Load persona data ----
    clusters <- reactive(readRDS(system.file(
      "extdata/persona.rds",
      package = "gretan"
    )), label = "Load persona.rds")
    
    
    ## Switch mode ----
    observe({
      is_mode <- isTRUE(input$mode)
      shinyjs::toggleElement("option", condition = !is_mode)
      
      if (!is_mode) {
        if (identical(input$item, "cluster")) {
          choices <- lapply(get_text("results", "personas"), "[[", "name")
          choices <- setNames(seq(8), choices)
        } else {
          idx <- itemToIdx(input$item) + 1
          choices <- setdiff(
            names(get_text("steps")[[idx]]$choices),
            "None selected"
          )
          choices <- setNames(seq_along(choices), choices)
        }
        
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "option",
          choices = choices
        )
      }
    }, label = "Switch mode/percentage") %>%
      bindEvent(input$mode, input$item)
    
    
    ## Render item ----
    output[["item-text"]] <- renderUI({
      if (identical(input$item, "cluster")) {
        HTML(paste(
          "<b>Persona:</b> The GRETA energy personas! Based on the",
          "questionnaire, our models show that you are likely similar to the",
          tags$b(results()[[1]]$name), "persona."
        ))
      } else {
        idx <- itemToIdx(input$item)
        question <- get_text("steps", idx + 1, "question")
        HTML(paste0(tags$b(paste("Question", idx)), ": ", question))
      }
    })
    
    
    ## Compute map parameters ----
    params <- reactive({
      execute_safely({
        clusters <- isolate(clusters())
        clusters <- clusters[[input$aggr]]
        
        is_dummy <- grepl("\\_[0-9]+$", names(clusters))
        if (isTRUE(input$mode)) {
          var <- input$item
          clusters <- clusters[!is_dummy]
          if (identical(input$item, "cluster")) {
            choices <- vapply(
              txts$main$persona$results$personas,
              "[[",
              "name",
              FUN.VALUE = character(1)
            )
            t <- try(choices[clusters[[var]]])
            if (inherits(t, "try-error")) browser()
            clusters[[var]] <- factor(
              choices[clusters[[var]]],
              levels = choices
            )
          } else {
            idx <- itemToIdx(input$item)
            choices <- names(get_text("steps", idx + 1, "choices"))[-1]
            choices <- choices[seq(length(choices))]
            clusters[[var]] <- choices[clusters[[var]]]
          }
        } else {
          var <- paste0(input$item, "_", input$option)
          clusters <- clusters[is_dummy | names(clusters) %in% c(
            "uuid", "nuts0", "nuts1", "nuts2"
          )]
          clusters[[var]] <- clusters[[var]] * 100
        }
        
        clusters <- clusters[c(
          var,
          if (input$aggr %in% c("nuts0", "nuts1", "nuts2")) "nuts0",
          if (input$aggr %in% c("nuts1", "nuts2")) "nuts1",
          if (input$aggr %in% "nuts2") "nuts2"
        )]
        
        if (isTRUE(input$mode)) {
          if (identical(input$item, "cluster")) {
            palette <- "Dark2"
            clusters[[var]] <- factor(clusters[[var]], levels = choices)
          } else {
            palette <- "YlGn"
            clusters[[var]] <- ordered(clusters[[var]], levels = choices)
          }
          pal <- leaflet::colorFactor(
            palette = palette,
            domain = clusters[[var]],
            ordered = TRUE
          )
          values <- levels(clusters[[var]])
        } else {
          pal <- leaflet::colorNumeric(
            palette = "Oranges",
            domain = clusters[[var]]
          )
          values <- clusters[[var]]
        }
        
        unit <- ifelse(isTRUE(input$mode), "", " %")
        lgd <- ifelse(isTRUE(input$mode), "Median", "Share")
        
        largs <- list(
          if ("nuts0" %in% names(clusters)) clusters[["nuts0"]],
          if ("nuts1" %in% names(clusters)) clusters[["nuts1"]],
          if ("nuts2" %in% names(clusters)) clusters[["nuts2"]],
          paste(clusters[[var]], unit)
        )
        largs <- setNames(largs, c("NUTS-0", "NUTS-1", "NUTS-2", lgd))
        labels <- do.call(align_in_table, largs)
        
        clusters <- sf::st_transform(clusters, 4326)
        
        list(
          data = clusters,
          var = var,
          pal = pal,
          labels = labels,
          unit = unit,
          lgd = lgd,
          values = values
        )
      })
    }, label = "Construct map paramters")
    
    
    ## Render Leaflet map ----
    output$map <- leaflet::renderLeaflet({
      params <- params()
      p <- leaflet::leaflet(params$data) %>%
        leaflet::addTiles() %>%
        leaflet::setView(lng = 9, lat = 55, zoom = 4) %>%
        leaflet::addPolygons(
          fillColor = stats::as.formula(paste0("~params$pal(", params$var, ")")),
          fillOpacity = 0.7,
          weight = 1,
          color = "black",
          opacity = 0.5,
          label = params$labels,
          highlightOptions = highlight_opts
        ) %>%
        leaflet::addLegend(
          position = "bottomright",
          pal = params$pal,
          values = params$values,
          opacity = 0.9,
          title = params$lgd,
          labFormat = leaflet::labelFormat(suffix = params$unit)
        ) %>%
        leaflet.extras2::addEasyprint(
          options = leaflet.extras2::easyprintOptions(
            exportOnly = TRUE,
            sizeModes = "A4Landscape"
          )
        )
      ready(ready() + 1)
      p
    }) %>%
      bindEvent(input$submit)
    
    
    ## Update Leaflet map ----
    observe({
      params <- params()
      leaflet::leafletProxy("map", data = params$data) %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() %>%
        leaflet::addPolygons(
          fillColor = stats::as.formula(paste0("~params$pal(", params$var, ")")),
          fillOpacity = 0.7,
          weight = 1,
          color = "black",
          opacity = 0.5,
          label = params$labels,
          highlightOptions = highlight_opts
        ) %>%
        leaflet::addLegend(
          position = "bottomright",
          pal = params$pal,
          values = params$values,
          opacity = 0.9,
          title = params$lgd,
          labFormat = leaflet::labelFormat(suffix = params$unit)
        )
    }, label = "Update map parameters")

    outputOptions(output, "type", suspendWhenHidden = FALSE, priority = 1)
    outputOptions(output, "map", suspendWhenHidden = FALSE, priority = 2)
    
    
    # Effects of moving ----
    markers <- reactiveValues(start = FALSE, dest = FALSE)
    
    
    ## Capture place name ----
    location <- reactive({
      click <- input$map_shape_click
      aggr <- input$aggr
      click <- sf::st_sfc(sf::st_point(c(click$lng, click$lat)), crs = 4326)
      click <- sf::st_transform(click, 3035)
      nuts2 <- clusters()[[aggr]]
      nuts2[sf::st_nearest_feature(click, nuts2), ][[aggr]]
    }) %>%
      bindEvent(input$map_shape_click)
    
    
    ## Remove marker on click ----
    observe({
      id <- input$map_marker_click$id
      
      if (identical(id, session$ns("start"))) {
        markers$start <- FALSE
      } else if (identical(id, session$ns("dest"))) {
        markers$dest <- FALSE
      }
      
      leaflet::leafletProxy("map") %>%
        leaflet::removeMarker(id)
    }) %>%
      bindEvent(input$map_marker_click)
    
    
    ## Add marker on click ----
    observe({
      pt <- input$map_shape_click
      
      add_start <- !isTRUE(markers$start)
      
      if (add_start) {
        markers$start <- TRUE
        leaflet::leafletProxy("map") %>%
          leaflet::addMarkers(
            lng = pt$lng,
            lat = pt$lat,
            layerId = session$ns("dest"),
            icon = leaflet::makeIcon(
              "https://www.svgrepo.com/download/415954/destination-ensign-flag.svg",
              iconWidth = 25,
              iconHeight = 25,
              iconAnchorY = 25,
              iconAnchorX = 12.5
            )
          )
      } else {
        markers$dest <- TRUE
        leaflet::leafletProxy("map") %>%
          leaflet::addMarkers(
            lng = pt$lng,
            lat = pt$lat,
            layerId = session$ns("start"),
            icon = leaflet::makeIcon(
              "https://www.svgrepo.com/download/459977/home-alt-3.svg",
              iconWidth = 25,
              iconHeight = 25,
              iconAnchorY = 25,
              iconAnchorX = 12.5
            )
          )
      }
    }) %>%
      bindEvent(input$map_shape_click)
    
    
    ## Render start ----
    output$move_from <- renderUI({
      loc <- location()
      
      shinyjs::hide(id = "start-placeholder")
      
      if (isTRUE(markers$start)) {
        tags$b(loc)
      } else {
        tags$i("None selected")
      }
    })
    
    
    ## Render destination ----
    output$move_to <- renderUI({
      loc <- location()
      
      shinyjs::hide(id = "dest-placeholder")
      
      if (isTRUE(markers$dest)) {
        tags$b(loc)
      } else {
        tags$i("None selected")
      }
    })
    
    
    observe({
      # click <- 
      # leaflet::leafletProxy("map") %>%
      #   leaflet.extras2::addMovingMarker(
      #     
      #   )
    }) %>%
      bindEvent(input$move_go)
  })
}

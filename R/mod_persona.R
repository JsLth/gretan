mod_persona_ui <- function(id) {
  ns <- NS(id)
  
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
      title = "Persona-based clustering of energy citizens",
      authors = c("Ajesh Kumar", "Toni Kuronen", "Annika Wolff"),
      affil = "Lappeenranta-Lahti University of Technology LUT",
      date = "DD-MM-YYYY"
    ),
    fluidRow(
      col_1(),
      col_10(
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
            title = with_literata("How do you compare?"),
            position = "topleft",
            p("This map shows you how you compare to other regions in the EU.
              Where do other energy citizens live? Which regions perform
              better than others? What changes can you expect when moving to
              a different region?")
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
            tags$h1(class = "display-4", "Welcome to the persona grouping tool!"),
            tags$p(
              class = "lead",
              "Take this survey and learn about your role in the energy transition.",
              br(),
              "Discover how you compare to other people in the EU.",
              br(),
              "Find out how moving to a different country
            can change your outlook."
            ),
            tags$hr(class = "my-4"),
            shinyWidgets::prettyToggle(
              inputId = ns("consent"),
              label_on = "I give consent to the GRETA research team to analyze the data that I provide in this survey.", 
              label_off = "I give consent to the GRETA research team to analyze the data that I provide in this survey.",
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
              label = "How would you asses your knowledge and financial resources to participate in cooperative self-generation of renewable energy?",
              choices = c(
                "None selected", "Very low", "Low", "Somewhat low", "Neutral",
                "Somewhat high", "High", "Very high", "I do not know"
              ),
              status = "info"
            )
          ),
          persona_step(
            step = 3,
            h2("Question 5"),
            shinyWidgets::awesomeRadio(
              ns("question5"),
              label = HTML("To what degree do you agree to the following
                           statement?<br>\u201cI feel proud about the idea of
                           cooperative self-generation of renewable energy for
                           my household use\u201d"),
              choices = c(
                "None selected", "Strongly disagree", "Disagree",
                "Somewhat disagree", "Neutral", "Somewhat agree", "Agree",
                "Strongly agree", "I do not know"
              ),
              status = "info"
            )
          ),
          persona_step(
            step = 4,
            h2("Thank you!"),
            p("You have finished the survey. Click \u201cSubmit\u201d
              now to have your information analyzed."),
            bs4Dash::actionButton(
              ns("submit"),
              label = "Submit!",
              icon = icon("wand-magic-sparkles"),
              size = "lg",
              status = "success"
            )
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
    page <- reactiveVal(1)
    prev_page <- reactiveVal(NULL)
    
    waiter <- waiter::Waiter$new(
      id = ns("map"),
      html = tagList(waiter::spin_pulse(), h4("Determining your energy citizen type...")),
      color = "rgba(179, 221, 254, 1)"
    )
    
    observe({
      shinyjs::toggleState(
        id = "prevBtn",
        condition = page() > 1
      )
      shinyjs::toggleState(
        id = "nextBtn",
        condition = page() < 4 && input$consent
      )
      shinyjs::toggleState(
        id = "submit",
        condition = all(has_answered())
      )
      shinyjs::toggleElement(
        id = "surveybox",
        condition = !isTruthy(input$submit)
      )
      shinyjs::toggleElement(
        id = "mapbox",
        condition = isTruthy(input$submit)
      )
      shinyjs::hide(id = paste0("step", prev_page()))
      log_it(sprintf(
        "{persona} - Switching from %s to %s",
        ns(paste0("step", prev_page())),
        ns(paste0("step", page()))
      ))
      shinyjs::show(paste0("step", page()))
    })
    
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
        function(i) {
          answer <- input[[paste0("question", i)]]
          if (!is.null(answer)) {
            !identical(answer, "None selected")
          } else {
            TRUE
          }
        } 
      ) %>%
        setNames(paste0("question", seq(5)))
    })
    
    
    output$map <- leaflet::renderLeaflet({
      waiter$show()
      on.exit(waiter$hide())
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::setView(lng = 9, lat = 55, zoom = 4) %>%
        leaflet::addPolygons(
          fill = FALSE,
          weight = 1,
          color = "black",
          opacity = 0.5,
          #label = params$labels,
          #highlightOptions = highlight_opts,
          group = "NUTS-0",
          data = sf::st_transform(srv_nuts0, 4326)
        ) %>%
        leaflet::addPolygons(
          fill = FALSE,
          weight = 1,
          color = "black",
          opacity = 0.5,
          #label = params$labels,
          #highlightOptions = highlight_opts,
          group = "NUTS-1",
          data = sf::st_transform(srv_nuts1, 4326)
        ) %>%
        leaflet::addPolygons(
          fill = FALSE,
          weight = 1,
          color = "black",
          opacity = 0.5,
          #label = params$labels,
          #highlightOptions = highlight_opts,
          group = "NUTS-2",
          data = sf::st_transform(srv_nuts2, 4326)
        ) %>%
        leaflet::addLayersControl(baseGroups = c("NUTS-2", "NUTS-1", "NUTS-0"))
    })
  })
}
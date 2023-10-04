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
        solidHeader = FALSE,
        status = "primary",
        tabPanel(
          title = "Initial Yes values",
          h4(with_literata("Product")),
          fluidRow(
            col_3( # autonomous_cars, sustainable_transport, cooperative_self_generation
              shinyWidgets::noUiSliderInput(
                ns("initial_yes__autonomous_-")
              )
            ),
            col_3(),
            col_3(),
            col_3()
          )
        ),
        tabPanel(
          title = "Intention weights"
        ),
        tabPanel(
          title = "Survey topics"
        )
      )
    )
  )
}


mod_stakeholder_server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}

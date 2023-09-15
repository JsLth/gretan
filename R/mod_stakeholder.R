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
          plotly::plotlyOutput(ns("plot"), height = "600px")
        ),
        tabPanel(
          title = "Stability maps",
          leaflet::leafletOutput(ns("map"))
        )
      )
    ),
    fluidRow(
      col_4(
        div(style = "margin: auto", sliderInput(
          ns("slider1"),
          label = "Slider 1",
          min = 0,
          max = 100,
          value = 20,
          step = 1
        ))
      ),
      col_4(
        div(style = "margin: auto", sliderInput(
          ns("slider2"),
          label = "Slider 2",
          min = 0,
          max = 100,
          value = 50,
          step = 1
        ))
      ),
      col_4(
        div(style = "margin: auto", sliderInput(
          ns("slider3"),
          label = "Slider 3",
          min = 0,
          max = 100,
          value = 20,
          step = 1
        ))
      ),
      col_4(
        div(style = "margin: auto", sliderInput(
          ns("slider4"),
          label = "Slider 4",
          min = 0,
          max = 100,
          value = 10,
          step = 1
        ))
      ),
      col_4(
        div(style = "margin: auto", sliderInput(
          ns("slider5"),
          label = "Slider 5",
          min = 0,
          max = 100,
          value = 50,
          step = 1
        ))
      ),
      col_4(
        div(style = "margin: auto", sliderInput(
          ns("slider6"),
          label = "Slider 6",
          min = 0,
          max = 100,
          value = 90,
          step = 1
        ))
      )
    )
  )
}


mod_stakeholder_server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}

mod_cs4_ui <- function(id) {
  ns <- NS(id)
  get_text <- dispatch_to_txt(id)

  bs4Dash::tabItem(
    "cs4",
    make_header(
      title = get_text("title"),
      authors = get_text("authors"),
      affil = get_text("affil"),
      date = get_text("date")
    ),
    fluidRow(
      col_6(
        bs4Dash::box(
          title = with_literata(get_text("introduction", "title")),
          status = "primary",
          width = 12,
          get_text("introduction", "content")
        ),
        bs4Dash::box(
          title = with_literata(get_text("how", "title")),
          status = "primary",
          width = 12,
          get_text("how", "content")
        )
      ),
      col_6(
        bs4Dash::box(
          title = with_literata(get_text("analysis", "title")),
          status = "primary",
          width = 12,
          get_text("analysis", "content"),
          actionLink(
            ns("exp_link"),
            label = "Click here to learn more.",
            class = "fancy"
          )
        ),
        bs4Dash::box(
          title = with_literata(get_text("findings", "title")),
          status = "primary",
          width = 12,
          get_text("findings", "content")
        )
      )
    )
  )
}

mod_cs4_server <- function(id, tab) {
  moduleServer(id, function(input, output, session) {
    
  })
}

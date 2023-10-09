mod_cs3_ui <- function(id) {
  ns <- NS(id)
  get_text <- dispatch_to_txt(id)

  bs4Dash::tabItem(
    "cs3",
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
          title = with_literata(get_text("analysis", "title")),
          status = "primary",
          width = 12,
          get_text("analysis", "content")
        )
      ),
      col_6(
        bs4Dash::box(
          title = with_literata(get_text("how", "title")),
          status = "primary",
          width = 12,
          get_text("how", "content")
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

mod_cs3_server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}

mod_cs2_ui <- function(id) {
  ns <- NS(id)
  get_text <- dispatch_to_txt(id)

  bs4Dash::tabItem(
    "cs2",
    make_header(
      title = get_text("title"),
      authors = get_text("authors"),
      date = get_text("date"),
      affil = get_text("affil")
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
          title = with_literata(get_text("findings", "title")),
          status = "primary",
          width = 12,
          get_text("findings", "content")
        )
      ),
      col_6(
        bs4Dash::box(
          title = with_literata(get_text("objectives", "title")),
          status = "primary",
          width = 12,
          get_text("objectives", "content")
        )
      )
    )
  )
}


mod_cs2_server <- function(id, tab) {
  moduleServer(id, function(input, output, session) {

  })
}

mod_enpov_ui <- function(id) {
  ns <- NS(id)

  get_text <- dispatch_to_txt(id)
  
  bs4Dash::tabItem(
    "enpov",
    make_header(
      title = get_text("title"),
      authors = get_text("authors"),
      affil = get_text("affil"),
      date = get_text("date")
    )
  )
}


mod_enpov_server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}

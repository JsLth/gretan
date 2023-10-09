mod_attitudes_ui <- function(id) {
  ns <- NS(id)

  get_text <- dispatch_to_txt(id)

  bs4Dash::tabItem(
    "attitudes",
    make_header(
      title = get_text("title"),
      authors = get_text("authors"),
      affil = get_text("affil"),
      date = get_text("date")
    )
  )
}


mod_attitudes_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observe({
      if (identical(get_tab(), "attitudes")) {
        send_warning("Seems like there is no content yet! Please come back later.")
      }
    }) %>%
      bindEvent(get_tab())
  })
}

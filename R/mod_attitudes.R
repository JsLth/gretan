mod_attitudes_ui <- function(id) {
  ns <- NS(id)

  bs4Dash::tabItem(
    "attitudes",
    make_header(
      title = txts$attitudes$title,
      authors = txts$attitudes$authors,
      affil = txts$attitudes$affil,
      date = txts$attitudes$date
    )
  )
}


mod_attitudes_server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}

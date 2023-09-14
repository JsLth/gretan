mod_research_ui <- function(id) {
  ns <- NS(id)

  bs4Dash::tabItem(
    "research",
    make_header(
      title = txts$research$title,
      authors = txts$research$authors,
      affil = txts$research$affil,
      date = txts$research$date
    )
  )
}


mod_research_server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}

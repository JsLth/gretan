mod_enpov_ui <- function(id) {
  ns <- NS(id)

  bs4Dash::tabItem(
    "enpov",
    make_header(
      title = txts$enpov$title,
      authors = txts$enpov$authors,
      affil = txts$enpov$affil,
      date = txts$enpov$date
    )
  )
}


mod_enpov_server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}

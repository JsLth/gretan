mod_cs2_ui <- function(id) {
  ns <- NS(id)

  bs4Dash::tabItem(
    "cs2",
    make_header(
      title = "Case study 2: Coop\u00e9rnico \u2013 renewable energy-driven cooperative",
      authors = c("Prepared by: Author A", "Author B"),
      affil = list(
        "Author A" = "Cleanwatts",
        "Author B" = "Cleanwatts"
      ),
      date = "2023-mm-dd"
    )
  )
}


mod_cs2_server <- function(id, tab) {
  moduleServer(id, function(input, output, session) {

  })
}

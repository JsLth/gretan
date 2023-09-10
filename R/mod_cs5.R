mod_cs5_ui <- function(id) {
  ns <- NS(id)

  bs4Dash::tabItem(
    "cs5",
    make_header(
      title = "Case study 5: UR BEROA \u2013 energy efficiency-driven cooperative",
      authors = c("Prepared by: Author A", "Author B"),
      affil = list(
        "Author A" = "Tecnalia Research and Innovation",
        "Author B" = "Tecnalia Research and Innovation"
      ),
      date = "2023-mm-dd"
    )
  )
}

mod_cs5_server <- function(id, tab) {
  moduleServer(id, function(input, output, session) {

  })
}

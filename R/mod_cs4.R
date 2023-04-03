mod_cs4_ui <- function(id) {
  ns <- NS(id)
  
  bs4Dash::tabItem(
    "cs4",
    make_header(
      title = "Case study 4: Natural gas-free neighbourhoods",
      authors = c("Prepared by: Author A", "Author B"),
      affil = list(
        "Author A" = "Netherlands Organisation for Applied Scientific Research",
        "Author B" = "Netherlands Organisation for Applied Scientific Research"
      ),
      date = "2023-mm-dd"
    )
  )
}


mod_cs4 <- function(input, output, session) {
  
}


mod_cs4_server <- function(id) {
  moduleServer(id, mod_cs4)
}
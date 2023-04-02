mod_cs3_ui <- function(id) {
  ns <- NS(id)
  
  bs4Dash::tabItem(
    "cs3",
    make_header(
      title = "Case study 3: The Earnest App \u2013 a virtual community for sustainable mobility in Darmstadt",
      authors = c("Prepared by: Author A", "Author B"),
      affil = list(
        "Author A" = "Fraunhofer Institute for Systems and Innovation Research",
        "Author B" = "Fraunhofer Institute for Systems and Innovation Research"
      ),
      date = "2023-mm-dd"
    )
  )
}


mod_cs3_server <- function(id) {
  
}
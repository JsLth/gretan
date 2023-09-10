mod_research_ui <- function(id) {
  ns <- NS(id)
  
  bs4Dash::tabItem(
    "research",
    make_header(
      title = "Mapping the spatial turn in energy transition research",
      authors = c("Dennis Abel", "Jonas Lieth", "Stefan J\u00fcnger"),
      affil = "GESIS - Leibniz Institute for the Social Sciences",
      date = "DD-MM-YYYY"
    )
  )
}


mod_research_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}
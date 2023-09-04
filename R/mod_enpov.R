mod_enpov_ui <- function(id) {
  ns <- NS(id)
  
  bs4Dash::tabItem(
    "enpov",
    make_header(
      title = "Landscapes of vulnerability to energy poverty in the EU",
      authors = c("Jonas Lieth", "Dennis Abel", "Stefan Jünger"),
      affil = "GESIS - Leibniz Institute for the Social Sciences",
      date = "DD-MM-YYYY"
    )
  )
}


mod_enpov_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}
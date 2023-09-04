mod_attitudes_ui <- function(id) {
  ns <- NS(id)
  
  bs4Dash::tabItem(
    "attitudes",
    make_header(
      title = "Digital geographies of environmental attitudes",
      authors = c("Jonas Lieth", "Stefan Jünger"),
      affil = "GESIS - Leibniz Institute for the Social Sciences",
      date = "DD-MM-YYYY"
    )
  )
}


mod_attitudes_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}
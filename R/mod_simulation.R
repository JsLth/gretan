mod_simulation_ui <- function(id) {
  ns <- NS(id)
  
  bs4Dash::tabItem(
    "simulation",
    make_header(
      title = "Title for a GRETA-related analysis",
      authors = c("Author 1", "Author 2"),
      affil = list(
        "Author 1" = c("Institution 1", "Institution 2"),
        "Author 2" = c("Institution 2")
      ),
      date = "DD-MM-YYYY"
    )
  )
}


mod_simulation_server <- function(input, output, session) {
  
}
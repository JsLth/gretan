mod_persona_ui <- function(id) {
  ns <- NS(id)
  
  bs4Dash::tabItem(
    "persona",
    make_header(
      title = "Persona-based clustering of energy citizens",
      authors = c("Ajesh Kumar", "Toni Kuronen", "Annika Wolff"),
      affil = "Lappeenranta-Lahti University of Technology LUT",
      date = "DD-MM-YYYY"
    )
  )
}


mod_persona_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}
mod_cs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_cs1_ui(ns("cs1")),
    mod_cs5_ui(ns("cs5"))
  )
}

mod_cs_server <- function(id, tab) {
  moduleServer(id, function(input, output, session) {
    mod_cs1_server("cs1", tab = tab)
    mod_cs5_server("cs5", tab = tab)
  })
}

mod_cs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_cs1_ui(ns("cs1")),
    mod_cs2_ui(ns("cs2")),
    mod_cs3_ui(ns("cs3")),
    mod_cs4_ui(ns("cs4")),
    mod_cs5_ui(ns("cs5"))
  )
}

mod_cs_server <- function(id, tab) {
  moduleServer(id, function(input, output, session) {
    mod_cs1_server("cs1", tab = tab)
    mod_cs2_server("cs2", tab = tab)
    mod_cs3_server("cs3", tab = tab)
    mod_cs4_server("cs4", tab = tab)
    mod_cs5_server("cs5", tab = tab)
  })
}

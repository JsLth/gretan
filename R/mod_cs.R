mod_cs_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    mod_cs_ui(ns("cs1-ui")),
    mod_cs_ui(ns("cs2-ui")),
    mod_cs_ui(ns("cs3-ui")),
    mod_cs_ui(ns("cs4-ui")),
    mod_cs_ui(ns("cs5-ui")),
  )
}

mod_cs_server <- function(input, output, session) {
  callModule(mod_cs1_server, "cs1-ui")
  callModule(mod_cs2_server, "cs2-ui")
  callModule(mod_cs3_server, "cs3-ui")
  callModule(mod_cs4_server, "cs4-ui")
  callModule(mod_cs5_server, "cs5-ui")
}
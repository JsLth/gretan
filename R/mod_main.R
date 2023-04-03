mod_main_ui <- function(id) {
  ns <- NS(id)

  shiny::div(
    mod_home_ui(ns("home")),
    mod_exp_ui(ns("exp")),
    mod_ind_ui(ns("ind")),
    mod_cs_ui(ns("cs")),
    mod_sandbox_ui(ns("sandbox")),
    class = "tab-content"
  )
}


mod_main <- function(input, output, session) {
  ns <- session$ns
  
  shinyjs::onclick("tab-cs2", send_info(
    "This tab has not yet been filled with contents."
  ))
  shinyjs::onclick("tab-cs3", send_info(
    "This tab has not yet been filled with contents."
  ))
  shinyjs::onclick("tab-cs4", send_info(
    "This tab has not yet been filled with contents."
  ))
  shinyjs::onclick("tab-cs5", send_info(
    "This tab has not yet been filled with contents."
  ))
  shinyjs::onclick("tab-simulation", send_info(
    "This tab has not yet been filled with contents."
  ))
  
  init("exp")

  mod_home_server("home")
  mod_exp_server("exp")
  mod_cs_server("cs")
  mod_ind_server("ind")
  mod_sandbox_server("sandbox")
}


mod_main_server <- function(id) {
  moduleServer(id, mod_main)
}
mod_main_ui <- function(id) {
  ns <- NS(id)

  shiny::div(
    mod_home_ui(ns("home-ui")),
    mod_exp_ui(ns("exp-ui")),
    mod_ind_ui(ns("ind-ui")),
    mod_cs_ui(ns("cs-ui")),
    mod_sandbox_ui(ns("sandbox-ui")),
    class = "tab-content"
  )
}


mod_main_server <- function(input, output, session) {
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

  callModule(mod_home_server, "home-ui")
  callModule(mod_exp_server, "exp-ui")
  callModule(mod_cs_server, "cs-ui")
  callModule(mod_ind_server, "ind-ui")
  callModule(mod_sandbox_server, "sandbox-ui")
}
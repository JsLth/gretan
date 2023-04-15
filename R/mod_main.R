mod_main_ui <- function(id) {
  ns <- NS(id)

  categories <- unique(cb_ext$topic[!is.na(cb_ext$topic)])
  titles <- stats::setNames(lapply(categories, function(x) {
    as.list(unique(cb_ext[cb_ext$topic %in% x, ]$title))
  }), categories)

  shiny::div(
    mod_home_ui(ns("home")),
    mod_exp_ui(ns("exp"), categories, titles),
    mod_cmp_ui(ns("cmp"), categories, titles),
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
  mod_cmp_server("cmp")
  mod_cs_server("cs")
  mod_ind_server("ind")
  mod_sandbox_server("sandbox")
}


mod_main_server <- function(id) {
  moduleServer(id, mod_main)
}
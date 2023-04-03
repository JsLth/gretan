mod_ind_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    mod_simulation_ui(ns("simulation")),
    mod_spatial_ui(ns("spatial")),
    mod_income_ui(ns("income")),
  )
}

mod_ind <- function(input, output, session) {
  mod_simulation_server("simulation")
  mod_spatial_server("spatial")
  mod_income_server("income")
}


mod_ind_server <- function(id) {
  moduleServer(id, mod_ind)
}
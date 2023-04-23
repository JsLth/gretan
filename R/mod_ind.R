mod_ind_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    mod_taxonomy_ui(ns("taxonomy")),
    mod_simulation_ui(ns("simulation")),
    mod_spatial_ui(ns("spatial")),
    mod_income_ui(ns("income"))
  )
}

mod_ind_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    mod_taxonomy_server("taxonomy")
    mod_simulation_server("simulation")
    mod_spatial_server("spatial")
    mod_income_server("income")
  })
}
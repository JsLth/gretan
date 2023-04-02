mod_ind_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    mod_simulation_ui(ns("simulation-ui")),
    mod_spatial_ui(ns("spatial-ui")),
    mod_income_ui(ns("income-ui")),
  )
}

mod_ind_server <- function(input, output, session) {
  init("spatial_clusters", FALSE)
  init("spatial_scatter", FALSE)
  
  callModule(mod_simulation_server, "simulation-ui")
  callModule(mod_spatial_server, "spatial-ui")
  callModule(mod_income_server, "income-ui")
}
mod_ind_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    mod_taxonomy_ui(ns("taxonomy"))
  )
}

mod_ind_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    mod_taxonomy_server("taxonomy")
  })
}
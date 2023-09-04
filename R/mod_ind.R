mod_ind_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    mod_taxonomy_ui(ns("taxonomy")),
    mod_stakeholder_ui(ns("stakeholder")),
    mod_persona_ui(ns("persona")),
    mod_enpov_ui(ns("enpov")),
    mod_attitudes_ui(ns("attitudes")),
    mod_research_ui(ns("research"))
  )
}

mod_ind_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    mod_taxonomy_server("taxonomy")
    mod_stakeholder_server("stakeholder")
    mod_persona_server("persona")
    mod_enpov_server("enpov")
    mod_attitudes_server("attitudes")
    mod_research_server("research")
  })
}
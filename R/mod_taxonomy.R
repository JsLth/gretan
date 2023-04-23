mod_taxonomy_ui <- function(id) {
  ns <- NS(id)
  
  bs4Dash::tabItem(
    "taxonomy",
    make_header(
      title = "GRETA taxonomy of geographical levels",
      authors = c("Saveria Olga Murielle Boulanger", "Martina Massari"),
      affil = "University of Bologna",
      date = "DD-MM-YYYY"
    ),
    fluidRow(
      col_6(
        bs4Dash::box(
          title = "Introduction",
          status = "primary",
          width = 12,
          p2(txts$taxonomy$introduction)
        )
      ),
      col_6(
        bs4Dash::box(
          title = "Methodology",
          status = "primary",
          width = 12,
          p2(txts$taxonomy$methodology)
        )
      )
    ),
    bs4Dash::box(
      title = "Taxonomy",
      status = "primary",
      width = 12,
      make_taxonomy("taxonomy")
    )
  )
}


mod_taxonomy_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}
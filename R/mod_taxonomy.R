tax <- function(...) {
  make_action_button(div(..., class = "tax-elem"))
}

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
      div(
        id = "taxonomy-container",
        class = "tax-container",
        tax("GRETA levels", class = "tax-header", id = "gretaLevels"),
        div(style = "display: block; height: 2.5%"),
        div(
          id = "levels-container",
          class = "tax-levels-container",
          tax("Virtual", class = "tax-virtual", id = "gretaVirtual"),
          tax(
            "Local",
            id = "gretaLocal",
            class = "tax-levels",
            style = "background-color: #EE3727; margin-top: -150px;"
          ),
          tax(
            "Regional",
            id = "gretaRegional",
            class = "tax-levels",
            style = "background-color: #E73E27; margin-top: 25px;"
          ),
          tax(
            "National",
            id = "gretaNational",
            class = "tax-levels",
            style = "background-color: #C33728; margin-top: 25px"
          ),
          tax(
            "Supranational",
            id = "gretaSupranational",
            class = "tax-levels", 
            style = "background-color: #972C24; margin-top: 25px"
          )
        ),
        div("proximity domains"),
        div("spatial"),
        div("policy")
      )
    )
  )
}


mod_taxonomy_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    popover2(
      "gretaLevels",
      title = "GRETA levels",
      content = "Content here"
    )
    
    popover2(
      "gretaVirtual",
      title = "Virtual level",
      content = "Content here"
    )

    popover2(
      "gretaLocal",
      title = "Local level",
      content = "Content here"
    )
    
    popover2(
      "gretaRegional",
      title = "Regional level",
      content = "Content here"
    )
    
    popover2(
      "gretaNational",
      title = "National level",
      content = "Content here"
    )
    
    popover2(
      "gretaSupranational",
      title = "Supranational level",
      content = "Content here"
    )
    
    bs4Dash::addTooltip(
      "gretaLevels",
      options = list(
        title = "Click to learn more"
      )
    )
  })
}
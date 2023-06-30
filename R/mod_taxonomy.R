tax <- function(..., type = NULL) {
  if (!is.null(type)) {
    type <- paste0("tax-", type)
  }
  tags$button(..., type = "button", class = c("tax-elem", type))
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
    helpBox(
      title = "Taxonomy",
      help_id = ns("taxHelp"),
      status = "primary",
      width = 12,
      div(
        id = ns("taxonomy-container"),
        class = "tax-container",
        tax("GRETA levels", id = ns("greta-header"), type = "header"),
        div(
          id = ns("levels-container"),
          class = "tax-levels-container",
          tax("Virtual", class = "tax-virtual", id = ns("greta-virtual"), type = "level"),
          tax("Local", class = "tax-local", id = ns("greta-local"), type = "level"),
          tax("Regional", class = "tax-regional", id = ns("greta-regional"), type = "level"),
          tax("National", class = "tax-national", id = ns("greta-national"), type = "level"),
          tax("Supranational", class = "tax-supranational", id = ns("greta-supranational"), type = "level")
        ),
        div(
          id = ns("domains-container"),
          class = "tax-domains-container",
          tax("proximity domains", class = "tax-prox", id = ns("greta-prox"), type = "domain-sidebar"),
          tax("spatial", class = "tax-spatial", id = ns("greta-spatial"), type = "domain"),
          tax("policy", class = "tax-policy", id = ns("greta-policy"), type = "domain"),
          tax("social", class = "tax-social", id = ns("greta-social"), type = "domain"),
          tax("technological", class = "tax-tech", id = ns("greta-tech"), type = "domain"),
          tax("economic", class = "tax-econ", id = ns("greta-econ"), type = "domain"),
          tax("dimensions & indicators", class = "tax-dims", id = ns("greta-dims"), type = "domain-sidebar")
        )
      )
    )
  )
}


mod_taxonomy_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    popover2(
      id = "taxHelp",
      title = "Explore GRETAs geo-taxonomy",
      content = txts$taxonomy$help$scheme
    )
    
    for (name in names(txts$taxonomy$scheme)) {
      id <- paste0("greta-", name)
      bs4Dash::addTooltip(id, options = list(title = "Click to learn more"))
      popover2(
        id = id,
        title = txts$taxonomy$scheme[[name]]$title,
        content = txts$taxonomy$scheme[[name]]$content
      )
    }
  })
}
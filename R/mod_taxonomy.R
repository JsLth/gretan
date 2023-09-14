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
      title = txts$taxonomy$title,
      authors = txts$taxonomy$authors,
      affil = txts$taxonomy$affil,
      date = txts$taxonomy$date
    ),
    fluidRow(
      bs4Dash::column(5,
        bs4Dash::box(
          title = with_literata("Introduction"),
          status = "primary",
          width = 12,
          p2(txts$taxonomy$introduction)
        )
      ),
      bs4Dash::column(7,
        bs4Dash::box(
          title = with_literata("Methodology"),
          status = "primary",
          width = 12,
          p2(txts$taxonomy$methodology)
        )
      )
    ),
    helpBox(
      title = with_literata("Taxonomy"),
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
          tax(
            p("Virtual", style = "writing-mode: vertical-lr;"),
            class = "tax-virtual",
            id = ns("greta-virtual"),
            type = "level"
          ),
          tax(
            "Local",
            class = "tax-local",
            id = ns("greta-local"),
            type = "level"
          ),
          tax(
            "Regional",
            class = "tax-regional",
            id = ns("greta-regional"),
            type = "level"
          ),
          tax(
            "National",
            class = "tax-national",
            id = ns("greta-national"),
            type = "level"
          ),
          tax(
            "Supranational",
            class = "tax-supranational",
            id = ns("greta-supranational"),
            type = "level"
          )
        ),
        div(
          id = ns("domains-container"),
          class = "tax-domains-container",
          tax(
            p("proximity domains",
              style = "writing-mode: vertical-lr;"
            ),
            class = "tax-prox",
            id = ns("greta-prox"),
            type = "domain-sidebar"
          ),
          tax(
            "spatial",
            class = "tax-spatial",
            id = ns("greta-spatial"),
            type = "domain"
          ),
          tax(
            "policy",
            class = "tax-policy",
            id = ns("greta-policy"),
            type = "domain"
          ),
          tax(
            "social",
            class = "tax-social",
            id = ns("greta-social"),
            type = "domain"
          ),
          tax(
            "technological",
            class = "tax-tech",
            id = ns("greta-tech"),
            type = "domain"
          ),
          tax(
            "economic",
            class = "tax-econ",
            id = ns("greta-econ"),
            type = "domain"
          ),
          tax(
            p("dimensions & indicators", style = "writing-mode: vertical-lr;"),
            class = "tax-dims",
            id = ns("greta-dims"),
            type = "domain-sidebar"
          )
        )
      )
    )
  )
}


mod_taxonomy_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    popover2(
      id = "taxHelp",
      title = with_literata("Explore GRETAs geo-taxonomy"),
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

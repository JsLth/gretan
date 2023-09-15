tax <- function(..., type = NULL) {
  if (!is.null(type)) {
    type <- paste0("tax-", type)
  }
  tags$button(..., type = "button", class = c("tax-elem", type))
}

mod_taxonomy_ui <- function(id) {
  ns <- NS(id)
  
  get_text <- dispatch_to_txt(id)

  bs4Dash::tabItem(
    "taxonomy",
    make_header(
      title = get_text("title"),
      authors = get_text("authors"),
      affil = get_text("affil"),
      date = get_text("date")
    ),
    fluidRow(
      bs4Dash::column(5,
        bs4Dash::box(
          title = with_literata(get_text("introduction", "title")),
          status = "primary",
          width = 12,
          p2(get_text("introduction", "content"))
        )
      ),
      bs4Dash::column(7,
        bs4Dash::box(
          title = with_literata(get_text("methodology", "title")),
          status = "primary",
          width = 12,
          p2(get_text("methodology", "content"))
        )
      )
    ),
    helpBox(
      title = with_literata(get_text("scheme", "title")),
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
    get_text <- dispatch_to_txt(session$ns(NULL))
    
    popover2(
      id = "taxHelp",
      title = with_literata(get_text("help", "scheme", "title")),
      content = get_text("help", "scheme", "content")
    )

    for (name in setdiff(names(get_text("scheme")), "title")) {
      id <- paste0("greta-", name)
      bs4Dash::addTooltip(id, options = list(title = "Click to learn more"))
      popover2(
        id = id,
        title = get_text("scheme", name, "title"),
        content = get_text("scheme", name, "content")
      )
    }
  })
}

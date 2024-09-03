mod_enpov_ui <- function(id) {
  ns <- NS(id)

  get_text <- dispatch_to_txt(id)

  bs4Dash::tabItem(
    "enpov",
    make_header(
      title = get_text("title"),
      authors = get_text("authors"),
      affil = get_text("affil"),
      date = get_text("date")
    ),
    fluidRow(
      col_6(
        bs4Dash::box(
          title = get_text("introduction", "title"),
          status = "primary",
          width = 12,
          get_text("introduction", "content")
        ),
        bs4Dash::box(
          title = get_text("")
        )
      ),
      col_6(
        bs4Dash::tabBox(
          title = get_text("how", "approach", "title"),
          status = "primary",
          width = 12,
          solidHeader = FALSE,
          type = "tabs",
          side = "right",
          tabPanel(
            title = "Approach",
            get_text("how", "approach", "content")
          ),
          tabPanel(
            title = get_text("how", "variables", "title"),
            DT::dataTableOutput(ns("variables"), height = "100%", width = "auto")
          )
        ),
        bs4Dash::box(
          title = get_text("references", "title"),
          status = "primary",
          width = 12,
          get_text("references", "content")
        )
      )
    )
  )
}


mod_enpov_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    get_text <- dispatch_to_txt(session$ns(NULL))

    output$variables <- DT::renderDataTable(
      DT::datatable(
        get_text("how", "variables", "content"),
        class = c("compact", "order-column"),
        extensions = c("RowGroup"),
        style = "bootstrap4",
        rownames = FALSE,
        options = list(
          autoWidth = TRUE,
          columnDefs = list(list(targets = 0, visible = FALSE)),
          pageLength = 5,
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          scrollY = 400,
          rowGroup = list(
            dataSrc = 0,
            emptyDataGroup = NULL,
            startRender = DT::JS("function(rows, group, level) {
              return 'Subindex: ' + group
            }")
          )
        )
      )
    )
  })
}

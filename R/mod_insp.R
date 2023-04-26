mod_insp_ui <- function(id) {
  ns <- NS(id)
  
  bs4Dash::tabItem(
    "insp",
    shinyWidgets::pickerInput(
      ns("aggr"),
      label = "Aggregation level",
      choices = c(
        "Countries" = "nuts0",
        "Major regions" = "nuts1",
        "Minor regions" = "nuts2"
      ),
      inline = TRUE
    ),
    shinyWidgets::materialSwitch(
      ns("mode"),
      "Aggregate options",
      value = FALSE,
      inline = TRUE
    ),
    DT::dataTableOutput(ns("table"))
  )
}


mod_insp_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    table <- reactive({
      poly <- get(paste0("srv_", input$aggr))
      mns_pivot_longer(poly)
    })

    output$table <- DT::renderDataTable(
      table(),
      server = TRUE,
      extensions = c("Buttons", "Scroller", "KeyTable", "RowGroup"),
      selection = "none",
      filter = list(position = "top", clear = FALSE),
      options = list(
        pageLength = 50,
        scroller = TRUE,
        scrollY = 600,
        deferRender = TRUE,
        keys = TRUE,
        rowGroup = list(
          dataSrc = c(2, 3, 4),
          emptyDataGroup = NULL,
          startRender = DT::JS("function (rows, group, level) {
            if (level == 0) {
              return group +' ('+rows.count()+' rows)';
            } else {
              return group
            }
          }")
        ),
        columnDefs = list(list(targets = c(2, 3, 4), visible = FALSE)),
        lengthMenu = list(list(100)
        ),
        buttons = list(
          list(
            extend = "copyHtml5",
            text = "Copy to Clipboard",
            className = "dt-button-2",
            modifier = list(order = "applied", page = "all", search = "applied")
          ),
          list(
            extend = "csvHtml5",
            text = "Save as CSV",
            className = "dt-button-2",
            modifier = list(order = "applied", page = "all", search = "applied")
          ),
          list(
            extend = "collection",
            text = "Save as JSON",
            className = "dt-button-2",
            action = DT::JS(
              "function (e, dt, button, config) {
                var data = dt.buttons.exportData();
                $.fn.dataTable.fileSave(
                  new Blob([JSON.stringify(data)]),
                  'Export.json'
                );
              }"
            )
          ),
          list(
            extend = "pdf",
            text = "Save as PDF",
            download = "open",
            className = "dt-button-2",
            modifier = list(order = "applied", page = "all", search = "applied")
          )
        ),
        autoWidth = TRUE
      )
    )
  })
}
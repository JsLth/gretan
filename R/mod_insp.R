mod_insp_ui <- function(id, titles) {
  ns <- NS(id)
  
  bs4Dash::tabItem(
    "insp",
    shinyWidgets::pickerInput(
      ns("title"),
      "Topics",
      choices = titles,
      selected = NULL,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        windowPadding = c(30, 0, 0, 0),
        liveSearch = TRUE
      ),
      inline = TRUE
    ),
    shinyWidgets::pickerInput(
      ns("subitem"),
      "Subitems",
      choices = character(),
      multiple = TRUE,
      inline = TRUE
    ),
    shinyWidgets::pickerInput(
      ns("option"),
      "Options",
      choices = character(),
      multiple = TRUE,
      inline = TRUE
    ),
    hr(),
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
    hr(),
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
      class = c("compact", "hover", "stripe", "order-column"),
      extensions = c("Buttons", "Scroller", "KeyTable", "RowGroup"),
      selection = "none",
      filter = list(position = "top", clear = FALSE),
      style = "bootstrap4",
      rownames = FALSE,
      options = list(
        dom = "Bfrtip",
        pageLength = 50,
        scroller = TRUE,
        scrollY = 600,
        deferRender = TRUE,
        keys = TRUE,
        rowGroup = list(
          dataSrc = 1:3,
          emptyDataGroup = NULL,
          startRender = DT::JS("function (rows, group, level) {
            if (group == null) {
              return null
            }
            if (level == 0) {
              return 'Question: ' + group + ' (' + rows.count() + ' rows)';
            } else if (level == 1) {
              return 'Subitem: ' + group;
            } else {
              return 'Option: ' + group
            }
          }")
        ),
        columnDefs = list(list(targets = 1:3, visible = FALSE)),
        pageLength = 100,
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
        # ajax = list(
        #   url = "https://datatables.net/examples/server_side/scripts/jsonp.php",
        #   dataType = "jsonp",
        #   processing = TRUE
        # )
      )
    )
  })
}
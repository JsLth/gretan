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
        actionsBox = TRUE,
        windowPadding = c(30, 0, 0, 0),
        liveSearch = TRUE,
        selectedTextFormat = "count",
        countSelectedText = "{0} topics selected",
        noneSelectedText = "No filters applied"
      ),
      inline = TRUE
    ),
    shinyWidgets::pickerInput(
      ns("subitem"),
      "Subitems",
      choices = character(),
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        windowPadding = c(30, 0, 0, 0),
        liveSearch = TRUE,
        selectedTextFormat = "count",
        countSelectedText = "{0} subitems selected",
        noneSelectedText = "No filters applied"
      ),
      inline = TRUE
    ),
    shinyWidgets::pickerInput(
      ns("option"),
      "Options",
      choices = character(),
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        windowPadding = c(30, 0, 0, 0),
        liveSearch = TRUE,
        selectedTextFormat = "count",
        countSelectedText = "{0} options selected",
        noneSelectedText = "No filters applied"
      ),
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
    hr(),
    downloadButton(ns("csv"), "", class = "dt-button-2", icon = NULL),
    downloadButton(ns("json"), "", class = "dt-button-2", icon = NULL),
    DT::dataTableOutput(ns("table"))
  )
}


mod_insp_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    invar <- reactive({
      entries <- cb_ext[cb_ext$title %in% input$title, ]

      if (length(input$subitem) > 0) {
        entries <- entries[entries$subitem %in% input$subitem, ]
      }

      if (length(input$option) > 0) {
        entries <- entries[entries$option %in% input$option, ]
      }

      entries$variable
    }) %>%
      bindEvent(input$title, input$subitem, input$option)

    observe({
      entry <- cb_ext[cb_ext$variable %in% invar(), ]
      titles <- entry$title
      subitems <- entry$subitem

      if (length(subitems) > 0 || !any(is.na(subitems))) {
        headers <- paste("Topic:", titles)
        headers <- factor(headers, levels = unique(headers))
        choices <- drop_nulls(tapply(subitems, INDEX = headers, FUN = unique))
        choices <- choices[!is.na(choices)]
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "subitem",
          choices = choices
        )
      } else {
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "subitem",
          choices = character()
        )
      }
    }) %>%
      bindEvent(input$title)

    observe({
      entry <- cb_ext[cb_ext$variable %in% invar(), ]
      titles <- entry$title
      subitems <- entry$subitem
      options <- entry$option

      if (length(options) > 1) {
        headers <- if (length(subitems) > 1 && !any(is.na(subitems))) {
          paste("Subitem:", subitems)
        } else {
          paste("Topic:", titles)
        }
        headers <- factor(headers, levels = unique(headers))
        choices <- tapply(options, INDEX = headers, FUN = unique)
        choices <- choices[!is.na(choices)]
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "option",
          choices = choices
        )
      } else {
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "option",
          choices = character()
        )
      }
    }) %>%
      bindEvent(input$title, input$subitem)

    table <- reactive({
      invar <- invar()
      poly <- get(paste0("srv_", input$aggr))
      if (length(invar) > 0) {
        poly <- subset_mns(poly, invar)
      }
      poly <- mns_pivot_longer(poly)
      poly[["gid"]] <- NULL
      for (col in colnames(poly)) {
        if (is.character(poly[[col]]) && !col %in% "question") {
          poly[[col]] <- as.factor(poly[[col]])
        }
      }
      poly
    })

    dt <- reactive({
      DT::datatable(
        table(),
        class = c("compact", "hover", "stripe", "order-column"),
        extensions = c("Buttons", "Scroller", "KeyTable", "RowGroup"),
        selection = "none",
        filter = list(position = "top", clear = FALSE),
        style = "bootstrap4",
        rownames = FALSE,
        # Callback that appends shiny download buttons to DT download buttons
        # This is needed because server-side processing prevents DT buttons
        # from downloading the entire dataset
        callback = DT::JS("
          var dwn1 = document.createElement('a');
          var dwn2 = document.createElement('a');
          $(dwn1).addClass($('a.dt-button-2')[0].className);
          $(dwn2).addClass($('a.dt-button-2')[0].className);
          dwn1.href = document.getElementById('main-insp-csv').getAttribute('href');
          dwn2.href = document.getElementById('main-insp-json').getAttribute('href');
          dwn1.download = '';
          dwn2.download = '';
          $(dwn1).attr('target', '_blank');
          $(dwn2).attr('target', '_blank');
          $(dwn1).text('Save as CSV');
          $(dwn2).text('Save as JSON');
          $('div.dwn-wrapper').append(dwn1);
          $('div.dwn-wrapper').append(dwn2);
          $('#main-insp-csv').hide();
          $('#main-insp-json').hide();
        "),
        options = list(
          dom = "B<'dwn-wrapper'>frtip",
          pageLength = 50,
          scroller = TRUE,
          scrollY = 600,
          deferRender = TRUE,
          keys = TRUE,
          rowGroup = list(
            dataSrc = 1:3,
            emptyDataGroup = NULL,
            # Add context info to row groups
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
          # Hide columns that are shown as row groups
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

    output$table <- DT::renderDataTable(execute_safely(dt()), server = TRUE)

    output$csv <- downloadHandler(
      filename = function() {
        paste0("greta-mns-", input$aggr, ".csv")
      },
      content = function(file) {
        utils::write.csv(table(), file, row.names = FALSE)
      }
    )

    output$json <- downloadHandler(
      filename = function() {
        paste0("greta-mns-", input$aggr, ".json")
      },
      content = function(file) {
        jsonlite::write_json(table(), file, pretty = TRUE)
      }
    )
  })
}

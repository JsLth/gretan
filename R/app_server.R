server <- function(input, output, session) {
  # Capture search term
  searchbox_input <- reactive({
    search_input <- input$textSearch
    isTRUE(nzchar(search_input))
  })
  
  observe({
    searchbox_input <- searchbox_input()
    if (isTRUE(searchbox_input)) {
      shinyjs::removeClass("listbox", class = "hide-listbox")
    } else {
      shinyjs::addClass("listbox", class = "hide-listbox")
    }
  })
  
  # Change tab when search output is clicked
  for (x in names(txts)) {
    expr <- protect_arg(bs4Dash::updateTabItems(
      session, # TODO
      inputId = "sidebar",
      selected = x
    ), arg = "selected")
    shinyjs::onclick(paste0("suggestion-", x), expr = eval(expr))
  }
  
  # Fix sidebar when search is triggered (otherwise search bar won't show)
  shinyjs::onclick("textSearch", expr = {
    if (isFALSE(input$sidebarState)) {
      bs4Dash::updateSidebar("sidebarState")
    }
  })

  # Search texts and determine which search items to show
  output$listbox <- renderUI({
    print("a")
    req(searchbox_input())
    print("b")
    search_term <- input$textSearch
    idx <- vapply(txts, function(x) {
      x <- tag_to_text(x)
      any(grepl(search_term, x, ignore.case = TRUE))
    }, FUN.VALUE = logical(1))
    idx <- names(txts)[idx]
    options <- lapply(idx, function(x) {
      opt <- txts[[x]]
      opt <- span(
        id = paste0("suggestion-", x),
        class = "form-suggestion-row",
        div(opt$icon, class = "form-suggestion-icon"),
        div(opt$title)
      )
      id <- paste0("search_option_", x)
      div(class = "form-suggestion", role = "option", opt)
    })
    cat("search results!")
    do.call(div, c(options, class = "form-suggestions"))
  })
  
  mod_main_server("main")
  withCallingHandlers({
      mod_main_server("main")
    },
    error = function(e) send_error(div(
      style = "text-align: left",
      paste(
        "Something went wrong! If this keeps happening, consider notifying the",
        "tool maintainer (jonas.lieth@gesis.org).",
      ),
      br(), br(),
      "Error details:", br(),
      tags$code(as.character(e$message))
    ))
  )
  
}
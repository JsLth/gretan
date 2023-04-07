server <- function(input, output, session) {
  # Capture search term
  searchbox_input <- reactive({
    search_input <- input$textSearch
    isTRUE(nzchar(search_input))
  })
  
  # Change tab when search output is clicked
  for (x in names(txts)) {
    with_eval_args(
      shinyjs::onclick(paste0("suggestion-", x), expr = bs4Dash::updateTabItems(
        session,
        inputId = "sidebar",
        selected = x
      ))
    )
  }
  
  # Fix sidebar when search is triggered (otherwise search bar won't show)
  shinyjs::onclick("textSearch", expr = {
    if (isFALSE(input$sidebarState)) {
      bs4Dash::updateSidebar("sidebarState")
    }
  })
  
  # Search texts and determine which search items to show
  search_results <- reactive({
    req(searchbox_input())
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
    do.call(div, c(options, class = "form-suggestions"))
  })
  
  # If no search term is provided, hide search results UI
  # If a search term is provided, show it
  observe({
    has_input <- searchbox_input()
    
    if (has_input) {
      insertUI(
        selector = "#textSearch",
        where = "afterEnd",
        ui = div(id = "listbox", class = "form-results", search_results())
      )
    } else {
      removeUI(
        selector = "div:has(> .form-suggestions)",
        multiple = TRUE,
        immediate = TRUE
      )
    }
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
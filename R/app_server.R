#' @export
#' @keywords internal
#' @rdname mod_base
app_server <- function(input, output, session) {
  # Start setup ----
  log_it("Starting app")

  onSessionEnded(fun = function() {
    log_it("Shutting down app")
    stopApp()
  })
  
  # Track tab selection ----
  tabsel <- reactive({
    log_it(sprintf("Changed active module to {%s}", input$sidebar))
    input$sidebar
  }, label = "track tab selection")

  # Hide help switch
  shinyjs::hideElement(selector = "ul.navbar-right")

  # Capture search term ----
  searchbox_input <- reactive({
    search_input <- input$textSearch
    isTRUE(nzchar(search_input))
  }, label = "capture search term")
  
  # Change tab after search ----
  for (x in names(txts)) {
    with_eval_args(
      shinyjs::onclick(paste0("suggestion-", x), expr = bs4Dash::updateTabItems(
        session,
        inputId = "sidebar",
        selected = x
      ))
    )
  }

  # Fix sidebar on search ----
  shinyjs::onclick("textSearch", expr = {
    if (isFALSE(input$sidebarState)) {
      bs4Dash::updateSidebar("sidebarState")
    }
  })

  # Search texts ----
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
    if (!length(options)) {
      options <- list(div(
        class = "form-suggestion",
        role = "option",
        "No results"
      ))
    }
    do.call(div, c(options, class = "form-suggestions"))
  }, label = "search texts")

  # Show/hide search bar ----
  observe({
    has_input <- searchbox_input()

    if (has_input) {
      insertUI(
        selector = "#textSearch",
        where = "afterEnd",
        ui = div(
          id = "listbox",
          class = "form-results",
          execute_safely(search_results())
        )
      )
    } else {
      removeUI(
        selector = "div:has(> .form-suggestions)",
        multiple = TRUE,
        immediate = TRUE
      )
    }
  }, label = "show/hide search bar")


  # Forward from home section ----
  all_tabs <- c(
    "exp", "taxonomy", "cs1italy", "stakeholder",
    "persona", "enpov", "attitudes", "research"
  )
  for (tab in all_tabs) {
    env <- new.env()
    env[["tab"]] <- tab
    obs_label <- paste("forward to", tab)
    welcome_id <- paste0("welcome-li-", tab)
    with_eval_args({
      observe(
        {
          bs4Dash::updateTabItems(inputId = "sidebar", selected = tab)
        },
        label = paste("forward to", obs_label)
      ) %>%
        bindEvent(input[[welcome_id]])
    })
  }
  
  
  # Situational info alerts ----
  send_info(
    text = tagList(
      "Are you here for the persona quiz? If so, follow the link below
      to jump straight to the quiz!",
      br(), br(),
      actionLink(
        "persona-quiz-info",
        label = "Click here to jump to the persona quiz!",
        icon = icon("arrow-right")
      ),
      br(), br(),
      "In any case, feel free to explore this webtool and discover some of the
      interactive data analyses that the GRETA project has produced over
      the last years!"
    ),
    title = "Welcome to the GRETA Analytics webtool!"
  )
  
  observe({
    shinyWidgets::closeSweetAlert()
    bs4Dash::updateTabItems(inputId = "sidebar", selected = "persona")
  }) %>%
    bindEvent(input[["persona-quiz-info"]])

  mod_main_server("main", tab = tabsel)
}

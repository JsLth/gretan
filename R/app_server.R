#' @export
#' @keywords internal
#' @rdname mod_base
app_server <- function(input, output, session) {
  log_it("Starting app")
  
  onSessionEnded(fun = function() {
    log_it("Shutting down app")
  })
  
  tabsel <- reactive(input$sidebar)
  
  # Hide help switch
  shinyjs::hideElement(selector = "ul.navbar-right")
  
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
    if (!length(options)) {
      options <- list(div(
        class = "form-suggestion",
        role = "option",
        "No results"
      ))
    }
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
  })
  
  observe({
    guide <- cicerone::Cicerone$new(
      allow_close = FALSE,
      stage_background = "transparent"
    )$step(
      el = "greta-logo",
      title = "What is GRETA Analytics?",
      description = "GRETA Analytics is a tool dedicated to interactively explore the results of the GRETA project on energy citizenship. The tool is filled with info material, figures, diagrams, plots, and tables \u2013 all of which can be interactively explored.",
      position = "bottom"
    )$step(
      el = "sidebarItemExpanded",
      title = "Select a subsection",
      description = "GRETA Analytics features several tools, frameworks and analyses. The sidebar allows you to switch between them.",
      position = "right"
    )$step(
      el = "mnsHighlight",
      title = "Multinational survey",
      description = "The subsection \u201cMultinational survey\u201d allows you to explore the results of GRETA's survey conducted among citizens in 16 EU countries.",
      position = "right"
    )$step(
      el = "csHighlight",
      title = "Case studies",
      description = "In the context of GRETA, six case studies have been conducted. Here, you can explore the results of a selection of case studies.",
      position = "right"
    )$step(
      el = "indHighlight",
      title = "Individual analyses",
      description = "Besides, the multinational survey and case studies, many more analyses have been performed in the context of GRETA tasks. Some of them are included in GRETA Analytics to explore.",
      position = "right"
    )$init()$start()
  }, priority = 0) %>%
    bindEvent(input$tour)
  
  mod_main_server("main", tab = tabsel)
}
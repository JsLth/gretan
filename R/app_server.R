#' @export
#' @keywords internal
#' @rdname mod_base
app_server <- function(input, output, session) {
  # Start setup ----
  gopts <- getGretaOption()
  log_it("Starting app", priority = TRUE)
  if (length(gopts)) {
    log_it(
      "The following options are set:",
      details = paste(names(gopts), gopts, sep = ": "),
      priority = TRUE
    )
  }

  onSessionEnded(fun = function() {
    log_it("Shutting down app")
    stopApp()
  })

  # Track tab selection ----
  tabsel <- reactive(
    {
      log_it(sprintf("Changed active module to {%s}", input$sidebar))
      input$sidebar
    },
    label = "track tab selection"
  )

  changed <- reactive({
    req(
      !startsWith(input$changed, "."),
      !endsWith(input$changed, "hidden"),
      !startsWith(input$changed, "waiter")
    )
    input$changed
  })

  # Hide help switch
  shinyjs::hideElement(selector = "ul.navbar-right")

  # Capture search term ----
  searchbox_input <- reactive(
    {
      search_input <- input$textSearch
      isTRUE(nzchar(search_input))
    },
    label = "capture search term"
  )

  # Change tab after search ----
  for (x in names(txts$main)) {
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
  search_results <- reactive(
    {
      req(searchbox_input())
      main <- txts$main
      search_term <- input$textSearch
      idx <- vapply(main, function(x) {
        x <- tag_to_text(x)
        any(grepl(search_term, x, ignore.case = TRUE))
      }, FUN.VALUE = logical(1))
      idx <- names(main)[idx]
      options <- lapply(idx, function(x) {
        opt <- main[[x]]
        opt <- span(
          id = paste0("suggestion-", x),
          class = "form-suggestion-row",
          div(icon(opt$icon), class = "form-suggestion-icon"),
          div(opt$shortitle)
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
    },
    label = "search texts"
  )

  # Show/hide search bar ----
  observe(
    {
      has_input <- searchbox_input()
      
      removeUI(
        selector = "div:has(> .form-suggestions)",
        multiple = TRUE,
        immediate = TRUE
      )
      
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
      }
    },
    label = "show/hide search bar"
  )


  # Forward from home section ----
  all_tabs <- c(
    "exp", "taxonomy", "cs1", "cs2", "cs3", "cs4", "cs5", "stakeholder",
    "persona", "enpov", "attitudes", "research"
  )
  for (tab in all_tabs) {
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

  observe({
    bs4Dash::updateTabItems(session, "sidebar", "exp")
  }) %>%
    bindEvent(input[["main-cs4-exp_link"]])


  # Change URL path to tab selection ----
  observe(
    {
      tab <- tabsel()
      cqstring <- getQueryString(session)$tab
      pqstring <- paste0("?tab=", tab)

      if (is.null(cqstring) || !identical(cqstring, tab)) {
        freezeReactiveValue(input, "sidebar")
        updateQueryString(pqstring, mode = "push", session = session)
      }
    },
    priority = 0
  ) %>%
    bindEvent(tabsel())


  # Select tab from URL path ----
  observe(
    {
      tab <- tabsel()
      cqstring <- parseQueryString(session$clientData$url_search)$tab

      if (is.null(tab) || !is.null(cqstring) && !identical(cqstring, tab)) {
        freezeReactiveValue(input, "sidebar")
        bs4Dash::updateTabItems(session, "sidebar", cqstring)
      }
    },
    priority = 1
  ) %>%
    bindEvent(getQueryString(session)$tab)


  mod_main_server("main", tab = tabsel, changed = changed)
}

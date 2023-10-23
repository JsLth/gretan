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


  if (Sys.info()[["user"]] == "shiny") {
    Sys.setenv(PYTHON_PATH = "/usr/bin/python3")
    Sys.setenv(VIRTUALENV_NAME = "gretan")
    Sys.setenv(RETICULATE_PYTHON = "/home/shiny/.virtualenvs/gretan/bin/python")
    envdir <- Sys.getenv("VIRTUALENV_NAME")
    envs <- reticulate::virtualenv_list()

    if (!envdir %in% envs) {
      deps <- c("numpy", "lightgbm", "pLAtYpus_TNO==1.0.4")
      reticulate::virtualenv_create(
        envname = envdir,
        python = Sys.getenv("PYTHON_PATH")
      )
      reticulate::virtualenv_install(
        envdir,
        packages = deps,
        ignore_installed = TRUE,
        pip_options = "--ignore-requires-python"
      )
    }

    reticulate::use_virtualenv(virtualenv = envdir, required = TRUE)
    print(reticulate::py_version())
  }


  # Track user metrics ----
  if (isTRUE(getGretaOption("track"))) {
    required <- NULL
    required[1] <- requireNamespace("googledrive", quietly = TRUE)
    required[2] <- requireNamespace("gargle", quietly = TRUE)
    required[3] <- requireNamespace("shinylogs", quietly = TRUE)

    if (!all(required)) {
      stop(paste0(
        "Packages googledrive, gargle and shinylogs",
        "are needed to track user metrics."
      ))
    }

    if (is.null(Sys.getenv("GDRIVE_KEY"))) {
      stop("A valid auth key must be provided to track user metrics.")
    }

    googledrive::drive_auth(
      path = gargle::secret_decrypt_json(
        app_sys("app/greta-analytics-6a4bb2fe4e5f.json"),
        key = "GDRIVE_KEY"
      )
    )

    # exclude internal inputs
    exclude <- c(
      "^\\.", "^waiter", "^plotly", "box$", "font$", "config$", "zoom$",
      "mouseover$", "mouseout$", "center$", "bounds$"
    )

    shinylogs::track_usage(
      shinylogs::store_custom(force_store_googledrive),
      exclude_input_regex = paste(exclude, collapse = "|"),
      what = c("session", "input", "output")
    )
  } else {
    onSessionEnded(fun = shutdown)
  }


  # Track tab selection ----
  tabsel <- reactive(
    {
      log_it(sprintf("Changed active module to {%s}", input$sidebar))
      input$sidebar
    },
    label = "track tab selection"
  )


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


  mod_main_server("main", tab = tabsel)
}

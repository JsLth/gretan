align_in_table <- function(..., sep = " ", bold = TRUE, .list = NULL) {
  .list <- .list %||% drop_nulls(list(...))

  lhs <- names(.list)
  labels <- mapply(
    align_td,
    x = lhs,
    y = .list,
    char = sep,
    bold = bold,
    SIMPLIFY = FALSE
  )

  lapply(
    do.call(paste, labels),
    function(x) protect_html(tags$table(HTML(x)))
  )
}

align_td <- function(x, y, char = " ", bold = TRUE) {
  div2 <- noWS(div)
  table2 <- noWS(tags$table)
  tr2 <- noWS(tags$tr)
  td2 <- noWS(tags$td)
  mapply(function(left, right) {
    if (is.null(right) || is.na(right)) {
      return("")
    }
    if (is.character(right) && startsWith(as.character(right), "NA")) {
      right <- "N/A"
    }
    style <- NULL
    if (identical(left, "Sample")) {
      if (right <= 10) style <- "color: red"
      pl <- ifelse(right == 1, "", "s")
      right <- paste0(right, " respondent", pl)
    }
    if (bold) left <- tags$b(left)
    protect_html(tr2(
      td2(left, style = style),
      td2(char, style = style),
      td2(right, style = style),
      style = "line-height: 10px;"
    ))
  }, left = x, right = y, SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

# Selects a map element on click
leaflet_select <- function(id, geom, action, tol = 1) {
  if (!is.null(action)) {
    marker <- sf::st_sfc(sf::st_point(c(action$lng, action$lat)), crs = 4326)

    target <- geom[sf::st_is_within_distance(
      geom,
      marker,
      dist = tol,
      sparse = FALSE
    ), ]
  }
}

# Selects a text in `txts` that corresponds to a selected Leaflet element
leaflet_text_on_click <- function(id, geom, texts, click, col = "name", tol = 1) {
  target <- leaflet_select(id, geom, click, tol = tol)

  if (is.null(target)) {
    target <- "none"
  } else {
    target <- target[[col]]
  }

  texts[[target]]
}


#' Track Leaflet coordinates
#' @description
#' Adds a Javascript Hook to Leaflet map widget that creates a input value
#' called `mousemove` which makes it possible to access map coordinates in a
#' server environment.
#'
#' @param map Leaflet map widget
#' @param id ID of the leaflet widget
#'
#' @noRd
track_coordinates <- function(map, id) {
  map$jsHooks[["render"]] <- c(
    map$jsHooks[["render"]],
    list(list(
      code = sprintf("function(el, x) {
        this.on('mousemove', function(e) {
          var lng = e.latlng.lng;
          var lat = e.latlng.lat;
          var coord = [lng, lat];
          Shiny.onInputChange('%s', coord)
        });
        this.on('mouseout', function(e) {
          Shiny.onInputChange('%s', null)
        })
      }", id, id),
      data = NULL
    ))
  )

  map
}

# Convert ANSI formatting of rlang errors to HTML
rlang_error_to_html <- function(e, ...) {
  e <- fansi::to_html(format(e), ...)
  tags$pre(HTML(gsub("\n", "<br>", e)))
}

# Send info message
send_info <- function(text,
                      title = "Info",
                      btn_labels = "Got it!",
                      closeOnClickOutside = FALSE,
                      ...,
                      session = getDefaultReactiveDomain()) {
  shinyWidgets::sendSweetAlert(
    title = title,
    text = text,
    type = "info",
    html = TRUE,
    btn_colors = "#5E81AC",
    btn_labels = btn_labels,
    closeOnClickOutside = closeOnClickOutside,
    ...
  )
}

# Send error message
send_error <- function(text,
                       title = "Oops!",
                       ...,
                       session = getDefaultReactiveDomain()) {
  shinyWidgets::sendSweetAlert(
    title = title,
    text = text,
    type = "error",
    html = TRUE,
    btn_colors = "#BF616A",
    btn_labels = "Got it!",
    closeOnClickOutside = FALSE,
    ...
  )
}


send_warning <- function(text,
                         title = "Attention!",
                         ...,
                         session = getDefaultReactiveDomain()) {
  shinyWidgets::sendSweetAlert(
    title = title,
    text = text,
    type = "warning",
    html = TRUE,
    btn_colors = "#FFCA2B",
    btn_labels = "Got it!",
    closeOnClickOutside = FALSE,
    ...
  )
}


#' Execute expression safely in server
#' @description
#' Errors in a server environment usually lead to the Shiny app crashing. To
#' prevent this, `execute_safely` catches errors and displays a user message
#' instead. This function is a modified version of
#' `shinyWidgets::execute_safely`. With other defaults, different formatting,
#' and the ability to handle silent expressions.
#'
#' @param expr Expression to evaluate
#' @param title Title to display
#' @param message Message to display
#' @param stopOperation Whether to stop the reactive chain after showing the
#' error message.
#' @param session Session object.
#'
#' @noRd
execute_safely <- function(expr,
                           title = "Oops!",
                           message = NULL,
                           stopOperation = TRUE,
                           session = getDefaultReactiveDomain()) {
  message <- message %||% HTML(paste0(
    "Something went wrong! If this keeps happening, consider ",
    "opening a <a href='https://github.com/JsLth/gretan/issues'>Github ",
    "issue</a> or email the tool maintainer (",
    "<a href = 'mailto:jonas.lieth@gesis.org'>jonas.lieth@gesis.org</a>)."
  ))

  tryCatch(
    expr = {
      # In case of warning, return expression
      withCallingHandlers(
        expr = expr,
        warning = function(w) {
          log_it(
            log = "A warning was produced",
            type = "warn",
            details = w$message,
            priority = TRUE
          )
        }
      )
    },
    error = function(e) {
      # Stop without error message
      if (inherits(e, "shiny.silent.error")) req(FALSE)

      if (any(startsWith(class(e), "python"))) {
        e <- reticulate::py_last_error()$message
      }

      send_error(div(
        style = "text-align: left",
        message,
        br(), br(),
        "Error details:", br(),
        rlang_error_to_html(e, warn = FALSE)
      ), session = session, title = title)

      log_it(
        log = "An error occurred",
        type = "error",
        details = format(e),
        priority = TRUE
      )

      # Send error message and then stop
      if (stopOperation) req(FALSE)

      return(e)
    }
  )
}


observe2 <- function(expr, ..., env = parent.frame(), quoted = FALSE) {
  expr <- substitute(expr)
  env <- parent.frame()
  observe(execute_safely(eval(expr, envir = env)), ...)
}


reactive2 <- function(expr, ...) {
  expr <- substitute(expr)
  env <- parent.frame()
  reactive(execute_safely(eval(expr, envir = env)), ...)
}


# global waiter defaults
waiter_default <- list(
  color = "rgba(179, 221, 254, 0.8)", # greta blue
  html = tagList(
    waiter::spin_pulse(),
    h4("Loading figure...")
  ),
  hide_on_render = TRUE,
  hide_on_error = FALSE # on error, spin forever
)

# global highlight defaults
highlight_opts <- leaflet::highlightOptions(
  weight = 2,
  color = "black",
  opacity = 0.5,
  fillOpacity = 1,
  bringToFront = TRUE,
  sendToBack = TRUE
)


# popover with custom defaults
popover2 <- function(id,
                     title = NULL,
                     content,
                     trigger = "focus",
                     placement = "right",
                     fallbackPlacement = "flip",
                     offset = 0,
                     boundary = "scrollParent",
                     selector = NULL) {
  bs4Dash::addPopover(
    id = id,
    selector = selector,
    options = list(
      title = title,
      content = content,
      placement = placement,
      fallbackPlacement = fallbackPlacement,
      offset = offset,
      boundary = boundary,
      trigger = trigger,
      html = TRUE
    )
  )
}

#' Execute call with evaluated arguments
#' @description
#' Forces the evaluation of arguments in a call in a given environment.
#' Useful for looping through function factories that defuse expressions
#' and evaluate them later when the returned function is called.
#' Examples:
#' - shiny::onclick
#' - shiny update functions
#'
#' @param call Function call or expression
#' @param envir Environment that arguments in `call` should be evaluated in.
#'
#' @examples
#' funfact <- function(expr) {
#'   expr <- deparse(substitute(expr))
#'   pframe <- parent.frame()
#'   function() eval(parse(text = expr), envir = pframe)
#' }
#'
#' fun1 <- lapply(1:3, testf)
#' fun2 <- lapply(1:3, \(x) with_eval_args(testf(x)))
#' for (f in fun1) print(f())
#' for (f in fun2) print(f())
#' @noRd
with_eval_args <- function(call, envir = parent.frame()) {
  call <- substitute(call)
  call <- eval_args(call, envir = envir)
  eval(call, envir = envir)
}

# Forces evaluation of arguments in a call before passing it on.
# Useful for looping through function calls that defuse expressions and
# evaluate them in a different environment.
eval_args <- function(call, envir) {
  for (i in seq_along(call)[-1]) {
    arg <- call[[i]]
    if (is.name(arg)) {
      val <- eval(arg, envir = envir)
      if (!typeof(val) %in% c("environment", "function")) {
        call[[i]] <- eval(arg, envir = envir)
      }
    } else if (is.call(arg)) {
      call[[i]] <- eval_args(arg, envir = envir)
    }
  }
  call
}

# Get caller source, if possible with code line
srcloc <- function(idx = 1) {
  x <- .traceback(x = 1)
  srcref <- attr(x[[idx]], "srcref")
  if (!is.null(srcref)) {
    srcfile <- attr(srcref, "srcfile")
    sprintf(
      "Calling %s at %s#%s",
      x[[idx + 1]],
      basename(srcfile$filename),
      srcref[1]
    )
  }
}

# Get ID of a Shiny module based on namespace
get_module_id <- function(session = getDefaultReactiveDomain()) {
  if (!is.null(session)) {
    id <- session$ns("")
    if (nzchar(id)) {
      ns <- strsplit(session$ns(""), "-")[[1]]
      id <- ns[length(ns)]
    }

    id
  }
}


get_tab <- function(session = getDefaultReactiveDomain()) {
  if (!is.null(session)) {
    parseQueryString(session$clientData$url_search)$tab
  }
}


# Alternative to cat that prints line breaks
cat2 <- function(...) {
  cat(..., "\n")
}

#' Internal logging
#'
#' Basic Shiny app logging
#' @param log Log message
#' @param type Type of the message
#' @param details Optional details to append to the logs
#' @param priority Whether the log is a priority log. Priority logs are always
#' printed, even if the input parameters from `run_app` restrict log output.
#' @param session Session object
#'
#' @noRd
log_it <- function(log = NULL,
                   type = c("info", "warn", "error", "success"),
                   details = NULL,
                   priority = FALSE,
                   session = getDefaultReactiveDomain()) {
  if (is.null(session)) {
    stop("log_it must be called within a Shiny session", call. = FALSE)
  }

  out <- getGretaOption("logging", "")

  if (isFALSE(out)) {
    return(invisible())
  }
  type <- match.arg(type)
  time <- format(Sys.time(), "%F %T")
  ns <- get_module_id(session)
  valid_ns <- if (!nzchar(out)) ns else out

  if (!dir.exists(out) && isFALSE(ns %in% valid_ns)) {
    return(invisible())
  }

  if (!nzchar(out) && interactive()) {
    col <- switch(type,
      info = "\033[32m[%s]\033[39m",
      warn = "\033[33m[%s]\033[39m",
      error = "\033[31m[%s]\033[39m",
      success = "\033[34m[%s]\033[39m"
    )
  } else {
    col <- "[%s]"
  }
  type <- sprintf(col, toupper(type))
  log <- log %||% srcloc(idx = 2L)

  if (nzchar(ns)) {
    ns <- sprintf(" {%s} ", ns)
  } else {
    ns <- " "
  }

  cat2(sprintf("%s %s%s%s", time, type, ns, log), file = out, append = TRUE)

  if (!is.null(details)) {
    log_details(details)
  }
}

log_details <- function(logs, session = getDefaultReactiveDomain()) {
  for (l in logs) {
    cat2(paste0("\t", l))
  }
}


shutdown <- function() {
  log_it("Shutting down app")
  stopApp()
}


try_gdrive_upload <- function(media, name, ...) {
  try(
    googledrive::drive_upload(
      media = media,
      path = "gretan-metrics",
      name = name
    ),
    silent = TRUE
  )
}


force_store_googledrive <- function(logs, ...) {
  path <- normalizePath(tempfile(fileext = ".json"), "/", mustWork = FALSE)
  jsonlite::write_json(logs, path = path, auto_unbox = TRUE, pretty = TRUE)
  timestamp <- format(as.numeric(Sys.time()) * 1e4, scientific = FALSE)
  file_name <- paste0("log_gretan_", timestamp, ".json")

  log_it("Uploading user metrics")

  uperr <- try_gdrive_upload(media = path, name = file_name)
  while (inherits(uperr, "try-error")) {
    uperr <- try_gdrive_upload(media = path, name = file_name)
  }

  shutdown()
}


with_greta_options <- function(app, options) {
  app$appOptions$greta_options <- options

  log <- options$logging
  if (!is.null(log) && file.exists(log)) {
    cat2(sprintf("Saving logs to %s", substitute(log)))
  }

  app
}

getGretaOption <- function(name, default = NULL) {
  opts <- getShinyOption("greta_options")

  if (missing(name)) {
    return(opts)
  }

  option <- if (!is.null(opts)) {
    opts[[name]] %||% default
  } else {
    default
  }

  option
}

protect_html <- function(x) HTML(as.character(x))

quietly <- function(x) {
  capture.output(
    return(suppressMessages(suppressWarnings(x)))
  )
}

match_regex <- function(pattern, text, ...) {
  regmatches(text, regexec(pattern, text, ...))
}

to_title <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

rm_ns <- function(id, ns) substr(id, nchar(ns(NULL)) + 2, nchar(id))

#' Riffle-merges two vectors, possibly of different lengths
#'
#' Takes two vectors and interleaves the elements.  If one vector is longer than
#' the other, it appends on the tail of the longer vector to the output vector.
#' @param a First vector
#' @param b Second vector
#' @return Interleaved vector as described above.
#' @author Matt Pettis
#' @noRd
riffle <- function(a, b) {
  len_a <- length(a)
  len_b <- length(b)
  len_comm <- pmin(len_a, len_b)
  len_tail <- abs(len_a - len_b)

  if (len_a < 1) stop("First vector has length less than 1")
  if (len_b < 1) stop("Second vector has length less than 1")

  riffle_common <- c(rbind(a[1:len_comm], b[1:len_comm]))

  if (len_tail == 0) {
    return(riffle_common)
  }

  if (len_a > len_b) {
    return(c(riffle_common, a[(len_comm + 1):len_a]))
  } else {
    return(c(riffle_common, b[(len_comm + 1):len_b]))
  }
}

#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %|NA|% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

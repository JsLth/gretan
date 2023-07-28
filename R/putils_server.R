locm_colors_abel <- function(locm, df) {
  n <- nrow(df)
  vec <- seq(1, n)
  vec <- locm[, 5] < 0.05
  q <- attributes(locm)$quadr$mean
  colors <- seq(1, n)
  for (i in 1:n) {
    if (q[i] == "High-High") colors[i] <- "red"
    if (q[i] == "Low-Low")   colors[i] <- "blue"
    if (q[i] == "Low-High")  colors[i] <- "lightblue"
    if (q[i] == "High-Low")  colors[i] <- "pink"
  }
  locm.dt <- as.numeric(as.factor(q)) * vec
  colors1 <- colors
  for (i in 1:n) {
    if (!is.na(locm.dt[i]) )  {
      if (locm.dt[i] == 0) colors1[i] <- "white"
    }
  }
  
  list(outline = colors, fill = colors1)
}

list_to_css <- function(x) {
  paste(paste0(names(x), ": ", x, ";"), collapse = " ")
}


as_likert <- function(x, scale = NULL) {
  if (length(scale) > 7) {
    stop(sprintf("Likert scale is too long (%s items)", length(x)))
  }
  
  if (is.null(scale)) {
    scale <- c(
      "Strongly disagree", "Disagree", "Somewhat disagree",
      "Neutral",
      "Somewhat agree", "Agree", "Strongly agree"
    )
  }
  
  if (is.factor(scale)) {
    scale <- as.character(scale)
  }

  values <- vapply(x, function(i) scale[i], FUN.VALUE = character(1))
  ordered(values, levels = scale)
}

align_dl <- function(..., sep = " ", bold = TRUE) {
  dots <- drop_nulls(list(...))
  lhs <- names(dots)
  labels <- mapply(
    align_td,
    x = lhs,
    y = dots,
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
    if (is.null(right) || is.na(right)) return("")
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


leaflet_text_on_click <- function(id, geom, texts, click, col = "name", tol = 1) {
  target <- leaflet_select(id, geom, click, tol = tol)
  
  if (is.null(target)) {
    target <- "none"
  } else {
    target <- target[[col]]
  }
  
  texts[[target]]
}


track_coordinates <- function(map, id, session = getDefaultReactiveDomain()) {
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

scroll <- function(id, block = c("start", "center", "end", "nearest")){
  if (missing(id))
    stop("Missing `id`", call. = FALSE)
  
  blk <- match.arg(block)
  
  session <- getDefaultReactiveDomain()
  session$sendCustomMessage("scroll", list(id = id, block = blk))
  invisible()
}

rlang_error_to_html <- function(x) {
  x <- as.character(x)
  if (requireNamespace("fansi", quietly = TRUE)) {
    x <- fansi::sgr_to_html(x)
  }
  HTML(gsub("\n", "<br>", x))
}


send_info <- function(text,
                      title = "Info",
                      session = getDefaultReactiveDomain()) {
  shinyWidgets::sendSweetAlert(
    title = title,
    text = text,
    type = "info",
    html = TRUE,
    btn_colors = "#5E81AC",
    btn_labels = "Got it!"
  )
}

send_error <- function(text,
                       title = "Oops!",
                       session = getDefaultReactiveDomain()) {
  shinyWidgets::sendSweetAlert(
    title = title,
    text = text,
    type = "error",
    html = TRUE,
    btn_colors = "#BF616A",
    btn_labels = "Got it!"
  )
}


execute_safely <- function(expr,
                           title = "Oops!",
                           message = NULL,
                           stopOperation = TRUE,
                           session = getDefaultReactiveDomain()
  ) {
  if (is.null(message)) {
    message <- paste(
      "Something went wrong! If this keeps happening, consider",
      "notifying the tool maintainer (jonas.lieth@gesis.org)."
    )
  }
  
  tryCatch(
    expr = expr,
    error = function(e) {
      send_error(div(
        style = "text-align: left",
        message,
        br(), br(),
        "Error details:", br(),
        tags$code(rlang_error_to_html(e))
      ), session = session, title = title)
      if (stopOperation) req(FALSE)
    }
  )
}


# 4, wobblebar, pulse, throbber, riplle, ring, wave
waiter_default <- list(
  color = "rgba(179, 221, 254, 0.8)", # greta blue
  html = tagList(
    waiter::spin_pulse(),
    h4("Loading figure...")
  ),
  hide_on_render = TRUE,
  hide_on_error = FALSE # on error, spin forever
)

highlight_opts <- leaflet::highlightOptions(
  weight = 2,
  color = "black",
  opacity = 0.5,
  fillOpacity = 1,
  bringToFront = TRUE,
  sendToBack = TRUE
)

plotly_config_default <- function(p) {
  plotly::config(
    p,
    modeBarButtonsToRemove = c(
      "sendDataToCloud", "editInChartStudio", "drawclosedpath", "drawopenpath",
      "drawline", "drawrect", "drawcircle", "eraseshape", "zoomIn2d", "zoomOut2d",
      "autoScale2d", "resetScale2d", "hoverClosestCartesian",
      "hoverCompareCartesian"
    ),
    scrollZoom = TRUE,
    responsive = TRUE,
    displaylogo = FALSE
  )
}

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

with_eval_args <- function(call, envir = parent.frame()) {
  call <- substitute(call)
  call <- eval_args(call, envir = envir)
  eval(call, envir = envir)
}

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

cat2 <- function(...) {
  cat(..., "\n")
}

log_it <- function(log = NULL, type = c("info", "warn", "error", "success")) {
  out <- getOption("greta_log", "")
  if (isFALSE(getOption("greta_log"))) return(invisible(NULL))
  type <- match.arg(type)
  time <- format(Sys.time(), "%F %T")
  if (!nzchar(out)) {
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
  if (is.null(log)) {
    log <- srcloc(idx = 2L)
  }
  cat2(sprintf("%s %s %s", time, type, log), file = out, append = TRUE)
}

log_details <- function(log) {
  cat2(paste0("\t", log))
}

with_logging <- function(code, value) {
  old <- options(greta_log = value)
  on.exit(options(old))
  force(code)
}

protect_html <- function(x) HTML(as.character(x))
with_html <- function(.f) function(...) protect_html(.f(...))

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
  
  if (len_tail == 0) return(riffle_common)
  
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

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- function(...) shiny::reactiveValues(...)
rvtl <- function(...) shiny::reactiveValuesToList(...)

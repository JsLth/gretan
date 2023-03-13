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
  
  list(colors, colors1)
}

make_html_label <- function(..., sep = " ", bold = TRUE) {
  dots <- drop_nulls(list(...))
  lhs <- names(dots)
  labels <- mapply(
    align_dl,
    x = lhs,
    y = dots,
    char = sep,
    bold = bold,
    SIMPLIFY = FALSE
  )
  lapply(do.call(paste, labels), HTML)
}

html_align_at_char <- function(x, y, char = " ", bold = TRUE) {
  div2 <- noWS(div)
  span2 <- noWS(span)
  mapply(FUN = function(x, y) {
    if (bold) x <- tags$b(paste(x, ":"))
    as.character(div(
      class = "progress-ww",
      div2(span2(x), char, span2(y))
    ))
  }, x = x, y = y, SIMPLIFY = FALSE, USE.NAMES = FALSE) %>%
  unlist()
}

align_dl <- function(..., sep = " ", bold = TRUE) {
  dots <- drop_nulls(list(...))
  lhs <- names(dots)
  labels <- mapply(
    align_dl_items,
    x = lhs,
    y = dots,
    char = sep,
    bold = bold,
    SIMPLIFY = FALSE
  )
  lapply(
    do.call(paste, c(labels, sep = "<br>")),
    function(x) protect_html(tags$dl(HTML(x), class = "map-labels-align"))
  )
}

align_dl_items <- function(x, y, char = " ", bold = TRUE) {
  div2 <- noWS(div)
  dl2 <- noWS(tags$dl)
  aligned <- mapply(FUN = function(dti, ddi) {
    dtlst <- unlist(lapply(
      dti,
      function(it) as.character(tags$dt(it, class = "map-labels-item"))
    ))
    ddlst <- unlist(lapply(
      ddi,
      function(it) as.character(tags$dd(it, class = "map-labels-item"))
    ))
    HTML(riffle(dtlst, ddlst))
  }, dti = x, ddi = y, SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

leaflet_select_click <- function(id, on, ref, input, tol = 1) {
  click <- input[[paste(id, on, "click", sep = "_")]]
  target <- NULL
  if (!is.null(click)) {
    marker <- sf::st_sfc(sf::st_point(c(click$lng, click$lat)), crs = 4326)
    
    target <- ref[sf::st_is_within_distance(
      ref$geometry,
      marker,
      dist = tol,
      sparse = FALSE
    ), ]
  }
  
  target
}

leaflet_text_on_click <- function(id,
                                  ref,
                                  texts,
                                  on = "marker",
                                  do = "click",
                                  tol = 1) {
  input <- get("input", envir = parent.frame())
  renderUI({
    target <- leaflet_select_click(
      id = id,
      on = on,
      ref = ref,
      input = input,
      tol = tol
    )
    
    if (is.null(target)) {
      target <- "none"
    } else {
      target <- target$name
    }
    
    texts[[target]]
  })
}

info_popup <- function(text, title = "Info") {
  shinyalert::shinyalert(
    title = title,
    text = text,
    type = "info",
    confirmButtonCol = "#5E81AC"
  )
}

highlight_opts <- leaflet::highlightOptions(
  weight = 2,
  color = "black",
  opacity = 0.5,
  fillOpacity = 1,
  bringToFront = TRUE,
  sendToBack = TRUE
)


protect_html <- function(x) HTML(as.character(x))

#' Riffle-merges two vectors, possibly of different lengths
#'
#' Takes two vectors and interleaves the elements.  If one vector is longer than
#' the other, it appends on the tail of the longer vector to the output vector.
#' @param a First vector
#' @param b Second vector
#' @return Interleaved vector as described above.
#' @author Matt Pettis
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

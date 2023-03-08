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

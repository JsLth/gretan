init <- function(..., session = getDefaultReactiveDomain()) {
  lapply(list(...), function(x) {
    session$userData[[x]] <- reactiveVal(0)
  })
}

trigger <- function(..., session = getDefaultReactiveDomain()) {
  lapply(list(...), function(x) {
    session$userData[[x]](session$userData[[x]]() + 1)
  })
}

watch <- function(..., session = getDefaultReactiveDomain()) {
  vapply(list(...), function(x) {
    as.integer(session$userData[[x]]())
  }, FUN.VALUE = integer(1))
}

on <- function(name, expr, session = getDefaultReactiveDomain()) {
  if (is.null(session$userData[[name]])) {
    stop(sprintf("Flag %s has not been initiated: can't listen to it."))
  }
  
  observe({
    subsitute(expr)
  }) %>%
    bindEvent(substitute(watch(name)), ignoreInit = TRUE, env = parent.frame())
}
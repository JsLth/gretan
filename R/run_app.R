#' Run the "greta" Shiny Application
#'
#' @examples
#' \dontrun{
#' library(greta)
#' greta::run_app()
#' greta::run_greta()
#' }
#' 
#' @param ... Arguments passed on to \code{\link[shiny]{shinyApp}}
#' @param log Whether to enable logging using the reactlog package.
#' For debugging purposes.
#'
#' @export
#' @import shiny
#' @importFrom dplyr %>% filter mutate select
run_app <- function(..., log = FALSE) {
  if (log) {
    if (!requireNamespace("reactlog")) {
      stop("The package reactlog is required to enable logging.")
    }
    reactlog::reactlog_enable()
  }
  shiny::shinyApp(ui = app_ui, server = server, ...)
}


#' @rdname run_app
#' @export
run_greta <- function(...) {
  run_app(...)
}


greta_bg <- function(...) {
  rstudioapi::jobRunScript("shiny-run.R")
}
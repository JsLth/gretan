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
#' @param log Where to output app logs. Accepts a directory path where a log
#' file is created. If `""`, prints directly to the console. If `FALSE`,
#' disables logging. Also accepts a character vector containing module IDs.
#' In this case, logging is enabled only for these modules.
#' @param reactlog Whether to enable logging using the reactlog package.
#' For debugging purposes.
#' @param options Options for \code{\link[shiny]{shinyApp}}. Included for use
#' in `electricShine`.
#'
#' @export
#' @import shiny
#' @importFrom leaflet %>%
run_app <- function(..., log = NULL, reactlog = FALSE, options = list()) {
  if (reactlog) {
    if (!requireNamespace("reactlog")) {
      stop("The package reactlog is required to enable logging.")
    }
    reactlog::reactlog_enable()
  }
  
  with_logging({
    shinyApp(ui = app_ui, server = app_server, options = options, ...)
  }, value = log)
}


#' @rdname run_app
#' @export
run_greta <- function(...) {
  run_app(...)
}


greta_bg <- function(...) {
  rstudioapi::jobRunScript("shiny-run.R")
}
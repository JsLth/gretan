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
#' @param log Whether to activate print logging.
#' @param reactlog Whether to enable logging using the reactlog package.
#' For debugging purposes.
#'
#' @export
#' @import shiny
#' @importFrom leaflet %>%
#' @importFrom stats lm fitted setNames
#' @importFrom utils stack write.csv
run_app <- function(..., log = NULL, reactlog = FALSE) {
  if (reactlog) {
    if (!requireNamespace("reactlog")) {
      stop("The package reactlog is required to enable logging.")
    }
    reactlog::reactlog_enable()
  }
  
  oldopt <- getOption("shiny.autoload.r")
  options(shiny.autoload.r = FALSE)
  on.exit(options(shiny.autoload.r = oldopt))
  
  with_logging({
    shinyApp(ui = app_ui, server = server, ...)
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
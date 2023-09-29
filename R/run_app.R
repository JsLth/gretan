#' Run GRETA Analytics
#'
#' @examples
#' \dontrun{
#' library(gretan)
#' gretan::run_app()
#' gretan::run_greta()
#' }
#'
#' @param log Where to output app logs. Accepts a directory path where a log
#' file is created. If \code{""}, prints directly to the console. If
#' \code{FALSE}, disables logging. Also accepts a character vector containing
#' module IDs. In this case, logging is enabled only for these modules.
#' @param reactlog Whether to enable logging using the reactlog package.
#' For debugging purposes.
#' @param set_python Python path to set before starting the app. This should
#' preferably be done before running the function.
#' @param console Whether to enable the in-app console. The console can be
#' accessed by pressing Ctrl + Shift + D and is located in the main module.
#' For debugging only!
#' @param options Options for \code{\link[shiny]{shinyApp}}. Included for use
#' in \code{electricShine}.
#' @param ... Further options to be passed to the app. Can be accessed using
#' \code{getGretaOption}.
#'
#' @export
#' @import shiny
#' @importFrom leaflet %>%
run_app <- function(log = NULL,
                    reactlog = FALSE,
                    set_python = NULL,
                    console = FALSE,
                    options = list(),
                    ...) {
  if (reactlog) {
    if (!requireNamespace("reactlog")) {
      stop("The package reactlog is required to enable logging.")
    }
    reactlog::reactlog_enable()
  }
  
  if (isTRUE(getOption("app.prod"))) {
    if (missing(log)) {
      file.create("gretan.log")
      log <- "gretan.log"
    }
    
    if (missing(reactlog))
      reactlog <- FALSE
    
    console <- FALSE
  }

  if (!is.null(set_python)) {
    reticulate::use_python(set_python)
  }
  
  with_greta_options(
    shinyApp(
      ui = app_ui,
      server = app_server,
      options = options,
      enableBookmarking = "disable"
    ),
    options = list(logging = log, console = console, ...)
  )
}


#' @rdname run_app
#' @export
run_greta <- function(...) {
  run_app(...)
}


greta_bg <- function(...) {
  rstudioapi::jobRunScript("shiny-run.R")
}

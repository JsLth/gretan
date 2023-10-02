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
#' @param options Options for \code{\link[shiny]{shinyApp}}. Included for use
#' in \code{electricShine}.
#' @param ... Further options to be passed to the app. Can be accessed using
#' \code{getGretaOption}. Currently this includes the options \code{"console"},
#' \code{"collect"} and \code{"github_available"}
#'
#' @export
#' @import shiny
#' @importFrom leaflet %>%
run_app <- function(log = NULL, reactlog = FALSE, options = list(), ...) {
  .dots <- list(...)
  if (reactlog) {
    if (!requireNamespace("reactlog")) {
      stop("The package reactlog is required to enable logging.")
    }
    reactlog::reactlog_enable()
  }

  if (isTRUE(getOption("app.prod"))) {
    if (missing(log) && Sys.info()[["user"]] != "shiny") {
      file.create("gretan.log")
      log <- "gretan.log"
    }

    if (missing(reactlog)) {
      reactlog <- FALSE
    }

    .dots$console <- FALSE
    .dots$collect <- TRUE
  }

  with_greta_options(
    shinyApp(
      ui = app_ui,
      server = app_server,
      options = options,
      enableBookmarking = "disable"
    ),
    options = c(.dots, logging = log)
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

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
#' @param python Path to a Python binary. Generally, \code{run_app()} requires a
#' Python version of 3.8 or higher and depends on the modules \code{numpy},
#' \code{lightgbm}, and \code{pLAtYpus_TNO}.
#' @param options Options for \code{\link[shiny]{shinyApp}}. Included for use
#' in \code{electricShine}.
#' @param prompt Whether to show a confirmation prompt before downloading
#' data dependencies.
#' @param ... Further options to be passed to the app. Can be accessed using
#' \code{getGretaOption}. This includes the following (mostly internal)
#' arguments:
#' \describe{
#'   \item{\code{console}}{Enables internal live console to execute R
#'   commands within the app. Strictly for internal use. Console can be
#'   accessed by pressing Ctrl + Shift + D.}
#'   \item{\code{track}}{Enables user metrics tracking. Requires a valid
#'   key to upload collected data to Google Drive stored in the environment
#'   variable \code{GDRIVE_KEY}.}
#'   \item{\code{proc}}{Background process of class \code{process}
#'   specifying code to execute while configuring. Internal use only.}
#' }
#'
#' @export
#' @import shiny
#' @importFrom leaflet %>%
run_app <- function(log = NULL,
                    reactlog = FALSE,
                    python = NULL,
                    options = list(),
                    prompt = interactive(),
                    ...) {
  .dots <- list(...)

  proc <- check_python(python, prompt = prompt)
  check_download_prompt(prompt = prompt)

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
    .dots$track <- TRUE
    .dots$proc <- proc
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
run_greta <- function(log = NULL,
                      reactlog = FALSE,
                      python = NULL,
                      options = list(),
                      prompt = interactive(),
                      ...) {
  run_app(
    log = log,
    reactlog = reactlog,
    python = python,
    options = options,
    prompt = prompt,
    ...
  )
}


greta_bg <- function(...) {
  rstudioapi::jobRunScript("shiny-run.R")
}

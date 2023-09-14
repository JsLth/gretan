#' Access files in the current app
#'
#' @param ... character vectors, specifying subdirectory
#' and file(s) within your package.
#' The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  path <- system.file(..., package = "gretan")

  # for rsconnect
  if (!nchar(path)) {
    path <- file.path("inst", ...)
  }

  path
}


add_external_resources <- function() {
  addResourcePath("www", app_sys("app/www"))
  tags$head(
    waiter::useWaiter(),
    shinyWidgets::useSweetAlert(theme = "bootstrap-4"),
    shinyjs::useShinyjs(),
    if (isTRUE(getGretaOption("console", FALSE))) keys::useKeys(),
    tags$script(src = "www/textsearch.js"),
    tags$script(src = "www/loadingbutton.js"),
    includeCSS(app_sys("app/www/styles.css")),
    tags$link(rel = "shortcut icon", href = "www/favicon.ico")
  )
}

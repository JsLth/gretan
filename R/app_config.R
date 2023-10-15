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
    waiter::useWaitress(),
    shinyWidgets::useSweetAlert(theme = "bootstrap-4"),
    shinyWidgets::chooseSliderSkin("Flat"),
    shinyjs::useShinyjs(),
    if (isTRUE(getGretaOption("console", FALSE))) keys::useKeys(),
    tags$script(src = "www/textsearch.js"),
    tags$script(src = "www/loadingbutton.js"),
    tags$script(src = "www/modules.js"),
    includeCSS(app_sys("app/www/styles.css")),
    tags$link(rel = "shortcut icon", href = "www/favicon.ico")
  )
}


download_dependencies <- function(prompt = interactive()) {
  if (isTRUE(prompt)) {
    answer <- readline("Install dependencies? [y/N]")
    if (!identical(answer, "y"))
      stop("Dependencies needed to start the Shiny app")
  }
  
  plat_db_url <- "https://github.com/TNO/pLAtYpus/raw/main/output/pLAtYpus.sqlite3"
  plat_alt_url <- "https://github.com/TNO/pLAtYpus/raw/main/output/pLAtYpus_only_survey.sqlite3"
  plat_db_dest <- app_sys("extdata/stakeholder/output/pLAtYpus.sqlite3")
  plat_alt_dest <- app_sys("extdata/stakeholder/output/pLAtYpus_only_survey.sqlite3")
  
  if (!file.exists(plat_db_dest))
    download.file(stk_db, destfile = plat_db_dest)
  
  if (!file.exists(plat_alt_dest))
    download.file(stk_alt, destfile = plat_alt_dest)
}
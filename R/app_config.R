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


gretan_deps <- function() {
  list(
    plat_db = list(
      src = "https://github.com/TNO/pLAtYpus/raw/main/output/pLAtYpus.sqlite3",
      dest = app_sys("extdata/stakeholder/output/pLAtYpus.sqlite3")
    ),
    plat_alt = list(
      src = "https://github.com/TNO/pLAtYpus/raw/main/output/pLAtYpus_only_survey.sqlite3",
      dest = app_sys("extdata/stakeholder/output/pLAtYpus_only_survey.sqlite3")
    )
  )
}


has_dependencies <- function(vec = FALSE) {
  deps <- gretan_deps()
  needed <- c(
    !file.exists(deps$plat_db$dest),
    !file.exists(deps$plat_alt$dest)
  )

  if (vec) !needed else !any(needed)
}


download_dependencies <- function(prompt = interactive()) {
  if (!dir.exists(app_sys("extdata/stakeholder/output"))) {
    dir.create(app_sys("extdata/stakeholder/output"))
  }
  
  needed <- !has_dependencies(vec = TRUE)

  if (all(!needed)) {
    return(invisible())
  }

  if (isTRUE(prompt)) {
    sizes <- c(28.7, 28.7)[needed]
    answer <- readline(sprintf(
      "Install dependencies? This will download %s MB of data. [y/N] ",
      sum(sizes)
    ))

    if (!identical(answer, "y")) {
      stop("Dependencies needed to start the Shiny app")
    }
  }

  deps <- gretan_deps()

  if (needed[1]) {
    curl::curl_download(
      url = deps$plat_db$src,
      destfile = deps$plat_db$dest,
      quiet = FALSE
    )
  }

  if (needed[2]) {
    curl::curl_download(
      url = deps$plat_alt$src,
      destfile = deps$plat_alt$dest,
      quiet = FALSE
    )
  }
}

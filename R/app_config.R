#' Access files in the current app
#' 
#' NOTE: If you manually change your package 
#' name in the DESCRIPTION, don't forget to change it here too,
#' and in the config file. For a safer name change mechanism, 
#' use the `golem::set_golem_name()` function.
#' 
#' @param ... character vectors, specifying subdirectory 
#' and file(s) within your package. 
#' The default, none, returns the root of the app.
#' 
#' @noRd
app_sys <- function(...){
  system.file(..., package = "greta")
}


golem_add_external_resources <- function() {
  golem::add_resource_path("www", app_sys("app/www"))
  tags$head(
    golem::favicon(),
    waiter::useWaiter(),
    shinyWidgets::useSweetAlert(theme = "bootstrap-4"),
    shinyjs::useShinyjs(),
    includeCSS(app_sys("app/www/styles.css"))
  )
}
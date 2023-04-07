#' Access files in the current app
#' 
#' @param ... character vectors, specifying subdirectory 
#' and file(s) within your package. 
#' The default, none, returns the root of the app.
#' 
#' @noRd
app_sys <- function(...){
  system.file(..., package = "greta")
}


add_external_resources <- function() {
  addResourcePath("www", app_sys("app/www"))
  tags$head(
    waiter::useWaiter(),
    shinyWidgets::useSweetAlert(theme = "bootstrap-4"),
    shinyjs::useShinyjs(),
    tags$script(HTML("$('html').on('click', function(e) {
      if (e.target.id != 'listbox' || e.target.id != 'textSearch') {
        $('#listbox').hide();
      }
    })")),
    tags$script(HTML("$('html').on('click', function(e) {
      if (e.target.id == 'textSearch') {
        $('#listbox').show()
      }
    })")),
    includeCSS(app_sys("app/www/styles.css")),
    tags$link(rel = "shortcut icon", href = app_sys("app/www/favicon.ico"))
  )
}
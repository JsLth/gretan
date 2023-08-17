#' Access files in the current app
#' 
#' @param ... character vectors, specifying subdirectory 
#' and file(s) within your package. 
#' The default, none, returns the root of the app.
#' 
#' @noRd
app_sys <- function(...) {
  path <- system.file(..., package = "greta")
  
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
    singleton(tags$script(HTML("function scroll_to(id, block) {
      document.querySelector('#' + id).scrollIntoView({block: block, behavior: 'smooth'});
    }
    
    Shiny.addCustomMessageHandler('scroll', function(opts) {
      scroll_to(opts.id, opts.block);
    });"))),
    includeCSS(app_sys("app/www/styles.css")),
    tags$link(rel = "shortcut icon", href = app_sys("app/www/favicon.ico"))
  )
}
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
#' @param golem_opts Options for \code{\link[golem]{with_golem_options}}
#'
#' @export
#' @import shiny
#' @importFrom dplyr %>% filter mutate select
run_app <- function(..., golem_opts = list()) {
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui, 
      server = server,
      ...
    ), 
    golem_opts = golem_opts
  )
}


#' @rdname run_app
#' @export
run_greta <- function(...) {
  run_app(...)
}
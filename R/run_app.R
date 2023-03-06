#' Run the "greta" Shiny Application
#'
#' @examples
#' \dontrun{
#' library(greta)
#' greta::run_app()
#' greta::run_greta()
#' }
#'
#' @export
#' @import shiny
#' @importFrom dplyr %>% filter mutate select
run_app <- function(...) {
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui, 
      server = server
    ), 
    golem_opts = list(...)
  )
}


#' @rdname run_app
#' @export
run_greta <- function(...) {
  run_app(...)
}
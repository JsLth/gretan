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
#'
#' @export
#' @import shiny
#' @importFrom dplyr %>% filter mutate select
run_app <- function(...) {
  shiny::shinyApp(ui = app_ui, server = server, ...)
}


#' @rdname run_app
#' @export
run_greta <- function(...) {
  run_app(...)
}
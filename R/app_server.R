server <- function(input, output, session) {
  cat_where(whereami())
  callModule(mod_main_server, "main_ui")
}
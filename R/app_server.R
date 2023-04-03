server <- function(input, output, session) {
  callModule(mod_main_server, "main")
}
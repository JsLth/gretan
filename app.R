# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("app.prod" = TRUE)

cat("User:", Sys.info()[["user"]])
cat("Reticulate Python:", Sys.getenv("RETICULATE_PYTHON"))
cat("Python path:", Sys.getenv("PYTHON_PATH"))
cat("venv name:", Sys.getenv("VIRTUALENV_NAME"))

if (Sys.info()[["user"]] == "shiny") {
  envs <- reticulate::virtualenv_list()
  
  if (!"gretan" %in% envs) {
    envdir <- Sys.getenv("VIRTUALENV_NAME")
    deps <- c("numpy", "lightgbm")
    reticulate::virtualenv_create(envname = envdir, python = Sys.getenv("PYTHON_PATH"))
    reticulate::virtualenv_install(envdir, packages = deps, ignore_installed = TRUE)
    reticulate::use_virtualenv(virtualenv = envdir,required = TRUE)
  }
}



gretan::run_app()

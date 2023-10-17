# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("app.prod" = TRUE)

if (Sys.info()[["user"]] == "shiny") {
  Sys.setenv(PYTHON_PATH = "/usr/bin/python3")
  Sys.setenv(VIRTUALENV_NAME = "gretan")
  Sys.setenv(RETICULATE_PYTHON = "/home/shiny/.virtualenvs/gretan/bin/python")
  envdir <- Sys.getenv("VIRTUALENV_NAME")
  deps <- c("numpy", "lightgbm", "pLAtYpus_TNO")

  reticulate::virtualenv_create(
    envname = envdir,
    python = Sys.getenv("PYTHON_PATH"),
    version = "3.11"
  )
  reticulate::virtualenv_install(
    envdir,
    packages = deps,
    ignore_installed = TRUE,
    pip_options = "--ignore-requires-python"
  )
  reticulate::use_virtualenv(virtualenv = envdir, required = TRUE)
}

gretan::run_app()

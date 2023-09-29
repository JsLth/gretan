if (Sys.info()[["user"]] == "shiny") {
  Sys.setenv(PYTHON_PATH = "/usr/bin/python3")
  Sys.setenv(VIRTUALENV_NAME = "gretan")
  Sys.setenv(RETICULATE_PYTHON = "/home/shiny/.virtualenvs/gretan/bin/python")
  envs <- reticulate::virtualenv_list()
  
  if (!"gretan" %in% envs) {
    envdir <- Sys.getenv("VIRTUALENV_NAME")
    deps <- c("numpy", "lightgbm")
    reticulate::virtualenv_create(envname = envdir, python = Sys.getenv("PYTHON_PATH"))
    reticulate::virtualenv_install(envdir, packages = deps, ignore_installed = TRUE)
    reticulate::use_virtualenv(virtualenv = envdir, required = TRUE)
  }
}

Sys.setenv(RSCONNECT_PACKRAT = TRUE)

if (Sys.info()[["user"]] == "shiny") {
  Sys.setenv(PYTHON_PATH = "/usr/bin/python3")
  Sys.setenv(VIRTUALENV_NAME = "gretan")
  Sys.setenv(RETICULATE_PYTHON = "/home/shiny/.virtualenvs/gretan/bin/python")
  envdir <- Sys.getenv("VIRTUALENV_NAME")
  envs <- reticulate::virtualenv_list()

  if (!envdir %in% envs) {
    deps <- c("numpy", "lightgbm", "pLAtYpus_TNO==1.0.4")
    reticulate::virtualenv_create(envname = envdir, python = Sys.getenv("PYTHON_PATH"))
    reticulate::virtualenv_install(envdir, packages = deps, ignore_installed = TRUE, pip_options = "--ignore-requires-python")
  }

  reticulate::use_virtualenv(virtualenv = envdir, required = TRUE)
}

Sys.setenv(RSCONNECT_PACKRAT = FALSE)

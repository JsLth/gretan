# This script is used to run the application in the background
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options(shiny.autoreload = TRUE)
greta::run_app()
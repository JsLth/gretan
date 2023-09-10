if (!require(remotes)) {
  install.packages("remotes")
}

if (!require("electricShine")) {
  remotes::install_github("JsLth/electricShine")
  library(electricShine)
}

if (!dir.exists("electron")) {
  dir.create("electron")
}

if (dir.exists("electron/greta-analytics")) {
  unlink("electron/greta-analytics", recursive = TRUE)
}

nodejs <- normalizePath(Sys.which("node"), winslash = "/")

electrify(
  app_name = "greta-analytics",
  pkg = ".",
  product_name = "GRETA Analytics",
  build_path = "electron",
  cran_like_url = "https://cran.r-project.org/",
  function_name = "run_greta",
  nodejs_version = "v14.17.3",
  r_bitness = "x64",
  r_version = "4.3.1",
  license = "GPLv3",
  website = "https://projectgreta.eu/",
  author = "GESIS - Leibniz Institute for the Social Sciences",
  keywords = c("GRETA", "Energy Citizenship", "Science")
)

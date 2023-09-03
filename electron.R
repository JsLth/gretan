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

nodejs <- normalizePath(Sys.which("node"), winslash = "/")

electrify(
  app_name = "greta-analytics",
  product_name = "GRETA Analytics",
  build_path = "electron",
  local_package_path = getwd(),
  function_name = "run_greta",
  nodejs_version = "v14.17.3",
  r_bitness = "x64",
  r_version = "4.3.1",
  cran_like_url = "https://cran.r-project.org",
  license = "GPLv3",
  website = "https://projectgreta.eu/",
  author = "GESIS - Leibniz Institute for the Social Sciences",
  keywords = c("GRETA", "Energy Citizenship", "Science"),
  platforms = list(
    win = list(
      target = list(target = "nsis", arch = c("x64", "ia32"))
    ),
    mac = list(
      target = list(target = "default", arch = c("x64", "ia32")),
      category = "public.app-category.education"
    ),
    linux = list(
      target = list(target = "AppImage", arch = c("x64", "ia32")),
      category = "Science"
    )
  )
)

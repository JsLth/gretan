# remotes::install_github("erikvona/electricShine", ref = "fe47ec971ead80c696c99d59984c503f47c381f3")

library(electricShine)

dir.create("electron")

nodejs <- normalizePath(Sys.which("node"), winslash = "/")

electrify(
  app_name = "greta-analytics",
  product_name = "GRETA Analytics",
  short_description = "Visualize and explore the results from the GRETA project. Quickly get insights from GRETA's multi-national survey and case studies. Understand statistical and spatial relationships of energy citizenship in 16 European countries among citizens, businesses and policymakers.",
  build_path = "electron",
  local_package_path = getwd(),
  function_name = "run_greta",
  # nodejs_path = gsub(basename(nodejs), "", nodejs),
  nodejs_version = "v14.17.3",
  r_bitness = "x64",
  r_version = "4.3.1",
  cran_like_url = "https://cran.r-project.org",
  license = "GPLv3",
  website = "https://projectgreta.eu/",
  author = "Jonas Lieth"
)

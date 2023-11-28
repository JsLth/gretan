if (!require("electricShine")) {
  if (!require(remotes)) install.packages("remotes")
  remotes::install_github("JsLth/electricShine")
}

library(electricShine)

if (!dir.exists("electron")) {
  dir.create("electron")
}

if (dir.exists("electron/gretan")) {
  unlink("electron/gretan", recursive = TRUE)
}

nodejs <- normalizePath(Sys.which("node"), winslash = "/")

# Set up electron structure, install nodejs install R and install gretan package
electrify(
  app_name = "gretan",
  pkg = ".",
  product_name = "GRETA Analytics",
  build_path = "electron",
  cran_like_url = "https://cran.r-project.org",
  function_name = "run_greta",
  nodejs_version = "v14.17.3",
  r_bitness = "x64",
  r_version = "4.3.1",
  license = "GPLv3",
  website = "https://projectgreta.eu/",
  author = "GESIS - Leibniz Institute for the Social Sciences",
  keywords = c("GRETA", "Energy Citizenship", "Science"),
  permission = TRUE,
  run_build = FALSE
)

# Unpack WinPython
app_dir <- normalizePath("electron/gretan/app")
system2("Winpython64-3.11.5.0dot.exe", c(
  "-y", paste0("-o", normalizePath("electron/gretan/app"))
))
file.rename(file.path(app_dir, "WPy64-31150"), file.path(app_dir, "python"))

# Install python dependencies
system2(
  file.path(app_dir, "python", "python-3.11.5.amd64", "python.exe"),
  args = c("-m", "pip", "install", "-r", normalizePath("inst/requirements.txt"))
)

bgjs <- readLines("electron/gretan/src/background.js")

bgjs <- bgjs[-seq(240, 244)]
bgjs <- append(bgjs, after = 239, values = c(
  "      if (process.env.GRETAN_EXIT === 'safe') {",
  "        dialog.showMessageBoxSync({",
  "          message: \"The R Shiny process quit unexpectedly.\\nCheck the logs under  %USERPROFILE%\\\\AppData\\\\Roaming\\\\\" + app.getName() + \"\\\\main.log\",",
  "          type: \"error\",",
  "          title: \"The R Shiny process quit unexpectedly\"",
  "        })",
  "      }"
))
writeLines(bgjs, "electron/gretan/src/background.js")

# Build electron app
run_build_release(
  "C:/Users/PC/AppData/Local/R/win-library/4.3/electricShine/nodejs/node-v14.17.3-win-x64",
  app_path = normalizePath("electron/gretan"),
  nodejs_version = "v14.17.3"
)
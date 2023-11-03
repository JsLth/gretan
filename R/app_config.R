#' Access files in the current app
#'
#' @param ... character vectors, specifying subdirectory
#' and file(s) within your package.
#' The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  path <- system.file(..., package = "gretan")

  # for rsconnect
  if (!nchar(path)) {
    path <- file.path("inst", ...)
  }

  path
}


add_external_resources <- function() {
  addResourcePath("www", app_sys("app/www"))
  tags$head(
    waiter::useWaiter(),
    waiter::useWaitress(),
    shinyWidgets::useSweetAlert(theme = "bootstrap-4"),
    shinyWidgets::chooseSliderSkin("Flat"),
    shinyjs::useShinyjs(),
    if (isTRUE(getGretaOption("console", FALSE))) keys::useKeys(),
    tags$script(src = "www/textsearch.js"),
    tags$script(src = "www/loadingbutton.js"),
    tags$script(src = "www/modules.js"),
    includeCSS(app_sys("app/www/styles.css")),
    tags$link(rel = "shortcut icon", href = "www/favicon.ico")
  )
}


gretan_deps <- function() {
  list(
    plat_db = list(
      src = "https://github.com/TNO/pLAtYpus/raw/main/output/pLAtYpus.sqlite3",
      dest = file.path(app_sys("extdata/stakeholder"), "output/pLAtYpus.sqlite3")
    ),
    plat_alt = list(
      src = "https://github.com/TNO/pLAtYpus/raw/main/output/pLAtYpus_only_survey.sqlite3",
      dest = file.path(app_sys("extdata/stakeholder"), "output/pLAtYpus_only_survey.sqlite3")
    ),
    plat_src = list(
      src = c(
        "https://raw.githubusercontent.com/TNO/pLAtYpus/main/src/pLAtYpus_TNO/GRETA_tool.py",
        "https://raw.githubusercontent.com/TNO/pLAtYpus/main/src/pLAtYpus_TNO/scores.py",
        "https://raw.githubusercontent.com/TNO/pLAtYpus/main/src/pLAtYpus_TNO/solver.py",
        "https://raw.githubusercontent.com/TNO/pLAtYpus/main/src/pLAtYpus_TNO/maps.py",
        "https://raw.githubusercontent.com/TNO/ETS_CookBook/main/src/ETS_CookBook/ETS_CookBook.py"
      ),
      dest = c(
        file.path(app_sys("extdata/stakeholder"), "src/GRETA_tool.py"),
        file.path(app_sys("extdata/stakeholder"), "src/scores.py"),
        file.path(app_sys("extdata/stakeholder"), "src/solver.py"),
        file.path(app_sys("extdata/stakeholder"), "src/maps.py"),
        file.path(app_sys("extdata/stakeholder"), "src/ETS_CookBook.py")
      )
    )
  )
}


has_dependencies <- function(vec = FALSE) {
  deps <- gretan_deps()
  needed <- c(
    !all(file.exists(deps$plat_db$dest)),
    !all(file.exists(deps$plat_alt$dest)),
    !all(file.exists(deps$plat_src$dest))
  )

  if (vec) !needed else !any(needed)
}


check_download_prompt <- function(prompt = interactive()) {
  needed <- !has_dependencies(vec = TRUE)

  if (all(!needed)) {
    return(invisible())
  }

  if (isTRUE(prompt)) {
    sizes <- c(28.7, 28.7, 0.06)[needed]
    answer <- readline(sprintf(
      "Install dependencies? This will download %s MB of data. [y/N] ",
      sum(sizes)
    ))

    if (!identical(answer, "y")) {
      stop("Dependencies needed to start the Shiny app")
    }
  }
}


download_dependencies <- function() {
  needed <- !has_dependencies(vec = TRUE)

  if (all(!needed)) {
    return(invisible())
  }

  deps <- gretan_deps()

  if (needed[1]) {
    out_dir <- file.path(app_sys("extdata/stakeholder"), "output")
    if (!dir.exists(out_dir)) dir.create(out_dir)
    cat("Downloading pLAtYpus model database\n")
    curl::curl_download(
      url = deps$plat_db$src,
      destfile = deps$plat_db$dest,
      quiet = FALSE
    )
  }

  if (needed[2]) {
    cat("Downloading pLAtYpus reset database\n")
    curl::curl_download(
      url = deps$plat_alt$src,
      destfile = deps$plat_alt$dest,
      quiet = FALSE
    )
  }

  if (needed[3]) {
    src_dir <- file.path(app_sys("extdata/stakeholder"), "src")
    if (!dir.exists(src_dir)) dir.create(src_dir)
    curl::multi_download(
      urls = deps$plat_src$src,
      destfiles = deps$plat_src$dest
    )

    # Replace some lines that are specific for Python modules
    for (file in deps$plat_src$dest) {
      lines <- readLines(file)
      lines <- gsub("ModuleNotFoundError", "ImportError", lines, fixed = TRUE)
      lines <- gsub("from ETS_CookBook import", "import", lines, fixed = TRUE)
      writeLines(lines, file)
    }
  }
}


check_python <- function(python = NULL, prompt = interactive()) {
  if (isTRUE(getOption("app.prod"))) {
    Sys.setenv(PYTHON_PATH = "/usr/bin/python3")
    Sys.setenv(VIRTUALENV_NAME = "gretan")
    Sys.setenv(RETICULATE_PYTHON = "/home/shiny/.virtualenvs/gretan/bin/python")
    envdir <- Sys.getenv("VIRTUALENV_NAME")
    envs <- reticulate::virtualenv_list()

    if (!envdir %in% envs) {
      proc <- processx::process$new(
        command = "sh",
        args = "python_setup.sh",
        echo_cmd = TRUE,
        stdout = "|",
        stderr = "|"
      )
      return(proc)
    }
    return(invisible())
  }

  if (!is.null(python)) {
    reticulate::use_python(python, required = TRUE)

    if (prompt) {
      pkgs <- reticulate::py_list_packages(python = python)$package
      deps <- readLines(app_sys("requirements.txt"))
      dep_names <- vapply(
        strsplit(deps, "==", fixed = TRUE),
        FUN = "[",
        1,
        FUN.VALUE = character(1)
      )

      if (!all(dep_names %in% pkgs)) {
        cat2(
          "GRETA Analytics requires the following Python dependencies",
          "which are not currently installed:"
        )
        cat2(paste(
          paste("  -", dep_names[!dep_names %in% pkgs]),
          collapse = "\n"
        ))
        if (!prompt) {
          stop(
            "Missing Python dependencies: ",
            paste(dep_names[!dep_names %in% pkgs], collapse = ", ")
          )
        } else {
          answer <- readline("Install Python dependencies? [y/N]")
        }

        if (!identical(answer, "y")) {
          return(invisible())
        }

        reticulate::py_install(packages = deps)
      }
    }
  } else {
    cat2("Looking for Python...")
    if (!reticulate::py_available(initialize = TRUE)) {
      stop(paste0(
        "The gretan package needs a working installation ",
        "of Python 3.8 or higher.\nPython can be downloaded under ",
        "\033]8;;https://www.python.org/downloads/\ahttps://www.",
        "python.org/downloads/\033]8;;\a, or by running ",
        "\033]8;;rstudio:run:reticulate::install_python()\a",
        "`reticulate::install_python()`\033]8;;\a"
      ), call. = FALSE)
    } else {
      cat2("Using:")
      print(reticulate::py_config())
    }
  }

  return(invisible())
}

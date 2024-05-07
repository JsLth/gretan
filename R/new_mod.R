#' Add new module
#'
#' @description
#' Adds a new module to the dashboard.
#' In particular, adds two new files \code{mod_{id}.R} and \code{txt_{id}.R}
#' and modifies two more files \code{app_ui.R} and \code{mod_main.R}. After
#' running this function, you should have a minimum working prototype of a
#' new tab that can be manually filled with content.
#'
#' You need to run \code{devtools::load_all()} before using this function to
#' ensure that code files are editable.
#'
#' @param id Identifier of the new module. Should be an abbreviation of the
#' title, i.e. "Data explorer" -> "exp" or "Case Study 5" -> "cs5"
#' @param write Whether to write the new code to files or just print it to
#' the console. Set to FALSE by default to prevent accidents.
#'
#' @details
#' Since this function modifies source code, run \code{devtools::load_all()}
#' after adding a new module. Otherwise, the changes made by this function will
#' not be immediately adopted.
#'
#' Before modifying any code files, \code{add_module} stores the old files
#' in the folder \code{backup}.
#'
#' @note
#' These functions are not exported because they can only be used in developer
#' mode. To use these functions, run gretan from source.
add_module <- function(id, write = FALSE) {
  if (!is_devtools()) {
    stop(paste(
      "Only use `add_module()` when developing a new module.\n",
      " Run `devtools::load_all()` to load gretan in developer mode."
    ))
  }

  # find paths
  mod_file <- file.path(
    base::system.file(package = "gretan"),
    sprintf("R/mod_%s.R", id)
  )
  txt_file <- file.path(
    base::system.file(package = "gretan"),
    sprintf("R/txt_%s.R", id)
  )
  app_ui <- base::system.file("R/app_ui.R", package = "gretan")
  mod_main <- base::system.file("R/mod_main.R", package = "gretan")

  # create code strings
  mod_code <- new_mod(id)
  txt_code <- new_txt(id)
  ui_code <- add_tab_to_app_ui(id)
  main_code <- add_mod_to_mod_main(id)

  if (write) {
    # create backup
    dir.create(app_sys("backup"), showWarnings = FALSE)
    file.copy(app_ui, file.path(
      base::system.file("backup", package = "gretan"),
      "app_ui.R"
    ))
    file.copy(mod_main, file.path(
      base::system.file("backup", package = "gretan"),
      "mod_main.R"
    ))

    # write new files
    cat(sprintf("- Adding %s", basename(mod_file)), "\n")
    cat(mod_code, file = mod_file, sep = "\n")
    cat(sprintf("- Adding %s", basename(txt_file)), "\n")
    cat(txt_code, file = txt_file, sep = "\n")
    cat(sprintf("- Modifying %s", basename(app_ui)), "\n")
    cat(ui_code, file = app_ui, sep = "\n")
    cat(sprintf("- Modifying %s", basename(mod_main)), "\n")
    cat(main_code, file = mod_main, sep = "\n")
  } else {
    cat(
      paste("\033[90m\033[1m#", basename(mod_file), "----\033[22m\033[39m"),
      mod_code, "\n",
      paste("\033[90m\033[1m#", basename(txt_file), "----\033[22m\033[39m"),
      txt_code, "\n",
      paste("\033[90m\033[1m#", basename(app_ui), "----\033[22m\033[39m"),
      ui_code, "\n",
      paste("\033[90m\033[1m#", basename(mod_main), "----\033[22m\033[39m"),
      main_code,
      sep = "\n"
    )
  }
}


#' @rdname add_module
#' @description
#' \code{recover_app} recovers \code{app_ui.R} and \code{mod_main.R} from
#' the backup. Use this to return to the original state of the app if you
#' have messed something up after using \code{add_module()}.
recover_app <- function() {
  app_file <- app_sys("backup/app_ui.R")
  main_file <- app_sys("backup/mod_main.R")

  if (file.exists(app_file)) {
    cat("- Recovering app_ui.R", "\n")
    file.copy(app_file, to = app_sys("R/app_ui.R"), overwrite = TRUE)
  } else {
    warning("Backup file for app_ui.R does not exist")
  }

  if (file.exists(app_file)) {
    cat("- Recovering mod_main.R", "\n")
    file.copy(main_file, to = app_sys("R/mod_main.R"), overwrite = TRUE)
  } else {
    warning("Backup file for mod_main.R does not exist")
  }
  invisible()
}


add_mod_to_mod_main <- function(id) {
  main_code <- readLines(app_sys("R/mod_main.R"))
  main_code <- add_mod_to_mod_main_fun(main_code, id, type = "ui")
  main_code <- add_mod_to_mod_main_fun(main_code, id, type = "server")
  main_code
}


add_mod_to_mod_main_fun <- function(code, id, type = "ui") {
  is_mod <- grepl(sprintf("mod_.+_%s\\(", type), code)
  mods <- seq_along(code)[is_mod]
  last_mod <- mods[sum(is_mod)]
  last_line <- code[last_mod]
  ws <- strrep(" ", measure_ws(last_line))
  mod_new <- sprintf(
    "%smod_%s_%s(%s\"%s\")%s",
    ws,
    id,
    type,
    ifelse(identical(type, "ui"), "ns(", ""),
    id,
    ifelse(identical(type, "ui"), "),", "")
  )
  code <- append(code, mod_new, after = last_mod)
  code
}


add_tab_to_app_ui <- function(id) {
  ui_code <- readLines(app_sys("R/app_ui.R"))
  is_tabname <- grepl("tabName", ui_code, fixed = TRUE)
  tabnames <- seq_along(ui_code)[is_tabname]
  last_tab <- tabnames[sum(is_tabname)] + 1
  i <- 0
  while (!endsWith(last_line <- ui_code[last_tab], ")")) {
    last_tab <- last_tab + 1
    i <- i + 1
    if (i > 3) stop("app_ui.R is corrupted. Please check.")
  }
  ws <- strrep(" ", measure_ws(last_line))
  ui_new <- sprintf(paste(
    "%sbs4Dash::menuItem(",
    "%s  text = get_text(\"%s\", \"shortitle\"),",
    "%s  icon = icon(get_text(\"%s\", \"icon\")),",
    "%s  tabName = \"%s\"",
    "%s)",
    sep = "\n"
  ), ws, ws, id, ws, id, ws, id, ws)

  ui_code[last_tab] <- paste0(last_line, ",")
  ui_code <- append(ui_code, ui_new, after = last_tab)
  ui_code
}


new_txt <- function(id) {
  sprintf(paste(
    "# This file contains the text contents for your new module %s.",
    "# Add new list items or change existing list items as needed and",
    "# refer to them inside mod_%s.R using the get_text function.",
    "txts$main$%s <- list(",
    "  shortitle = \"New module\",",
    "  title = \"A new module for the GRETA Analytics Shiny app\",",
    "  icon = \"screwdriver-wrench\",",
    "  tags = c(\"new\", \"module\"),",
    "  authors = c(\"Author 1\", \"Author 2\"),",
    "  affil = \"University of Shiny\",",
    "  date = \"%s\",",
    "  introduction = list(",
    "    title = \"Introduction\",",
    "    content = \"This is an example text.\"",
    "  )",
    ")",
    sep = "\n"
  ), id, id, id, Sys.Date())
}


new_mod <- function(id) {
  sprintf(paste(
    "# This file contains the UI and server elements of your new module %s.",
    "# It is divided into two functions: a UI function holding all UI elements",
    "# and a server function holding the R logic that is applied on the server",
    "# side. Add UI elements by modifying the body of mod_%s_ui() or add",
    "# reactive elements / render plots by adding R code to the body of",
    "# mod_%s_server().",
    "mod_%s_ui <- function(id) {",
    "  # the `ns()` function establishes the place of this module in the app",
    "  # hierarchy. Always wrap IDs within ns() calls to explicitly refer to",
    "  # this module level.",
    "  ns <- NS(id)",
    "",
    "  # the dispatch_to_txt function is a function factory that creates the",
    "  # `get_text` function. Use `get_text(\"item\")` to retrieve content",
    "  # defined in txt_%s.R.",
    "  get_text <- dispatch_to_txt(id)",
    "",
    "  bs4Dash::tabItem(",
    "    \"%s\",",
    "    make_header(",
    "      title = get_text(\"title\"),",
    "      authors = get_text(\"authors\"),",
    "      affil = get_text(\"affil\"),",
    "      date = get_text(\"date\")",
    "    ),",
    "    bs4Dash::column(",
    "      width = 12,",
    "      bs4Dash::box(",
    "        id = ns(\"box\"), # always wrap UI IDs in `ns()`",
    "        status = \"primary\",",
    "        width = 6,",
    "        title = get_text(\"introduction\", \"title\"),",
    "        get_text(\"introduction\", \"content\")",
    "      )",
    "    )",
    "  )",
    "}",
    "",
    "mod_%s_server <- function(id) {",
    "  moduleServer(id, function(input, output, session) {",
    "    # add R code here that adds interactive plots, maps, tables, or figures",
    "    # to learn more about Shiny programming, take a look at Hadley Wickham's",
    "    # \"Mastering Shiny\": https://mastering-shiny.org/",
    "    #",
    "    # To retrieve texts on the server side, run the following code:",
    "    get_text <- dispatch_to_txt(session$ns(NULL))",
    "    #",
    "    # The get_text function is then able to retrieve texts defined in",
    "    # txt_%s.R.",
    "    #",
    "    # When adding new UI elements from the server side, their IDs must",
    "    # be namespaced just like in the UI function:",
    "    ns <- session$ns",
    "  })",
    "}",
    sep = "\n"
  ), id, id, id, id, id, id, id, id)
}


measure_ws <- function(x) {
  lengths(regmatches(x, gregexpr(" ", x)))
}


is_devtools <- function() {
  !is.null(.getNamespace("gretan")$.__DEVTOOLS__)
}

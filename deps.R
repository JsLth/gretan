regex_match <- function(text, pattern, ...) {
  regmatches(text, regexec(pattern, text, ...))
}

identify_deps <- function(path = ".", export = FALSE) {
  files <- tools::list_files_with_exts(path, "R", full.names = TRUE)
  code <- lapply(files, function(x) {
    lin <- readLines(x)
    lin[grepl("library", lin) | grepl(":{2,3}", lin)]
  }) %>% unlist()
  
  attached <- regex_match(code, "library\\((.+)\\)") %>%
    lapply("[", 2) %>%
    unlist() %>%
    unique() %>%
    .[!is.na(.)]
  
  loaded <- regex_match(code, "([A-Za-z0-9_\\.]+):{2,3}") %>%
    lapply("[", 2) %>%
    unlist() %>%
    unique() %>%
    .[!is.na(.)]
  
  inst_pkgs <- as.data.frame(installed.packages())
  
  deps <- union(attached, loaded) %>%
    setdiff(rownames(inst_pkgs[inst_pkgs$Priority %in% "base", ])) %>%
    paste0(" (>= ", inst_pkgs[., ]$Version, ")")
  
  cat(deps, file = ifelse(export, "dependencies.txt", ""), sep = "\n")
  invisible(deps)
}

install_deps <- function(deps = "dependencies.txt") {
  lin <- readLines(deps)
  pkgs <- vapply(strsplit(lin, " "), "[", 1, FUN.VALUE = character(1))
  for (pkg in pkgs) {
    install.packages(pkg)
  }
}

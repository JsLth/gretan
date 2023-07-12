clean_ris <- function(file) {
  ris <- readLines(file, encoding = "UTF-8")
  ris <- paste(ris[!grepl("^AB|^N2", ris)], collapse = "\n")
  cat(ris, "\n", file = file)
  file
}

read_ris <- function(file) {
  handlr::handl_to_df(handlr::ris_reader(clean_ris(file)))
}
clean_ris <- function(file) {
  ris <- readLines(file, encoding = "UTF-8")
  ris <- paste(ris[!grepl("^AB|^N2", ris)], collapse = "\n")
  cat(ris, "\n", file = file)
  file
}

read_ris <- function(file) {
  handlr::ris_reader(clean_ris(file))
}

read_ris <- function(file) {
  revtools::read_bibliography(file)
}

create_bib <- function(ref) {
  revtools::format_citation()
}


create_bib <- function(ref_df) {
  switch(tolower(resource_type_general),
    journal = NULL,
    webpage = bib_software(ref_df)
  
  )
}

bib_software <- function(x) {
  sprintf(
    "%s",
    x$author
  )
}

format_author <- function(author) {
  lapply(author, function(x) {
    x <- strsplit(x, ", ")[[1]]
    name <- x[1]
    first_name <- x[2]
  })
}
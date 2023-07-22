#' Recursively searches a list for strings of the following form:
#' `{@authorYYYY,narr,pages}`
#' 
#' where authorYYYY refers to the unique ID given by `read_ris`, narr determines
#' whether narrative citation is applied and pages refers to the suffix of a
#' citation.
#' The keyword strings are then replaced with properly formatted citations.
#' @noRd
reference <- function(lst, bib) {
  ref <- NULL
  idx <- seq_along(lst)
  for (i in idx) {
    if (is.list(lst[[i]])) {
      lst[[i]] <- reference(lst[[i]], bib)
      ref <- c(ref, attr(lst[[i]], "ref"))
    } else if (is.character(lst[[i]])) {
      cit <- match_regex(lst[[i]], "\\{@(.+[0-9]{4}),?([a-z]+)?,?(.*)\\}")

      for (x in cit) {
        if (length(x)) {
          x <- x[-1]
          id <- x[1]
          narr <- ifelse(x[2] %in% "true", TRUE, FALSE)
          pages <- if (nzchar(x[3])) x[3] else NULL
          ref <- c(ref, id)
          lst[[i]] <- gsub(
            sprintf("\\{@%s.*\\}", id),
            paste0(" ", as_ref(bib[[id]], narr = narr, pages = pages), " "),
            lst[[i]]
          )
        }
      }
    }
  }
  
  structure(lst, ref = ref)
}


#' Converts a RIS entry to an in-text reference
#' @noRd
as_ref <- function(x, narr = FALSE, pages = NULL) {
  author <- lapply(x$author, function(x) strsplit(x, ", ")[[1]][1])
  
  if (length(author) == 1) {
    author <- author[[1]]
  } else if (length(author) == 2) {
    author <- paste(author[[1]], "&", author[[2]])
  } else if (length(author) >= 3) {
    author <- paste(author[[1]], "et al. ")
  }
  
  if (narr) {
    paste0(
      author,
      " (",
      x$year,
      if (!is.null(pages)) paste0(": ", pages),
      ")"
    )
  } else {
    paste0(
      "(",
      author,
      " ",
      x$year,
      if (!is.null(pages)) paste0(": ", pages),
      ")"
    )
  }
}


#' @title Read RIS files
#' 
#' @description
#' Takes a RIS file and parses it as a named list
#' 
#' @param file Path to an RIS file
#' @param encoding Encoding to be assumed for input strings
#' @returns A named list containing reference metadata of the RIS file
read_ris <- function(file, encoding = "UTF-8") {
  stopifnot(length(file) == 1)
  
  if (!grepl("\\.ris$", file)) {
    stop("`file` must be a RIS file.")
  }
  
  ris <- readLines(file, encoding = encoding)
  ris <- split(ris, cumsum(!nzchar(ris)))
  ids <- NULL
  
  ref <- lapply(ris, function(x) {
    x <- gsub("^\\s*$", "", gsub("ER  -", "", x, fixed = TRUE))
    x <- x[nzchar(x)]
    x <- strsplit(x, "  - ", fixed = TRUE)
    nms <- vapply(x, "[[", FUN.VALUE = character(1), 1)
    
    fields_known <- nms %in% names(ris_fields)
    if (!all(fields_known)) {
      stop(
        "Unkown RIS fields: ",
        paste(nms[!fields_known], collapse = ", "),
        call. = FALSE
      )
    }
    
    nms <- unlist(ris_fields[nms], use.names = FALSE)
    val <- lapply(x, function(v) {
      if (length(v) > 1) {
        v[2]
      }
    })

    x <- as.list(setNames(val, nms))
    n_authors <- sum(names(x) == "author")
    if (n_authors > 1) {
      aidx <- which(names(x) %in% "author")
      authors <- x[aidx]
      x[aidx[1:length(aidx) - 1]] <- NULL
      x$author <- unname(authors)
    }
    
    n_editors <- sum(names(x) == "editor")
    if (sum(names(x) == "editor") > 1) {
      eidx <- which(names(x) %in% "editor")
      editors <- x[eidx]
      x[eidx[1:length(eidx) - 1]] <- NULL
      x$editor <- unname(editors)
    }
    
    id <- gsub(
      "[[:blank:]]|[[:punct:]]", "",
      paste0(strsplit(tolower(x$author[[1]]), ",")[[1]][1], x$year)
    )
    i <- 1
    while (id %in% ids) {
      id <- paste0(id, letters[i])
      i <- i + 1
    }
    ids <<- append(ids, id)
    
    x
  })

  structure(setNames(ref, ids), class = "ris")
}

#' @export
print.ris <- function(x, ...) {
  x <- x[1:10]
  x <- paste0(
    "first 10 references:\n",
    paste(paste0(
      "   id: ",
      names(x),
      ", type: ",
      sapply(x, "[[", "type")),
      collapse = "\n"
    )
  )
  
  cat("<ris>\n", x, "\n")
}


#' @title Create a bibliography
#' 
#' @description
#' Converts an object of class `ris` to a bibliography
#' 
#' @param ref Object of class `ris`
#' @param ... Arguments passed from or to methods
#' @param returns Named list of class `bib` containing formatted bibliography
#' entries. List names are IDs consisting of first author and year.
#' 
#' @references American Psychological Association (2019). Publication Manual
#' of the American Psychological Association (7th Edition). American
#' Psychological Association.
#' 
#' @export
as_bib <- function(ref, ...) {
  UseMethod("as_bib", ref)
}

#' @export
as_bib.default <- function(ref, ...) {
  as_bib(structure(ref, class = "ris"))
}

#' @export
as_bib.ris <- function(ref, ...) {
  ref <- lapply(ref, bib_entry)
  ref <- as.list(sort(unlist(ref)))
  class(ref) <- "bib"
  ref
}


#' Creates a single bibliography entry
#' @noRd
bib_entry <- function(x, type) {
  fun <- paste0("bib_", tolower(x$type))
  if (exists(fun)) {
    do.call(fun, x)
  } else {
    do.call("bib_gen", x)
  }
}


bib_jour <- function(...) {
  dots <- list(...)
  author <- format_author(dots$author, dots$editor)
  year <- format_year(dots$year)
  pages <- format_pages(dots$start_page, dots$end_page)
  volume <- format_volume_issue(dots$volume, dots$issue)
  
  paste0(
    author,
    paste0(" (", year, "). "),
    dots$title, ". ",
    dots$journal, if (!is.null(volume)) ", ",
    volume, if (!is.null(pages)) ", ",
    pages, "."
  )
}

bib_book <- function(...) {
  dots <- list(...)
  author <- format_author(dots$author, dots$editor)
  year <- format_year(dots$year)
  
  paste0(
    author,
    paste0(" (", year, "). "),
    dots$title, if (!is.null(dots$publisher)) ". ",
    dots$publisher, if (!is.null(dots$doi) || !is.null(dots$url)) ". ",
    dots$doi %||% dots$url,  "."
  )
}

bib_chap <- function(...) {
  dots <- list(...)
  author <- format_author(dots$author, NULL)
  year <- format_year(dots$year)
  editor <- format_author(NULL, dots$editor, anon = FALSE)
  pages <- format_pages(dots$start_page, dots$end_page)
  
  paste0(
    author,
    paste0(" (", year, "). "),
    dots$title, ". ",
    "In ",
    if (!is.null(editor)) paste0(editor, ", "),
    dots$journal,
    if (!all(is.null(pages), is.null(dots$volume), is.null(dots$edition))) {
      paste0(
        " (",
        if (!is.null(dots$edition)) paste0(dots$edition, ", "),
        if (!is.null(dots$volume)) paste0(dots$volume, ", "),
        if (!is.null(pages)) paste0("pp. ", pages),
        ")"
      )
    },
    if (!is.null(dots$publisher)) ". ",
    dots$publisher, if (!is.null(dots$doi) || !is.null(dots$url)) ". ",
    dots$doi %||% dots$url, "."
  )
}

bib_elec <- function(...) {
  bib_gen(...)
}

bib_gen <- function(...) {
  dots <- list(...)
  author <- format_author(dots$author)
  year <- format_year(dots$year)
  
  paste0(
    author,
    paste0(" (", year, "). "),
    dots$title, if (!is.null(dots$publisher)) ". ",
    dots$publisher, if (!is.null(dots$url)) ". ",
    dots$url, "."
  )
}


#' @export
format.bib <- function(x, html = FALSE, ...) {
  if (html) {
    do.call(
      tags$ul,
      c(
        unname(lapply(x, pbib)),
        class = "list-style: none",
        style = "margin-left: -30px;"
      )
    )
  } else {
    x <- strwrap(x, indent = 1, exdent = 5)
    paste(x, collapse = "\n")
  }
}


#' @export
print.bib <- function(x, ...) {
  cat(format(x))
}


format_author <- function(author = NULL, editor = NULL, anon = TRUE) {
  prt <- author
  if (is.null(prt)) prt <- editor
  if (!is.null(prt)) {
    prt <- lapply(prt, function(a) {
      if (any(grepl(",", a))) {
        a <- strsplit(a, ", ")[[1]]
        surname <- a[1]
        name <- a[2]
        name <- paste0(substr(name, 1, 1), ".")
        a <- paste0(surname, ", ", name)
      }
      a
    })

    if (length(prt) > 20) {
      prt <- c(prt[1:19], "...", prt[length(prt)])
    }
    
    prt <- paste(prt, collapse = ", ")
    
    if (is.null(author) && !is.null(editor)) {
      prt <- sprintf("%s (%s)", prt, ifelse(length(editor) > 1, "Eds.", "Ed."))
    }
  } else if (anon) {
    prt <- "Anon."
  }
  
  prt
}

format_year <- function(year) {
  if (is.null(year)) {
    year <- "n.d."
  }
  year
}

format_pages <- function(first_page, last_page) {
  if (!is.null(first_page) && !is.null(last_page)) {
    pages <- paste0(
      match_regex(first_page, "[0-9]+"),
      "\u2013",
      match_regex(last_page, "[0-9]+")
    )
  }
}

format_volume_issue <- function(volume, issue) {
  if (!is.null(volume) && is.null(issue)) {
    volume
  } else if (is.null(volume) && !is.null(issue)) {
    issue
  } else {
    sprintf("%s(%s)", volume, issue)
  }
}


dummy_bibliography <- function() {
  tags$ul(
    class = "list-style: none",
    style = "margin-left: -30px",
    pbib("Bollinger, B., & Gillingham, K. (2012). Peer Effects in the Diffusion of
      Solar Photovoltaic Panels. Marketing Science, 31(6), 900\u2013912.
      https://doi.org/10.1287/mksc.1120.0727"),
    pbib("Boschma, R. (2005). Proximity and Innovation: A Critical Assessment.
      Regional Studies, 39(1), 61\u201374.
      https://doi.org/10.1080/0034340052000320887"),
    pbib("Bouzarovski, S., & Simcock, N. (2017). Spatializing energy justice.
      Energy Policy, 107, 640\u2013648.
      https://doi.org/10.1016/j.enpol.2017.03.064"),
    pbib("Bridge, G., Bouzarovski, S., Bradshaw, M., & Eyre, N. (2013). Geographies
      of energy transition: Space, place and the low-carbon economy. Energy
      Policy, 53, 331\u2013340. https://doi.org/10.1016/j.enpol.2012.10.066"),
    pbib("Graziano, M., & Gillingham, K. (2015). Spatial patterns of solar
      photovoltaic system adoption: The influence of neighbors and the built
      environment. Journal of Economic Geography, 15(4), 815\u2013839.
      https://doi.org/10.1093/jeg/lbu036"),
    pbib("Irwin, N. B. (2021). Sunny days: Spatial spillovers in photovoltaic
      system adoptions. Energy Policy, 151, 112192.
      https://doi.org/10.1016/j.enpol.2021.112192")
  )
}


ris_fields <- list(
  TY = "type",
  ID = "id",
  TI = "title",
  AU = "author",
  A1 = "author",
  A2 = "editor",
  A3 = "author",
  A4 = "author",
  CY = "place",
  DA = "date",
  PY = "year",
  VL = "volume",
  IS = "issue",
  PB = "publisher",
  J1 = "journal",
  AB = "abstract",
  AD = "author_address",
  AN = "accession_number",
  AV = "archive_location",
  C1 = "custom_1",
  C2 = "custom_2",
  C3 = "custom_3",
  C4 = "custom_4",
  C5 = "custom_5",
  C6 = "custom_6",
  C7 = "custom_7",
  C8 = "custom_8",
  CA = "caption",
  CN = "call_number",
  DB = "database",
  DO = "doi",
  DP = "database_provider",
  ED = "editor",
  EP = "end_page",
  ET = "edition",
  J2 = "journal_alt",
  JA = "journal_abbr",
  JF = "journal_full",
  JO = "journal_full",
  KW = "keywords",
  L1 = "link_pdf",
  L2 = "link_text",
  L3 = "link_records",
  L4 = "link_img",
  LA = "language",
  LB = "label",
  LK = "website_link",
  M1 = "number",
  M2 = "misc",
  M3 = "type_of_work",
  N1 = "notes",
  N2 = "abstract",
  NV = "number_volumes",
  OP = "original_publication",
  PP = "publishing_place",
  RI = "reviewed_item",
  RN = "research_notes",
  RP = "reprint_edition",
  SE = "section",
  SN = "isbn_issn",
  SP = "start_page",
  ST = "short_title",
  T1 = "title",
  T2 = "journal",
  T3 = "tertiary_title",
  TA = "translated_author",
  TT = "translated_title",
  UR = "url",
  VO = "published_standard_number",
  Y1 = "primary_date",
  Y2 = "access_date"
)
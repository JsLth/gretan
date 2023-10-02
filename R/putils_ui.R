style <- function(...) {
  dots <- list(...)
  paste0(paste(paste(names(dots), dots, sep = ": "), collapse = "; "), ";")
}

dispatch_to_txt <- function(id) {
  ids <- strsplit(id, shiny::ns.sep)[[1]]
  target <- txts

  for (id in ids) {
    target <- target[[id]]
  }

  function(...) {
    dots <- list(...)
    out <- target
    for (name in dots) out <- out[[name]]
    out
  }
}

helpBox <- function(..., help_id = NULL, tabBox = FALSE) {
  bx <- bs4Dash::box(...)
  if (is.null(help_id)) {
    return(bx)
  }

  help <- tags$button(
    id = help_id,
    class = "btn btn-tool btn-sm action-button",
    type = "button",
    icon("question", lib = "font-awesome")
  )

  dots <- list(...)
  if (isFALSE(dots$collapsible) && !c("collapsible", "maximizable" %in% dots)) {
    bx$children[[1]]$children[[1]]$children[[2]] <- div(
      class = "card-tools float-right",
      help
    )
  } else {
    bx$children[[1]]$
      children[[1]]$
      children[[2]]$
      children[[2]] <- c(list(help), bx$children[[1]]$
      children[[1]]$
      children[[2]]$
      children[[2]])
  }
  bx
}


leafletPanel <- function(inputId,
                         ...,
                         title = NULL,
                         position = c("topleft", "topright", "bottomleft", "bottomright"),
                         width = 350,
                         collapsible = TRUE,
                         draggable = TRUE,
                         top = NULL,
                         bottom = NULL,
                         right = NULL,
                         left = NULL) {
  position <- match.arg(position)
  if (any(!vapply(list(top, bottom, right, left), is.null, logical(1)))) {
    gaps <- list(left = left, right = right, top = top, bottom = bottom)
  } else {
    if (position %in% "topleft") {
      gaps <- list(left = 10, right = NULL, top = 150, bottom = NULL)
    } else if (position %in% "topright") {
      gaps <- list(left = NULL, right = 10, top = 150, bottom = NULL)
    } else if (position %in% "bottomleft") {
      gaps <- list(left = 10, right = NULL, top = NULL, bottom = 150)
    } else {
      gaps <- list(left = NULL, right = 10, top = NULL, bottom = 150)
    }
  }


  div(
    class = paste0("leaflet-", position),
    absolutePanel(
      width = width,
      class = "leaflet-info",
      draggable = draggable,
      div(
        span(
          h5(title, style = "display: inline-block; margin: 0.2rem;"),
          if (collapsible) {
            div(
              class = "card-tools float-right",
              tags$button(
                class = "btn btn-tool btn-sm",
                # style = "margin-bottom: 0px;",
                `data-toggle` = "collapse",
                `data-target` = paste0("#", inputId),
                type = "button",
                tags$i(
                  class = "fas fa-minus",
                  role = "presentation",
                  `aria-label` = "minus icon"
                )
              )
            )
          }
        ),
        class = "leaflet-info-header"
      ),
      div(
        id = inputId,
        class = if (collapsible) "collapse show",
        ...,
        class = "leaflet-info-body"
      ),
      left = gaps$left,
      top = gaps$top,
      right = gaps$right,
      bottom = gaps$bottom
    )
  )
}


groupRadioButtons <- function(widget,
                              index,
                              groups,
                              type = c("default", "pretty", "awesome"),
                              style = NULL) {
  stopifnot("shiny.tag" %in% class(widget))
  stopifnot(is.numeric(index))
  type <- match.arg(type)
  if (is.null(style)) {
    style <- style(
      `margin-bottom` = "10px",
      `font-size` = "16px",
      `font-weight` = "bold"
    )
  }
  if (length(style) != length(groups)) style <- rep(style, length(groups))
  is_awesome <- type %in% "awesome"
  is_pretty <- type %in% "pretty"

  for (i in seq_along(index)) {
    idx <- index[i]
    grp <- groups[i]
    sty <- style[i]
    fidx <- ifelse(is_awesome, 3, 2)

    if (is_pretty) {
      if (idx == 1) {
        widget$children[[2]]$children[[1]]$children <- grp
        widget$children[[2]]$children[[1]]$attribs$style <- sty
      } else {
        widget$children[[2]]$children[[2]][[idx - 1]][[2]]$children <- grp
        widget$children[[2]]$children[[2]][[idx - 1]][[2]]$attribs$style <- sty
      }
    } else {
      widget$children[[fidx]]$children[[1]][[idx]]$children <- append(
        widget$children[[fidx]]$children[[1]][[idx]]$children,
        tagList(div(grp, style = style[i])),
        after = 0
      )
    }
  }

  widget
}


loadingButton <- function(inputId,
                          label,
                          ...,
                          loadingLabel = "Loading...",
                          loadingSpinner = "spinner") {
  opts <- list(
    label = label, loadingLabel = loadingLabel,
    loadingSpinner = loadingSpinner
  )

  opts <- jsonlite::toJSON(opts, auto_unbox = TRUE)

  span(
    class = "sf-loading-button",
    id = paste0("sf-loading-button", inputId),
    bs4Dash::actionButton(
      inputId = inputId,
      label = label,
      ...
    ),
    tags$head(tags$script(
      sprintf("loadingButtons.create('%s', %s)", inputId, opts)
    ))
  )
}


with_supref <- function(text, ns = get0("ns", parent.frame())) {
  ns <- ns %||% identity
  HTML(gsub(
    "\\{@([0-9]+)\\}",
    as.character(tags$sup(tags$a(
      id = ns(paste0("biblink-", "\\1")),
      href = "#",
      "\\1"
    ))),
    text
  ))
}


with_literata <- function(x, ...) {
  p(x, style = "font-family: Literata; margin-bottom: 0px;", ...)
}

with_gothic <- function(x, ...) {
  p(x, style = "font-family: Tablet Gothic", ...)
}

#' Columns wrappers
#'
#' These are convenient wrappers around
#' `column(12, ...)`, `column(6, ...)`, `column(4, ...)`...
#'
#' @noRd
col_12 <- function(...) bs4Dash::column(12, ...)
col_11 <- function(...) bs4Dash::column(11, ...)
col_10 <- function(...) bs4Dash::column(10, ...)
col_9 <- function(...) bs4Dash::column(9, ...)
col_8 <- function(...) bs4Dash::column(8, ...)
col_7 <- function(...) bs4Dash::column(7, ...)
col_6 <- function(...) bs4Dash::column(6, ...)
col_5 <- function(...) bs4Dash::column(5, ...)
col_4 <- function(...) bs4Dash::column(4, ...)
col_3 <- function(...) bs4Dash::column(3, ...)
col_2 <- function(...) bs4Dash::column(2, ...)
col_1 <- function(...) bs4Dash::column(1, ...)


match_regex <- function(string, pattern, perl = FALSE, fixed = FALSE) {
  regmatches(string, regexec(pattern, string, perl = perl, fixed = fixed))
}


#' Converts a shiny.tag object to unformatted raw text
#'
#' @param x A shiny.tag, shiny.tag.list or list
#' @param ... Passed to or from other methods
#' @export
tag_to_text <- function(x, ...) {
  UseMethod("tag_to_text")
}

#' @export
tag_to_text.default <- function(x, ...) {
  if (!is.null(x) && !length(intersect(class(x), c("html_dependency")))) {
    as.character(x, ...)
  } else {
    ""
  }
}

#' @export
tag_to_text.shiny.tag <- function(x, ...) {
  if (identical(class(x), "html_dependency")) {
    return("")
  }
  if (x$name %in% c("script", "head", "meta", "style")) {
    return("")
  }
  x <- x$children

  if (!length(x)) {
    ""
  } else {
    text <- lapply(x, tag_to_text)
    text <- text[!vapply(text, is.null, logical(1))]
    trimws(paste(text, collapse = "\n"))
  }
}

#' @export
tag_to_text.list <- function(x, ...) {
  trimws(paste(lapply(x, tag_to_text, ...), collapse = "\n"))
}


make_header <- function(title,
                        authors,
                        affil = NULL,
                        date = Sys.Date()) {
  if (!is.null(affil)) {
    if (length(affil) == 1) {
      affil <- as.list(rep(affil, length(authors)))
      names(affil) <- authors
    }

    aff_df <- data.frame(
      author = rep(authors, lengths(affil)),
      affil = unlist(affil),
      num = cumsum(!duplicated(unlist(affil))),
      row.names = NULL
    )
    authors <- lapply(authors, function(x) {
      paste0(x, paste(
        lapply(
          aff_df[aff_df$author == x, ]$num,
          \(x) as.character(tags$sup(x))
        ),
        collapse = ","
      ))
    })

    affil <- lapply(unique(aff_df$num), function(x) {
      unique(paste0(tags$sup(x), aff_df[aff_df$num == x, ]$affil))
    })
  }

  div(
    id = "header",
    h2(with_literata(HTML(title)), class = "title toc-ignore"),
    h5(HTML(paste(
      "Prepared by:",
      paste(authors, collapse = ", ")
    )), class = "author"),
    h5(date, class = "date"),
    h6(HTML(paste(affil, collapse = "; ")), class = "affil"),
    align = "center"
  )
}


corp_logo <- function(inst) {
  web <- list(
    gesis = "https://www.gesis.org/",
    unibo = "https://www.unibo.it/",
    tecnalia = "https://www.tecnalia.com/",
    lut = "https://www.lut.fi/",
    kaskas = "https://kaskas.fi/",
    tno = "https://www.tno.nl/",
    cleanwatts = "https://www.cleanwatts.energy/",
    isi = "https://www.isi.fraunhofer.de/"
  )

  div(class = "logo", a(
    class = "logo",
    href = web[[inst]],
    img(src = sprintf("www/%s_logo.png", inst), style = "height: 1.3em")
  ))
}

invert <- function(x) {
  stats::setNames(names(x), unname(x))
}

p2 <- function(...) p(..., class = "fancy")

noWS <- function(.f) function(...) .f(..., .noWS = c("inside", "outside"))

list_palettes <- function(type = NULL) {
  if (missing(type)) {
    type <- c("seq", "viridis")
  }

  type <- vapply(
    type,
    switch,
    seq = "Sequential",
    div = "Diverging",
    qual = "Qualitative",
    viridis = "Colorblind",
    FUN.VALUE = character(1)
  )

  pal <- list(
    Sequential = c(
      "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd",
      "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu",
      "YlOrBr", "YlOrRd"
    ),
    Diverging = c(
      "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn",
      "Spectral"
    ),
    Qualitative = c(
      "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
      "Set2", "Set3"
    ),
    Colorblind = c(
      "Magma", "Inferno", "Plasma", "Viridis", "Cividis", "Rocket", "Mako",
      "Turbo"
    )
  )

  pal[type]
}



#' Turn an R list into an HTML list
#'
#' @param list An R list
#' @param class a class for the list
#'
#' @return an HTML list
#' @noRd
#'
#' @examples
#' list_to_li(c("a", "b"))
list_to_li <- function(list, class = NULL) {
  tags$ul(lapply(list, tags$li))
}



named_to_dl <- function(.list) {
  dt <- lapply(names(.list), \(x) protect_html(tags$dt(x)))
  dd <- lapply(.list, \(x) protect_html(tags$dd(x)))
  tags$dl(riffle(dt, dd))
}



#' Repeat tags$br
#'
#' @param times the number of br to return
#'
#' @return the number of br specified in times
#' @noRd
#'
#' @examples
#' rep_br(5)
rep_br <- function(times = 1) {
  HTML(rep("<br/>", times = times))
}



#' Make the current tag behave like an action button
#'
#' Only works with compatible tags like button or links
#'
#' @param tag Any compatible tag.
#' @param inputId Unique id. This will host the input value to be used
#' on the server side.
#'
#' @return The modified tag with an extra id and the action button class.
#' @noRd
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   link <- a(href = "#", "My super link", style = "color: lightblue;")
#'
#'   ui <- fluidPage(
#'     make_action_button(link, inputId = "mylink")
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$mylink, {
#'       showNotification("Pouic!")
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
make_action_button <- function(tag, inputId = NULL) {
  # some obvious checks
  if (!inherits(tag, "shiny.tag")) stop("Must provide a shiny tag.")
  if (!is.null(tag$attribs$class)) {
    if (any(grepl("action-button", tag$attribs$class))) {
      stop("tag is already an action button")
    }
  }
  if (is.null(inputId) && is.null(tag$attribs$id)) {
    stop("tag does not have any id. Please use inputId to be able to
           access it on the server side.")
  }

  # handle id
  if (!is.null(inputId)) {
    if (!is.null(tag$attribs$id)) {
      warning(
        paste(
          "tag already has an id. Please use input$",
          tag$attribs$id,
          "to access it from the server side. inputId will be ignored."
        )
      )
    } else {
      tag$attribs$id <- inputId
    }
  }

  # handle class
  if (is.null(tag$attribs$class)) {
    tag$attribs$class <- "action-button"
  } else {
    tag$attribs$class <- paste(tag$attribs$class, "action-button")
  }
  # return tag
  tag
}

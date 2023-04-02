#' Columns wrappers
#'
#' These are convenient wrappers around
#' `column(12, ...)`, `column(6, ...)`, `column(4, ...)`...
#'
#' @noRd
#'
#' @importFrom shiny column
col_12 <- function(...) {
  bs4Dash::column(12, ...)
}

#' @importFrom shiny column
col_10 <- function(...) {
  bs4Dash::column(10, ...)
}

#' @importFrom shiny column
col_8 <- function(...) {
  bs4Dash::column(8, ...)
}

#' @importFrom shiny column
col_6 <- function(...) {
  bs4Dash::column(6, ...)
}


#' @importFrom shiny column
col_4 <- function(...) {
  bs4Dash::column(4, ...)
}


#' @importFrom shiny column
col_3 <- function(...) {
  bs4Dash::column(3, ...)
}


#' @importFrom shiny column
col_2 <- function(...) {
  bs4Dash::column(2, ...)
}


#' @importFrom shiny column
col_1 <- function(...) {
  bs4Dash::column(1, ...)
}

make_header <- function(title,
                        authors,
                        affil = NULL,
                        date = Sys.Date()) {
  if (!is.null(affil)) {
    if (length(affil) == 1) {
      affil <- purrr::set_names(as.list(rep(affil, length(authors))), authors)
    }
    aff_df <- data.frame(
      author = rep(authors, lengths(affil)),
      affil = unlist(affil),
      num = as.numeric(as.factor(unlist(affil))),
      row.names = NULL
    )
    authors <- lapply(authors, function(x) {
      paste0(x, paste(
        lapply(
          aff_df[aff_df$author == x, ]$num,
          \(x) as.character(tags$sup(x))), collapse = ","
        )
      )
    })
    
    affil <- lapply(unique(aff_df$num), function(x) {
      unique(paste0(tags$sup(x), aff_df[aff_df$num == x, ]$affil))
    })
  }

  div(
    id = "header",
    h2(HTML(title), class = "title toc-ignore"),
    h5(HTML(paste(authors, collapse = ", ")), class = "author"),
    h5(date, class = "date"),
    h6(HTML(paste(affil, collapse = "; ")), class = "affil"),
    align = "center"
  )
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

pbib <- function(...) p(..., class = "bib")

p2 <- function(...) p(..., class = "running-text")

noWS <- function(.f) function(...) .f(..., .noWS = c("inside", "outside")) 

list_palettes <- function() {
  list(
    "Common palettes" = list(
      "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd",
      "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu",
      "YlOrBr", "YlOrRd"
    ),
    "Colorblind palettes" = list(
      "Magma", "Inferno", "Plasma", "Viridis",
      "Cividis", "Rocket", "Mako", "Turbo"
    )
  )
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
#' @importFrom shiny tags tagAppendAttributes tagList
list_to_li <- function(list, class = NULL) {
  if (is.null(class)) {
    tagList(
      lapply(
        list,
        tags$li
      )
    )
  } else {
    res <- lapply(
      list,
      tags$li
    )
    res <- lapply(
      res,
      function(x) {
        tagAppendAttributes(
          x,
          class = class
        )
      }
    )
    tagList(res)
  }
}
#' Turn an R list into corresponding HTML paragraph tags
#'
#' @param list an R list
#' @param class a class for the paragraph tags
#'
#' @return An HTML tag
#' @noRd
#'
#' @examples
#' list_to_p(c("This is the first paragraph", "this is the second paragraph"))
#' @importFrom shiny tags tagAppendAttributes tagList
#'
list_to_p <- function(list, class = NULL) {
  if (is.null(class)) {
    tagList(
      lapply(
        list,
        tags$p
      )
    )
  } else {
    res <- lapply(
      list,
      tags$p
    )
    res <- lapply(
      res,
      function(x) {
        tagAppendAttributes(
          x,
          class = class
        )
      }
    )
    tagList(res)
  }
}

#' @importFrom shiny tags tagAppendAttributes tagList
named_to_li <- function(list, class = NULL) {
  if (is.null(class)) {
    res <- mapply(
      function(x, y) {
        tags$li(
          HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list,
      names(list),
      SIMPLIFY = FALSE
    )
    tagList(res)
  } else {
    res <- mapply(
      function(x, y) {
        tags$li(
          HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list,
      names(list),
      SIMPLIFY = FALSE
    )
    res <- lapply(
      res,
      function(x) {
        tagAppendAttributes(
          x,
          class = class
        )
      }
    )
    tagList(res)
  }
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
#' @importFrom shiny HTML
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
    if (grep("action-button", tag$attribs$class)) {
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


# UNCOMMENT AND USE
#
# attachment::att_amend_desc()
#
# To use this part of the UI
#
#' #' Include Content From a File
#' #'
#' #' Load rendered RMarkdown from a file and turn into HTML.
#' #'
#' #' @rdname includeRMarkdown
#' #' @export
#' #'
#' #' @importFrom rmarkdown render
#' #' @importFrom markdown markdownToHTML
#' #' @importFrom shiny HTML
#' includeRMarkdown <- function(path){
#'
#'   md <- tempfile(fileext = '.md')
#'
#'   on.exit(unlink(md),add = TRUE)
#'
#'   rmarkdown::render(
#'     path,
#'     output_format = 'md_document',
#'     output_dir = tempdir(),
#'     output_file = md,quiet = TRUE
#'     )
#'
#'   html <- markdown::markdownToHTML(md, fragment.only = TRUE)
#'
#'   Encoding(html) <- "UTF-8"
#'
#'   return(HTML(html))
#' }

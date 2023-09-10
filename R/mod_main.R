mod_main_ui <- function(id) {
  ns <- NS(id)

  categories <- unique(cb_ext$topic[!is.na(cb_ext$topic)])
  titles <- stats::setNames(lapply(categories, function(x) {
    as.list(unique(cb_ext[cb_ext$topic %in% x, ]$title))
  }), categories)

  shiny::div(
    mod_home_ui(ns("home")),
    mod_exp_ui(ns("exp"), titles),
    mod_cmp_ui(ns("cmp"), titles),
    mod_insp_ui(ns("insp"), titles),
    mod_ind_ui(ns("ind")),
    mod_cs_ui(ns("cs")),
    class = "tab-content"
  )
}


mod_main_server <- function(id, tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mod_home_server("home")
    exp_params <- mod_exp_server("exp")
    mod_cmp_server("cmp")
    mod_insp_server("insp")
    mod_cs_server("cs", tab = tab)
    mod_ind_server("ind")

    output[["exp-download"]] <- downloadHandler(
      filename = function() {
        params <- exp_params()
        var <- params$invar
        aggr <- params$aggr
        if (is.factor(params$values)) {
          var <- gsub("_([^_]*)$", "", var)
        }
        filename <- sprintf("greta_mns_%s_%s.geojson", var, aggr)
      },
      content = function(file) {
        params <- exp_params()
        poly <- params$poly
        var <- params$invar
        values <- params$values
        is_mode <- is.factor(values)
        cb_entry <- cb_ext[cb_ext$variable %in% var, ]

        question <- cb_entry$question
        subitem <- cb_entry$subitem
        option <- cb_entry$option
        if (is_mode) {
          option <- paste(levels(values), collapse = ", ")
        }

        desc <- sprintf(
          "DESCRIPTION=Question: %s - Subitem: %s - Option%s: %s",
          question, subitem, ifelse(is_mode, "s", ""), option
        )

        names(poly)[ncol(poly) - 1] <- "value"

        sf::st_write(poly, file, layer_options = c(desc, "WRITE_NAME=NO"))
      }
    )
  })
}

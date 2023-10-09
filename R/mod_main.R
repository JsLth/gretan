mod_main_ui <- function(id) {
  ns <- NS(id)

  categories <- unique(cb_ext$topic[!is.na(cb_ext$topic)])
  titles <- stats::setNames(lapply(categories, function(x) {
    as.list(unique(cb_ext[cb_ext$topic %in% x, ]$title))
  }), categories)

  shiny::div(
    shinydisconnect::disconnectMessage(
      text = "Something went wrong! Try refreshing the page.",
      refresh = "Refresh",
      background = "#FFFFFF",
      colour = "#444444",
      refreshColour = "#337AB7",
      overlayColour = "#000000",
      overlayOpacity = 0.6,
      width = 450,
      top = "center",
      size = 22,
      css = ""
    ),
    mod_home_ui(ns("home")),
    mod_exp_ui(ns("exp"), titles),
    mod_cmp_ui(ns("cmp"), titles),
    mod_insp_ui(ns("insp"), titles),
    mod_taxonomy_ui(ns("taxonomy")),
    mod_stakeholder_ui(ns("stakeholder")),
    mod_persona_ui(ns("persona")),
    mod_enpov_ui(ns("enpov")),
    mod_attitudes_ui(ns("attitudes")),
    mod_research_ui(ns("research")),
    mod_cs1_ui(ns("cs1")),
    mod_cs2_ui(ns("cs2")),
    mod_cs3_ui(ns("cs3")),
    mod_cs4_ui(ns("cs4")),
    mod_cs5_ui(ns("cs5")),
    if (isTRUE(getGretaOption("console", FALSE))) {
      keys::keysInput(ns("debug"), "ctrl+shift+d")
    },
    class = "tab-content"
  )
}


mod_main_server <- function(id, tab, changed) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mod_home_server("home")
    exp_params <- mod_exp_server("exp")
    mod_cmp_server("cmp")
    mod_insp_server("insp")
    mod_cs1_server("cs1")
    mod_cs2_server("cs2")
    mod_cs3_server("cs3")
    mod_cs4_server("cs4")
    mod_cs5_server("cs5")
    mod_taxonomy_server("taxonomy")
    mod_stakeholder_server("stakeholder", changed = changed)
    mod_persona_server("persona")
    mod_enpov_server("enpov")
    mod_attitudes_server("attitudes")
    mod_research_server("research")

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

    if (isTRUE(getGretaOption("console", FALSE))) {
      observe({
        send_info(
          title = "In-app console (for debugging only!)",
          text = tagList(
            shinyAce::aceEditor(
              ns("runcode_expr"),
              mode = "r",
              value = "",
              height = "200px",
              theme = "github",
              fontSize = 16
            ),
            bs4Dash::actionButton(
              ns("runcode_run"),
              "Run",
              class = "btn-success"
            ),
            shinyjs::hidden(
              div(
                id = ns("runcode_error"),
                style = "color: red; font-weight: bold;",
                tags$code("Oops, that resulted in an error! Try again."),
                div(
                  "Error: ",
                  br(),
                  tags$i(span(
                    id = ns("runcode_errorMsg"),
                    style = "margin-left: 10px;"
                  ))
                )
              )
            )
          )
        )
      }) %>%
        bindEvent(input$debug)

      shinyjs::runcodeServer()
    }
  })
}

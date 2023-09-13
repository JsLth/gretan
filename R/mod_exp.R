mod_exp_ui <- function(id, titles) {
  ns <- NS(id)
  cb <- cb_ext

  bs4Dash::tabItem(
    "exp",
    fluidRow(
      bs4Dash::column(
        width = 3,
        helpBox(
          title = "Data selection",
          id = ns("databox"),
          help_id = ns("databoxHelp"),
          width = 12,
          solidHeader = FALSE,
          collapsible = TRUE,
          status = "primary",
          shinyWidgets::pickerInput(
            ns("title"),
            "Topic",
            choices = titles,
            selected = "Energy behavior",
            options = shinyWidgets::pickerOptions(
              windowPadding = c(30, 0, 0, 0),
              liveSearch = TRUE
            )
          ),
          htmlOutput(ns("question")),
          tags$br(),
          shinyWidgets::pickerInput(ns("subitem"), "Subitem", character()),
          shinyWidgets::pickerInput(ns("option"), "Option", character())
        ),
        helpBox(
          title = "Map configuration",
          id = ns("config"),
          help_id = ns("configHelp"),
          width = 12,
          solidHeader = FALSE,
          collapsible = TRUE,
          status = "primary",
          shinyWidgets::pickerInput(
            ns("aggr"),
            "Aggregation level",
            choices = c(
              "Countries" = "nuts0",
              "Major regions" = "nuts1",
              "Minor regions" = "nuts2",
              "100 km grid" = "grid"
            ),
            selected = "nuts0"
          ),
          shinyWidgets::pickerInput(
            ns("pal"),
            "Color palette",
            choices = list_palettes(c("seq", "viridis", "qual"))
          ),
          shinyWidgets::prettyRadioButtons(
            ns("fixed"),
            "Legend values",
            choices = c("Full contrast", "Full range"),
            selected = "Full contrast",
            inline = TRUE
          ),
          shinyWidgets::materialSwitch(
            ns("mode"),
            label = "Aggregate options",
            value = FALSE,
            status = "primary",
            right = TRUE
          )
        ),
        bs4Dash::box(
          title = "About",
          width = 12,
          solidHeader = FALSE,
          collapsible = TRUE,
          collapsed = TRUE,
          status = "primary",
          p(txts$exp$about, style = "text-align: justify;"),
          div(
            style = "margin: auto; width: 50%;",
            tagAppendAttributes(
              downloadButton(
                outputId = ns("download"),
                label = "Get the data!",
                icon = icon("floppy-disk"),
                style = "width: 100%;"
              ),
              class = c("btn-app", "bg-primary")
            )
          )
        )
      ),
      bs4Dash::column(
        width = 9,
        bs4Dash::box(
          id = ns("mapbox"),
          width = 12,
          collapsible = FALSE,
          headerBorder = FALSE,
          status = "primary",
          leaflet::leafletOutput(ns("explorer"), width = "100%", height = 800)
        )
      )
    )
  )
}

mod_exp_server <- function(id, track = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cb <- cb_ext

    # Has the explorer rendered for the first time?
    init <- FALSE

    popover2(
      "databoxHelp",
      title = "Select a topic",
      content = txts$exp$help$databox
    )
    popover2(
      "configHelp",
      title = "Configure the map",
      content = txts$exp$help$confbox
    )

    # Show question
    output$question <- renderUI({
      render_question(input$title, input$subitem, input$option)
    })

    # Hide or show selectors for subitems or options depending on the question
    # Input pickers are updated to show the correct options and subitems for the
    # active question. This triggers and event, which would lead to double
    # reactivity as both changing the title and updating the subitem/option would
    # lead trigger the recalculation of the input variable. To prevent this, the
    # update flags are sets after updating. Update flags need to be FALSE for
    # the input variable to be re-calculated.
    observe(
      {
        varsel <- cb[cb$title %in% input$title, ]$variable
        items <- unique(cb[cb$variable %in% varsel, ]$subitem)
        options <- unique(cb[cb$variable %in% varsel, ]$option)
        show_subitems <- length(varsel) > 1 & !all(is.na(items))
        show_options <- length(varsel) > 1 & !all(is.na(options))

        if (show_subitems) {
          freezeReactiveValue(input, "subitem")
          freezeReactiveValue(input, "mode")
          shinyWidgets::updatePickerInput(
            session,
            inputId = "subitem",
            choices = items,
            clearOptions = TRUE
          )
          shinyjs::show("subitem", anim = TRUE)
        } else {
          shinyjs::hide("subitem", anim = TRUE)
        }

        if (show_options) {
          freezeReactiveValue(input, "option")
          freezeReactiveValue(input, "mode")
          shinyWidgets::updatePickerInput(
            session,
            inputId = "option",
            choices = options,
            clearOptions = TRUE
          )
          shinyjs::enable("mode")
          shinyjs::show("option", anim = TRUE)
        } else {
          freezeReactiveValue(input, "mode")
          shinyWidgets::updateMaterialSwitch(session, "mode", FALSE)
          freezeReactiveValue(input, "mode")
          shinyjs::disable("mode")
          shinyjs::hide("option", anim = TRUE)
        }
      },
      label = "show or hide options/subitems"
    )

    # Determine the variable based on combination of topic, subitem and option
    invar <- reactive(
      get_mns_variable(input$title, input$subitem, input$option, input$mode),
      label = "select input variable"
    )

    # Hide options depending on whether they are displayed individually or as mode
    # Three cases are defined:
    # 1. No options are available: Hide options and change to sequential palettes
    # 2. Options are available, but they are displayed as mode: Hide options and
    #    change to qualitative palettes
    # 3. Options are available and they are displayed individually: Show options
    #    and change to sequential palettes
    observe(
      {
        has_option <- all(!is.na(cb[cb$variable %in% invar(), ]$option))
        freezeReactiveValue(input, "pal")
        if (!has_option) {
          shinyjs::hide("option", anim = TRUE)
          palettes <- list_palettes()
          shinyWidgets::updatePickerInput(session, "pal", choices = palettes)
        } else if (isTRUE(input$mode) && has_option) {
          shinyjs::hide("option", anim = TRUE)
          palettes <- list_palettes("qual")
          shinyWidgets::updatePickerInput(session, "pal", choices = palettes)
        } else {
          shinyjs::show("option", anim = TRUE)
          palettes <- list_palettes()
          shinyWidgets::updatePickerInput(session, "pal", choices = palettes)
        }
      },
      label = "update palettes"
    )

    # Disable options to fix legend values when input variable is anything else
    # than percentages. If they are metric values (e.g. age), then we can't fix
    # legend values because the value range is not limited in theory. If they are
    # likert values or mode values, the full range is always displayed.
    observe(
      {
        invar <- invar()
        req(!is.null(invar))
        is_metric <- cb[cb$variable %in% invar, ]$is_metric
        if (all(is_metric)) {
          shinyjs::disable("fixed")
        } else {
          shinyjs::enable("fixed")
        }
      },
      label = "disable or enable fixed"
    )

    pal <- reactive({
      pal <- input$pal
      palettes <- list_palettes("viridis")
      if (pal %in% palettes[["Colorblind"]]) {
        pal <- viridisLite::viridis(5, option = tolower(pal))
      }
      pal
    })

    # Compile all parameters into a list
    exp_params <- reactive(
      {
        log_it("Attempting to change map parameters")
        execute_safely({
          invar <- invar()
          fixed <- input$fixed
          pal <- pal()
          aggr <- input$aggr
          get_mns_params(invar, fixed, pal, aggr)
        })
      },
      label = "compile parameters"
    )

    # Render initial leaflet map. Isolate reactives so they are only called once.
    output$explorer <- leaflet::renderLeaflet({
      params <- isolate(exp_params())
      init <- TRUE
      log_it("Initializing explorer")
      map_mns(params, track = track)
    })

    # Continuously update leaflet map
    observe(
      {
        req(init)
        params <- exp_params()
        log_it("Updating explorer")
        update_mns_map("explorer", params)
      },
      label = "update explorer"
    )

    return(exp_params)
  })
}

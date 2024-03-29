mod_exp_ui <- function(id, titles) {
  # Setup UI ----
  ns <- NS(id)
  cb <- cb_ext

  get_text <- dispatch_to_txt(id)

  bs4Dash::tabItem(
    "exp",
    fluidRow(
      bs4Dash::column(
        width = 3,
        # Data selection ----
        helpBox(
          title = "Data selection",
          id = ns("databox"),
          help_id = ns("databoxHelp"),
          width = 12,
          solidHeader = FALSE,
          collapsible = TRUE,
          status = "primary",
          ## Title ----
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
          ## Subitem and option ----
          shinyWidgets::pickerInput(ns("subitem"), "Subitem", character()),
          shinyWidgets::pickerInput(ns("option"), "Option", character())
        ),
        helpBox(
          # Map config ----
          title = "Map configuration",
          id = ns("config"),
          help_id = ns("configHelp"),
          width = 12,
          solidHeader = FALSE,
          collapsible = TRUE,
          status = "primary",
          ## Aggregation level ----
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
          ## Palette ----
          shinyWidgets::pickerInput(
            ns("pal"),
            "Color palette",
            choices = list_palettes(c("seq", "viridis", "qual"))
          ),
          ## Fixed ----
          shinyWidgets::prettyRadioButtons(
            ns("fixed"),
            "Legend values",
            choices = c("Full contrast", "Full range"),
            selected = "Full contrast",
            inline = TRUE
          ),
          ## Mode ----
          shinyWidgets::prettyToggle(
            ns("mode"),
            label_on = "Show as most popular",
            label_off = "Show as percentage",
            icon_on = icon("fire"),
            icon_off = icon("percent"),
            status_on = "warning",
            status_off = "warning",
            bigger = TRUE,
            value = FALSE
          )
        ),
        # About ----
        bs4Dash::box(
          title = "About",
          width = 12,
          solidHeader = FALSE,
          collapsible = TRUE,
          collapsed = TRUE,
          status = "primary",
          p(get_text("about"), style = "text-align: justify;"),
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
      # Explorer map ----
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
    # Server setup ----
    cb <- cb_ext
    get_text <- dispatch_to_txt(session$ns(NULL))

    popover2(
      "databoxHelp",
      title = "Select a topic",
      content = get_text("help", "databox")
    )
    popover2(
      "configHelp",
      title = "Configure the map",
      content = get_text("help", "confbox")
    )

    # Show question ----
    output$question <- renderUI({
      render_question(input$title, input$subitem, input$option)
    })


    # Show/hide subitems/options ----
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

    # Get MNS variable ----
    invar <- reactive(
      get_mns_variable(input$title, input$subitem, input$option, input$mode),
      label = "select input variable"
    )

    # Handle palette updates ----
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

    # Disable/enable fixed ----
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

    # Get palette ----
    pal <- reactive({
      pal <- input$pal
      palettes <- list_palettes("viridis")
      if (pal %in% palettes[["Colorblind"]]) {
        pal <- viridisLite::viridis(5, option = tolower(pal))
      }
      pal
    })

    # Compile parameters ----
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

    # Render initial map ----
    # Isolate reactives so they are only called once.
    output$explorer <- leaflet::renderLeaflet({
      params <- isolate(exp_params())
      log_it("Initializing explorer")
      map_mns(params, track = track)
    })

    # Update leaflet map ----
    observe(
      {
        params <- exp_params()
        log_it("Updating explorer")
        update_mns_map("explorer", params)
      },
      label = "update explorer"
    )

    return(exp_params)
  })
}

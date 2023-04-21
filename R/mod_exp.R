mod_exp_ui <- function(id, categories, titles) {
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
          shinyjs::hidden(div(
            id = ns("subitemHide"),
            shinyWidgets::pickerInput(ns("subitem"), "Subitem", character())
          )),
          shinyjs::hidden(div(
            id = ns("optionHide"),
            shinyWidgets::pickerInput(ns("option"), "Option", character())
          ))
        ),
        bs4Dash::box(
          title = "Map configuration",
          id = ns("config"),
          width = 12,
          solidHeader = FALSE,
          collapsible = TRUE,
          status = "primary",
          shinyWidgets::pickerInput(
            ns("aggr"),
            "Aggregation level",
            choices = c(
              "Countries" = "NUTS-0",
              "Major regions" = "NUTS-1",
              "Minor regions" = "NUTS-2"
            ),
            selected = "NUTS-0"
          ),
          shinyWidgets::pickerInput(
            ns("pal"),
            "Color palette",
            choices = list_palettes()
          ),
          shinyjs::disabled(div(
            id = ns("fixedHide"),
            shinyWidgets::prettyRadioButtons(
              ns("fixed"),
              "Legend values",
              choices = c("Full contrast", "Full range"),
              selected = "Full contrast",
              inline = TRUE
            )
          )),
          shinyWidgets::materialSwitch(
            ns("mode"),
            label = "Show options as mode",
            value = FALSE,
            status = "primary",
            right = TRUE
          )
        ),
        bs4Dash::box(
          title = "Download",
          id = ns("download"),
          width = 12,
          solidHeader = FALSE,
          collapsible = TRUE,
          collapsed = TRUE,
          status = "primary",
          bs4Dash::actionButton(
            "download_button",
            "Download data",
            icon = icon("download", lib = "font-awesome")
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


mod_exp <- function(input, output, session) {
  ns <- session$ns
  cb <- cb_ext
  
  # Initialize flag that specifies whether an input comes from the user or an
  # update function
  updated <- reactiveValues(
    subitem = FALSE,
    option = FALSE,
    palette = FALSE
  )
  
  # Has the explorer rendered for the first time?
  init <- reactiveVal(FALSE, label = "is initialized?")
  
  # Flag to turn switch event into logical: has the switch been triggered?
  mode_switched <- reactiveVal(FALSE)
  
  # Reactives to store the last values of inputs
  last_title <- reactiveVal(label = "last title")
  last_pal <- reactiveVal("Blues", label = "last palette")
  
  bs4Dash::addTooltip(
    id = "databoxHelp",
    options = tooltip_opts(txts$exp$help$databox)
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
  observe({
    varsel <- cb[cb$title %in% input$title, ]$variable
    items <- unique(cb[cb$variable %in% varsel, ]$subitem)
    options <- unique(cb[cb$variable %in% varsel, ]$option)
    show_subitems <- length(varsel) > 1 & !all(is.na(items))
    show_options <- length(varsel) > 1 & !all(is.na(options))
    
    if (show_subitems) {
      shinyWidgets::updatePickerInput(
        session,
        inputId = "subitem",
        choices = items,
        clearOptions = TRUE
      )

      updated$subitem <- "server"
      log_it("Changed subitems from server side", "warn")
      shinyjs::show("subitemHide", anim = TRUE)
    } else {
      shinyjs::hide("subitemHide", anim = TRUE)
    }

    if (show_options) {
      shinyWidgets::updatePickerInput(
        session,
        inputId = "option",
        choices = options,
        clearOptions = TRUE
      )

      updated$option <- "server"
      log_it("Changed options from server side", "warn")
      shinyjs::show("optionHide", anim = TRUE)
    } else {
      shinyjs::hide("optionHide", anim = TRUE)
    }
  }, label = "show or hide options/subitems") %>%
    bindEvent(input$title)
  
  # If an the subitem and option input pickers are triggered and this update
  # was done by the user directly, update the reactive values accordingly
  observe({
    if (isFALSE(updated$subitem)) updated$subitem <- "user"
  }, label = "indicate subitem user input") %>%
    bindEvent(input$subitem)
  
  observe({
    if (isFALSE(updated$option)) updated$option <- "user"
  }, label = "indicate option user input") %>%
    bindEvent(input$option)
  
  # Determine the variable based on combination of topic, subitem and option
  invar <- reactive({
    subitem_user <- identical(updated$subitem, "user")
    option_user <- identical(updated$option, "user")
    has_user_input <- subitem_user && option_user
    has_new_title <- !identical(input$title, last_title())
    is_init_run <- isFALSE(init())
    mode_switched <- mode_switched()
    can_change <- any(has_user_input, has_new_title, is_init_run, mode_switched)
    
    if (can_change) {
      log_it("Permitted to change input variable", "success")
    } else {
      log_it("Suspended from changing input variable", "warn")
    }
    cat2("\tsubitem input:", subitem_user)
    cat2("\toption input:", option_user)
    cat2("\tnew title:", has_new_title)
    cat2("\tmode switch:", mode_switched)

    updated$subitem <- FALSE
    updated$option <- FALSE
    
    # Requirements to change the variable (one of them must be true):
    # - Subitem and option are changed by user, not by updater. This is to
    #   prevent double reactivity, when changing the title triggers a
    #   programmatic update of option and subitem pickers.
    # - The selected title is different than the previous title. This is needed
    #   to force a variable change each time the title is explicitly changed.
    # - The explorer is being initialized. This is needed to ensure that the
    #   explorer can render for the first time, despite initial values of other
    #   reactives.
    # - The mode switch has been triggered. This is needed because when
    #   displaying options individually, only one variable is needed as opposed
    #   to displaying it as mode which requires all variables for each
    #   individual option.
    req(can_change)
    last_title(input$title)
    mode_switched(FALSE)

    get_mns_variable(input$title, input$subitem, input$option, input$mode)
  }, label = "select input variable") %>%
    bindEvent(input$title, input$subitem, input$option, input$mode)
  
  # Convert mode switch event to a logical reactive usable in conditional
  # statements
  observe({
    mode_switched(TRUE)
  }) %>%
    bindEvent(input$mode)
  
  # Hide options depending on whether they are displayed individually or as mode
  # Three cases are defined:
  # 1. No options are available: Hide options and change to sequential palettes
  # 2. Options are available, but they are displayed as mode: Hide options and
  #    change to qualitative palettes
  # 3. Options are available and they are displayed individually: Show options
  #    and change to sequential palettes
  observe({
    has_option <- all(!is.na(cb[cb$variable %in% invar(), ]$option))

    if (!has_option) {
      shinyjs::hide("optionHide")
      #palettes <- list_palettes("seq")
      #shinyWidgets::updatePickerInput(session, "pal", choices = palettes)
    } else if (isTRUE(input$mode) && has_option) {
      shinyjs::hide("optionHide")
      #palettes <- list_palettes("qual")
      #shinyWidgets::updatePickerInput(session, "pal", choices = palettes)
    } else {
      shinyjs::show("optionHide")
      #palettes <- list_palettes("seq")
      #shinyWidgets::updatePickerInput(session, "pal", choices = palettes)
    }
  }, label = "show or hide mode switch") %>%
    bindEvent(input$mode, invar())
  
  # Disable options to fix legend values when input variable is anything else
  # than percentages. If they are metric values (e.g. age), then we can't fix
  # legend values because the value range is not limited in theory. If they are
  # likert values or mode values, the full range is always displayed.
  observe({
    invar <- invar()
    req(!is.null(invar))
    is_metric <- cb[cb$variable %in% invar, ]$is_metric
    if (all(is_metric)) {
      shinyjs::disable("fixedHide")
    } else {
      shinyjs::enable("fixedHide")
    }
  }, label = "disable or enable fixed")

  # Compile all parameters into a list
  exp_params <- reactive({
    invar <- invar()
    #cat2("invar ready")
    fixed <- input$fixed
    pal <- input$pal
    aggr <- input$aggr
    get_mns_params(invar, fixed, pal, aggr)
  }, label = "compile parameters")
  
  # Render initial leaflet map. Isolate reactives so they are only called once.
  output$explorer <- leaflet::renderLeaflet({
    params <- isolate(exp_params())
    isolate(init(TRUE))
    log_it("Initializing explorer")
    map_mns(params)
  })
  
  # Continuously update leaflet map
  observe({
    req(isTRUE(init()))
    params <- exp_params()
    log_it("Updating explorer")
    update_mns_map("explorer", params)
  }, label = "update explorer")
}

mod_exp_server <- function(id) {
  moduleServer(id, mod_exp)
}

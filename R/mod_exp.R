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
          shinyWidgets::pickerInput(ns("subitem"), "Subitem", character()),
          shinyWidgets::pickerInput(ns("option"), "Option", character())
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
              "Countries" = "nuts0",
              "Major regions" = "nuts1",
              "Minor regions" = "nuts2"
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
            label = "Options as mode",
            value = FALSE,
            status = "primary",
            right = TRUE
          )
        ),
        bs4Dash::box(
          title = "Download",
          width = 12,
          solidHeader = FALSE,
          collapsible = TRUE,
          collapsed = TRUE,
          status = "primary",
          downloadButton(ns("download"), "Download")
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
    
    # Initialize flag that specifies whether an input comes from the user or an
    # update function
    updated <- reactiveValues(
      subitem = FALSE,
      option = FALSE,
      pal = FALSE
    )
    
    # Has the explorer rendered for the first time?
    init <- reactiveVal(FALSE, label = "is initialized?")
    
    # Flag to turn switch event into logical: has the switch been triggered?
    mode_switched <- reactiveVal(FALSE)
    
    invar <- reactiveVal(NULL, label = "input variable")
    
    # Reactives to store the last values of inputs
    last_title <- reactiveVal(label = "last title")
    last_var <- reactiveVal(label = "last variable")
    
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
        shinyjs::show("subitem", anim = TRUE)
      } else {
        shinyjs::hide("subitem", anim = TRUE)
      }
      
      if (show_options) {
        shinyWidgets::updatePickerInput(
          session,
          inputId = "option",
          choices = options,
          clearOptions = TRUE
        )
        updated$option <- "server"
        shinyjs::enable("mode")
        shinyjs::show("option", anim = TRUE)
      } else {
        shinyjs::disable("mode")
        shinyjs::hide("option", anim = TRUE)
      }
    }, label = "show or hide options/subitems") %>%
      bindEvent(input$title)
    
    # If an the subitem and option input pickers are triggered and this update
    # was done by the user directly, update the reactive values accordingly
    # Princriple:
    # 1. Widgets are updated
    # 2. Update flag is set to TRUE
    # 3. Target reactive is triggered and checks for update flag
    # 4. Target reactive sets update flag to FALSE after check
    # 5. Target reactive is cancelled because it requires update flag to be FALSE
    # 6. Subsequent calls of target reactive are successful because update flag
    #    is FALSE now
    
    # Convert mode switch event to a logical reactive usable in conditional
    # statements
    observe({
      mode_switched(TRUE)
    }) %>%
      bindEvent(input$mode)
    
    # Determine the variable based on combination of topic, subitem and option
    observe({
      subitem_server <- identical(updated$subitem, "server")
      option_server <- identical(updated$option, "server")
      was_updated <- any(subitem_server, option_server)
      has_new_title <- !identical(input$title, last_title())
      is_init_run <- isFALSE(init())
      mode_switched <- mode_switched()
      can_change <- any(!was_updated, has_new_title, is_init_run, mode_switched)
      
      if (can_change) {
        log_it(
          sprintf("{%s} - Permitted to change input variable; details:", id)
        )
      } else {
        log_it(
          sprintf("{%s} - Suspended from changing input variable; details:", id)
        )
      }
      log_details(sprintf(
        "update: subitem - %s, option - %s (%s)",
        subitem_server, option_server, ifelse(was_updated, "not passed", "passed")
      ))
      log_details(sprintf(
        "new title: %s (%s)",
        has_new_title, ifelse(has_new_title, "passed", "not passed")
      ))
      log_details(sprintf(
        "mode switch: %s (%s)",
        mode_switched, ifelse(mode_switched, "passed", "not passed")
      ))
      log_details(sprintf(
        "is init run: %s (%s)",
        is_init_run, ifelse(is_init_run, "passed", "not passed")
      ))

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
      req(can_change, cancelOutput = TRUE)
      last_title(input$title)
      mode_switched(FALSE)
      
      invar(get_mns_variable(input$title, input$subitem, input$option, input$mode))
    }, label = "select input variable", priority = 1) %>%
      bindEvent(input$title, input$subitem, input$option, input$mode)
    
    # Hide options depending on whether they are displayed individually or as mode
    # Three cases are defined:
    # 1. No options are available: Hide options and change to sequential palettes
    # 2. Options are available, but they are displayed as mode: Hide options and
    #    change to qualitative palettes
    # 3. Options are available and they are displayed individually: Show options
    #    and change to sequential palettes
    # TODO:
    # The reason why this is commented out is that it's implementation is not very
    # clean right now. Switching to mode triggers an update of the input variable
    # and an update of the palette picker UI. The server-side update of the
    # palette UI however also triggers an update of the input palette. Hence,
    # the map parameters are updated twice leading to double reactivity of the
    # observer updating the explorer map. Technically, there is no downside to
    # this as all maps are updated as designed, but visually and performance-wise
    # this is undesirable and not very appealing.
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
        shinyjs::disable("fixed")
      } else {
        shinyjs::enable("fixed")
      }
    }, label = "disable or enable fixed")
    
    pal <- reactive({
      pal <- input$pal
      palettes <- list_palettes("viridis")
      if (pal %in% palettes[["Colorblind"]]) {
        pal <- viridis::viridis_pal(option = tolower(pal))(5)
      }
      pal
    }) %>%
      bindEvent(input$pal)
    
    # Compile all parameters into a list
    exp_params <- reactive({
      log_it("Attempting to change map parameters")
      invar <- invar()
      fixed <- input$fixed
      pal <- pal()
      aggr <- input$aggr
      get_mns_params(invar, fixed, pal, aggr)
    }, label = "compile parameters")
    
    # Render initial leaflet map. Isolate reactives so they are only called once.
    output$explorer <- leaflet::renderLeaflet({
      params <- isolate(exp_params())
      isolate(init(TRUE))
      log_it("Initializing explorer")
      map_mns(params, track = track)
    })
    
    # Continuously update leaflet map
    observe({
      req(isTRUE(init()))
      params <- exp_params()
      log_it("Updating explorer")
      update_mns_map("explorer", params)
    }, label = "update explorer")
    
    return(exp_params)
  })
}

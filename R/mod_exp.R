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
  init <- reactiveVal(FALSE, label = "is initialized?")
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
  # This part also triggers events for input$option and input$subitem when their
  # values are updated. The update flag is therefore set to TRUE to prevent
  # event binding.
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
      updated$subitem <- TRUE
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
      updated$option <- TRUE
      log_it("Changed options from server side", "warn")
      shinyjs::show("optionHide", anim = TRUE)
    } else {
      shinyjs::hide("optionHide", anim = TRUE)
    }
  }, label = "show or hide options/subitems") %>%
    bindEvent(input$title)
  
  # If an event is triggered by a user, update reactive values
  observe({
    if (isTRUE(updated$subitem)) updated$subitem <- FALSE
  }, label = "indicate subitem user input") %>%
    bindEvent(input$subitem)
  
  observe({
    if (isTRUE(updated$option)) updated$option <- FALSE
  }, label = "indicate option user input") %>%
    bindEvent(input$option)
  
  # Determine the variable based on combination of topic, subitem and option
  invar <- reactive({
    has_user_input <- !any(updated$subitem, updated$option)
    has_new_title <- !identical(input$title, last_title())
    is_init_run <- isFALSE(init())
    mode_switched <- is.logical(input$mode)
    can_change <- any(has_user_input, has_new_title, is_init_run, mode_switched)
    
    if (can_change) {
      log_it("Permitted to change input variable", "success")
    } else {
      log_it("Suspended from changing input variable", "warn")
      cat2("\tsubitem input:", !updated$subitem)
      cat2("\toption input:", !updated$option)
      cat2("\tnew title:", has_new_title)
      cat2("\tmode switch:", is.logical(input$mode))
    }

    # Cancel if subitem and option are not explicitly changed by user
    req(has_user_input || has_new_title || isFALSE(init()) || is.logical(input$mode))
    last_title(input$title)

    get_mns_variable(input$title, input$subitem, input$option, input$mode)
  }, label = "select input variable") %>%
    bindEvent(input$title, input$subitem, input$option, input$mode)
  
  observe({
    has_option <- all(!is.na(cb[cb$variable %in% invar(), ]$option))

    if (!has_option) {
      print("hide and seq")
      shinyjs::hide("optionHide")
      #palettes <- list_palettes("seq")
      #shinyWidgets::updatePickerInput(session, "pal", choices = palettes)
    } else if (isTRUE(input$mode) && has_option) {
      print("hide and qual")
      shinyjs::hide("optionHide")
      #palettes <- list_palettes("qual")
      #shinyWidgets::updatePickerInput(session, "pal", choices = palettes)
    } else {
      print("show and seq")
      shinyjs::show("optionHide")
      #palettes <- list_palettes("seq")
      #shinyWidgets::updatePickerInput(session, "pal", choices = palettes)
    }
  }, label = "show or hide mode switch") %>%
    bindEvent(input$mode, invar())
  
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
  
  pal <- reactive({
    pal <- input$pal
    palettes <- list_palettes()
    if (pal %in% palettes[["Colorblind palettes"]]) {
      pal <- viridis::viridis_pal(option = tolower(pal))(5)
    }
    pal
  }, label = "select input palette")
  
  poly <- reactive({
    switch(
      input$aggr,
      "NUTS-0" = srv_nuts0,
      "NUTS-1" = srv_nuts1,
      "NUTS-2" = srv_nuts2
    )
  }, label = "select input polygons") %>%
    bindEvent(input$aggr)

  exp_params <- reactive({
    invar <- invar()
    cat2("invar ready")
    fixed <- input$fixed
    pal <- pal()
    cat2("pal ready")
    poly <- poly()
    cat2("poly ready")
    get_mns_params(invar, fixed, pal, poly)
  }, label = "compile parameters")
  
  output$explorer <- leaflet::renderLeaflet({
    params <- isolate(exp_params())
    isolate(init(TRUE))
    log_it("Initializing explorer")
    map_mns(params)
  })
  
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

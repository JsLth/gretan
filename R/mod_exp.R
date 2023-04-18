mod_exp_ui <- function(id, categories, titles) {
  ns <- NS(id)
  cb <- cb_ext
  categories <- unique(cb$topic[!is.na(cb$topic)])
  titles <- categories %>%
    purrr::map(~dplyr::filter(cb, topic == .x) %>%
                 dplyr::pull(title) %>% unique() %>%
                 as.list()) %>%
    purrr::set_names(categories)
  
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
            choices = c("NUTS-0", "NUTS-1", "NUTS-2"),
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
          shinyjs::disable(div(
            id = ns("modeHide"),
            shinyWidgets::materialSwitch(
              ns("hide"),
              label = "Show options as mode",
              value = FALSE,
              status = "primary",
              right = TRUE
            )
          ))
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
  updated <- reactiveValues(subitem = FALSE, option = FALSE)
  
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
      shinyjs::show("optionHide", anim = TRUE)
    } else {
      shinyjs::hide("optionHide", anim = TRUE)
    }
  }) %>%
    bindEvent(input$title)
  
  # If an event is triggered by a user, update reactive values
  observe({
    if (isTRUE(updated$subitem)) updated$subitem <- FALSE
  }) %>%
    bindEvent(input$subitem, updated$subitem)
  
  observe({
    if (isTRUE(updated$option)) updated$option <- FALSE
  }) %>%
    bindEvent(input$option, updated$option)
  
  # Determine the variable based on combination of topic, subitem and option
  invar <- reactive({
    has_user_input <- !any(updated$subitem, updated$option)
    is_init <- !as.logical(watch("exp"))

    # Cancel if subitem and option are not explicitly changed by user
    req(has_user_input || is_init)
    
    get_mns_variable(input$title, input$subitem, input$option)
  }) %>%
    bindEvent(input$title, input$subitem, input$option)
  
  observe({
    invar <- invar()
    req(!is.null(invar))
    is_metric <- cb[cb$variable %in% invar, ]$is_metric
    if (is_metric) {
      shinyjs::disable("fixedHide")
    } else {
      shinyjs::enable("fixedHide")
    }
  })
  
  pal <- reactive({
    palettes <- list_palettes()
    if (input$pal %in% palettes[["Colorblind palettes"]]) {
      viridis::viridis_pal(option = tolower(input$pal))(5)
    } else {
      input$pal
    }
  })
  
  poly <- reactive({
    switch(
      input$aggr,
      "NUTS-0" = srv_nuts0,
      "NUTS-1" = srv_nuts1,
      "NUTS-2" = srv_nuts2
    )
  }) %>%
    bindEvent(input$aggr)
  
  exp_params <- reactive({
    get_mns_params(invar(), input$fixed, pal(), poly())
  })
  
  output$explorer <- leaflet::renderLeaflet({
    isolate(trigger("exp"))
    map_mns(isolate(exp_params()))
  })
  
  observe({
    req(watch("exp"))
    update_mns_map("explorer", exp_params())
  })
}

mod_exp_server <- function(id) {
  moduleServer(id, mod_exp)
}

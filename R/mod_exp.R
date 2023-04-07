mod_exp_ui <- function(id) {
  ns <- NS(id)
  cb <- cb_ext
  categories <- unique(cb$topic[!is.na(cb$topic)])
  titles <- categories %>%
    purrr::map(~dplyr::filter(cb, topic == .x) %>%
                 dplyr::pull(title) %>% unique() %>%
                 as.list()) %>%
    purrr::set_names(categories)
  
  bs4Dash::tabItem(
    "explorer",
    fluidRow(
      bs4Dash::column(
        width = 3,
        bs4Dash::box(
          title = "Data selection", 
          id = ns("databox"),
          width = 12,
          solidHeader = FALSE, 
          collapsible = TRUE,
          status = "primary",
          shinyWidgets::pickerInput(
            ns("title"),
            "Topic",
            titles,
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
            c("NUTS-0", "NUTS-1", "NUTS-2")
          ),
          shinyWidgets::pickerInput(ns("pal"), "Color palette", list_palettes()),
          shinyjs::disabled(div(
            id = ns("fixedHide"),
            shinyWidgets::prettyRadioButtons(
              ns("fixed"),
              "Legend values",
              choices = c("Full contrast", "Full range"),
              selected = "Full contrast",
              inline = TRUE
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
          solidHeader = FALSE,
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
  
  # Show question
  output$question <- renderUI({
    if (!is.null(input$title)) {
      indat <- cb[cb$title %in% input$title, ]
      
      if (!all(is.na(indat$subitem))) {
        indat <- indat[indat$subitem %in% input$subitem, ]
      }
      
      if (!all(is.na(indat$option))) {
        indat <- indat[indat$option %in% input$option, ]
      }
      
      HTML(sprintf(
        "<b>Question %s:</b><br>%s",
        toupper(indat$og_var),
        indat$label
      ))
    } else {
      ""
    }
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
      cli::cli_rule("updating options: {updated$subitem}")
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
      cli::cli_rule("updating options: {updated$option}")
      shinyjs::show("optionHide", anim = TRUE)
    } else {
      shinyjs::hide("optionHide", anim = TRUE)
    }
  }) %>%
    bindEvent(input$title)
  
  # Set update flag to FALSE when input comes directly from user
  observe({
    cli::cli_rule("observing subitem")
    updated$subitem <- FALSE
  }, priority = 2L) %>%
    bindEvent(input$subitem)
  
  observe({
    cli::cli_rule("observing option")
    updated$option <- FALSE
  }, priority = 1L) %>%
    bindEvent(input$option)
  
  # Determine the variable based on combination of topic, subitem and option
  invar <- reactiveVal(NULL)
  observe({
    cli::cli_rule("trying invar / option: {updated$option}, subitem: {updated$subitem}")
    # Cancel if subitem and option are not explicitly changed by user
    req(isFALSE(updated$subitem) && isFALSE(updated$option))
    cli::cli_rule("success :)")
    has_title <- cb$title %in% input$title
    invar <- cb[has_title, ]

    # case: multiple items exist, look for subitems
    if (length(invar$variable) > 1) {
      has_subitem <- invar$subitem %in% input$subitem
      
      # only select subitem if any exist
      if (any(has_subitem)) {
        invar <- invar[has_subitem, ]
      }
    }

    # case: there's still multiple items, look for options
    if (length(invar$variable) > 1 || !length(invar$variable)) {
      has_option <- invar$option %in% input$option
      
      # only select option if any exist
      if (any(has_option)) {
        invar <- invar[has_option, ]
      }
    }

    # if all strings fail, just select the first one
    if (length(invar$variable) > 1) {
      invar <- invar[1, ]
    }
    invar(invar$variable)
  }, priority = 0L) %>%
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
    poly <- poly()
    invar <- invar()
    pal <- pal()
    
    is_metric <- cb[cb$variable %in% invar, ]$is_metric
    is_dummy <- cb[cb$variable %in% invar, ]$is_dummy ||
      cb[cb$variable %in% invar, ]$is_pdummy
    
    domain <- NULL
    values <- as.formula(paste0("~", invar))
    
    if (identical(invar, "c1")) {
      lgd <- "Mean age"
      unit <- " years"
    } else if (is_metric) {
      lgd <- "Mean"
      unit <- ""
    } else {
      if (identical(input$fixed, "Full range")) {
        domain <- seq(0, 100, 10)
        values <- domain
      }
      lgd <- "Share"
      unit <- "%"
      poly[[invar]] <- poly[[invar]] * 100
    }
    
    pal <- leaflet::colorNumeric(pal, domain = domain)
    
    label_values <- list(
      poly[["nuts0"]], poly[["nuts1"]], poly[["nuts2"]],
      paste0(round(poly[[invar]], 2), unit), ":"
    )
    names(label_values) <- c("NUTS-0", "NUTS-1", "NUTS-2", lgd, "sep")
    labels <- do.call(align_dl, label_values)
    
    poly <- sf::st_transform(poly, 4326)
    
    list(
      poly = poly,
      invar = invar,
      pal = pal,
      values = values,
      labels = labels,
      lgd = lgd,
      unit = unit
    )
  })
  
  output$explorer <- leaflet::renderLeaflet({
    params <- isolate(exp_params())
    trigger("exp")
    
    leaflet::leaflet(params$poly) %>%
      leaflet::addTiles() %>%
      leaflet::setView(lng = 9, lat = 55, zoom = 4) %>%
      leaflet::addPolygons(
        fillColor = as.formula(paste0("~params$pal(", params$invar, ")")),
        fillOpacity = 0.7,
        weight = 1,
        color = "black",
        opacity = 0.5,
        label = params$labels,
        highlightOptions = highlight_opts
      ) %>%
      leaflet::addLegend(
        position = "bottomright",
        na.label = "No data",
        pal = params$pal,
        values = params$values,
        opacity = 0.9,
        title = params$lgd,
        labFormat = leaflet::labelFormat(suffix = params$unit)
      )
  })
  
  observe({
    req(isTRUE(watch("exp")))
    params <- exp_params()
    
    leaflet::leafletProxy("explorer", data = params$poly) %>%
      leaflet::clearShapes() %>%
      leaflet::clearControls() %>%
      leaflet::addPolygons(
        fillColor = as.formula(paste0("~params$pal(", params$invar, ")")),
        fillOpacity = 0.7,
        weight = 1,
        color = "black",
        opacity = 0.5,
        label = params$labels,
        highlightOptions = highlight_opts
      ) %>%
      leaflet::addLegend(
        position = "bottomright",
        na.label = "No data",
        pal = params$pal,
        values = params$values,
        opacity = 0.9,
        title = params$lgd,
        labFormat = leaflet::labelFormat(suffix = params$unit)
      )
  })
}


mod_exp_server <- function(id) {
  moduleServer(id, mod_exp)
}

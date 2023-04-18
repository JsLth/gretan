mod_cmp_ui <- function(id, categories, titles) {
  ns <- NS(id)
  
  bs4Dash::tabItem(
    "cmp",
    fluidRow(
      bs4Dash::column(
        6,
        bs4Dash::tabBox(
          id = ns("setupLeft"),
          status = "primary",
          width = 12,
          type = "tabs",
          tabPanel(
            title = "Data", 
            id = ns("databoxLeft"),
            icon = icon("filter"),
            shinyWidgets::pickerInput(
              ns("titleLeft"),
              "Topic",
              choices = titles,
              selected = "Heating system usage",
              options = shinyWidgets::pickerOptions(
                windowPadding = c(30, 0, 0, 0),
                liveSearch = TRUE
              )
            ),
            htmlOutput(ns("questionLeft")),
            tags$br(),
            shinyjs::hidden(div(
              id = ns("subitemHideLeft"),
              shinyWidgets::pickerInput(ns("subitemLeft"), "Subitem", character())
            )),
            shinyjs::hidden(div(
              id = ns("optionHideLeft"),
              shinyWidgets::pickerInput(ns("optionLeft"), "Option", character())
            ))
          ),
          tabPanel(
            title = "Config",
            id = ns("configLeft"),
            icon = icon("gears"),
            shinyWidgets::pickerInput(
              ns("aggrLeft"),
              "Aggregation level",
              list(
                "NUTS-0 (nations)" = "NUTS-0",
                "NUTS-1 (major regions)" = "NUTS-1",
                "NUTS-2 (minor regions)" = "NUTS-2"
              )
            ),
            shinyWidgets::pickerInput(
              ns("palLeft"),
              "Color palette",
              list_palettes()
            ),
            shinyjs::disabled(div(
              id = ns("fixedHideLeft"),
              shinyWidgets::prettyRadioButtons(
                ns("fixedLeft"),
                "Legend values",
                choices = c("Full contrast", "Full range"),
                selected = "Full contrast",
                inline = TRUE
              )
            ))
          )
        )
      ),
      bs4Dash::column(
        6,
        bs4Dash::tabBox(
          id = ns("setupRight"),
          status = "primary",
          width = 12,
          type = "tabs",
          tabPanel(
            title = "Data", 
            id = ns("databoxRight"),
            class = "box-nav-link",
            icon = icon("filter"),
            shinyWidgets::pickerInput(
              ns("titleRight"),
              "Topic",
              choices = titles,
              selected = "Cooling system usage",
              options = shinyWidgets::pickerOptions(
                windowPadding = c(30, 0, 0, 0),
                liveSearch = TRUE
              )
            ),
            htmlOutput(ns("questionRight")),
            tags$br(),
            shinyjs::hidden(div(
              id = ns("subitemHideRight"),
              shinyWidgets::pickerInput(ns("subitemRight"), "Subitem", character())
            )),
            shinyjs::hidden(div(
              id = ns("optionHideRight"),
              shinyWidgets::pickerInput(ns("optionRight"), "Option", character())
            ))
          ),
          tabPanel(
            title = "Config",
            id = ns("configRight"),
            icon = icon("gears"),
            shinyWidgets::pickerInput(
              ns("aggrRight"),
              "Aggregation level",
              list(
                "NUTS-0 (nations)" = "NUTS-0",
                "NUTS-1 (major regions)" = "NUTS-1",
                "NUTS-2 (minor regions)" = "NUTS-2"
              )
            ),
            shinyWidgets::pickerInput(
              ns("palRight"),
              "Color palette",
              list_palettes()
            ),
            shinyjs::disabled(div(
              id = ns("fixedHideRight"),
              shinyWidgets::prettyRadioButtons(
                ns("fixedRight"),
                "Legend values",
                choices = c("Full contrast", "Full range"),
                selected = "Full contrast",
                inline = TRUE
              )
            ))
          )
        )
      )
    ),
    fluidRow(
      bs4Dash::column(
        6,
        bs4Dash::box(
          id = ns("mapboxLeft"),
          class = "leaflet-box",
          width = 12,
          collapsible = FALSE,
          headerBorder = FALSE,
          status = "primary",
          leaflet::leafletOutput(
            ns("explorerLeft"),
            width = "100%",
            height = 800
          )
        )
      ),
      bs4Dash::column(
        6,
        bs4Dash::box(
          id = ns("mapboxRight"),
          class = "leaflet-box",
          width = 12,
          collapsible = FALSE,
          headerBorder = FALSE,
          status = "primary",
          leaflet::leafletOutput(
            ns("explorerRight"),
            width = "100%",
            height = 800
          )
        )
      )
    )
  )
}


mod_cmp <- function(input, output, session) {
  ns <- session$ns
  cb <- cb_ext

  # Initialize flag that specifies whether an input comes from the user or an
  # update function
  updated <- reactiveValues(
    subitemLeft = FALSE,
    subitemRight = FALSE,
    optionLeft = FALSE,
    optionRight = FALSE,
  )
  
  map_init <- reactiveValues(left = FALSE, right = FALSE)
  
  # Show question
  output$questionLeft <- renderUI({
    render_question(input$titleLeft, input$subitemLeft, input$option$Left)
  })
  
  # Show question
  output$questionRight <- renderUI({
    render_question(input$titleRight, input$subitemRight, input$optionRight)
  })
  
  # Hide or show selectors for subitems or options depending on the question
  # This part also triggers events for input$option and input$subitem when their
  # values are updated. The update flag is therefore set to TRUE to prevent
  # event binding.
  observe({
    varsel <- cb_ext[cb_ext$title %in% input$titleLeft, ]$variable
    items <- unique(cb_ext[cb_ext$variable %in% varsel, ]$subitem)
    options <- unique(cb_ext[cb_ext$variable %in% varsel, ]$option)
    show_subitems <- length(varsel) > 1 & !all(is.na(items))
    show_options <- length(varsel) > 1 & !all(is.na(options))
    
    if (show_subitems) {
      shinyWidgets::updatePickerInput(
        session,
        inputId = "subitemLeft",
        choices = items,
        clearOptions = TRUE
      )
      updated$subitem <- TRUE
      shinyjs::show("subitemHideLeft", anim = TRUE)
    } else {
      shinyjs::hide("subitemHideLeft", anim = TRUE)
    }
    
    if (show_options) {
      shinyWidgets::updatePickerInput(
        session,
        inputId = "optionLeft",
        choices = options,
        clearOptions = TRUE
      )
      updated$option <- TRUE
      shinyjs::show("optionHideLeft", anim = TRUE)
    } else {
      shinyjs::hide("optionHideLeft", anim = TRUE)
    }
  }) %>%
    bindEvent(input$titleLeft)
  
  observe({
    varsel <- cb_ext[cb_ext$title %in% input$titleRight, ]$variable
    items <- unique(cb_ext[cb_ext$variable %in% varsel, ]$subitem)
    options <- unique(cb_ext[cb_ext$variable %in% varsel, ]$option)
    show_subitems <- length(varsel) > 1 & !all(is.na(items))
    show_options <- length(varsel) > 1 & !all(is.na(options))
    
    if (show_subitems) {
      shinyWidgets::updatePickerInput(
        session,
        inputId = "subitemRight",
        choices = items,
        clearOptions = TRUE
      )
      updated$subitem <- TRUE
      shinyjs::show("subitemHideRight", anim = TRUE)
    } else {
      shinyjs::hide("subitemHideRight", anim = TRUE)
    }
    
    if (show_options) {
      shinyWidgets::updatePickerInput(
        session,
        inputId = "optionRight",
        choices = options,
        clearOptions = TRUE
      )
      updated$option <- TRUE
      shinyjs::show("optionHideRight", anim = TRUE)
    } else {
      shinyjs::hide("optionHideRight", anim = TRUE)
    }
  }) %>%
    bindEvent(input$titleRight)
  
  # If an event is triggered by a user, update reactive values
  observe({
    if (isTRUE(updated$subitemLeft)) updated$subitemLeft <- FALSE
  }) %>%
    bindEvent(input$subitemLeft, updated$subitemLeft)
  
  observe({
    if (isTRUE(updated$subitemRight)) updated$subitemRight <- FALSE
  }) %>%
    bindEvent(input$subitemRight, updated$subitemRight)
  
  observe({
    if (isTRUE(updated$optionLeft)) updated$optionLeft <- FALSE
  }) %>%
    bindEvent(input$optionLeft, updated$optionLeft)
  
  observe({
    if (isTRUE(updated$optionRight)) updated$optionRight <- FALSE
  }) %>%
    bindEvent(input$optionRight, updated$optionRight)
  
  # Determine the variable based on combination of topic, subitem and option
  invar_left <- reactive({
    has_user_input <- !any(updated$subitemLeft, updated$optionLeft)
    is_init <- map_init$left
    
    # Cancel if subitem and option are not explicitly changed by user
    req(has_user_input || is_init)

    get_mns_variable(input$titleLeft, input$subitemLeft, input$optionLeft)
  }) %>%
    bindEvent(input$titleLeft, input$subitemLeft, input$optionLeft)

  invar_right <- reactive({
    has_user_input <- !any(updated$subitemRight, updated$optionRight)
    is_init <- map_init$right

    # Cancel if subitem and option are not explicitly changed by user
    req(has_user_input || is_init)
    
    get_mns_variable(input$titleRight, input$subitemRight, input$optionRight)
  }) %>%
    bindEvent(input$titleRight, input$subitemRight, input$optionRight)
  
  observe({
    invar_left <- invar_left()
    req(!is.null(invar_left))
    is_metric_left <- cb[cb$variable %in% invar_left, ]$is_metric
    if (is_metric_left) {
      shinyjs::disable("fixedHideLeft")
    } else {
      shinyjs::enable("fixedHideLeft")
    }
  })
  
  observe({
    invar_right <- invar_right()
    req(!is.null(invar_right))
    is_metric_right <- cb[cb$variable %in% invar_right, ]$is_metric
    if (is_metric_right) {
      shinyjs::disable("fixedHideRight")
    } else {
      shinyjs::enable("fixedHideRight")
    }
  })
  
  pal_left <- reactive({
    palettes <- list_palettes()
    sel_pal <- input$palLeft
    if (sel_pal %in% palettes[["Colorblind palettes"]]) {
      viridis::viridis_pal(option = tolower(sel_pal))(5)
    } else {
      sel_pal
    }
  })
  
  pal_right <- reactive({
    palettes <- list_palettes()
    sel_pal <- input$palRight
    if (sel_pal %in% palettes[["Colorblind palettes"]]) {
      viridis::viridis_pal(option = tolower(sel_pal))(5)
    } else {
      sel_pal
    }
  })
  
  poly_left <- reactive({
    switch(
      input$aggrLeft,
      "NUTS-0" = srv_nuts0,
      "NUTS-1" = srv_nuts1,
      "NUTS-2" = srv_nuts2
    )
  }) %>%
    bindEvent(input$aggrLeft)
  
  poly_right <- reactive({
    switch(
      input$aggrRight,
      "NUTS-0" = srv_nuts0,
      "NUTS-1" = srv_nuts1,
      "NUTS-2" = srv_nuts2
    )
  }) %>%
    bindEvent(input$aggrRight)
  
  exp_params_left <- reactive({
    get_mns_params(invar_left(), input$fixedLeft, pal_left(), poly_left())
  })
  
  exp_params_right <- reactive({
    get_mns_params(invar_right(), input$fixedRight, pal_right(), poly_right())
  })
  
  output$explorerLeft <- leaflet::renderLeaflet({
    params <- isolate(exp_params_left())
    isolate(map_init$left <- TRUE)
    map_mns(params) %>%
      track_coordinates("explorerLeft_mousemove")
  })
  
  output$explorerRight <- leaflet::renderLeaflet({
    params <- isolate(exp_params_right())
    isolate(map_init$right <- TRUE)
    map_mns(params) %>%
      track_coordinates("explorerRight_mousemove")
  })
  
  observe({
    req(map_init$left)
    update_mns_map("explorerLeft", exp_params_left())
  })
  
  observe({
    req(map_init$right)
    update_mns_map("explorerRight", exp_params_right())
  })
  
  null_indicator <- reactiveValues(left = TRUE, right = TRUE)
  
  observe({
    mouse <- unlist(input$explorerRight_shape_mouseout)
    if (is.null(mouse)) return(TRUE)
    params <- exp_params_right()
    mouse <- sf::st_sfc(sf::st_point(mouse[c("lng", "lat")]), crs = 4326)
    has_mouse <- sf::st_contains(mouse, params$poly, sparse = FALSE)
    null_indicator$left <- !any(has_mouse)
  }) %>%
    bindEvent(input$explorerRight_shape_mouseout)
  
  observe({
    mouse <- unlist(input$explorerLeft_shape_mouseout)
    if (is.null(mouse)) return(TRUE)
    params <- exp_params_right()
    mouse <- sf::st_sfc(sf::st_point(mouse[c("lng", "lat")]), crs = 4326)
    has_mouse <- sf::st_contains(mouse, params$poly, sparse = FALSE)
    null_indicator$right <- !any(has_mouse)
  }) %>%
    bindEvent(input$explorerLeft_shape_mouseout)
  
  indicator_label_left <- reactive({
    mouse <- unlist(input$explorerRight_shape_mouseover)
    req(!is.null(mouse))
    params <- exp_params_left()
    mouse <- sf::st_sfc(sf::st_point(mouse[c("lng", "lat")]), crs = 4326)
    country_idx <- sf::st_nearest_feature(mouse, params$poly, longlat = TRUE)
    null_indicator$left <- FALSE
    params$labels[[country_idx]]
  }) %>%
    bindEvent(input$explorerRight_shape_mouseover)
  
  indicator_label_right <- reactive({
    mouse <- unlist(input$explorerLeft_shape_mouseover)
    req(!is.null(mouse))
    params <- exp_params_right()
    mouse <- sf::st_sfc(sf::st_point(mouse[c("lng", "lat")]), crs = 4326)
    country_idx <- sf::st_nearest_feature(mouse, params$poly, longlat = TRUE)
    null_indicator$right <- FALSE
    params$labels[[country_idx]]
  }) %>%
    bindEvent(input$explorerLeft_shape_mouseover)
  
  observe({
    hover <- input$explorerLeft_mousemove
    label <- indicator_label_right()
    outside <- null_indicator$right

    if (is.null(hover) || is.null(label) || outside)  {
      leaflet::leafletProxy("explorerRight") %>%
        leaflet::removeMarker(ns("indicator-right"))
    } else {
      leaflet::leafletProxy("explorerRight") %>%
        leaflet::addLabelOnlyMarkers(
          lng = hover[1],
          lat = hover[2],
          layerId = ns("indicator-right"),
          label = label,
          labelOptions = leaflet::labelOptions(noHide = TRUE)
        )
    }
  })
  
  observe({
    hover <- input$explorerRight_mousemove
    label <- indicator_label_left()
    outside <- null_indicator$left
    
    if (is.null(hover) || is.null(label) || outside)  {
      leaflet::leafletProxy("explorerLeft") %>%
        leaflet::removeMarker(ns("indicator-left"))
    } else {
      leaflet::leafletProxy("explorerLeft") %>%
        leaflet::addLabelOnlyMarkers(
          lng = hover[1],
          lat = hover[2],
          layerId = ns("indicator-left"),
          label = label,
          labelOptions = leaflet::labelOptions(noHide = TRUE)
        )
    }
  })
}

mod_cmp_server <- function(id) {
  moduleServer(id, mod_cmp)
}

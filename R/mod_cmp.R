mod_cmp_side_ui <- function(id, titles) {
  ns <- NS(id)

  bs4Dash::column(
    6,
    bs4Dash::tabBox(
      id = ns("setup"),
      status = "primary",
      width = 12,
      type = "tabs",
      tabPanel(
        title = "Data", 
        id = ns("databox"),
        icon = icon("filter"),
        shinyWidgets::pickerInput(
          ns("title"),
          "Topic",
          choices = titles,
          selected = "Heating system usage",
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
      tabPanel(
        title = "Config",
        id = ns("config"),
        icon = icon("gears"),
        shinyWidgets::pickerInput(
          ns("aggr"),
          "Aggregation level",
          list(
            "Countries" = "nuts0",
            "Major regions" = "nuts1",
            "Minor regions" = "nuts2"
          )
        ),
        shinyWidgets::pickerInput(
          ns("pal"),
          "Color palette",
          list_palettes()
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
      )
    ),
    bs4Dash::box(
      id = ns("mapbox"),
      class = "leaflet-box",
      width = 12,
      collapsible = FALSE,
      headerBorder = FALSE,
      status = "primary",
      leaflet::leafletOutput(
        ns("explorer"),
        width = "100%",
        height = 800
      )
    )
  )
}

mod_cmp_ui <- function(id, titles) {
  ns <- NS(id)
  
  bs4Dash::tabItem(
    "cmp",
    fluidRow(
      mod_cmp_side_ui(ns("left"), titles),
      mod_cmp_side_ui(ns("right"), titles)
    )
  )
}


mod_cmp <- function(input, output, session) {
  ns <- session$ns
  
  exp_params_left <- mod_exp_server("left", track = TRUE)
  exp_params_right <- mod_exp_server("right", track = TRUE)
  
  null_indicator <- reactiveValues(left = TRUE, right = TRUE)
  indicator_label <- reactiveValues(left = NULL, right = NULL)
  
  observe({
    mouse <- unlist(input[["right-explorer_shape_mouseout"]])
    if (is.null(mouse)) return(TRUE)
    params <- exp_params_right()
    mouse <- sf::st_sfc(sf::st_point(mouse[c("lng", "lat")]), crs = 4326)
    has_mouse <- sf::st_contains(mouse, params$poly, sparse = FALSE)
    null_indicator$left <- !any(has_mouse)
  }) %>%
    bindEvent(input[["right-explorer_shape_mouseout"]])
  
  observe({
    mouse <- unlist(input[["left-explorer_shape_mouseout"]])
    if (is.null(mouse)) return(TRUE)
    params <- exp_params_left()
    mouse <- sf::st_sfc(sf::st_point(mouse[c("lng", "lat")]), crs = 4326)
    has_mouse <- sf::st_contains(mouse, params$poly, sparse = FALSE)
    null_indicator$right <- !any(has_mouse)
  }) %>%
    bindEvent(input[["left-explorer_shape_mouseout"]])
  
  observe({
    mouse <- unlist(input[["right-explorer_shape_mouseover"]])
    req(!is.null(mouse))
    params <- exp_params_left()
    mouse <- sf::st_sfc(sf::st_point(mouse[c("lng", "lat")]), crs = 4326)
    country_idx <- sf::st_nearest_feature(mouse, params$poly, longlat = TRUE)
    null_indicator$left <- FALSE
    indicator_label$left <- params$labels[[country_idx]]
  }) %>%
    bindEvent(input[["right-explorer_shape_mouseover"]])
  
  observe({
    mouse <- unlist(input[["left-explorer_shape_mouseover"]])
    req(!is.null(mouse))
    params <- exp_params_right()
    mouse <- sf::st_sfc(sf::st_point(mouse[c("lng", "lat")]), crs = 4326)
    country_idx <- sf::st_nearest_feature(mouse, params$poly, longlat = TRUE)
    null_indicator$right <- FALSE
    indicator_label$right <- params$labels[[country_idx]]
  }) %>%
    bindEvent(input[["left-explorer_shape_mouseover"]])
  
  observe({
    hover <- input[["left-explorer_mousemove"]]
    label <- indicator_label$right
    outside <- null_indicator$right
    
    if (is.null(hover) || is.null(label) || outside)  {
      leaflet::leafletProxy("right-explorer") %>%
        leaflet::removeMarker(ns("right-indicator"))
    } else {
      leaflet::leafletProxy("right-explorer") %>%
        leaflet::addLabelOnlyMarkers(
          lng = hover[1],
          lat = hover[2],
          layerId = ns("right-indicator"),
          label = label,
          labelOptions = leaflet::labelOptions(noHide = TRUE)
        )
    }
  })
  
  observe({
    hover <- input[["right-explorer_mousemove"]]
    label <- indicator_label$left
    outside <- null_indicator$left
    
    if (is.null(hover) || is.null(label) || outside)  {
      leaflet::leafletProxy("left-explorer") %>%
        leaflet::removeMarker(ns("left-indicator"))
    } else {
      leaflet::leafletProxy("left-explorer") %>%
        leaflet::addLabelOnlyMarkers(
          lng = hover[1],
          lat = hover[2],
          layerId = ns("left-indicator"),
          label = label,
          labelOptions = leaflet::labelOptions(noHide = TRUE)
        )
    }
  })
}

mod_cmp_server <- function(id) {
  moduleServer(id, mod_cmp)
}

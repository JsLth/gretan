mod_spatial_ui <- function(id) {
  ns <- NS(id)
  
  bs4Dash::tabItem(
    "spatial",
    make_header(
      title = "Investment in the Coop\u00e9rnico project: An examplary analysis of two projects",
      authors = c("Dennis Abel", "Jonas Lieth"),
      affil = "GESIS - Leibniz Institute for the Social Sciences",
      date = "08-03-2023"
    ),
    fluidRow(
      col_6(
        bs4Dash::box(
          title = "Introduction",
          status = "primary",
          width = 12,
          collapsible = TRUE,
          p2(shinipsum::random_text(nwords = 200))
        )
      ),
      col_6(
        bs4Dash::box(
          title = "Methodology",
          status = "primary",
          width = 12,
          collapsible = TRUE,
          p2(shinipsum::random_text(nwords = 200))
        )
      )
    ),
    fluidRow(
      col_6(
        bs4Dash::box(
          width = 12,
          status = "primary",
          title = "Geographical distribution of Coop\u00e9rnico investments",
          leaflet::leafletOutput(ns("dist"), height = 700)
        )
      ),
      col_6(
        bs4Dash::box(
          width = 12,
          status = "primary",
          title = "Spatial clusters of Coop\u00e9rnico investments",
          leaflet::leafletOutput(ns("clusters"), height = 700),
          sidebar = bs4Dash::boxSidebar(
            id = ns("clusters_sidebar"),
            background = "#CFCFCF",
            shinyWidgets::prettyRadioButtons(
              ns("clusters_sidebar_scheme"),
              label = "Coding scheme",
              choices = c(
                "Raw" = "raw",
                "Binary" = "B",
                "Row standardized" = "W",
                "Globally standardized" = "C",
                "Universally standardized" = "U",
                "Minmax" = "minmax",
                "Variance stabilizing" = "S"
              ),
              selected = "raw",
              status = "default",
              animation = "smooth"
            ),
            shinyWidgets::pickerInput(
              ns("clusters_sidebar_dist"),
              label = "Distance modelling",
              choices = c(
                "Inverse distance weighting" = "idw",
                "Exponential distance decay" = "exp",
                "Double-power distance weighting" = "dpd"
              ),
              width = "90%"
            ),
            numericInput(
              ns("clusters_sidebar_alpha"),
              label = "Distance modelling parameter",
              min = 0, max = NA, value = 1, step = 0.1
            ),
            numericInput(
              ns("clusters_sidebar_dmax"),
              label = "Maximum distance threshold",
              min = 1, max = NA, value = 1
            ),
            actionButton(
              ns("clusters_sidebar_apply"),
              label = "Apply changes",
              icon = icon("refresh", lib = "font-awesome")
            )
          )
        )
      )
    ),
    fluidRow(
      col_6(
        bs4Dash::box(
          title = "Discussion",
          status = "primary",
          width = 12,
          collapsible = TRUE,
          p2(shinipsum::random_text(nwords = 550))
        )
      ),
      col_6(
        bs4Dash::box(
          width = 12,
          status = "primary",
          title = "Scatterplot of Moran's I",
          plotly::plotlyOutput(ns("scatter"))
        ),
        bs4Dash::box(
          title = "References",
          status = "primary",
          width = 12,
          dummy_bibliography()
        )
      )
    )
  )
}


mod_spatial <- function(input, output, session) {
  ns <- session$ns
  rct <- reactiveValues()
  
  # Employ waiters
  w_dist <- do.call(waiter::Waiter$new, c(id = ns("distribution"), waiter_default))
  w_clusters <- do.call(waiter::Waiter$new, c(id = ns("clusters"), waiter_default))
  w_scatter <- do.call(waiter::Waiter$new, c(id = ns("scatter"), waiter_default))
  
  output$dist <- leaflet::renderLeaflet({
    w_dist$show()
    pal <- leaflet::colorNumeric("Spectral", NULL)
    
    coopernico_projects <- data.frame(
      project = c("Escola JG Zarco", "Lar S. Silvestre"), 
      lat = c(38.700671, 39.879235), lng = c(-9.237519, -7.396333),
      label = c("<b>Project name</b>: Escola JG Zarco", "<b>Project name</b>: Lar S. Silvestre")
    )
    
    coopernico_projects <- sf::st_as_sf(
      coopernico_projects,
      coords = c("lng", "lat"),
      remove = FALSE, 
      crs = 4326,
      agr = "constant"
    )
    
    # Create regional capitals database (five continental regions = NUTS 2)
    capitals <- data.frame(
      city = c("Lisboa", "Porto", "Coimbra", "Faro", "Evora"), 
      lat = c(38.725267, 41.162142, 40.211111, 37.016111, 38.566667), 
      lng = c(-9.150019, -8.621953, -8.429167, -7.935, -7.9)
    )
    
    capitals <- sf::st_as_sf(
      capitals,
      coords = c("lng", "lat"),
      remove = FALSE,
      crs = 4326,
      agr = "constant"
    )
    
    labels <- align_dl("Total investment" = coopernico$total_amount)
    
    m <- leaflet::leaflet(coopernico, TRUE) %>%
      leaflet::addTiles() %>%
      leaflet::setView(lng = -7.5, lat = 39.5, zoom = 7) %>%
      leaflet::addPolygons(
        fillColor = ~pal(total_amount),
        fillOpacity = 1,
        color = "black",
        weight = 0.5,
        smoothFactor = 0, # necessary to remove gaps between polygons
        highlightOptions = highlight_opts,
        label = labels
      ) %>%
      leaflet::addLegend(
        position = "bottomright",
        na.label = "No data",
        pal = pal,
        values = ~total_amount,
        opacity = 0.9,
        title = "Total investment",
        labFormat = leaflet::labelFormat(suffix = " \u20ac")
      ) %>%
      leaflet::addCircleMarkers(
        color = "black",
        fillColor = "red",
        fillOpacity = 1,
        opacity = 1,
        weight = 1,
        radius = 5,
        data = capitals
      ) %>%
      leaflet::addAwesomeMarkers(data = coopernico_projects, popup = ~label)
    
    w_dist$hide()
    m
  })
  
  lw <- reactive({
    nb <- spdep::poly2nb(coopernico, queen = TRUE)
    spdep::nb2listwdist(
      nb,
      sf::st_centroid(sf::st_geometry(coopernico)),
      type = input$clusters_sidebar_dist,
      alpha = input$clusters_sidebar_alpha,
      style = input$clusters_sidebar_scheme,
      dmax = input$clusters_sidebar_dmax,
      longlat = TRUE,
      zero.policy = TRUE
    )
  })
  
  lag <- reactive({
    spdep::lag.listw(
      lw(),
      coopernico$total_amount,
      zero.policy = TRUE
    )
  })
  
  locm <- reactive({
    spdep::localmoran(
      coopernico$total_amount,
      listw = lw(),
      zero.policy = TRUE
    )
  })
  
  sym <- reactive({
    locm <- locm()
    colors <- locm_colors_abel(locm, coopernico)
    labels <- do.call(align_dl, list(
      "Moran's I" = round(locm[, "Ii"], 4),
      "p-value" = round(locm[, "Pr(z != E(Ii))"], 4)
    ))
    
    list(colors = colors, labels = labels)
  })
  
  observe({
    w_clusters$show()
    sym <- sym()
    
    leaflet::leafletProxy("clusters") %>%
      leaflet::clearShapes() %>%
      leaflet::addPolygons(
        fillColor = sym$colors$fill,
        color = sym$colors$outline,
        fillOpacity = 1,
        weight = 1,
        smoothFactor = 0,
        label = sym$labels,
        highlightOptions = highlight_opts,
        data = coopernico
      )
    w_clusters$hide()
  }) %>%
    bindEvent(input$clusters_sidebar_apply)
  
  output$clusters <- leaflet::renderLeaflet({
    params <- isolate(sym())
    locm <- isolate(locm())
    
    m <- leaflet::leaflet(coopernico) %>%
      leaflet::addTiles() %>%
      leaflet::setView(lng = -7.5, lat = 39.5, zoom = 7) %>%
      leaflet::addPolygons(
        fillColor = params$colors$fill,
        color = params$colors$outline,
        fillOpacity = 1,
        weight = 1,
        smoothFactor = 0,
        label = params$labels,
        highlightOptions = highlight_opts
      ) %>%
      leaflet::addLegend(
        position = "bottomright",
        colors = c(unique(params$colors$outline), "white"),
        labels = c(levels(attributes(locm)$quadr$mean), "Not significant"),
        title = "LISA"
      )
    
    w_clusters$hide()
    m
  })
  
  output$scatter <- plotly::renderPlotly({
    w_scatter$show()
    lag <- isolate(lag())

    p <- plotly::plot_ly(coopernico, type = "scatter", mode = "markers") %>%
      plotly::add_trace(
        x = lag,
        y = ~fitted(lm(total_amount ~ lag)),
        line = list(dash = "dash", color = "darkred", width = 4),
        showlegend = F,
        mode = "lines"
      ) %>%
      plotly::add_trace(
        x = lag,
        y = ~total_amount,
        text = ~sprintf(
          "Spatial lag: %s m<br>Total amount: %s €<br>Municipality: %s",
          round(lag), total_amount, name
        ),
        marker = list(width = 2, color = "black"),
        showlegend = F,
        mode = "markers"
      ) %>%
      plotly::layout(
        xaxis = list(title = "Spatial lag (in m)"),
        yaxis = list(title = "Total investment (in \u20ac)")
      ) %>%
      plotly::config(displayModeBar = FALSE)
    w_scatter$hide()
    p
  })
  
  observe({
    w_scatter$show()
    lag <- lag()
    print(lag)
    plotly::plotlyProxy("scatter") %>%
      plotly::plotlyProxyInvoke("deleteTraces", list(-1, -2)) %>%
      plotly::plotlyProxyInvoke(
        "addTraces",
        list(
          list(
            x = lag,
            y = fitted(lm(coopernico$total_amount ~ lag)),
            line = list(dash = "dash", color = "darkred", width = 4),
            showlegend = F,
            mode = "lines"
          ),
          list(
            x = lag,
            y = coopernico$total_amount,
            text = sprintf(
              "Spatial lag: %s m<br>Total amount: %s€<br>Municipality: %s",
              round(lag), coopernico$total_amount, coopernico$name
            ),
            marker = list(width = 2, color = "black"),
            showlegend = F,
            type = "scatter",
            mode = "markers"
          )
        )
      )
    w_scatter$hide()
  }) %>%
    bindEvent(input$clusters_sidebar_apply)
}


mod_spatial_server <- function(id) {
  moduleServer(id, mod_spatial)
}
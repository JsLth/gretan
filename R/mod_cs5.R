mod_cs5_ui <- function(id) {
  ns <- NS(id)

  get_text <- dispatch_to_txt(id)

  bs4Dash::tabItem(
    "cs5",
    # Header ----
    make_header(
      title = get_text("title"),
      authors = names(get_text("affil")),
      affil = get_text("affil"),
      date = get_text("date")
    ),
    fluidRow(
      bs4Dash::column(
        width = 6,
        # Box 1 ----
        bs4Dash::box(
          title = with_literata(get_text("introduction", "title")),
          width = 12,
          status = "primary",
          get_text("introduction", "content")
        )
      ),
      bs4Dash::column(
        width = 6,
        # Box 2 ----
        bs4Dash::box(
          title = with_literata(get_text("how", "title")),
          width = 12,
          status = "primary",
          with_supref(get_text("how", "content"))
        )
      )
    ),
    fluidRow(
      bs4Dash::column(
        width = 12,
        # Buildings ----
        bs4Dash::box(
          id = ns("mapbox"),
          width = 12,
          status = "primary",
          headerBorder = FALSE,
          collapsible = FALSE,
          leaflet::leafletOutput(ns("buildings"), width = "100%", height = 800),
          ## Info panel ----
          leafletPanel(
            ns("buildings-info"),
            title = with_literata("Buildings in Bera Bera"),
            position = "topleft",
            p(get_text("buildings_info")),
            hr(),
            htmlOutput(ns("buildings-info-layer-desc")),
            hr(),
            helpText(with_gothic("Data source: UNIBO, Tecnalia"))
          ),
          ## Control panel ----
          leafletPanel(
            inputId = ns("buildings-control"),
            title = with_literata("Map control"),
            position = "topright",
            top = 80,
            right = 10,
            width = 200,
            shinyWidgets::prettyRadioButtons(
              inputId = ns("buildings-basemap"),
              label = "Basemap",
              choices = c("OpenStreetMap", "Satellite"),
              selected = "OpenStreetMap"
            ),
            shinyWidgets::prettyRadioButtons(
              inputId = ns("buildings-layer"),
              label = "Select layer",
              choices = invert(lapply(
                get_text("dict", "buildings"),
                "[[",
                "title"
              )),
              selected = "year_constr"
            )
          ),
          # bring leaflet panels to front when selected
          tags$script(HTML("$('.leaflet-info').on('mousedown', function() {
              $('.leaflet-info-front').removeClass('leaflet-info-front');
              $(this).addClass('leaflet-info-front');
            });"))
        )
      )
    ),
    fluidRow(
      bs4Dash::column(
        width = 6,
        # Box 3 ----
        bs4Dash::box(
          title = with_literata(get_text("analysis", "title")),
          width = 12,
          status = "primary",
          get_text("analysis", "content")
        )
      ),
      bs4Dash::column(
        width = 6,
        # Box 4 ----
        bs4Dash::box(
          title = with_literata(get_text("findings", "title")),
          width = 12,
          status = "primary",
          get_text("findings", "content")
        )
      )
    )
  )
}


mod_cs5_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server setup ----
    get_text <- dispatch_to_txt(session$ns(NULL))

    waiter <- waiter::Waiter$new(
      id = session$ns("buildings"),
      html = tagList(waiter::spin_pulse(), h4("Loading figure...")),
      color = "rgba(179, 221, 254, 1)"
    )

    popover2(
      "biblink-2",
      title = "",
      content = "Tecnalia Innovation & Research (2019)<br>
        ENERKAD. More information: <a href='https://www.enerkad.net/'>
        https://www.enerkad.net/</a>",
      trigger = "click",
      placement = "top"
    )

    buildings <- reactive({
      sf::read_sf(app_sys("extdata/cs5spain.gpkg"), layer = "buildings")
    })

    output[["buildings-info-layer-desc"]] <- renderUI({
      layer <- input$`buildings-layer`
      p(get_text("desc", layer))
    })

    ## Parameters ----
    labels <- NULL
    params <- reactive({
      req(identical(get_tab(), "cs5"))
      dt <- isolate(buildings())
      layer <- input$`buildings-layer`

      # Only create labels once and then save them to the server module for
      # re-use
      if (is.null(labels)) {
        lab_values <- dt[names(get_text("dict", "buildings"))] %>%
          sf::st_drop_geometry() %>%
          as.list() %>%
          stats::setNames(sapply(get_text("dict", "buildings"), "[[", "title")) %>%
          lapply(\(x) if (is.numeric(x)) round(x, 2) else x)
        lab_values$`Heating demand` <- paste(lab_values$`Heating demand`, "kWh/m\u00b2")
        labels <<- do.call(align_in_table, lab_values)
      }

      pal <- switch(layer,
        substation = leaflet::colorFactor(
          palette = c(
            "#ccb15a", "#cb2b29", "#d520a8", "#32de60",
            "#9a56df", "#9adf5a", "#61e0e2"
          ),
          domain = dt$substation
        ),
        year_constr = leaflet::colorBin(
          palette = "Greens",
          domain = dt$year_constr
        ),
        number_of_dw = leaflet::colorNumeric(
          palette = "Reds",
          domain = dt$number_of_dw
        ),
        a_heat_dem_m2 = leaflet::colorNumeric(
          palette = "Blues",
          domain = dt$a_heat_dem_m2
        )
      )

      list(data = dt, layer = layer, pal = pal, labels = labels)
    })

    ## Render ----
    output$buildings <- leaflet::renderLeaflet({
      params <- isolate(params())

      leaflet::leaflet() %>%
        leaflet::setView(lng = -1.994929, lat = 43.302187, zoom = 17) %>%
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) %>%
        leaflet::addPolygons(
          data = sf::st_transform(params$data, 4326),
          fillColor = stats::as.formula(paste0("~params$pal(", params$layer, ")")),
          fillOpacity = 1,
          color = "black",
          opacity = 1,
          weight = 1,
          highlightOptions = leaflet::highlightOptions(
            weight = 2,
            stroke = TRUE,
            opacity = 1,
            bringToFront = TRUE,
            sendToBack = TRUE,
            fillOpacity = 1
          ),
          label = params$labels
        ) %>%
        leaflet::addLegend(
          position = "bottomleft",
          pal = params$pal,
          title = get_text("dict", "buildings", params$layer, "title"),
          values = params$data[[params$layer]],
          labFormat = leaflet::labelFormat(
            suffix = get_text("dict", "buildings", params$layer, "lab")
          )
        )
    }) %>%
      bindEvent(params())

    ## Select layer ----
    updates <- 0

    observe({
      # Only show loading screen on first two updates
      # First one on startup
      # Second one when loading the tab item
      if (updates < 2) {
        waiter$show()
        updates <<- updates + 1
        on.exit(waiter$hide())
      }

      params <- params()
      leaflet::leafletProxy("buildings") %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() %>%
        leaflet::addPolygons(
          data = sf::st_transform(params$data, 4326),
          fillColor = stats::as.formula(paste0("~params$pal(", params$layer, ")")),
          fillOpacity = 1,
          color = "black",
          opacity = 1,
          weight = 1,
          highlightOptions = leaflet::highlightOptions(
            weight = 2,
            stroke = TRUE,
            opacity = 1,
            bringToFront = TRUE,
            sendToBack = TRUE,
            fillOpacity = 1
          ),
          label = params$labels
        ) %>%
        leaflet::addLegend(
          position = "bottomleft",
          pal = params$pal,
          title = get_text("dict", "buildings", params$layer, "title"),
          values = params$data[[params$layer]],
          labFormat = leaflet::labelFormat(
            suffix = get_text("dict", "buildings", params$layer, "lab")
          )
        )
    })

    ## Basemap ----
    observe({
      basemap <- input$`buildings-basemap` %||% "OpenStreetMap"
      basemap <- switch(basemap,
        "OpenStreetMap" = leaflet::providers$OpenStreetMap,
        "Satellite" = leaflet::providers$Esri.WorldImagery
      )

      leaflet::leafletProxy("buildings") %>%
        leaflet::clearTiles() %>%
        leaflet::addProviderTiles(basemap)
    })
  })
}

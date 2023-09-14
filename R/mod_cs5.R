mod_cs5_ui <- function(id) {
  ns <- NS(id)
  
  bs4Dash::tabItem(
    "cs5spain",
    # Header ----
    make_header(
      title = txts$cs5$title,
      authors = names(txts$cs5$affil),
      affil = txts$cs5$affil,
      date = txts$cs5$date
    ),
    fluidRow(
      bs4Dash::column(
        width = 6,
        # Box 1 ----
        bs4Dash::box(
          title = with_literata("Energy modelling"),
          width = 12,
          status = "primary",
          shinipsum::random_text(nwords = 250)
        )
      ),
      bs4Dash::column(
        width = 6,
        # Box 2 ----
        bs4Dash::box(
          title = with_literata("Case study"),
          width = 12,
          status = "primary",
          shinipsum::random_text(nwords = 250)
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
            p(txts$cs5$buildings_info),
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
              choices = invert(lapply(txts$cs5$dict$buildings, "[[", "title")),
              selected = "substation"
            )
          ),
          # bring leaflet panels to front when selected
          tags$script(HTML("$('.leaflet-info').on('mousedown', function() {
              $('.leaflet-info-front').removeClass('leaflet-info-front');
              $(this).addClass('leaflet-info-front');
            });"))
        )
      )
    )
  )
}


mod_cs5_server <- function(id, tab) {
  moduleServer(id, function(input, output, session) {
    waiter <- waiter::Waiter$new(
      id = session$ns("buildings"),
      html = tagList(waiter::spin_pulse(), h4("Loading figure...")),
      color = "rgba(179, 221, 254, 1)"
    )
    
    buildings <- reactive({
      sf::read_sf(app_sys("db/cs5spain.gpkg"), layer = "buildings")
    })
    
    output[["buildings-info-layer-desc"]] <- renderUI({
      layer <- input$`buildings-layer`
      p(txts$cs5$desc[[layer]])
    })
    
    labels <- NULL
    
    ## Parameters ----
    params <- reactive({
      req(identical(tab(), "cs5spain"))
      dt <- isolate(buildings())
      layer <- input$`buildings-layer`
      
      # Only create labels once and then save them to the server module for
      # re-use
      if (is.null(labels)) {
        lab_values <- dt[names(txts$cs5$dict$buildings)] %>%
          sf::st_drop_geometry() %>%
          as.list() %>%
          stats::setNames(sapply(txts$cs5$dict$buildings, "[[", "title")) %>%
          lapply(\(x) if (is.numeric(x)) round(x, 2) else x)
        lab_values$`Electricity demand` <- paste(lab_values$`Electricity demand`, "kWh/m\u00b2")
        lab_values$`Heating demand` <- paste(lab_values$`Heating demand`, "kWh/m\u00b2")
        lab_values$`Installed PV capacity` <- paste(lab_values$`Installed PV capacity`, "kW")
        labels <<- do.call(align_dl, lab_values)
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
          title = txts$cs5$dict$buildings[[params$layer]]$title,
          values = params$data[[params$layer]],
          labFormat = leaflet::labelFormat(
            suffix = txts$cs5$dict$buildings[[params$layer]]$lab
          )
        )
    })
    
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
          title = txts$cs5$dict$buildings[[params$layer]]$title,
          values = params$data[[params$layer]],
          labFormat = leaflet::labelFormat(
            suffix = txts$cs5$dict$buildings[[params$layer]]$lab
          )
        )
    })
    
    ## Basemap ----
    observe({
      basemap <- switch(input$`buildings-basemap`,
        "OpenStreetMap" = leaflet::providers$OpenStreetMap,
        "Satellite" = leaflet::providers$Esri.WorldImagery
      )
      
      leaflet::leafletProxy("buildings") %>%
        leaflet::clearTiles() %>%
        leaflet::addProviderTiles(basemap)
    })
  })
}

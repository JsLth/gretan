mod_cs1_ui <- function(id) {
  ns <- NS(id)
  
  get_text <- dispatch_to_txt(id)
  
  bs4Dash::tabItem(
    "cs1italy",
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
          title = with_literata(get_text("energy_model", "title")),
          width = 12,
          status = "primary",
          with_supref(get_text("energy_model", "content"))
        )
      ),
      bs4Dash::column(
        width = 6,
        # Box 2 ----
        bs4Dash::box(
          title = with_literata(get_text("case_study", "title")),
          width = 12,
          status = "primary",
          get_text("case_study", "content")
        )
      )
    ),
    fluidRow(
      bs4Dash::column(
        width = 12,
        # Buildings ----
        bs4Dash::tabBox(
          id = ns("map"),
          width = 12,
          title = with_literata("Case study maps"),
          status = "primary",
          type = "tabs",
          maximizable = TRUE,
          tabPanel(
            title = "Buildings",
            leaflet::leafletOutput(ns("buildings"), width = "100%", height = 800),
            ## Info panel ----
            leafletPanel(
              ns("buildings-info"),
              title = with_literata("Buildings in Pilastro-Roveri"),
              position = "topleft",
              p(get_text("buildings_info")),
              hr(),
              htmlOutput(ns("buildings-info-layer-desc")),
              hr(),
              shiny::helpText(with_gothic("Data source: UNIBO, Tecnalia"))
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
          ),
          # Fragility ----
          tabPanel(
            title = "Fragility",
            leaflet::leafletOutput(ns("fragility"), width = "100%", height = 800),
            ## Info panel ----
            leafletPanel(
              inputId = ns("fragility-info"),
              title = with_literata("Fragility in Pilastro-Roveri"),
              position = "topleft",
              with_gothic(get_text("fragility_info")),
              hr(),
              helpText(with_gothic("Data source: UNIBO, Tecnalia"))
            ),
            ## Control panel ----
            leafletPanel(
              inputId = ns("fragility-control"),
              title = with_literata("Map control"),
              position = "topright",
              top = 80,
              right = 10,
              width = 300,
              groupRadioButtons(
                shinyWidgets::prettyRadioButtons(
                  inputId = ns("fragility_layer"),
                  label = "",
                  choices = invert(lapply(
                    get_text("dict", "fragility"),
                    "[[",
                    "title"
                  )),
                  selected = "frag_compl"
                ),
                index = c(1, 16),
                groups = list(
                  tagList(
                    tags$b("Social indicators"),
                    br()
                  ),
                  tagList(
                    hr(),
                    tags$b("Fragility indices"),
                    br()
                  )
                ),
                type = "pretty"
              )
            )
          )
        )
      )
    )
  )
}


mod_cs1_server <- function(id, tab) {
  moduleServer(id, function(input, output, session) {
    get_text <- dispatch_to_txt(session$ns(NULL))
    
    # Waiter setup ----
    bwaiter <- waiter::Waiter$new(
      id = session$ns("buildings"),
      html = tagList(waiter::spin_pulse(), h4("Loading figure...")),
      color = "rgba(179, 221, 254, 1)"
    )
    fwaiter <- waiter::Waiter$new(
      id = session$ns("fragility"),
      html = tagList(waiter::spin_pulse(), h4("Loading figure...")),
      color = "rgba(179, 221, 254, 1)"
    )
    
    popover2(
      "biblink-1",
      title = "",
      content = "Tecnalia Innovation & Research (2019)<br>
        ENERKAD. More information: <a href='https://www.enerkad.net/'>
        https://www.enerkad.net/</a>",
      trigger = "click",
      placement = "top"
    )

    observe(
      bs4Dash::updateAccordion("fig", selected = 0)
    ) %>%
      bindEvent(input[["fig-link"]])

    # Data reading ----
    buildings <- reactive(
      sf::read_sf(app_sys("extdata/cs1italy.gpkg"), layer = "buildings")
    )

    fragility <- reactive(
      sf::read_sf(app_sys("extdata/cs1italy.gpkg"), layer = "fragility")
    )

    # Buildings ----
    output[["buildings-info-layer-desc"]] <- renderUI({
      layer <- input$`buildings-layer`
      p(get_text("desc", layer))
    })

    blabels <- NULL

    ## Parameters ----
    bparams <- reactive({
      req(identical(tab(), "cs1italy"))
      dt <- isolate(buildings())
      layer <- input$`buildings-layer`

      # Only create labels once and then save them to the server module for
      # re-use
      if (is.null(blabels)) {
        lab_values <- dt[names(get_text("dict", "buildings"))] %>%
          sf::st_drop_geometry() %>%
          as.list() %>%
          stats::setNames(sapply(get_text("dict", "buildings"), "[[", "title")) %>%
          lapply(\(x) if (is.numeric(x)) round(x, 2) else x)
        lab_values$`Electricity demand` <- paste(lab_values$`Electricity demand`, "kWh/m\u00b2")
        lab_values$`Heating demand` <- paste(lab_values$`Heating demand`, "kWh/m\u00b2")
        lab_values$`Installed PV capacity` <- paste(lab_values$`Installed PV capacity`, "kW")
        blabels <<- do.call(align_in_table, lab_values)
      }

      pal <- switch(layer,
        use = leaflet::colorFactor(
          palette = c(
            "#AA0DFE", "#3283FE", "#85660D", "#782AB6", "#565656", "#1C8356",
            "#16FF32", "#F7E1A0", "#E2E2E2", "#1CBE4F", "#C4451C", "#DEA0FD",
            "#FE00FA", "#325A9B", "#FEAF16", "#F8A19F", "#90AD1C"
          ),
          domain = dt$use
        ),
        year_constr = leaflet::colorBin(
          palette = "Greens",
          domain = dt$year_constr
        ),
        property = leaflet::colorFactor(
          palette = c("#ECECEC", "#e627c6", "#1E24CE", "#DB9A77"),
          domain = dt$property
        ),
        electricity_demand_m2 = leaflet::colorNumeric(
          palette = "Reds",
          domain = dt$electricity_demand_m2
        ),
        heating_demand_m2 = leaflet::colorNumeric(
          palette = "Blues",
          domain = dt$heating_demand_m2
        ),
        installed_pv_capacity_k_w = leaflet::colorNumeric(
          palette = "Purples",
          domain = dt$installed_pv_capacity_k_w
        )
      )

      list(data = dt, layer = layer, pal = pal, labels = blabels)
    })

    ## Render ----
    output$buildings <- leaflet::renderLeaflet({
      params <- isolate(bparams())

      leaflet::leaflet() %>%
        leaflet::setView(lng = 11.399926, lat = 44.507145, zoom = 15) %>%
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
    })

    ## Select layer ----
    updates <- 0

    observe({
      # Only show loading screen on first two updates
      # First one on startup
      # Second one when loading the tab item
      if (updates < 2) {
        bwaiter$show()
        updates <<- updates + 1
        on.exit(bwaiter$hide())
      }

      params <- bparams()

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
      basemap <- switch(input$`buildings-basemap`,
        "OpenStreetMap" = leaflet::providers$OpenStreetMap,
        "Satellite" = leaflet::providers$Esri.WorldImagery
      )

      leaflet::leafletProxy("buildings") %>%
        leaflet::clearTiles() %>%
        leaflet::addProviderTiles(basemap)
    })


    # Fragility ----

    ## Parameters ----
    fparams <- reactive({
      req(identical(tab(), "cs1italy"))
      dt <- isolate(fragility())
      layer <- input$fragility_layer

      lab_values <- dt[c("area_stati", "nomezona", layer)] %>%
        sf::st_drop_geometry() %>%
        as.list() %>%
        stats::setNames(c(
          "Area",
          "Zone",
          get_text("dict", "fragility", layer, "title")
        )) %>%
        {
          .[[3]] <- paste(
            round(.[[3]], 2),
            get_text("dict", "fragility", layer, "lab")
          )
          .
        }
      labels <- do.call(align_in_table, lab_values)

      pal <- leaflet::colorBin(
        palette = c("#000004FF", "#51127CFF", "#B63679FF", "#FB8861FF", "#FCFDBFFF"),
        domain = dt[[layer]],
        na.color = NA
      )

      list(data = dt, layer = layer, palette = pal, labels = labels)
    })

    ## Render ----
    output$fragility <- leaflet::renderLeaflet({
      params <- isolate(fparams())

      leaflet::leaflet() %>%
        leaflet::setView(lng = 11.399926, lat = 44.507145, zoom = 15) %>%
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) %>%
        leaflet::addPolygons(
          data = sf::st_transform(params$data, 4326),
          fillColor = stats::as.formula(paste0("~params$pal(", params$layer, ")")),
          fillOpacity = 0.7,
          color = "black",
          opacity = 1,
          weight = 1,
          label = params$labels,
          highlightOptions = leaflet::highlightOptions(
            weight = 2,
            stroke = TRUE,
            opacity = 1,
            bringToFront = TRUE,
            sendToBack = TRUE,
            fillOpacity = 1
          )
        ) %>%
        leaflet::addLegend(
          position = "bottomleft",
          pal = params$pal,
          title = get_text("dict", "fragility", params$layer, "title"),
          values = params$data[[params$layer]],
          labFormat = leaflet::labelFormat(
            suffix = get_text("dict", "fragility", params$layer, "lab")
          )
        )
    })

    ## Select layer ----
    fupdates <- 0

    observe({
      # Only show loading screen on first two updates
      # First one on startup
      # Second one when loading the tab item
      if (fupdates < 2) {
        fwaiter$show()
        fupdates <<- fupdates + 1
        on.exit(fwaiter$hide())
      }

      params <- fparams()

      leaflet::leafletProxy("fragility") %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() %>%
        leaflet::addPolygons(
          data = sf::st_transform(params$data, 4326),
          fillColor = stats::as.formula(paste0("~params$pal(", params$layer, ")")),
          fillOpacity = 0.7,
          color = "black",
          opacity = 1,
          weight = 1,
          label = params$labels,
          highlightOptions = leaflet::highlightOptions(
            weight = 2,
            stroke = TRUE,
            opacity = 1,
            bringToFront = TRUE,
            sendToBack = TRUE,
            fillOpacity = 1
          )
        ) %>%
        leaflet::addLegend(
          position = "bottomleft",
          na.label = "N/A",
          pal = params$palette,
          title = get_text("dict", "fragility", params$layer, "title"),
          values = params$data[[params$layer]],
          labFormat = leaflet::labelFormat(
            suffix = get_text("dict", "fragility", params$layer, "lab")
          )
        )
    })
  })
}

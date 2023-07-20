mod_cs1_ui <- function(id) {
  ns <- NS(id)
  
  bs4Dash::tabItem(
    "cs1italy",
    # Header ----
    make_header(
      title = "Case study 1: Reneweable energy district Pilastro-Roveri",
      authors = c("Prepared by: Author A", "Author B"),
      affil = list(
        "Author A" = "University of Bologna, Department of Architecture",
        "Author B" = "University of Bologna, Department of Architecture"
      ),
      date = "2023-mm-dd"
    ),
    fluidRow(
      bs4Dash::column(
        width = 6,
        # Box 1 ----
        bs4Dash::box(
          title = "Energy modelling",
          width = 12,
          status = "primary",
          txts$cs1$energy_model
        )
      ),
      bs4Dash::column(
        width = 6,
        # Box 2 ----
        bs4Dash::box(
          title = "Case study",
          width = 12,
          status = "primary",
          txts$cs1$case_study
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
              p(
                "This map presents the energy models for buildings in",
                "Pilastro-Roveri. Hover over individual buildings to learn more",
                "about their energy-related properties. Using the controls on",
                "the right side of the map you can also switch between layers"
              ),
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
                inputId = ns("buildings_basemap"),
                label = "Basemap",
                choices = c("OpenStreetMap", "Satellite"),
                selected = "OpenStreetMap"
              ),
              shinyWidgets::prettyRadioButtons(
                inputId = ns("buildings_layer"),
                label = "Select layer",
                choices = invert(lapply(txts$cs1$dict$buildings, "[[", "title")),
                selected = "use"
              )
            ),
            ## Description panel ----
            leafletPanel(
              inputId = ns("buildings-desc"),
              title = with_literata("Description"),
              position = "bottomright",
              width = 350,
              top = 420,
              right = 10,
              with_gothic(
                "This panel could be used as a means to showcase descriptions",
                "of individual buildings or groups of buildings. Specifically,",
                "the idea is to click on a building and then have a description",
                "about either the building, the estate, the neighborhood",
                "or similar pop up in this panel to give further context about",
                "the role of it within the case study area.",
                hr(),
                tags$b("Example", style = "font-size: 13px;"),
                "This building is located next to the Parco Pier Paolo Pasolini.",
                "In 2019, residents have gathered in the park to lead a public",
                "discussion about the decision of the municipality of Bologna",
                "to extend PV coverage in the Pilastro area."
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
              with_gothic(
                "This map depicts the fragility index in Pilastro-Roveri",
                "consisting of a demographic, social and economic dimension.",
                "On the right you can also select additional",
                "indicators to learn more about the socio-economic divide",
                "between the Pilastro and Roveri neighborhoods of Bologna."
              ),
              hr(),
              shiny::helpText(with_gothic("Data source: UNIBO, Tecnalia"))
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
                  choices = invert(lapply(txts$cs1$dict$fragility, "[[", "title")),
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
    
    # Data reading ----
    buildings <- reactive({
      sf::read_sf(app_sys("db/cs1italy.gpkg"), layer = "buildings")
    })
    
    fragility <- reactive({
      sf::read_sf(app_sys("db/cs1italy.gpkg"), layer = "fragility")
    })

    # Buildings ----
    output[["buildings-info-layer-desc"]] <- renderUI({
      layer <- input$buildings_layer
      tagList(
        p(tags$b("Selected layer:"), txts$cs1$dict$buildings[[layer]]$title),
        p(txts$cs1$desc[[layer]])
      )
    })
    
    blabels <- NULL

    ## Parameters ----
    bparams <- reactive({
      req(identical(tab(), "cs1italy"))
      dt <- isolate(buildings())
      layer <- input$buildings_layer

      # Only create labels once and then save them to the server module for
      # re-use
      if (is.null(blabels)) {
        lab_values <- dt[names(txts$cs1$dict$buildings)] %>%
          sf::st_drop_geometry() %>%
          as.list() %>%
          setNames(sapply(txts$cs1$dict$buildings, "[[", "title")) %>%
          lapply(\(x) if (is.numeric(x)) round(x, 2) else x)
        lab_values$`Electricity demand` <- paste(lab_values$`Electricity demand`, "kWh/m\u00b2")
        lab_values$`Heating demand` <- paste(lab_values$`Heating demand`, "kWh/m\u00b2")
        lab_values$`Installed PV capacity` <- paste(lab_values$`Installed PV capacity`, "kW")
        blabels <<- do.call(align_dl, lab_values)
      }

      pal <- switch(layer,
        use = leaflet::colorFactor(
          palette = c(
            "#AA0DFE", "#3283FE", "#85660D", "#782AB6", "#565656", "#1C8356", 
            "#16FF32", "#F7E1A0", "#E2E2E2", "#1CBE4F", "#C4451C", "#DEA0FD", 
            "#FE00FA", "#325A9B", "#FEAF16", "#F8A19F", "#90AD1C"),
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
          fillColor = as.formula(paste0("~params$pal(", params$layer, ")")),
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
          title = txts$cs1$dict$buildings[[params$layer]]$title,
          values = params$data[[params$layer]],
          labFormat = leaflet::labelFormat(
            suffix = txts$cs1$dict$buildings[[params$layer]]$lab
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
          fillColor = as.formula(paste0("~params$pal(", params$layer, ")")),
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
          title = txts$cs1$dict$buildings[[params$layer]]$title,
          values = params$data[[params$layer]],
          labFormat = leaflet::labelFormat(
            suffix = txts$cs1$dict$buildings[[params$layer]]$lab
          )
        )
    })
    
    ## Basemap ----
    observe({
      basemap <- switch(input$buildings_basemap,
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
        setNames(c("Area", "Zone", txts$cs1$dict$fragility[[layer]]$title)) %>%
        {
          .[[3]] <- paste(round(.[[3]], 2), txts$cs1$dict$fragility[[layer]]$lab)
          .
        }
      labels <- do.call(align_dl, lab_values)
      
      pal <- leaflet::colorBin(
        palette = viridis::viridis(5),
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
          fillColor = as.formula(paste0("~params$pal(", params$layer, ")")),
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
          title = txts$cs1$dict$fragility[[params$layer]]$title,
          values = params$data[[params$layer]],
          labFormat = leaflet::labelFormat(
            suffix = txts$cs1$dict$fragility[[params$layer]]$lab
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
            fillColor = as.formula(paste0("~params$pal(", params$layer, ")")),
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
            title = txts$cs1$dict$buildings[[params$layer]]$title,
            values = params$data[[params$layer]],
            labFormat = leaflet::labelFormat(
              suffix = txts$cs1$dict$buildings[[params$layer]]$lab
            )
          )
      })
  })
}

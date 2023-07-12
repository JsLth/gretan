mod_cs1_ui <- function(id) {
  ns <- NS(id)
  
  bs4Dash::tabItem(
    "cs1italy",
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
        bs4Dash::box(
          title = "Renewable energy district",
          width = 12,
          status = "primary",
          p2("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."),
          p2("Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi. Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat."),
          p2("Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat. Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi."),
          h2("Subtitle"),
          p2("Nam liber tempor cum soluta nobis eleifend option congue nihil imperdiet doming id quod mazim placerat facer possim assum. Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat. Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat."),
          p2("Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis."),
          p2("At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, At accusam aliquyam diam diam dolore dolores duo eirmod eos erat, et nonumy sed tempor et et invidunt justo labore Stet clita ea et gubergren, kasd magna no rebum. sanctus sea sed takimata ut vero voluptua. est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur")
        ),
        bs4Dash::box(
          title = "Text with a figure",
          width = 12,
          status = "primary",
          p2(shinipsum::random_text(nwords = 150)),
          img(
            src = "https://projectgreta.eu/wp-content/uploads/2021/08/GRETA_3_KUVITUS_Valkotausta-1024x724.png",
            style = "width: 100%;"
          ),
          p(HTML("<b>Fig. 1:</b> An example figure"))
        )
      ),
      bs4Dash::column(
        width = 6,
        bs4Dash::box(
          title = "Another text",
          width = 12,
          status = "primary",
          p2(shinipsum::random_text(nwords = 700))
        )
      )
    ),
    fluidRow(
      bs4Dash::column(
        width = 12,
        bs4Dash::tabBox(
          id = ns("map"),
          width = 12,
          status = "primary",
          type = "tabs",
          maximizable = TRUE,
          tabPanel(
            title = "Buildings",
            leaflet::leafletOutput(ns("buildings"), width = "100%", height = 800),
            leafletPanel(
              ns("buildings-info"),
              title = with_literata("Buildings in Pilastro-Roveri"),
              position = "topleft",
              with_gothic(
                "This map is an overview of the buildings in the Pilastro-Roveri",
                "area of Bologna. Hover over individual buildings to learn more",
                "about their energy-related properties. Using the controls on",
                "right side of the map you can also change the basemap and select",
                "an energy-related layer."
              ),
              hr(),
              shiny::helpText(with_gothic("Data source: UNIBO, Tecnalia"))
            ),
            leafletPanel(
              inputId = ns("buildings-control"),
              title = with_literata("Map control"),
              position = "topright",
              width = 200,
              shinyWidgets::awesomeRadio(
                inputId = ns("basemap"),
                label = "Basemap",
                choices = c("OpenStreetMap", "Satellite"),
                selected = "OpenStreetMap"
              ),
              shinyWidgets::awesomeRadio(
                inputId = ns("blayer"),
                label = "Select layer",
                choices = c(
                  "Use" = "use",
                  "Construction year" = "year_constr",
                  "Property" = "property",
                  "Electr. demand" = "electricity_demand_m2",
                  "Heating demand" = "heating_demand_m2",
                  "PV capacity" = "installed_pv_capacity_k_w"
                ),
                selected = "use"
              )
            )
          ),
          tabPanel(
            title = "Fragility",
            leaflet::leafletOutput(ns("fragility"), width = "100%", height = 800)
          )
        )
      )
    )
  )
}


mod_cs1_server <- function(id, tab) {
  moduleServer(id, function(input, output, session) {
    dict <- list(
      use = list(title = "Use", lab = ""),
      year_constr = list(title = "Construction year", lab = ""),
      property = list(title = "Property", lab = ""),
      electricity_demand_m2 = list(title = "Electricity demand", lab = " kWh/m\u00b2"),
      heating_demand_m2 = list(title = "Heating demand", lab = " kWh/m\u00b2"),
      installed_pv_capacity_k_w = list(title = "Installed PV capacity", lab = " kW")
    )
    
    w_cs1 <- waiter::Waiter$new(
      id = session$ns("buildings"),
      html = tagList(waiter::spin_pulse(), h4("Loading figure...")),
      color = "rgba(179, 221, 254, 0.8)"
    )
    buildings <- reactive({
      if (identical(tab(), "cs1italy"))
        sf::read_sf(app_sys("db/cs1italy.gpkg"), layer = "buildings")
      
    })
    
    fragility <- reactive({
      if (identical(tab(), "cs1italy")) {
        sf::read_sf(app_sys("db/cs1italy.gpkg"), layer = "fragility")
      }
    })

    blabels <- reactiveVal()
    
    bparams <- reactive({
      dt <- isolate(buildings())
      labels <- isolate(blabels())
      layer <- input$blayer

      if (is.null(blabels())) {
        lab_values <- as.list(sf::st_drop_geometry(dt[c(
          "use", "year_constr", "property", "electricity_demand_m2",
          "heating_demand", "installed_pv_capacity_k_w"
        )])) %>%
          setNames(sapply(dict, "[[", "title")) %>%
          lapply(\(x) if (is.numeric(x)) round(x, 2) else x)
        lab_values$`Electricity demand` <- paste(lab_values$`Electricity demand`, "kWh/m\u00b2")
        lab_values$`Heating demand` <- paste(lab_values$`Heating demand`, "kWh/m\u00b2")
        lab_values$`Installed PV capacity` <- paste(lab_values$`Installed PV capacity`, "kW")
        blabels(do.call(align_dl, lab_values))
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
      
      list(data = dt, layer = layer, palette = pal, labels = isolate(blabels()))
    })
    
    output$buildings <- leaflet::renderLeaflet({
      w_cs1$show()
      
      params <- bparams()
      
      m <- leaflet::leaflet() %>%
        leaflet::setView(lng = 11.399926, lat = 44.507145, zoom = 15) %>%
        leaflet::addTiles() %>%
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
          pal = params$palette,
          title = dict[[params$layer]]$title,
          values = params$data[[params$layer]],
          labFormat = leaflet::labelFormat(suffix = dict[[params$layer]]$lab)
        )
      w_cs1$hide()
      m
    })
  })
}

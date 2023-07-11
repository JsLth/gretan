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
          title = "Case study map",
          width = 12,
          status = "primary",
          class = "tight-map-box",
          leaflet::leafletOutput(ns("map"), width = "100%", height = 450)
        ),
        bs4Dash::box(
          title = "",
          width = 12,
          status = "primary",
          solidHeader = FALSE,
          headerBorder = FALSE,
          collapsible = FALSE,
          uiOutput(ns("poi"), style = "padding-bottom: -10px; padding-top: -20px;")
        ),
        bs4Dash::box(
          title = "Another text",
          width = 12,
          status = "primary",
          p2(shinipsum::random_text(nwords = 700))
        )
      )
    )
  )
}


mod_cs1_server <- function(id, tab) {
  moduleServer(id, function(input, output, session) {
    w_cs1 <- waiter::Waiter$new(
      id = session$ns("map"),
      html = tagList(waiter::spin_pulse(), h4("Loading figure...")),
      color = "rgba(179, 221, 254, 0.8)"
    )
    cs1_data <- reactive({
      if (identical(tab(), "cs1italy"))
        list(
          buildings = sf::read_sf(app_sys("db/cs1italy.gpkg"), layer = "buildings"),
          quarters = sf::read_sf(app_sys("db/cs1italy.gpkg"), layer = "quarters"),
          zones = sf::read_sf(app_sys("db/cs1italy.gpkg"), layer = "zones"),
          areas = sf::read_sf(app_sys("db/cs1italy.gpkg"), layer = "areas")
        )
      
    })
    
    output$map <- leaflet::renderLeaflet({
      w_cs1$show()
      
      dt <- cs1_data()
      dt$buildings$property <- ifelse(
        is.na(dt$buildings$property),
        "Private",
        dt$buildings$property
      )
      pal_use <- leaflet::colorFactor(
        palette = c(
          "#AA0DFE", "#3283FE", "#85660D", "#782AB6", "#565656", "#1C8356", 
          "#16FF32", "#F7E1A0", "#E2E2E2", "#1CBE4F", "#C4451C", "#DEA0FD", 
          "#FE00FA", "#325A9B", "#FEAF16", "#F8A19F", "#90AD1C"),
        domain = dt$buildings$use
      )
      pal_year <- leaflet::colorBin(
        palette = "RdYlBu",
        domain = dt$buildings$year_constr
      )
      pal_property <- leaflet::colorFactor(
        palette = c("#ECECEC", "#e627c6", "#1E24CE", "#DB9A77"),
        domain = dt$buildings$property
      )
      pal_elec <- leaflet::colorNumeric(
        palette = c("#FCFCFC", "#99EB1D", "#FFF315", "#FC7D14", "#E01114"),
        domain = dt$buildings$electricity_demand_m2
      )
      pal_heat <- leaflet::colorNumeric(
        palette = c("#FCFCFC", "#99EB1D", "#FFF315", "#FC7D14", "#E01114"),
        domain = dt$buildings$heating_demand
      )
      pal_pv <- leaflet::colorNumeric(
        palette = "RdYlBu",
        domain = dt$buildings$installed_pv_capacity_k_w
      )
      
      lab_values <- as.list(sf::st_drop_geometry(dt$buildings[c(
        "use", "year_constr", "property", "electricity_demand_m2",
        "heating_demand", "installed_pv_capacity_k_w"
      )])) %>%
        setNames(c(
          "Use", "Construction year", "Property", "Electricity demand",
          "Heating demand", "Installed PV capacity"
        )) %>%
        lapply(\(x) if (is.numeric(x)) round(x, 2))
      lab_values$electricity_demand_kw <- paste(lab_values$electricity_demand_kw, "kWh/m\u00b2")
      lab_values$heating_demand_kw <- paste(lab_values$heating_demand_kw, "kWh/m\u00b2")
      lab_values$installed_pv_capacity_k_w <- paste(lab_values$installed_pv_capacity_k_w, "kW")
      labels <- do.call(align_dl, lab_values)

      m <- leaflet::leaflet() %>%
        leaflet::setView(lng = 11.399926, lat = 44.507145, zoom = 13) %>%
        leaflet::addProviderTiles("HERE.satelliteDay", group = "Satellite") %>%
        leaflet::addProviderTiles("CartoDB.PositronOnlyLabels", group = "Satellite") %>%
        leaflet::addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
        leaflet::addPolygons(
          data = sf::st_transform(dt$buildings, 4326),
          fillColor = ~pal_use(use),
          fillOpacity = 1,
          color = "black",
          opacity = 1,
          weight = 1,
          highlightOptions = highlight_opts,
          label = labels,
          group = "Use"
        ) %>%
        leaflet::addLegend(
          position = "bottomleft",
          pal = pal_use,
          title = "Use",
          values = dt$buildings$use,
          group = "Use"
        ) %>%
        leaflet::addPolygons(
          data = sf::st_transform(dt$buildings, 4326),
          fillColor = ~pal_year(year_constr),
          fillOpacity = 1,
          color = "black",
          opacity = 1,
          weight = 1,
          highlightOptions = highlight_opts,
          label = labels,
          group = "Construction year"
        ) %>%
        leaflet::addLegend(
          position = "bottomleft",
          pal = pal_year,
          values = dt$buildings$year_constr,
          title = "Construction year",
          group = "Construction year"
        ) %>%
        leaflet::addPolygons(
          data = sf::st_transform(dt$buildings, 4326),
          fillColor = ~pal_property(property),
          fillOpacity = 1,
          color = "black",
          opacity = 1,
          weight = 1,
          highlightOptions = highlight_opts,
          label = labels,
          group = "Property"
        ) %>%
        leaflet::addLegend(
          position = "bottomleft",
          pal = pal_property,
          values = dt$buildings$property,
          title = "Property",
          group = "Property"
        ) %>%
        leaflet::addPolygons(
          data = sf::st_transform(dt$buildings, 4326),
          fillColor = ~pal_elec(electricity_demand_m2),
          fillOpacity = 1,
          color = "black",
          opacity = 1,
          weight = 1,
          highlightOptions = highlight_opts,
          label = labels,
          group = "Electricity demand"
        ) %>%
        leaflet::addLegend(
          position = "bottomleft",
          pal = pal_elec,
          values = dt$buildings$electricity_demand_m2,
          labFormat = leaflet::labelFormat(suffix = "kWh / m\u00b2"),
          title = "Electricity demand",
          group = "Electricity demand"
        ) %>%
        leaflet::addPolygons(
          data = sf::st_transform(dt$buildings, 4326),
          fillColor = ~pal_heat(heating_demand_m2),
          fillOpacity = 1,
          color = "black",
          opacity = 1,
          weight = 1,
          highlightOptions = highlight_opts,
          label = labels,
          group = "Heating demand"
        ) %>%
        leaflet::addLegend(
          position = "bottomleft",
          pal = pal_heat,
          values = dt$buildings$heating_demand_m2,
          labFormat = leaflet::labelFormat(suffix = "kWh / m\u00b2"),
          title = "Heating demand",
          group = "Heating demand"
        ) %>%
        leaflet::addPolygons(
          data = sf::st_transform(dt$buildings, 4326),
          fillColor = ~pal_pv(installed_pv_capacity_k_w),
          fillOpacity = 1,
          color = "black",
          opacity = 1,
          weight = 1,
          highlightOptions = highlight_opts,
          label = labels,
          group = "PV capacity"
        ) %>%
        leaflet::addLegend(
          position = "bottomleft",
          pal = pal_pv,
          values = dt$buildings$installed_pv_capacity_k_w,
          labFormat = leaflet::labelFormat(suffix = "kW"),
          title = "Installed PV capacity",
          group = "PV capacity"
        ) %>%
        leaflet::addLayersControl(
          baseGroups = c("OpenStreetMap", "Satellite", "<hr>", "Use", "Construction year", "Property",
                         "Electricity demand", "Heating demand",
                         "PV capacity"),
          options = leaflet::layersControlOptions(collapsed = FALSE, sortLayers = FALSE)
        ) %>%
        leaflet::hideGroup(c("Construction year", "Property",
                             "Electricity demand", "Heating demand",
                             "PV capacity"))
      
      w_cs1$hide()
      m
    })
  })
}

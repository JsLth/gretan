mod_home_ui <- function(id) {
  # UI setup ----
  ns <- NS(id)
  get_text <- dispatch_to_txt(id)
  
  bs4Dash::tabItem(
    "home",
    fluidRow(
      bs4Dash::column(
        width = 6,
        # Welcome box ----
        bs4Dash::box(
          title = with_literata(get_text("welcome", "title")),
          width = 12,
          status = "primary",
          get_text("welcome", "content"),
          get_text("disclaimer")
        ),
        # About box ----
        bs4Dash::box(
          title = with_literata(get_text("about", "title")),
          width = 12,
          status = "primary",
          get_text("about", "content"),
          fluidRow(
            style = "margin-top: 5%;",
            col_6(
              div(
                style = style(
                  margin = "auto",
                  width = "70%",
                  `padding-top` = "15%"
                ),
                tags$img(src = "www/greta_logo.svg")
              )
            ),
            col_6(get_text("contact")),
            fluidRow(
              style = "margin-top: 5%;",
              col_12(
                div(
                  style = "display: flex",
                  tags$img(
                    src = "www/eu_flag.jpg",
                    width = "10%",
                    height = "10%",
                    style = "float: left;"
                  ),
                  tags$p(
                    get_text("funding"),
                    style = style(
                      float = "right",
                      `margin-left` = "15px"
                    )
                  )
                )
              )
            )
          )
        )
      ),
      bs4Dash::column(
        width = 6,
        # Geographical overview ----
        bs4Dash::box(
          title = with_literata("Geographical overview"),
          width = 12,
          status = "primary",
          class = "tight-map-box",
          leaflet::leafletOutput(ns("map"), width = "100%", height = 450)
        ),
        # Case study descriptions ----
        bs4Dash::box(
          title = with_literata("Case study descriptions"),
          width = 12,
          status = "primary",
          uiOutput(ns("desc"))
        )
      )
    )
  )
}


mod_home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server setup ----
    get_text <- dispatch_to_txt(session$ns(NULL))
    
    cs_coords <- sf::st_sf(
      name = setdiff(names(get_text("csdesc")), "none"),
      geometry = sf::st_sfc(
        sf::st_point(c(11.399926, 44.507145)),
        sf::st_point(c(-9.136693, 38.710479)),
        sf::st_point(c(8.651177, 49.872775)),
        sf::st_point(c(5.6343227, 52.2434979)),
        sf::st_point(c(-1.994286, 43.300075)),
        crs = 4326
      )
    )
    
    # Render geographical overview ----
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(cs_coords) %>%
        leaflet::addTiles() %>%
        leaflet::setView(lng = 9, lat = 55, zoom = 3) %>%
        leaflet::addMarkers(
          icon = leaflet::makeIcon(
            "https://www.svgrepo.com/download/352253/map-pin.svg",
            iconWidth = 25,
            iconHeight = 25,
            iconAnchorY = 25,
            iconAnchorX = 12.5
          )
        ) %>%
        leaflet::addPolylines(
          data = sf::st_transform(srv_nuts0, 4326),
          fillOpacity = 0,
          weight = 1,
          color = "red"
        ) %>%
        addLegendLine(
          label = "Surveyed countries",
          position = "bottomleft",
          width = 10,
          height = 10,
          labelStyle = "font-size: 12px; vertical-align: middle;"
        )
    })
    
    
    # Select case study ----
    observe({
      click <- input$map_marker_click
      target <- leaflet_select(
        id = "map",
        geom = cs_coords,
        action = click
      )
      
      if (!is.null(target)) {
        cc <- cs_coords[!cs_coords$name %in% target$name, ]
        leaflet::leafletProxy("map") %>%
          leaflet::clearMarkers() %>%
          leaflet::addMarkers(
            data = cc,
            icon = leaflet::makeIcon(
              "https://www.svgrepo.com/download/352253/map-pin.svg",
              iconWidth = 25,
              iconHeight = 25,
              iconAnchorY = 25,
              iconAnchorX = 12.5
            )
          ) %>%
          leaflet::addMarkers(
            data = target,
            icon = leaflet::makeIcon(
              "https://www.svgrepo.com/download/352253/map-pin.svg",
              iconWidth = 37.5,
              iconHeight = 37.5,
              iconAnchorY = 37.5,
              iconAnchorX = 18.75
            )
          )
      }
    }) %>%
      bindEvent(input$map_marker_click)
    
    
    # Show selected case study ----
    output$desc <- renderUI({
      click <- input$map_marker_click
      
      leaflet_text_on_click(
        id = "map",
        geom = cs_coords,
        texts = get_text("csdesc"),
        click = click
      )
    })
  })
}

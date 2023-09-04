mod_home_ui <- function(id) {
  ns <- NS(id)
  
  bs4Dash::tabItem(
    "home",
    fluidRow(
      bs4Dash::column(
        width = 6,
        bs4Dash::box(
          title = with_literata("Welcome to GRETA Analytics!"),
          width = 12,
          status = "primary",
          txts$home$welcome
        ),
        bs4Dash::box(
          title = with_literata("About GRETA"),
          width = 12,
          status = "primary",
          txts$home$about
        )
      ),
      bs4Dash::column(
        width = 6,
        bs4Dash::box(
          title = with_literata("Geographical overview"),
          width = 12,
          status = "primary",
          class = "tight-map-box",
          leaflet::leafletOutput(ns("map"), width = "100%", height = 450)
        ),
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


mod_home <- function(input, output, session) {
  cs_coords <- sf::st_sf(
    name = c("Italy", "Portugal", "Germany", "The Netherlands", "Spain"),
    geometry = sf::st_sfc(
      sf::st_point(c(11.399926, 44.507145)),
      sf::st_point(c(-9.136693, 38.710479)),
      sf::st_point(c(8.651177, 49.872775)),
      sf::st_point(c(5.6343227, 52.2434979)),
      sf::st_point(c(-1.994286, 43.300075)),
      crs = 4326
    )
  )

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
      leaflegend::addLegendImage(
        images = leaflegend::makeSymbol("line", width = 7, color = "red"),
        labels = "Surveyed countries",
        orientation = "vertical",
        position = "bottomleft",
        width = 10,
        height = 10,
        labelStyle = "font-size: 12px; vertical-align: middle;"
      )
  })
  
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
  
  # Show case study description based on map clicks
  output$desc <- renderUI({
    click <- input$map_marker_click

    leaflet_text_on_click(
      id = "map",
      geom = cs_coords,
      texts = txts$home$csdesc,
      click = click
    )
  })
}


mod_home_server <- function(id) {
  moduleServer(id, mod_home)
}

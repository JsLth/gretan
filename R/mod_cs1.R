mod_cs1_ui <- function(id) {
  ns <- NS(id)
  
  bs4Dash::tabItem(
    "cs1",
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
          fluidRow(
            bs4Dash::column(
              width = 3.5,
              shinyWidgets::pickerInput(
                ns("bounds"),
                label = "Boundaries",
                choices = c("Quarters", "Zones", "Statistical areas"),
                shinyWidgets::pickerOptions(windowPadding = c(0, 0, 1000, 0)),
                width = 200
              )
            ),
            tags$style("padding-left: 10px"),
            bs4Dash::column(
              width = 4,
              div(
                shinyWidgets::prettyRadioButtons(
                  ns("basemap"),
                  label = "Base map",
                  choices = c("OpenStreetMap", "Satellite"),
                  selected = "OpenStreetMap",
                  status = "default",
                  animation = "smooth",
                  shape = "curve"
                ), style = "padding-left: 35px"
              )
            ),
            bs4Dash::column(
              width = 4,
              sliderInput(
                ns("opacity"),
                label = "Opacity",
                min = 0,
                max = 1,
                step = 0.1,
                value = 0.2,
                ticks = FALSE
              )
            )
          ),
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


mod_cs1_server <- function(input, output, session) {
  ns <- session$ns
  
  output$map <- leaflet::renderLeaflet({
    cs_bounds <- switch(
      input$bounds,
      "Quarters" = bgn_1,
      "Zones" = bgn_2,
      "Statistical areas" = bgn_3
    )
    
    cs_bounds <- sf::st_geometry(cs_bounds)
    m <- leaflet::leaflet(cs_bounds) %>%
      leaflet::setView(lng = 11.399926, lat = 44.507145, zoom = 13) %>%
      leaflet::addPolygons(
        color = "black",
        weight = 2,
        opacity = 1,
        fillOpacity = input$opacity
      ) %>%
      leaflet::addMarkers(lng = 11.399926, lat = 44.507145)
    
    if (identical(input$basemap, "Satellite")) {
      m <- leaflet::addProviderTiles(m, "Esri.WorldImagery")
    } else {
      m <- leaflet::addTiles(m)
    }
  })
  
  output$poi <- leaflet_text_on_click(
    id = "map",
    ref = cs1coords,
    texts = txts$cs1poi
  )
}
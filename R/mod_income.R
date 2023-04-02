mod_income_ui <- function(id) {
  ns <- NS(id)
  
  bs4Dash::tabItem(
    "income",
    make_header(
      title = "Income stability: a trivial example to illustrate what a page can look like",
      authors = c("Jonas Lieth", "Dennis Abel", "Stefan J\u00fcnger"),
      affil = "GESIS - Leibniz Institute for the Social Sciences",
      date = "06-03-2023"
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
    ), ## end first row
    fluidRow(
      col_12(
        bs4Dash::box(
          width = 12,
          status = "primary",
          title = "Spatial distribution of income stability",
          leaflet::leafletOutput(ns("map"), height = 700)
        )
      )
    ),
    fluidRow(
      col_6(bs4Dash::box(
        plotly::plotlyOutput(ns("density")),
        width = 12,
        status = "primary",
        title = "Distribution of income stability"
      )),
      col_6(bs4Dash::box(
        plotly::plotlyOutput(ns("scatter")),
        width = 12,
        status = "primary",
        title = "Relationship between age and income stability"
      ))
    ),
    fluidRow(
      col_6(
        bs4Dash::box(
          width = 12,
          title = "Discussion",
          status = "primary",
          p2(shinipsum::random_text(nwords = 200))
        )
      ),
      col_6(
        bs4Dash::box(
          width = 12,
          title = "Conclusion",
          status = "primary",
          p2(shinipsum::random_text(nwords = 150))
        )
      )
    ),
    bs4Dash::box(
      title = "References",
      width = 12,
      status = "primary",
      dummy_bibliography()
    )
  )
}


mod_income_server <- function(input, output, session) {
  ns <- session$ns
  
  output$map <- leaflet::renderLeaflet({
    sf <- srv_nuts2
    pal <- viridis::viridis_pal(option = "D")(5)
    pal <- leaflet::colorNumeric(pal, NULL, n = 5)
    sf[["c53"]] <- round(sf[["c53"]], 2)
    leaflet::leaflet(sf::st_transform(sf["c53"], 4326)) %>%
      leaflet::setView(lng = 9, lat = 55, zoom = 4) %>%
      leaflet::addPolygons(
        fillColor = ~pal(c53),
        fillOpacity = 0.7,
        weight = 1,
        color = "black",
        opacity = 0.5,
        popup = paste0(
          "Share", ": ",
          round(sf[["c53"]], 2), " %"
        )
      ) %>%
      leaflet::addLegend(
        position = "bottomright",
        na.label = "No data",
        pal = pal,
        values = ~c53,
        opacity = 0.9,
        title = "Share",
        labFormat = leaflet::labelFormat(suffix = " %")
      ) %>%
      leaflet::addTiles()
  })
  
  output$density <- plotly::renderPlotly({
    df <- sf::st_drop_geometry(srv_nuts2[c("c1", "c53")])
    names(df) <- c("Age", "Stable income")
    
    p <- ggplot2::ggplot(df, ggplot2::aes(x = `Stable income`)) +
      ggplot2::geom_density(na.rm = TRUE) +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(expand = c(0, 0))
    
    if (isTRUE(input$tempref)) {
      p <- p +
        ggplot2::geom_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "green")
    }
    
    plotly::config(plotly::ggplotly(p), displayModeBar = FALSE)
  })
  
  output$scatter <- plotly::renderPlotly({
    df <- sf::st_drop_geometry(srv_nuts2[c("c1", "c53")])
    names(df) <- c("Age", "Stable income")
    
    p <- ggplot2::ggplot(df, ggplot2::aes(x = `Stable income`, y = Age)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm", na.rm = TRUE, formula = y ~ x) +
      ggplot2::theme_bw()
    
    plotly::config(plotly::ggplotly(p), displayModeBar = FALSE)
  })
}
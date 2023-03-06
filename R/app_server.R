cs_coords <- sf::st_sf(
  cs = c("Italy", "Portugal", "Germany", "The Netherlands", "Spain"),
  geometry = sf::st_sfc(
    sf::st_point(c(11.399926, 44.507145)),
    sf::st_point(c(-9.136693, 38.710479)),
    sf::st_point(c(8.651177, 49.872775)),
    sf::st_point(c(5.6343227, 52.2434979)),
    sf::st_point(c(-1.994286, 43.300075)),
    crs = 4326
  )
)

srv_nuts0 <- readRDS("data/srv_nuts0.rds")
srv_nuts1 <- readRDS("data/srv_nuts1.rds")
srv_nuts2 <- readRDS("data/srv_nuts1.rds")
bgn_1 <- readRDS("data/bgn_1.rds")
bgn_2 <- readRDS("data/bgn_2.rds")
bgn_3 <- readRDS("data/bgn_3.rds")
don_1 <- readRDS("data/don_1.rds")
don_2 <- readRDS("data/don_2.rds")
don_3 <- readRDS("data/don_3.rds")

server <- function(input, output, session) {
  # Home ----
  # Plot geographical overview map
  output$csmaps <- leaflet::renderLeaflet({
    leaflet::leaflet(cs_coords) %>%
      leaflet::addTiles() %>%
      leaflet::setView(lng = 9, lat = 55, zoom = 3) %>%
      leaflet::addMarkers(
        icon = leaflet::makeIcon(
          "https://www.svgrepo.com/download/352253/map-pin.svg",
          iconWidth = 25,
          iconHeight = 25
        )
      ) %>%
      leaflet::addPolylines(
        data = sf::st_transform(nuts0, 4326),
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
  
  # Show case study description based on map clicks
  output$csdesc <- renderUI({
    click <- input$csmaps_marker_click
    target <- NULL
    if (!is.null(click)) {
      marker <- sf::st_sfc(sf::st_point(c(click$lng, click$lat)), crs = 4326)
      target <- cs_coords[sf::st_is_within_distance(
        cs_coords$geometry,
        marker,
        dist = 1,
        sparse = FALSE
      ), ]$cs
    }
    
    if (identical(target, "Italy")) {
      HTML(paste(
        h2("Renewable energy district Pilastro-Roveri"),
        p(HTML(paste(
          "The case study takes place in the Pilastro-Roveri district in",
          "the northeast of Bologna, where a financed Green Energy",
          "Community project (GECO) has been running since 2019 to",
          "support communities in the process of designing and creating",
          "a green energy community. Pilastro-Roveri is a",
          "socio-economically stratified district composed of two",
          "areas:"
        )), style = "margin-bottom: 0.5cm;"),
        p(HTML(paste(
          "<b>Pilastro “Rione”</b> (neighbourhood): a residential neighborhood",
          "with a long history of activism but also of socio-economic issues."
        )), style = "margin-bottom: 0.5cm;"),
        p(HTML(paste(
          "<b>Roveri</b> area: distinctly separated from Pilastro by a former",
          "railway terminal, is an industrial and productive neighborhood",
          "hosting a variety of companies in sectors such as packaging,",
          "mechanics, and electric vehicles. The area also hosts the",
          "Agriculture and Food Center of Bologna’s (CAAB) food and",
          "agriculture theme park (FICO) with its industrial partners, which",
          "has the largest solar power plant on industrial roofs within the EU."
        )), style = "margin-bottom: 0.5cm;")
      ))
    } else if (identical(target, "Portugal")) {
      HTML(paste(
        h2("Coopérnico – renewable energy-driven cooperative"),
        p(paste(
          "The case study examines Coopérnico, Portugal’s first renewable",
          "energy cooperative, founded in 2013. Coopérnico has more than",
          "1,700 members, including citizens, small and medium-sized",
          "enterprises, and municipalities all over Portugal. Its mission is",
          "to involve its members in reshaping the energy sector to be more",
          "renewable, socially just and collaborative."
        ))
      ))
    } else if (identical(target, "Germany")) {
      HTML(paste(
        h2("The Earnest App – a virtual community for sustainable mobility in Darmstadt"),
        p(paste(
          "The case study explores how the regular use of a sustainability",
          "app can foster energy citizenship among members of a virtual",
          "community. The case study is conducted in Darmstadt – a city with",
          "160.000 inhabitants located in the state of Hesse in Germany. In",
          "cooperation with students from the University for Applied Science",
          "in Darmstadt (h_da), the case study explores how a virtual energy",
          "community – connected by the shared experience of using an app –",
          "affects citizens’ awareness and behaviour in regard to their",
          "mobility and energy consumption choices in everyday life."
        ))
      ))
    } else if (identical(target, "The Netherlands")) {
      HTML(paste(
        h2("Natural gas-free neighbourhoods"),
        p(paste(
          "Most Dutch households currently use natural gas to, for example,",
          "heat their homes. The gas has been mainly produced in the Groningen",
          "gas field, in the northeastern part of the Netherlands. However,",
          "the exploitation of Groningen has caused increasing earthquakes and",
          "damage to the city and nearby areas since the late 1980s. Because",
          "of this, the Netherlands has decided that all its neighbourhoods",
          "will become natural gas-free by 2050. The case study examines the",
          "transition towards natural gas-free homes in the Netherlands."
        ))
      ))
    } else if (identical(target, "Spain")) {
      HTML(paste(
        h2("UR BEROA – energy efficiency-driven cooperative"),
        p(paste(
          "The case study examines UR BEROA, a cooperative providing energy to",
          "the Bera Bera neighbourhood in San Sebastian, Spain. The",
          "cooperative was founded in 1985 to provide hot water and community",
          "heating to the residents and improve the energy efficiency of the",
          "neighbourhood. Since its establishment, the cooperative has",
          "successfully introduced cleaner energy sources and ways to measure",
          "the energy consumption of each household. Now, the cooperative is",
          "slowly making its way toward decarbonisation."
        ))
      ))
    } else {
      HTML(paste(
        p("Click on a map marker to learn more about the GRETA case studies.")
      ))
    }
  })
  
  
  
  # Data explorer ----
  # Show question
  output$question <- renderUI({
    if (!is.null(input$exp_title)) {
      indat <- cb_ext[cb_ext$title %in% input$exp_title, ]
      
      if (!all(is.na(indat$subitem))) {
        indat <- indat[indat$subitem %in% input$exp_subitem, ]
      }
      
      if (!all(is.na(indat$option))) {
        indat <- indat[indat$option %in% input$exp_option, ]
      }
      
      HTML(sprintf(
        "<b>Question %s:</b><br>%s",
        toupper(indat$og_var),
        indat$label
      ))
    } else {
      ""
    }
  })
  
  # Hide or show selectors for subitems or options depending on the question
  observeEvent(input$exp_title, {
    invar <- cb_ext[cb_ext$title %in% input$exp_title, ]$variable
    items <- unique(cb_ext[cb_ext$variable %in% invar, ]$subitem)
    options <- unique(cb_ext[cb_ext$variable %in% invar, ]$option)
    show_subitems <- length(invar) > 1 & !all(is.na(items))
    show_options <- length(invar) > 1 & !all(is.na(options))
    
    if (show_subitems) {
      shinyWidgets::updatePickerInput(
        session,
        inputId = "exp_subitem",
        choices = items
      )
      shinyjs::show("subitem_hide", anim = TRUE)
    } else {
      shinyjs::hide("subitem_hide", anim = TRUE)
    }
    
    if (show_options) {
      shinyWidgets::updatePickerInput(
        session,
        inputId = "exp_option",
        choices = options
      )
      shinyjs::show("option_hide", anim = TRUE)
    } else {
      shinyjs::hide("option_hide", anim = TRUE)
    }
  })
  
  # TODO: Remove reactivity and add a refresh button
  
  invar <- observeEvent(input$exp_refresh, {
    
  })
  
  output$explorer <- leaflet::renderLeaflet({
    poly <- switch(
      input$scale,
      "NUTS-0" = survey_nuts0,
      "NUTS-1" = survey_nuts1,
      "NUTS-2" = survey_nuts2
    )
    
    if (input$pal %in% all_pals[["Colorblind palettes"]]) {
      pal <- viridis::viridis_pal(option = tolower(input$pal))(5)
    } else {
      pal <- input$pal
    }
    pal <- leaflet::colorNumeric(pal, NULL, n = 5)
    
    has_title <- cb_ext$title %in% input$exp_title
    invar <- cb_ext[has_title, ]
    
    # case: multiple items exist, look for subitems
    if (length(invar$variable) > 1) {
      has_subitem <- invar$subitem %in% input$exp_subitem
      
      # only select subitems if any exist
      if (any(has_subitem)) {
        invar <- invar[has_subitem, ]
      }
    }
    
    # case: there's still multiple items, look for options
    if (length(invar$variable) > 1 || !length(invar$variable)) {
      has_option <- invar$option %in% input$exp_option
      
      # if neither option nor subitem exist, select first row to prevent errors
      if (all(!has_option)) {
        has_option <- 1
      }
      invar <- invar[has_option, ]
    }
    
    invar <- invar$variable
    
    is_metric <- cb_ext[cb_ext$variable %in% invar, ]$is_metric
    is_dummy <- cb_ext[cb_ext$variable %in% invar, ]$is_dummy ||
      cb_ext[cb_ext$variable %in% invar, ]$is_pdummy
    
    if (identical(invar, "c1")) {
      lgd <- "Mean age"
      unit <- " years"
    } else if (is_metric) {
      lgd <- "Mean"
      unit <- ""
    } else {
      lgd <- "Share"
      unit <- " %"
      poly[[invar]] <- poly[[invar]] * 100
    }
    
    leaflet::leaflet(sf::st_transform(poly[invar], 4326)) %>%
      leaflet::addTiles() %>%
      leaflet::setView(lng = 9, lat = 55, zoom = 4) %>%
      leaflet::addPolygons(
        fillColor = as.formula(paste0("~pal(", invar, ")")),
        fillOpacity = 0.7,
        weight = 1,
        color = "black",
        opacity = 0.5,
        popup = htmltools::htmlEscape(paste0(
          lgd, ": ",
          round(poly[[invar]], 2), unit
        ))
      ) %>%
      leaflet::addLegend(
        position = "bottomright",
        na.label = "No data",
        pal = pal,
        values = as.formula(paste0("~", invar)),
        opacity = 0.9,
        title = lgd,
        labFormat = leaflet::labelFormat(suffix = unit)
      )
  })
  
  
  
  # Case studies ----
  output$cs1map <- leaflet::renderLeaflet({
    cs_bounds <- switch(input$cs1bounds,
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
        fillOpacity = input$cs1opacity
      )
    
    if (identical(input$cs1bg, "Satellite")) {
      m <- leaflet::addProviderTiles(m, "Esri.WorldImagery")
    } else {
      m <- leaflet::addTiles(m)
    }
  })
  
  
  
  # Individual analyses ----
  output$tempmap <- leaflet::renderLeaflet({
    sf <- srv_nuts2
    pal <- viridis::viridis_pal(option = "D")(5)
    pal <- leaflet::colorNumeric(pal, NULL, n = 5)
    sf[["c53"]] <- round(sf[["c53"]], 2)
    leaflet::leaflet(st_transform(sf["c53"], 4326)) %>%
      leaflet::setView(lng = 9, lat = 55, zoom = 4) %>%
      leaflet::addPolygons(
        fillColor = ~pal(c53),
        fillOpacity = 0.7,
        weight = 1,
        color = "black",
        opacity = 0.5,
        popup = htmltools::htmlEscape(paste0(
          "Share", ": ",
          round(sf[["c53"]], 2), " %"
        ))
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
  
  output$tempdensity <- plotly::renderPlotly({
    df <- sf::st_drop_geometry(srv_nuts2[c("c1", "c53")])
    names(df) <- c("Age", "Stable income")
    df <- as.data.frame(scale(df))
    
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
  
  output$tempscatter <- plotly::renderPlotly({
    df <- sf::st_drop_geometry(srv_nuts2[c("c1", "c53")])
    names(df) <- c("Age", "Stable income")
    df <- as.data.frame(scale(df))
    
    p <- ggplot2::ggplot(df, ggplot2::aes(x = `Stable income`, y = Age)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm", na.rm = TRUE) +
      ggplot2::theme_bw()
    
    plotly::config(plotly::ggplotly(p), displayModeBar = FALSE)
  })
}
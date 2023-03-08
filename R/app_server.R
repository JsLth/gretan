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

txts <- list()

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
  rct <- reactiveValues()
  
  shinyjs::onclick("tab-cs5", info_popup(
    "This tab has not yet been filled with elements."
  ))
  
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
          iconHeight = 25,
          iconAnchorY = 25,
          iconAnchorX = 12.5
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
  
  observeEvent(input$csmaps_marker_click, {
    target <- leaflet_select_click(
      id = "csmaps",
      on = "marker",
      ref = cs_coords,
      input = input
    )
    
    if (!is.null(target)) {
      cc <- cs_coords[!cs_coords$name %in% target$name, ]
      leaflet::leafletProxy("csmaps") %>%
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
            iconAnchorX = 18.75,
            className = "hgl_marker"
          )
        )
    }
  })
  
  # Show case study description based on map clicks
  output$csdesc <- leaflet_text_on_click(
    id = "csmaps",
    ref = cs_coords,
    texts = txts$csdesc
  )
  
  
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
    varsel <- cb_ext[cb_ext$title %in% input$exp_title, ]$variable
    items <- unique(cb_ext[cb_ext$variable %in% varsel, ]$subitem)
    options <- unique(cb_ext[cb_ext$variable %in% varsel, ]$option)
    show_subitems <- length(varsel) > 1 & !all(is.na(items))
    show_options <- length(varsel) > 1 & !all(is.na(options))
    
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
  
  
  observeEvent(input$exp_title, {
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
    
    invar <- rct$invar <- invar$variable
    
    is_metric <- cb_ext[cb_ext$variable %in% invar, ]$is_metric
    if (is_metric) {
      shinyjs::disable("fixed_hide")
    } else {
      shinyjs::enable("fixed_hide")
    }
  })
  
  observeEvent(input$scale, {
    rct$poly <- switch(
      input$scale,
      "NUTS-0" = survey_nuts0,
      "NUTS-1" = survey_nuts1,
      "NUTS-2" = survey_nuts2
    )
  })
  
  output$explorer <- leaflet::renderLeaflet({
    poly <- rct$poly
    all_pals <- list_palettes()

    if (input$pal %in% all_pals[["Colorblind palettes"]]) {
      pal <- viridis::viridis_pal(option = tolower(input$pal))(5)
    } else {
      pal <- input$pal
    }
    
    invar <- rct$invar
    is_metric <- cb_ext[cb_ext$variable %in% invar, ]$is_metric
    is_dummy <- cb_ext[cb_ext$variable %in% invar, ]$is_dummy ||
      cb_ext[cb_ext$variable %in% invar, ]$is_pdummy
    
    domain <- NULL
    pal_values <- as.formula(paste0("~", invar))
    
    if (identical(invar, "c1")) {
      lgd <- "Mean age"
      unit <- " years"
    } else if (is_metric) {
      lgd <- "Mean"
      unit <- ""
    } else {
      if (identical(input$expfixed, "Full range")) {
        domain <- seq(0, 100, 10)
        pal_values <- domain
      }
      lgd <- "Share"
      unit <- " %"
      poly[[invar]] <- poly[[invar]] * 100
    }
    
    pal <- leaflet::colorNumeric(pal, domain = domain)
    
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
        )),
        highlightOptions = leaflet::highlightOptions(
          weight = 2,
          color = "black",
          opacity = 0.5,
          fillOpacity = 0.8,
          bringToFront = TRUE,
          sendToBack = TRUE
        )
      ) %>%
      leaflet::addLegend(
        position = "bottomright",
        na.label = "No data",
        pal = pal,
        values = pal_values,
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
      ) %>%
      leaflet::addMarkers(lng = 11.399926, lat = 44.507145)
    
    if (identical(input$cs1bg, "Satellite")) {
      m <- leaflet::addProviderTiles(m, "Esri.WorldImagery")
    } else {
      m <- leaflet::addTiles(m)
    }
  })
  
  output$cs1poi <- leaflet_text_on_click(
    id = "cs1map",
    ref = cs1coords,
    texts = txts$cs1poi
  )
  
  
  
  # Individual analyses ----
  output$tempmap <- leaflet::renderLeaflet({
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
    
    p <- ggplot2::ggplot(df, ggplot2::aes(x = `Stable income`, y = Age)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm", na.rm = TRUE) +
      ggplot2::theme_bw()
    
    plotly::config(plotly::ggplotly(p), displayModeBar = FALSE)
  })
}
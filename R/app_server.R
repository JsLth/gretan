# Coordinates and place names of the case studies
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

# Global list to store all texts so that they don't spam the code files
txts <- list()

server <- function(input, output, session) {
  rct <- reactiveValues()
  
  # Employ waiters
  w_coopmap1 <- do.call(waiter::Waiter$new, c(id = "coopmap2", waiter_default))
  w_coopmap2 <- do.call(waiter::Waiter$new, c(id = "coopmap2", waiter_default))
  w_coopscatter <- do.call(waiter::Waiter$new, c(id = "coopscatter", waiter_default))

  # Show popups as long as tabs are not filled with contents
  shinyjs::onclick("tab-cs2", info_popup(
    "This tab has not yet been filled with contents."
  ))
  shinyjs::onclick("tab-cs3", info_popup(
    "This tab has not yet been filled with contents."
  ))
  shinyjs::onclick("tab-cs4", info_popup(
    "This tab has not yet been filled with contents."
  ))
  shinyjs::onclick("tab-cs5", info_popup(
    "This tab has not yet been filled with contents."
  ))
  shinyjs::onclick("tab-simulation", info_popup(
    "This tab has not yet been filled with contents."
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
            iconAnchorX = 18.75
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
  
  
  # Determine the variable based on combination of topic, subitem and option
  observe({
    has_title <- cb_ext$title %in% input$exp_title
    invar <- cb_ext[has_title, ]

    # case: multiple items exist, look for subitems
    if (length(invar$variable) > 1) {
      has_subitem <- invar$subitem %in% input$exp_subitem
      
      # only select subitem if any exist
      if (any(has_subitem)) {
        invar <- invar[has_subitem, ]
      }
    }

    # case: there's still multiple items, look for options
    if (length(invar$variable) > 1 || !length(invar$variable)) {
      has_option <- invar$option %in% input$exp_option
      
      # only select option if any exist
      if (any(has_option)) {
        invar <- invar[has_option, ]
      }
    }
    
    # if all strings fail, just select the first one
    if (length(invar$variable) > 1) {
      invar <- invar[1, ]
    }
    
    rct$invar <- invar$variable
    
    is_metric <- cb_ext[cb_ext$variable %in% rct$invar, ]$is_metric
    if (is_metric) {
      shinyjs::disable("fixed_hide")
    } else {
      shinyjs::enable("fixed_hide")
    }
  })
  
  observeEvent(input$scale, {
    rct$poly <- switch(
      input$scale,
      "NUTS-0" = srv_nuts0,
      "NUTS-1" = srv_nuts1,
      "NUTS-2" = srv_nuts2
    )
  })

  output$explorer <- leaflet::renderLeaflet({
    poly <- rct$poly
    invar <- rct$invar
    all_pals <- list_palettes()

    if (input$pal %in% all_pals[["Colorblind palettes"]]) {
      pal <- viridis::viridis_pal(option = tolower(input$pal))(5)
    } else {
      pal <- input$pal
    }
    
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
      unit <- "%"
      poly[[invar]] <- poly[[invar]] * 100
    }
    
    pal <- leaflet::colorNumeric(pal, domain = domain)
    
    label_values <- list(
      poly[["nuts0"]], poly[["nuts1"]], poly[["nuts2"]],
      paste0(round(poly[[invar]], 2), unit), ":"
    )
    names(label_values) <- c("NUTS-0", "NUTS-1", "NUTS-2", lgd, "sep")
    labels <- do.call(align_dl, label_values)

    leaflet::leaflet(sf::st_transform(poly, 4326)) %>%
      leaflet::addTiles() %>%
      leaflet::setView(lng = 9, lat = 55, zoom = 4) %>%
      leaflet::addPolygons(
        fillColor = as.formula(paste0("~pal(", invar, ")")),
        fillOpacity = 0.7,
        weight = 1,
        color = "black",
        opacity = 0.5,
        label = labels,
        highlightOptions = highlight_opts
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
  
  
  
  # Spatial analysis ----
  output$coopmap1 <- leaflet::renderLeaflet({
    w_coopmap1$show()
    pal <- leaflet::colorNumeric("Spectral", NULL)
    
    coopernico_projects <- data.frame(
      project = c("Escola JG Zarco", "Lar S. Silvestre"), 
      lat = c(38.700671, 39.879235), lng = c(-9.237519, -7.396333),
      label = c("<b>Project name</b>: Escola JG Zarco", "<b>Project name</b>: Lar S. Silvestre")
    )
    
    coopernico_projects <- sf::st_as_sf(
      coopernico_projects,
      coords = c("lng", "lat"),
      remove = FALSE, 
      crs = 4326,
      agr = "constant"
    )
    
    # Create regional capitals database (five continental regions = NUTS 2)
    capitals <- data.frame(
      city = c("Lisboa", "Porto", "Coimbra", "Faro", "Evora"), 
      lat = c(38.725267, 41.162142, 40.211111, 37.016111, 38.566667), 
      lng = c(-9.150019, -8.621953, -8.429167, -7.935, -7.9)
    )
    
    capitals <- sf::st_as_sf(
      capitals,
      coords = c("lng", "lat"),
      remove = FALSE,
      crs = 4326,
      agr = "constant"
    )
    
    labels <- align_dl("Total investment" = coopernico$total_amount)
    
    m <- leaflet::leaflet(coopernico, TRUE) %>%
      leaflet::addTiles() %>%
      leaflet::setView(lng = -7.5, lat = 39.5, zoom = 7) %>%
      leaflet::addPolygons(
        fillColor = ~pal(total_amount),
        fillOpacity = 1,
        color = "black",
        weight = 0.5,
        smoothFactor = 0, # necessary to remove gaps between polygons
        highlightOptions = highlight_opts,
        label = labels
      ) %>%
      leaflet::addLegend(
        position = "bottomright",
        na.label = "No data",
        pal = pal,
        values = ~total_amount,
        opacity = 0.9,
        title = "Total investment",
        labFormat = leaflet::labelFormat(suffix = " &euro;")
       ) %>%
      leaflet::addCircleMarkers(
        color = "black",
        fillColor = "red",
        fillOpacity = 1,
        opacity = 1,
        weight = 1,
        radius = 5,
        data = capitals
      ) %>%
      leaflet::addAwesomeMarkers(data = coopernico_projects, popup = ~label)
    
    w_coopmap1$hide()
    m
  })
  
  observe({
    init <- isTRUE(rct$coopscatter_init) || isTRUE(rct$coopmap2_init)
    if (input$coopmap2_sidebar_apply == 1L || !init) {
      nb <- spdep::poly2nb(coopernico, queen = TRUE)
      rct$coopmap2_lw <- spdep::nb2listwdist(
        nb,
        sf::st_centroid(sf::st_geometry(coopernico)),
        type = input$coopmap2_sidebar_dist,
        alpha = input$coopmap2_sidebar_alpha,
        style = input$coopmap2_sidebar_scheme,
        dmax = input$coopmap2_sidebar_dmax,
        longlat = TRUE,
        zero.policy = TRUE
      )
      rct$coopmap2_locm <- spdep::localmoran(
        coopernico$total_amount,
        listw = rct$coopmap2_lw,
        zero.policy = TRUE
      )
      rct$coopmap2_colors <- locm_colors_abel(rct$coopmap2_locm, coopernico)
      rct$coopmap2_labels <- do.call(align_dl, list(
        "Moran's I" = round(rct$coopmap2_locm[, "Ii"], 4),
        "p-value" = round(rct$coopmap2_locm[, "Pr(z != E(Ii))"], 4)
      ))
      
      if (isTRUE(rct$coopmap2_init)) {
        leaflet::leafletProxy("coopmap2") %>%
          leaflet::clearShapes() %>%
          leaflet::addPolygons(
            fillColor = rct$coopmap2_colors[[2]],
            color = rct$coopmap2_colors[[1]],
            fillOpacity = 1,
            weight = 1,
            smoothFactor = 0,
            label = rct$coopmap2_labels,
            highlightOptions = highlight_opts,
            data = coopernico
          )
      }
    }
  })
  
  output$coopmap2 <- leaflet::renderLeaflet({
    w_coopmap2$show()
    locm <- rct$coopmap2_locm
    colors <- rct$coopmap2_colors
    rct$coopmap2_init <- TRUE
    pal <- leaflet::colorFactor(unique(colors[[1]]), domain = NULL)
    
    m <- leaflet::leaflet(coopernico) %>%
      leaflet::addTiles() %>%
      leaflet::setView(lng = -7.5, lat = 39.5, zoom = 7) %>%
      leaflet::addPolygons(
        fillColor = colors[[2]],
        color = colors[[1]],
        fillOpacity = 1,
        weight = 1,
        smoothFactor = 0,
        label = rct$labels,
        highlightOptions = highlight_opts
      ) %>%
      leaflet::addLegend(
        position = "bottomright",
        colors = c(unique(colors[[1]]), "white"),
        labels = c(levels(attributes(locm)$quadr$mean), "Not significant"),
        title = "LISA"
      )
    
    w_coopmap2$show()
    m
  })
  
  output$coopscatter <- plotly::renderPlotly({
    w_coopscatter$show()
    coopernico$sponsors_lag <- spdep::lag.listw(
      rct$coopmap2_lw,
      coopernico$total_amount,
      zero.policy = TRUE
    )
    rct$coopscatter_init <- TRUE

    coopernico <- coopernico %>%
      dplyr::rename(
        "Spatial lag" = "sponsors_lag",
        "Total amount" = "total_amount",
        "Municipality" = "name"
      )
    
    p <- ggplot2::ggplot(
      data = coopernico,
      ggplot2::aes(x = `Spatial lag`, y = `Total amount`)) +
      ggplot2::geom_point(
        ggplot2::aes(group = Municipality),
        shape = 1,
        size = 2,
        color = "black",
        stroke = 0.3
      ) +
      ggplot2::geom_smooth(
        method = "lm",
        se = FALSE,
        linetype = "dashed",
        color = "darkred"
      ) +
      ggplot2::xlab("Spatial lag") + 
      ggplot2::ylab("Total investment (in &euro;)") +
      ggplot2::theme(panel.grid.major = ggplot2::element_line(
        color = "#CCCCCC",
        linetype = "dashed", size = 0.5
      ))
    
    p <- plotly::config(plotly::ggplotly(p), displayModeBar = FALSE)
    w_coopscatter$hide()
    p
  })
  
  
  
  # Income stability ----
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
      ggplot2::geom_smooth(method = "lm", na.rm = TRUE, formula = y ~ x) +
      ggplot2::theme_bw()
    
    plotly::config(plotly::ggplotly(p), displayModeBar = FALSE)
  })
  
  
  
  #sandbox
  sbtext <- eventReactive(input$sandbox_button, {
    num <- sample(c("a","b","c","d","e"), 1)
    switch(
      num,
      a = "I'm a generated text.",
      b = "<b>I'm a bold text</b>",
      c = "<del>I'm not a text</del>",
      d = "<small>I'm a small text</small>",
      e = "<em>I'm an emphasized text</em>"
    )
  })
  
  output$sandbox_text <- renderUI({
    HTML(sbtext())
  })
  
  observeEvent(input$sandbox_switch, {
    bs4Dash::updateBoxSidebar("coopmap2_sidebar")
  })
  
  observeEvent(input$success, {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Success !!",
      text = "All in order",
      type = "success"
    )
  })
  
  observeEvent(input$error, {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Error...",
      text = "Oups !",
      type = "error"
    )
  })
  
  observeEvent(input$info, {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Information",
      text = "Something helpful",
      type = "info"
    )
  })
  
  observeEvent(input$warning, {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Warning !!!",
      text = NULL,
      type = "warning"
    )
  })
}
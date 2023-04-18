render_question <- function(title, subitem, option) {
  if (!is.null(title)) {
    indat <- cb_ext[cb_ext$title %in% title, ]
    
    if (!all(is.na(indat$subitem))) {
      indat <- indat[indat$subitem %in% subitem, ]
    }
    
    if (!all(is.na(indat$option))) {
      indat <- indat[indat$option %in% option, ]
    }
    
    HTML(sprintf(
      "<b>Question %s:</b><br>%s",
      toupper(indat$og_var),
      indat$label
    ))
  } else {
    ""
  }
}


get_mns_variable <- function(title, subitem, option) {
  has_title <- cb_ext$title %in% title
  invar <- cb_ext[has_title, ]

  # case: multiple items exist, look for subitems
  if (length(invar$variable) > 1) {
    has_subitem <- invar$subitem %in% subitem
    
    # only select subitem if any exist
    if (any(has_subitem)) {
      invar <- invar[has_subitem, ]
    }
  }
  
  # case: there's still multiple items, look for options
  if (length(invar$variable) > 1 || !length(invar$variable)) {
    has_option <- invar$option %in% option
    
    # only select option if any exist
    if (any(has_option)) {
      invar <- invar[has_option, ]
    }
  }
  
  # if all strings fail, just select the first one
  if (length(invar$variable) > 1) {
    invar <- invar[1, ]
  }
  
  invar$variable
}


get_mns_params <- function(invar, fixed, palette, poly) {
  cb_entry <- cb_ext[cb_ext$variable %in% invar, ]
  is_metric <- cb_entry$is_metric
  is_likert <- cb_entry$is_likert

  lgd_labels <- NULL
  domain <- NULL
  values <- as.formula(paste0("~", invar))

  if (identical(invar, "c1")) {
    lgd <- "Mean age"
    unit <- " years"
  } else if (is_likert) {
    lgd_labels <- cb_entry$labels[[1]]
    values <- as.formula(paste0("~as_likert(", invar, ")"))
    domain <- strtoi(lgd_labels)
    lgd_labels <- domain <- domain[!is.na(domain)]
    lgd <- "Median"
    unit <- ""
  } else if (is_metric) {
    lgd <- "Mean"
    unit <- ""
  } else {
    if (identical(fixed, "Full range")) {
      values <- domain <- seq(0, 100, 10)
    }
    lgd <- "Share"
    unit <- "%"
    poly[[invar]] <- poly[[invar]] * 100
  }

  if (is.null(lgd_labels)) {
    pal <- leaflet::colorNumeric(palette, domain = domain)
  } else {
    pal <- leaflet::colorFactor(palette, domain = domain, levels = domain)
  }


  label_values <- list(
    poly[["nuts0"]], poly[["nuts1"]], poly[["nuts2"]],
    paste0(round(poly[[invar]], 2), unit), poly$sample, ":"
  )
  names(label_values) <- c(
    "NUTS-0",
    "NUTS-1",
    "NUTS-2",
    lgd,
    "Sample",
    "sep"
  )
  labels <- do.call(align_dl, label_values)
  
  poly <- sf::st_transform(poly, 4326)
  
  list(
    poly = poly,
    invar = invar,
    pal = pal,
    values = values,
    labels = labels,
    lgd = lgd,
    lgd_labels = lgd_labels,
    unit = unit
  )
}


map_mns <- function(params) {
  leaflet::leaflet(params$poly) %>%
    leaflet::addTiles() %>%
    leaflet::setView(lng = 9, lat = 55, zoom = 4) %>%
    leaflet::addPolygons(
      fillColor = as.formula(paste0("~params$pal(", params$invar, ")")),
      fillOpacity = 0.7,
      weight = 1,
      color = "black",
      opacity = 0.5,
      label = params$labels,
      highlightOptions = highlight_opts
    ) %>%
    leaflet::addLegend(
      position = "bottomright",
      na.label = "No data",
      pal = params$pal,
      values = params$values,
      opacity = 0.9,
      title = params$lgd,
      labFormat = leaflet::labelFormat(suffix = params$unit)
    )
}


update_mns_map <- function(id, params, session = getDefaultReactiveDomain()) {
  print(params$values)
  leaflet::leafletProxy(id, data = params$poly, session = session) %>%
    leaflet::clearShapes() %>%
    leaflet::clearControls() %>%
    leaflet::addPolygons(
      fillColor = as.formula(paste0("~params$pal(", params$invar, ")")),
      fillOpacity = 0.7,
      weight = 1,
      color = "black",
      opacity = 0.5,
      label = params$labels,
      highlightOptions = highlight_opts
    ) %>%
    leaflet::addLegend(
      position = "bottomright",
      na.label = "No data",
      pal = params$pal,
      values = params$values,
      opacity = 0.9,
      title = params$lgd,
      labFormat = leaflet::labelFormat(suffix = params$unit)
    )
}
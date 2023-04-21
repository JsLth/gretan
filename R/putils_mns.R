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
      indat$question
    ))
  } else {
    ""
  }
}


get_mns_variable <- function(title, subitem, option, mode) {
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
  if ((length(invar$variable) > 1 || !length(invar$variable)) && isFALSE(mode)) {
    has_option <- invar$option %in% option
    
    # only select option if any exist
    if (any(has_option)) {
      invar <- invar[has_option, ]
    }
  }

  # if all strings fail, just select the first one
  if (length(invar$variable) > 1 && isFALSE(mode)) {
    invar <- invar[1, ]
  }

  invar$variable
}


get_mns_params <- function(invar, fixed, palette, aggr) {
  poly <- switch(aggr,
    "NUTS-0" = srv_nuts0,
    "NUTS-1" = srv_nuts1,
    "NUTS-2" = srv_nuts2
  )
  if (length(invar) > 1) {
    poly <- get_mns_mode(poly, cb_ext, invar)
    invar <- invar[1]
  } else {
    poly <- subset_mns(poly, invar)
  }
  cb_entry <- cb_ext[cb_ext$variable %in% invar, ]
  is_metric <- cb_entry$is_metric
  is_likert <- cb_entry$is_likert

  palettes <- list_palettes()
  if (palette %in% palettes[["Colorblind palettes"]]) {
    palette <- viridis::viridis_pal(option = tolower(palette))(5)
  }
  
  domain <- NULL
  values <- as.formula(paste0("~", invar))
  
  if (identical(invar, "c1")) {
    lgd <- "Mean age"
    unit <- " years"
  } else if (is.character(poly[[invar]])) {
    labs <- cb_entry$labels[[1]]
    values <- domain <- factor(labs, levels = labs)
    lgd <- "Mode"
    unit = ""
  } else if (is_likert) {
    labs <- cb_entry$labels[[1]]
    if (is_non_default_likert(labs)) {
      scale <- labs
    } else {
      scale <- NULL
    }
    labs <- strtoi(substr(cb_entry$labels[[1]], 1, 1))
    labs <- labs[!is.na(labs)]
    values <- domain <- as_likert(seq(1, length(labs)), scale = scale)
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

  lval <- poly[[invar]]
  if (!is_likert && !is.character(poly[[invar]])) {
    pal <- leaflet::colorNumeric(palette, domain = domain)
    lval <- round(lval, 2)
  } else {
    if (is_likert) poly <- num_to_likert(poly, cb_entry)
    pal <- leaflet::colorFactor(palette, domain = NULL, levels = domain)
  }

  label_values <- list(
    poly[["nuts0"]], poly[["nuts1"]], poly[["nuts2"]],
    paste0(lval, unit), poly$sample, ":"
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

is_non_default_likert <- function(x) {
  any(grepl("accurate|favour|willing|unfair", x))
}

num_to_likert <- function(df, cb) {
  var <- cb$variable
  labs <- cb$labels[[1]]
  if (!is_non_default_likert(labs)) labs <- NULL
  df[[var]] <- as_likert(df[[var]], scale = labs)
  df
}


get_mns_mode <- function(df, cb, var) {
  entry <- cb_ext[cb_ext$variable %in% var, ]
  var <- entry$variable
  options <- entry$option
  dummies <- as.data.frame(df)[var]
  if (nrow(dummies) > 100) browser()
  mode <- vapply(seq_len(nrow(dummies)), function(i) {
    row <- dummies[i, ]
    if (!all(is.na(row))) {
      options[which.max(row)]
    } else {
      NA_character_
    }
  }, FUN.VALUE = character(1))
  var <- var[1]
  df[var] <- mode
  subset_mns(df, var)
}

subset_mns <- function(df, var = NULL) {
  incl <- c("sample", "nuts0")
  if ("nuts1" %in% names(df)) incl <- c(incl, "nuts1")
  if ("nuts2" %in% names(df)) incl <- c(incl, "nuts2")
  df[c(incl, var)]
}

update_palettes <- function(type = c("seq", "div", "qual")) {
  type <- match.arg(type)
  shinyWidgets::updatePickerInput(
    getDefaultReactiveDomain(),
    "pal",
    choices = list_palettes(type = type)
  )
}
interactionSlider <- function(id) {
  params <- strsplit(id, "__")[[1]]
  stakeholder <- params[params %in% c("citizens", "business", "government")]

  shinyWidgets::sliderTextInput(
    inputId = id, label = to_title(stakeholder),
    choices = c("N/A", seq(0, 1, 0.05)),
    selected = "N/A",
    grid = TRUE
  )
}


plot_persona <- function(data, item = "cluster", diff = TRUE) {
  x <- y <- text <- ind <- values <- grp <- NULL

  # Subset data
  data <- as.matrix(data[grepl(
    paste0(item, "*_[0-9]"),
    names(data)
  )])

  # Prevent cases in which values are equal to zero
  data <- data + .Machine$double.xmin

  # Compute difference
  if (isTRUE(diff)) {
    data <- diff(data)
  } else {
    data[1, ] <- -data[1, ]
  }

  # Pivot longer
  data <- utils::stack(data.frame(data))

  # Safely fail if values are NA
  if (any(is.na(data$values))) {
    edf <- data.frame(
      x = 0.5,
      y = 0.5,
      text = "There is not enough information about start or\ndestination to create a fitting visualization"
    )
    p <- ggplot2::ggplot(edf) +
      ggplot2::geom_text(ggplot2::aes(x, y, label = text)) +
      ggplot2::theme_void()
    return(p)
  }

  # Format values
  nc <- nchar(as.character(data$ind))
  data$item <- substr(data$ind, 1, nc - 2)
  data$ind <- as.integer(substr(data$ind, nc, nc))
  data$grp <- factor((data$values >= 0) + 1)

  # Sort within
  if (isFALSE(diff)) {
    data <- tapply(data, list(data$grp), function(x) {
      x[order(abs(x$values), decreasing = TRUE), ]
    })
    data <- do.call(rbind.data.frame, data)
    row.names(data) <- NULL
  } else {
    data <- data[order(data$values), ]
  }

  # Replace integers with meaninful strings
  if (identical(item, "cluster")) {
    question <- "Energy persona"
    choices <- vapply(
      txts$main$persona$results$personas,
      function(x) x$name,
      FUN.VALUE = character(1)
    )[data$ind]
  } else {
    i <- as.integer(substr(item, 2, 2))
    step <- txts$main$persona$steps[[i + 1]]
    chs <- names(step$choices)[-1]
    question <- gsub("[[:space:]]+", " ", step$question)
    choices <- chs[data$ind]
  }

  data$ind <- choices
  data$item <- question

  # Flexible value breaks
  ybreaks <- if (max(abs(data$values)) <= 0.15) {
    seq(-0.25, 0.25, 0.05)
  } else if (max(abs(data$values)) <= 0.4) {
    seq(-0.5, 0.5, 0.1)
  } else {
    seq(-1, 1, 0.25)
  }

  lims <- if (isTRUE(diff)) {
    rev(data$ind)
  } else {
    rev(data[data$values <= 0, ]$ind)
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(ind, values))
  p <- if (isTRUE(diff)) {
    p + ggplot2::geom_bar(
      ggplot2::aes(fill = grp),
      color = NA,
      show.legend = TRUE,
      stat = "identity"
    )
  } else {
    p + ggplot2::geom_col(
      ggplot2::aes(fill = grp),
      color = NA,
      show.legend = TRUE
    )
  }
  p +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = if (isTRUE(diff)) "Difference (in %)" else "Proportion (in %)",
      subtitle = if (isFALSE(diff)) {
        paste(
          "Which characteristics are more pronounced at your destination?"
        )
      } else {
        "How much do personas change when moving to your destination?"
      }
    ) +
    ggplot2::scale_x_discrete(limits = lims) +
    ggplot2::scale_y_continuous(
      breaks = ybreaks,
      limits = ybreaks[c(1, length(ybreaks))],
      labels = if (isFALSE(diff)) abs(ybreaks) * 100 else ybreaks * 100
    ) +
    ggplot2::scale_fill_hue(
      name = "",
      direction = -1,
      labels = if (isFALSE(diff)) {
        c("1" = "Start", "2" = "Destination")
      } else {
        c("1" = "Decline", "2" = "Increase")
      }
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 14),
      panel.grid = ggplot2::element_blank(),
      legend.position = c(0.9, 0.1)
    )
}


itemToIdx <- function(x) as.integer(substr(x, 2, 2))


render_persona_item <- function(item, results, responses, ...) {
  if (identical(item, "cluster")) {
    p(HTML(paste(
      "<b>Persona:</b> The GRETA energy personas! Based on the",
      "questionnaire, our models show that you are likely similar to the",
      tags$b(results[[1]]$name), "persona."
    )), ...)
  } else {
    idx <- itemToIdx(item)
    steps <- txts$main$persona$steps
    question <- steps[[idx + 1]]$question
    choices <- names(steps[[idx + 1]]$choices)[-1]
    your_choice <- switch(as.character(responses[idx]),
      "-2" = "Prefer not to say",
      "-1" = "I do not know",
      choices[as.numeric(responses[idx])]
    )
    p(HTML(paste0(
      tags$b(paste("Question", idx)),
      ": ",
      question,
      br(),
      tags$b("Your choice: "),
      your_choice
    )), ...)
  }
}


addMovingMarker <- function(map,
                            lng = NULL,
                            lat = NULL,
                            layerId = NULL,
                            group = NULL,
                            duration = 2000,
                            icon = NULL,
                            popup = NULL,
                            popupOptions = NULL,
                            label = NULL,
                            labelOptions = NULL,
                            movingOptions = list(),
                            options = leaflet::markerOptions(),
                            data = leaflet::getMapData(map)) {
  if (missing(labelOptions)) {
    labelOptions <- leaflet::labelOptions()
  }

  if (is.null(layerId)) {
    layerId <- paste0("_", as.numeric(Sys.time()))
  }

  movingMarkerDependency <- list(structure(
    list(
      name = "lfx-movingmarker",
      version = "1.0.0",
      src = list(file = normalizePath(file.path(app_sys("app/www/")))),
      meta = NULL,
      script = c("MovingMarker.js", "movingmarker-bindings.js"),
      stylesheet = NULL,
      head = NULL,
      attachment = NULL,
      package = NULL,
      all_files = TRUE
    ),
    class = "html_dependency"
  ))

  pts <- leaflet::derivePoints(
    data,
    lng,
    lat,
    missing(lng),
    missing(lat),
    "addMovingMarker"
  )

  duration <- leaflet::evalFormula(duration, data)
  options <- leaflet::filterNULL(c(options, movingOptions))
  map$dependencies <- c(map$dependencies, movingMarkerDependency)
  leaflet::invokeMethod(
    map,
    data,
    "addMovingMarker",
    cbind(pts$lat, pts$lng),
    duration,
    icon,
    layerId,
    group,
    options,
    popup,
    popupOptions,
    leaflet::safeLabel(label, data),
    labelOptions
  ) %>%
    leaflet::expandLimits(pts$lat, pts$lng)
}


addLegendLine <- function(map,
                          label,
                          labelStyle = style(
                            `font-size` = "24px",
                            `vertical-align` = "middle"
                          ),
                          width = 20,
                          height = 20,
                          className = "info legend leaflet-control",
                          ...) {
  marginWidth <- max(0, (max(width) - width) / 2)
  imgStyle <- style(
    `vertical-align` = "middle",
    margin = "5px",
    `margin-right` = paste0(marginWidth, "px"),
    `margin-left` = paste0(marginWidth, "px")
  )
  imgTag <- tags$img(
    src = paste0(
      "data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2F",
      "www.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%",
      "3D%229%22%20height%3D%229%22%3E%0A%20%20%3Cline%20id%3D%",
      "22line%22%20x1%3D%220%22%20x2%3D%229%22%20y1%3D%224.5%22",
      "%20y2%3D%224.5%22%20stroke%3D%22red%22%20stroke-opacity%3",
      "D%221%22%20fill-opacity%3D%221%22%3E%3C%2Fline%3E%0A%3C%",
      "2Fsvg%3E"
    ),
    style = imgStyle,
    height = height,
    width = width
  )
  htmlElement <- div(imgTag, span(label, style = labelStyle))

  leaflet::addControl(
    map,
    html = tagList(htmlElement),
    className = className,
    ...
  )
}


as_likert <- function(x, scale = NULL) {
  if (length(scale) > 7) {
    stop(sprintf("Likert scale is too long (%s items)", length(x)))
  }

  scale <- scale %||% c(
    "Strongly disagree", "Disagree", "Somewhat disagree",
    "Neutral",
    "Somewhat agree", "Agree", "Strongly agree"
  )

  if (is.factor(scale)) {
    scale <- as.character(scale)
  }

  values <- vapply(x, function(i) scale[i], FUN.VALUE = character(1))
  ordered(values, levels = scale)
}

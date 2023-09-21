plot_persona <- function(data, item = "cluster", diff = TRUE) {
  # Pivot longer
  data <- as.matrix(data[grepl(
    paste0(item, "*_[0-9]"),
    names(data)
  )])
  if (isTRUE(diff))
    data <- diff(data)
  else
    data[1, ] <- -data[1, ]
  data <- utils::stack(data.frame(data))
  
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
  
  ybreaks <- if (max(abs(data$values)) <= 0.15)
    seq(-0.25, 0.25, 0.05)
  else if (max(abs(data$values)) <= 0.4)
    seq(-0.5, 0.5, 0.1)
  else
    seq(-1, 1, 0.25)
  
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
      subtitle = if (isFALSE(diff)) paste(
        "Which personas and characteristics are more and",
        "less pronounced at your travel destination?"
      )
      else
        "How much do personas change when moving to your destination?"
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
      labels = if (isFALSE(diff))
        c("1" = "Start", "2" = "Destination")
      else
        c("1" = "Decline", "2" = "Increase")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      # axis.text = ggplot2::element_text(size = 7),
      # title = ggplot2::element_text(size = 9),
      panel.grid = ggplot2::element_blank(),
      legend.position = c(0.9, 0.1)
    )
}
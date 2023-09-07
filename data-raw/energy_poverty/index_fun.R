filter_non_responses <- function(x) {
  lev <- levels(x)
  as.character(x) %>%
    na_if("I do not know") %>%
    na_if("Prefer not to say") %>%
    na_if("Do not know / prefer not to say") %>%
    na_if("Not applicable") %>%
    factor(levels = lev)
}


is_dichotomous <- function(x) {
  all(x %in% c(0, 1, NA))
}

is_polytomous <- function(x) {
  all(is.numeric(x) & length(unique(x)) < 8) && !is_dichotomous(x)
}

scale_minmax <- function(x, na.rm = TRUE) {
  (x - min(x, na.rm = na.rm)) / (max(x, na.rm = na.rm) - min(x, na.rm = na.rm))
}

# GWPCA bandwith selection with specific defaults
subindex_bw <- function(subindex, df = NULL, loc = NULL, vars = NULL) {
  if (is.null(df))
    df <- parent.frame()$pov_for_pca
  
  if (is.null(loc))
    loc <- parent.frame()$loc
  
  if (is.null(vars))
    vars <- parent.frame()$subind_vars
  
  bw.gwpca(
    data = as_Spatial(st_sf(df, geometry = loc)[vars[[subindex]]]),
    vars = names(df[vars[[subindex]]]),
    k = 1,
    kernel = "gaussian",
    robust = TRUE,
    adaptive = TRUE
  )
}

# GWPCA with some specific defaults
subindex_gwpca <- function(subindex, bw, robust = FALSE, df = NULL, loc = NULL, vars = NULL) {
  if (is.null(df))
    df <- parent.frame()$pov_for_pca
  
  if (is.null(loc))
    loc <- parent.frame()$loc
  
  if (is.null(vars))
    vars <- parent.frame()$subind_vars
  
  df <- df[vars[[subindex]]]
  
  gwpca(
    data = as_Spatial(st_sf(df, geometry = loc)),
    elocat = as_Spatial(loc),
    vars = names(df),
    k = 1,
    bw = bw,
    robust = robust,
    kernel = "gaussian",
    adaptive = TRUE,
    cv = FALSE
  )
}


tidy_gwpca <- function(x, rev = FALSE, names = NULL) {
  out <- st_as_sf(x$SDF)
  load <- x$loadings[, , 1]
  
  # Rescale to 0-1
  load <- apply(load, 2, function(x) {
    to <- c(0, 1)
    from <- range(x, na.rm = TRUE, finite = TRUE)
    (x - from[1]) / diff(from) * diff(to) + to[1]
  })
  
  # Reverse scales for consistency
  if (rev) {
    load[rev] <- apply(load, 2, function(x) 1 - x)
  }
  
  if (!is.null(names)) {
    out$win_var_PC1 <- unlist(names[out$win_var_PC1], use.names = FALSE)
  }
  
  # Aggregate using geometric mean
  load <- apply(load, 1, geometric.mean)
  
  out <- bind_cols(out, index = load)
  out
}


plot_gwpca <- function(x,
                       var = "Comp.1_PV",
                       title = NULL,
                       single_plot = FALSE,
                       env = parent.frame()) {
  subind_nm <- env$subind_nm
  gw_pca <- env$gw_pca
  
  if (is.null(title)) {
    title <- subind_nm[names(gw_pca)[[get("i", envir = parent.frame())]]]
  }
  
  size_args <- if (single_plot) {
    list(
      size = 0.7,
      key_size = 0.6,
      key_width = 1.5,
      text_size = 11,
      title_size = 13,
      qual_key_size = 6,
      qual_key_space = -0.3
    )
  } else {
    list(
      size = 0.2,
      key_size = 0.3,
      key_width = 0.6,
      text_size = 5,
      title_size = 7,
      qual_key_size = 3,
      qual_key_space = -0.1
    )
  }
  
  legend <- switch(
    var,
    "Comp.1_PV" = "Proportion of variance",
    "index" = "Local index",
    "win_var_PC1" = "Winning variable"
  )
  p <- ggplot(x) +
    geom_sf(aes(color = .data[[var]]), size = size_args$size) +
    coord_sf(xlim = c(2500000, 6000000), ylim = c(1500000, 5200000))
  
  if (!var %in% "win_var_PC1") {
    p <- p + scale_color_viridis_c(sprintf("**%s**<br>%s", title, legend))
  } else {
    p <- p + colorspace::scale_color_discrete_qualitative(
      palette = "Dynamic",
      name = sprintf("**%s**<br>%s", title, legend)
    )
  }

  p <- p +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = c(0.3, 0.8),
      legend.direction = "horizontal",
      legend.key.size = ggplot2::unit(size_args$key_size, 'cm'),
      legend.key.width = ggplot2::unit(size_args$key_width, "cm"),
      legend.text = element_text(size = size_args$text_size),
      legend.title = element_markdown(size = size_args$title_size),
      axis.text = element_blank()
    )
  
  if (!var %in% "win_var_PC1") {
    p + guides(color = guide_colorbar(title.position = "top"))
  } else {
    p + guides(color = guide_legend(
      title.position = "top",
      override.aes = list(shape = 15, size = size_args$qual_key_size),
      direction = "vertical",
      ncol = if (length(unique(x$win_var_PC1)) > 1) 2 else 1 
    )) +
      theme(legend.spacing.x = unit(size_args$qual_key_space, "cm"))
  }
}
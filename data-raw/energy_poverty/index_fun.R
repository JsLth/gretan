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


tidy_gwpca <- function(x) {
  out <- st_as_sf(x$SDF)
  load <- scale(x$loadings[, , 1])
  load <- apply(x$loadings[, , 1], 1, mean)
  out <- bind_cols(out, index = load)
  out
}


plot_gwpca <- function(x, var = "Comp.1_PV", title = NULL) {
  subind_nm <- parent.frame()$subind_nm
  gw_pca <- parent.frame()$gw_pca
  
  if (is.null(title)) {
    title <- subind_nm[names(gw_pca)[[get("i", envir = parent.frame())]]]
  }
  
  legend <- switch(
    var,
    "Comp.1_PV" = "Proportion of variance",
    "index" = "Index",
    "win_var_PC1" = "Winning variable"
  )
  p <- ggplot(x) +
    geom_sf(aes(color = .data[[var]]), size = 0.2) +
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
      legend.key.size = unit(0.3, 'cm'),
      legend.key.width = unit(0.6, "cm"),
      legend.text = element_text(size = 5),
      legend.title = element_markdown(size = 7),
      axis.text = element_blank()
    )
  
  if (!var %in% "win_var_PC1") {
    p + guides(color = guide_colorbar(title.position = "top"))
  } else {
    p + guides(color = guide_legend(
      title.position = "top",
      override.aes = list(shape = 15, size = 3),
      direction = "vertical"
    )) +
      theme(legend.spacing.x = unit(-0.1, "cm"))
  }
}
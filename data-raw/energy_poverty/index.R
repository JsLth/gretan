if (!require(pacman)) {
  install.packages("pacman")
}
library(pacman)

p_load(
  # data wrangling
  dplyr, stringr, purrr, tidyr, janitor,
  
  # spatial data
  sf,
  
  # PCA
  missMDA, factoextra, FactoMineR, psych,
  
  # GW PCA
  GWmodel, GWnnegPCA,
  
  # Visualization
  skimr, kableExtra, ggcorrplot, ggcharts, RColorBrewer, colorspace,
  viridis, patchwork, ggtext,
  
  # Meta
  cli, magrittr
)



# Fix some functions
body(ggcorrplot)[[20]][[3]][[2]][[3]][[3]] <- quote(ggplot2::geom_tile(
  color = outline.color, ggplot2::aes_string(width = "value", height = "value")
))
body(ggcorrplot)[[20]][[3]][[2]] <- quote(
  p <- p +
    ggplot2::geom_tile(color = "grey", fill = "white") +
    ggplot2::geom_tile(
      color = outline.color,
      ggplot2::aes_string(width = "value", height = "value"))
)

body(gwpca)[[26]][[3]] <- quote(cli::cli_progress_along(1:ep.n))
body(gwpca)[[34]][[3]][[2]][[2]] <- quote(matrix(d1[, 1:k]))
#body(gwfa)[[26]][[4]][[11]][[3]] <- quote(cli::cli_progress_along(1:ep.n))
#body(gwfa)[[10]] <- quote(data <- as(sdata, "data.frame")[seq_len(ncol(sdata))])
#body(gw_nsprcomp)[[27]][[3]] <- quote(cli::cli_progress_along(1:ep.n))
#body(gw_nsprcomp)[[27]][[4]][[8]][[3]] <- quote(temp$rotation[var.n, k])



## Read data ----
pov <- readRDS("data-ext/survey_resampled.rds") %>%
  select(starts_with(c(
    "d1", # Gender
    "d2", # Occupation
    "c1", # Age
    "c4", # Education
    "c6_",  # Energy saving #1
    "c11", # Energy complexity
    "c16", # Cooling system
    "c18", # Heating system
    "c19", # Heating system configuration
    "c26", # Energy costs
    "c28", # Consensual energy poverty
    "c29", # Housing type
    "c30", # Housing area
    "c31", # Housing age
    "c48", # Household size
    "c49", # Tenancy
    "c51", # Special conditions
    "c54"  # Income
  )), "id", "nuts0", "nuts1", "nuts2", -"c51_4")

cb <- readRDS("data-ext/codebook.rds") %>%
  filter(variable %in% names(pov))
cb[cb$category %in% "c18", "og_var"] <- "c18"

all_cats <- unique(tail(cb$category, -1))
all_vars <- unique(tail(cb$og_var, -1))


## Merge dummies ----
for (v in all_vars) {
  cb_v <- cb[cb$og_var %in% v, ]
  opts <- cb_v$option
  ext_var <- cb_v$variable
  if (!all(is.na(opts)) && length(opts) > 1) {
    for (o in seq_along(opts)) {
      col <- which(grepl(v, ext_var))[[o]]
      pov[ext_var[col]] <- if_else(
        pov[ext_var[col]][[1]] == 1, opts[o], NA
      )
    }
    if (is.character(pov[[ext_var[1]]])) {
      pov[[ext_var[1]]] <- factor(do.call(
        coalesce,
        as.list(st_drop_geometry(pov[ext_var]))
      ), levels = cb_v$labels[[1]])
    }
    pov[tail(ext_var, -1)] <- NULL
    names(pov)[names(pov) %in% ext_var[1]] <- v
  }
}



## Rename variables ----
## for readability
vars_lookup <- list(
  # demographic variables
  gender = "d1",
  occupation = "d2",
  age = "c1",
  education = "c4",
  household = "c48_1_open",
  cond_air = "c51_1",
  cond_heat = "c51_2",
  cond_trans = "c51_3",
  cond_support = "c51_5",
  income = "c54",
  
  # behavioral variables
  behav_unplug = "c6_1",
  behav_products = "c6_2",
  behav_lights = "c6_3",
  behav_share = "c6_4",
  behav_carpool = "c6_5",
  behav_car = "c6_6",
  
  # knowledge variables
  know_energy = "c11_1",
  know_heating = "c11_2",
  know_costs = "c11_3",
  know_share = "c11_4",
  know_solutions = "c11_5",
  
  # energy access variables
  has_cooling = "c16", # include as yes/no or divide by option?
  has_heating = "c18", # include as yes/no or divide by option?
  heat_config = "c19",
  
  # energy affordability variables
  energy_cost = "c26",
  ability_to_pay = "c28_1",
  supplier_threat = "c28_2",
  safety_winter = "c28_3",
  safety_summer = "c28_4",
  
  # housing variables
  house_type = "c29",
  house_area = "c30",
  house_age = "c31",
  tenancy = "c49"
)


pov <- do.call(select, c(list(pov), vars_lookup, "id", "nuts0", "nuts1", "nuts2"))



## Recode and transform ----
pov <- pov %>%
  # Convert non-responses to NA
  mutate(across(
    where(is.factor),
    .fns = function(x) {
      lev <- levels(x)
      as.character(x) %>%
        na_if("I do not know") %>%
        na_if("Prefer not to say") %>%
        na_if("Do not know / prefer not to say") %>%
        na_if("Not applicable") %>%
        factor(levels = lev)
    }
  )) %>%
  # Unify binary codings
  mutate(across(where(is.logical), .fns = as.numeric)) %>%
  # Convert Yes/No to binary
  mutate(across(
    where(~identical(unique(na.omit(as.character(.x))), c("Yes", "No"))),
    .fns = ~case_match(.x, "Yes" ~ 1, "No" ~ 0, .default = NA)
  )) %>%
  mutate(
    heat_config = if_else(
      heat_config %in% "Central heating (heating the whole building where I live)",
      FALSE, TRUE
    ),
    has_cooling = if_else(
      has_cooling %in% "No",
      TRUE, FALSE
    ),
    has_heating = if_else(
      has_heating %in% "I dont use any heating system at home",
      TRUE, FALSE
    ),
    tenancy = if_else(
      tenancy %in% c("Owner, with mortgage or loan",
                     "Owner, no outstanding mortgage or housing loan"),
      FALSE, TRUE
    ),
    income = if_else(
      income %in% c(
        "Finding it very difficult to live on current income",
        "Finding it difficult to live on current income", 
        "Coping on current income"
      ),
      TRUE, FALSE
    ),
    gender = if_else(
      gender %in% "Male",
      TRUE, FALSE
    ),
    occupation = if_else(
      occupation %in% c(
        "In education or training", "Unemployed", "Retired",
        "Taking care of the home or family",
        "Ill or disabled for a long time or permanently"
      ),
      TRUE, FALSE
    ),
    education = if_else(
      education %in% c("Not completed primary", "Completed primary"),
      TRUE, FALSE
    ),
    house_type = if_else(
      house_type %in% c("Detached house", "Semi-detached house"),
      TRUE, FALSE
    )
  ) %>%
  rename(
    no_central = heat_config,
    lacks_cooling = has_cooling,
    lacks_heating = has_heating,
    is_tenant = tenancy,
    not_male = gender,
    unemployed = occupation,
    lacks_education = education,
    detached = house_type
  )

pov <- pov %>%
  mutate(across(
    where(is.factor),
    .fns = as.numeric
  )) %>%
  select(where(~!all(.x == 0, na.rm = TRUE)))


## Data summary ----
print(skim(st_drop_geometry(pov)))



## Prepare data ----
loc <- st_geometry(pov)

pov_for_pca <- pov %>%
  select(!any_of(c("id", "nuts0", "nuts1", "nuts2", "geometry"))) %>%
  st_drop_geometry() %>%
  # scale() %>%
  # as_tibble() %>%
  imputePCA() %>%
  extract2("completeObs") %>%
  as_tibble()

is_dichotomous <- function(x) {
  all(x %in% c(0, 1, NA))
}

is_polytomous <- function(x) {
  all(is.numeric(x) & length(unique(x)) < 8)
}

# Create a mix of polychoric, tetrachoric and pearson correlation matrices
corr <- mixedCor(
  c = names(select(pov_for_pca, !where(is_polytomous) & !where(is_dichotomous))),
  p = names(select(pov_for_pca, where(is_polytomous) & !where(is_dichotomous))),
  d = names(select(pov_for_pca, where(is_dichotomous))),
  data = pov_for_pca,
  global = FALSE
)



## Perform global PCA ----
# TWO-STAGE PCA
# BI-FACTOR MODEL
# 
# Idea: 
# 1. Create subindices using PCA
# 2. Combine subindices using PCA again
# 
# Why?
# Take into account the latent nature of energy poverty. Creating sub-indices
# allows for better control of the causality of energy poverty. It prevents
# hard-to-interpret spikes of individual variables that have only very weak
# causal links to energy poverty. For example, using all variables directly,
# single knowledge or behavioral variables frequently occupy the 'winning'
# spots in GWPCA or the first component in global PCA.
# Generally, PCA is biased towards highly correlated variables leading to
# groups of variables 'lumping together', e.g. knowledge and behavior dominating
# the first two components.
# 
# Mishra 2007:
# PCA loadings are highly elitist – preferring highly correlated
# variables to poorly correlated variables, irrespective of the (possible)
# contextual importance of the latter set of variables. On many occasions it
# is found that some (evidently) very important variables are roughly dealt
# with by the PCA simply because those variables exhibited widely distributed
# scatter or they failed to fall within a narrow band around a straight line.
# 
# Further reading:
# https://www.bbvaresearch.com/wp-content/uploads/2014/09/WP14-26_Financial-Inclusion1.pdf
# https://mpra.ub.uni-muenchen.de/3377/1/MPRA_paper_3377.pdf
# https://doi.org/10.1016/j.qref.2021.01.003
# https://doi.org/10.3390/su8121287
# https://doi.org/10.1111/rsp3.12607
# https://doi.org/10.1162/ASEP_a_00009
# https://doi.org/10.1007/s11205-018-1933-0
# https://doi.org/10.1007/978-3-319-60595-1_7
# https://doi.org/10.1007/s11135-022-01345-5

subind_vars <- list(
  afford = c("ability_to_pay", "supplier_threat", "safety_summer", "safety_winter", "income"),
  access = c("no_central", "lacks_heating", "lacks_cooling"),
  housing = c("detached", "house_area", "house_age", "is_tenant", "energy_cost"),
  social = c("not_male", "unemployed", "age", "lacks_education", "household"),
  cond = c("cond_support", "cond_trans", "cond_heat", "cond_air"),
  behav = c("behav_unplug", "behav_products", "behav_lights", "behav_share", "behav_carpool", "behav_car"),
  know = c("know_energy", "know_heating", "know_costs", "know_share", "know_solutions")
)

subind_nm <- list(
  afford = "Affordability",
  access = "Energy access",
  housing = "Housing precarity",
  social = "Disempowerment",
  cond = "Special conditions",
  behav = "Energy behavior",
  know = "Energy literacy"
)

pca <- lapply(subind_vars, function(x) {
  povdf <- pov_for_pca[x]
  # PCA(povdf, graph = FALSE)
  corr <- mixedCor(povdf)
  princomp(covmat = corr$rho)
})

pca_df <- lapply(pca, function(x) {
  x$ind$coord[, 1]
}) %>%
  bind_cols(.name_repair = "minimal") %>%
  setNames(subind_nm) %>%
  clean_names()

pca_index <- PCA(pca_df)
pca_index <- st_sf(index = pca_index$ind$coord[, 1], geometry = st_geometry(pov))
pca_index_agg <- aggregate(pca_index, readRDS("data-ext/bounds/nuts2.rds"), FUN = mean)
ggplot(pca_index_agg) +
  geom_sf(aes(fill = index), color = NA) +
  coord_sf(xlim = c(2500000, 6000000), ylim = c(1500000, 5200000)) +
  scale_fill_viridis_b()

scree_plots <- lapply(seq_along(pca), function(i) {
  pca <- pca[i]
  title <- subind_nm[[names(pca)]]
  fviz_eig(pca[[1]], "eigenvalue", title = title)
})
wrap_plots(scree_plots, ncol = 3)

pca_plots <- lapply(seq_along(pca), function(i) {
  pca <- pca[i]
  title <- subind_nm[[names(pca)]]
  fviz_pca_var(pca[[1]], title = title)
})
wrap_plots(pca_plots, ncol = 3)

cos2_plots <- lapply(seq_along(pca), function(i) {
  pca <- pca[i]
  title <- subind_nm[[names(pca)]]
  ggcorrplot(pca[[1]]$var$cos2) +
    theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8))
})
wrap_plots(cos2_plots, ncol = 3, guides = "collect")




gw_pca <- list()
bw <- list()

subindex_bw <- function(subindex) {
  bw.gwpca(
    data = as_Spatial(st_sf(pov_for_pca, geometry = loc)[subind_vars[[subindex]]]),
    vars = names(pov_for_pca[subind_vars[[subindex]]]),
    k = 1,
    kernel = "gaussian",
    robust = TRUE,
    adaptive = TRUE
  )
}

subindex_gwpca <- function(subindex, bw) {
  df <- pov_for_pca[subind_vars[[subindex]]]
  
  gwpca(
    data = as_Spatial(st_sf(df, geometry = loc)),
    elocat = as_Spatial(loc),
    vars = names(df),
    k = 1,
    bw = bw,
    kernel = "gaussian",
    adaptive = TRUE,
    cv = FALSE
  )
}


#subindex_bw("afford")
bw <- list(
  afford = 982, #
  access = 1264, #
  housing = 9122,
  social = 8467,
  cond = 5702,
  behav = 7557 #
)
gw_pca$afford <- subindex_gwpca("afford", bw$afford)
gw_pca$access <- subindex_gwpca("access", bw$access)
gw_pca$housing <- subindex_gwpca("housing", bw$housing)
gw_pca$social <- subindex_gwpca("social", bw$social)
gw_pca$cond <- subindex_gwpca("cond", bw$cond)
gw_pca$behav <- subindex_gwpca("behav", bw$behav)
gw_pca$know <- subindex_gwpca("know", bw$know)
#gw_pca$access <- NULL

rescale_minmax <- function(.x) {
  (.x - min(.x)) / (max(.x) - min(.x))
}

t <- lapply(gw_pca, function(x) {
  out <- st_as_sf(x$SDF)
  # X <- mutate(
  #   pov_for_pca,
  #   across(everything(), .fns = rescale_minmax))
  # )
  
  load <- scale(x$loadings[, , 1])
  load <- apply(x$loadings[, , 1], 1, mean)
  out <- bind_cols(out, index = load)
  out
})


plot_gwpca <- function(x, var = "Comp.1_PV") {
  title <- subind_nm[names(gw_pca)[[get("i", envir = parent.frame())]]]
  legend <- switch(
    var,
    "Comp.1_PV" = "Proportion of variance",
    "index" = "Index",
    win_var_PC1 = "Winning variable"
  )
  ggplot(x) +
    geom_sf(aes(color = .data[[var]]), size = 0.2) +
    coord_sf(xlim = c(2500000, 6000000), ylim = c(1500000, 5200000)) +
    scale_color_viridis_c(sprintf("**%s**<br>%s", title, legend)) +
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
    ) +
    guides(color = guide_colorbar(title.position = "top"))
}

gwpca_pv1_plots <- lapply(gw_pca, plot_gwpca, var = "Comp.1_PV")
gwpca_pc1_plots <- lapply(t, plot_gwpca, var = "index")

wrap_plots(gwpca_pv1_plots, ncol = 3)
ggsave("data-raw/energy_poverty/gwpca_pv.png")
wrap_plots(gwpca_pc1_plots, ncol = 3)
ggsave("data-raw/energy_poverty/gwpca_index.png")


GGally::ggmatrix(
  setNames(gwpca_pc1_plots, subind_nm),
  nrow = 3,
  ncol = 3,
  showAxisPlotLabels = FALSE,
  showStrips = TRUE
)


gw_subind <- lapply(t, function(x) {
  x <- st_drop_geometry(x)
  x <- x["index"]
  names(x) <- names(gw_pca)[get("i", envir = parent.frame())]
  x
}) %>%
  bind_cols() %>%
  st_sf(geometry = loc)

gw_index <- gwpca(
  data = as_Spatial(gw_subind),
  elocat = as_Spatial(loc),
  vars = names(st_drop_geometry(gw_subind)),
  k = 1,
  bw = 500,
  kernel = "gaussian",
  adaptive = TRUE,
  scores = TRUE
)

gw_index <- gwpca(
  data = as_Spatial(st_sf(pov_for_pca, geometry = loc)),
  elocat = as_Spatial(loc),
  vars = names(pov_for_pca),
  k = 1,
  bw = 500,
  kernel = "gaussian",
  adaptive = TRUE,
  scores = TRUE
)

t <- gw_index
t$SDF <- st_as_sf(t$SDF)
t$SDF <- bind_cols(t$SDF, index = t$loadings[, "social", 1])
ggplot(t$SDF) + geom_sf(aes(color = index))



gwpca_df <- lapply(gw_pca, function(x) {
  st_drop_geometry(st_as_sf(x$SDF)["Comp.1_PV"])
}) %>%
  bind_cols(.name_repair = "minimal") %>%
  setNames(subind_nm) %>%
  st_sf(geometry = loc)

compbw <- bw.gwpca(
  as_Spatial(gwpca_df),
  vars = names(st_drop_geometry(gwpca_df)),
  k = 1,
  kernel = "gaussian",
  adaptive = TRUE
)
compindex <- gwpca(
  as_Spatial(clean_names(gwpca_df)),
  elocat = as_Spatial(loc),
  vars = make_clean_names(names(st_drop_geometry(gwpca_df))),
  k = 1,
  bw = 500,
  kernel = "gaussian",
  adaptive = TRUE
)



## Find optimal bandwidth ----
bw_gwpca <- bw.gwpca(
  as_Spatial(st_sf(pov_for_pca, geometry = loc)),
  vars = names(pov_for_pca),
  k = 5,
  kernel = "exp",
  adaptive = TRUE
)


# GWEFA
gw_efa <- gwfa(
  as_Spatial(st_sf(pov_for_pca2, geometry = loc)),
  elocat = as_Spatial(loc),
  vars = names(pov_for_pca),
  bw = 1000,
  kernel = "gaussian",
  adaptive = TRUE,
  cor = "mixed",
  timeout = 20
)


## Non-negative approach ----
## (takes a loooong time, ~16 hours)
options(future.globals.maxSize = +Inf)
#handlers("cli")
with_progress({
  gw_pca <- gw_nsprcomp(
    as_Spatial(st_sf(pov_for_pca, geometry = loc)),
    elocat = as_Spatial(loc),
    vars = names(pov_for_pca),
    bw = 1000 / nrow(pov_for_pca),
    k = 5,
    kernel = "gaussian",
    adaptive = TRUE,
    workers = 4
  )
})


## Traditional approach ----
## (takes 5 minutes)
gw_pca <- gwpca(
  as_Spatial(st_sf(pov_for_pca, geometry = loc)),
  elocat = as_Spatial(loc),
  vars = names(pov_for_pca),
  bw = 1000,
  k = 5,
  adaptive = TRUE,
  kernel = "gaussian"
)

## Winning variables ----
win1 <- colnames(gw_pca$loadings)[max.col(abs(gw_pca$loadings[, , 1]))] %>%
  st_sf(winner = ., geometry = loc)
win2 <- colnames(gw_pca$loadings)[max.col(abs(gw_pca$loadings[, , 2]))] %>%
  st_sf(winner = ., geometry = loc)
win3 <- colnames(gw_pca$loadings)[max.col(abs(gw_pca$loadings[, , 3]))] %>%
  st_sf(winner = ., geometry = loc)
win4 <- colnames(gw_pca$loadings)[max.col(abs(gw_pca$loadings[, , 4]))] %>%
  st_sf(winner = ., geometry = loc)
win <- bind_rows(PC1 = win1, PC2 = win2, PC3 = win3, PC4 = win4, .id = "comp")

ggplot(win) +
  geom_sf(aes(color = winner), key_glyph = "rect", size = 0.2) +
  facet_wrap(~comp) +
  theme_bw()
ggsave("data-raw/energy_poverty/gwpca_winner.png", bg = "white")

ggcharts::bar_chart(win, winner, facet = comp, fill = comp) + guides(fill="none")
ggsave("data-raw/energy_poverty/gwpca_winner_counts.png")


## Index composition ----
# Geometric mean of PC1-5 for all locations
evindex <- map_dbl(seq_len(nrow(gw_pca$loadings)), ~exp(mean(log(abs(gw_pca$loadings[.x, , 1]))))) %>%
  st_sf(index = ., geometry = loc)

ggplot(evindex) +
  geom_sf(aes(color = index * 100), size = 0.7) +
  scale_color_viridis_b() +
  theme_bw()
ggsave("data-raw/energy_poverty/gwpca_index.png")

# Package management ----
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



## Fix some functions ----
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


## Custom functions ----
source("index_fun.R")


# Read data ----
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


# Pre-processing ----
## Merge dummies ----
for (v in all_vars) {
  cb_v <- cb[cb$og_var %in% v, ]
  opts <- cb_v$option
  ext_var <- cb_v$variable
  if (!all(is.na(opts)) && length(opts) > 1) {
    # exchange 1/0 with actual values
    for (o in seq_along(opts)) {
      col <- which(grepl(v, ext_var))[[o]]
      pov[ext_var[col]] <- if_else(
        pov[ext_var[col]][[1]] == 1, opts[o], NA
      )
    }
    # combine dummy columns to a single one
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
  ### Convert non-responses to NA ----
  mutate(across(
    where(is.factor),
    .fns = filter_non_responses
  )) %>%
  ### Unify binary codings ----
  mutate(across(where(is.logical), .fns = as.numeric)) %>%
  ### Convert Yes/No to binary ----
  mutate(across(
    where(~identical(unique(na.omit(as.character(.x))), c("Yes", "No"))),
    .fns = ~case_match(.x, "Yes" ~ 1, "No" ~ 0, .default = NA)
  )) %>%
  ### Recode categorical variables ----
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

### Convert factors to ordinal
pov <- pov %>%
  mutate(across(
    where(is.factor),
    .fns = as.numeric
  )) %>%
  select(where(~!all(.x == 0, na.rm = TRUE)))


## Data summary ----
print(skim(st_drop_geometry(pov)))



## Prepare data for PCA ----
loc <- st_geometry(pov)

pov_for_pca <- pov %>%
  select(!any_of(c("id", "nuts0", "nuts1", "nuts2", "geometry"))) %>%
  st_drop_geometry() %>%
  # scale() %>%
  # as_tibble() %>%
  imputePCA() %>%
  extract2("completeObs") %>%
  as_tibble()

# Create a mix of polychoric, tetrachoric and pearson correlation matrices
corr <- mixedCor(
  c = names(select(pov_for_pca, !where(is_polytomous) & !where(is_dichotomous))),
  p = names(select(pov_for_pca, where(is_polytomous) & !where(is_dichotomous))),
  d = names(select(pov_for_pca, where(is_dichotomous))),
  data = pov_for_pca,
  global = FALSE
)




# Global index creation ----
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

## Define subindex indicators ----
subind_vars <- list(
  afford = c("ability_to_pay", "supplier_threat", "safety_summer", "safety_winter", "income"),
  access = c("no_central", "lacks_heating", "lacks_cooling"),
  housing = c("detached", "house_area", "house_age", "is_tenant", "energy_cost"),
  social = c("not_male", "unemployed", "lacks_education", "household"),
  cond = c("cond_support", "cond_trans", "cond_heat", "cond_air"),
  behav = c("behav_unplug", "behav_products", "behav_lights", "behav_share", "behav_carpool", "behav_car"),
  know = c("know_energy", "know_heating", "know_costs", "know_share", "know_solutions")
)

## Define subindex names ----
subind_nm <- list(
  afford = "Energy insecurity",
  access = "Energy exclusion",
  housing = "Housing precarity",
  social = "Disempowerment",
  cond = "Disability",
  behav = "Energy behavior",
  know = "Energy literacy"
)


## Perform 1st stage PCA ----
pca <- lapply(subind_vars, function(x) {
  povdf <- pov_for_pca[x]
  # PCA(povdf, graph = FALSE)
  corr <- mixedCor(povdf)
  princomp(covmat = corr$rho)
})

## Tidy PCA results
pca_df <- lapply(pca, function(x) {
  x$ind$coord[, 1]
}) %>%
  bind_cols(.name_repair = "minimal") %>%
  setNames(subind_nm) %>%
  clean_names()

## Perform 2nd stage PCA
pca_index <- PCA(pca_df)
pca_index <- st_sf(index = pca_index$ind$coord[, 1], geometry = st_geometry(pov))
pca_index_agg <- aggregate(pca_index, readRDS("data-ext/bounds/nuts2.rds"), FUN = mean)

## Plot PCA ----
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




# Local index creation ----
gw_pca <- list()
bw <- list()


## Identify 1st stage bandwidths ----
# subindex_bw("afford")
# error: system is computationally singular: reciprocal condition number
bw <- list(
  afford = 1200, #
  access = 1264, #
  housing = 1400, #
  social = 3000, #
  cond = 4686, #
  behav = 7557, #
  know = 1700 #
)

## Perform 1st stage GWPCA
gw_pca$afford <- subindex_gwpca("afford", bw$afford)
gw_pca$access <- subindex_gwpca("access", bw$access)
gw_pca$housing <- subindex_gwpca("housing", bw$housing)
gw_pca$social <- subindex_gwpca("social", bw$social)
gw_pca$cond <- subindex_gwpca("cond", bw$cond)
gw_pca$behav <- subindex_gwpca("behav", bw$behav)
gw_pca$know <- subindex_gwpca("know", bw$know)
#gw_pca$access <- NULL

## Tidy GWPCA results ----
gw_pca_tidy <- lapply(gw_pca, tidy_gwpca)

## Plot GWPCA ----
gwpca_pv1_plots <- lapply(gw_pca_tidy, plot_gwpca, var = "Comp.1_PV")
gwpca_pc1_plots <- lapply(gw_pca_tidy, plot_gwpca, var = "index")
gwpca_win_plots <- lapply(gw_pca_tidy, plot_gwpca, var = "win_var_PC1")

wrap_plots(gwpca_pv1_plots, ncol = 3)
ggsave("data-raw/energy_poverty/gwpca_pv.png", width = 12, height = 12)
wrap_plots(gwpca_pc1_plots, ncol = 3)
ggsave("data-raw/energy_poverty/gwpca_index.png", width = 12, height = 12)
wrap_plots(gwpca_win_plots, ncol = 3)
ggsave("data-raw/energy_poverty/gwpca_winner.png", width = 12, height = 12)


## Tidy subindex data ----
gw_subind <- lapply(gw_pca_tidy, function(x) {
  x <- st_drop_geometry(x)
  x <- x["index"]
  names(x) <- names(gw_pca)[get("i", envir = parent.frame())]
  x
}) %>%
  bind_cols() %>%
  st_sf(geometry = loc)


## Identify 2nd stage bandwidths ----
# bw_index <- bw.gwpca(
#   data = as_Spatial(gw_subind),
#   vars = names(st_drop_geometry(gw_subind)),
#   k = 1,
#   robust = TRUE,
#   kernel = "gaussian",
#   adaptive = TRUE
# )

# 1195
bw_index <- 1195

## Perform 2nd stage GWPCA ----
gw_index <- gwpca(
  data = as_Spatial(st_sf(scale(st_drop_geometry(gw_subind)), geometry = loc)),
  elocat = as_Spatial(loc),
  vars = names(st_drop_geometry(gw_subind)),
  k = 1,
  bw = bw_index,
  robust = TRUE,
  kernel = "gaussian",
  adaptive = TRUE
)

gw_index <- tidy_gwpca(gw_index)
plot_gwpca(gw_index, var = "Comp.1_PV", title = "Energy vulnerability")
plot_gwpca(gw_index, var = "index", title = "Energy vulnerability")
plot_gwpca(gw_index, var = "win_var_PC1", title = "Energy vulnerability")

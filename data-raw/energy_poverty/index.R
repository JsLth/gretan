if (!require(pacman)) {
  install.packages("pacman")
}

pacman::p_load(
  # data wrangling
  dplyr,
  stringr,
  purrr,
  tidyr,
  
  # spatial data
  sf,
  
  # PCA
  missMDA,
  factoextra,
  FactoMineR,
  psych,
  
  # GW PCA
  GWmodel,
  GWnnegPCA,
  
  # Visualization
  skimr,
  kableExtra,
  ggcorrplot,
  ggcharts,
  RColorBrewer,
  viridis,
  patchwork,
  
  # Meta
  cli,
  magrittr
)


## Read data ----
pov <- readRDS("data-ext/survey_resampled.rds") %>%
  select(starts_with(c(
    "d1", # Gender
    "d2", # Occupation
    "c1", # Age
    "c4", # Education
    "c6_",  # Energy saving #1
    "c11", # Energy complexity
    "c13", # Energy saving #2
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
  behav_temp = "c13_1",
  behav_shower = "c13_2",
  behav_drive = "c13_3",
  behav_dishwash = "c13_4",
  
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
print(skimr::skim(pov))



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
  data = pov_for_pca
)



## Perform global PCA ----
pca <- princomp(
  as.formula(paste("~", paste(names(pov_for_pca), collapse = " + "))),
  data = pov_for_pca
)

pca <- princomp(covmat = corr$rho)

pca <- nsprcomp::nsprcomp(
  as.formula(paste("~", paste(names(pov_for_pca), collapse = " + "))),
  data = pov_for_pca,
  nneg = TRUE
)

pca <- PCA(corr$rho, ncp = ncol(pov_for_pca), graph = FALSE)

pca <- principal(corr$rho, nfactors = 10, cor = "mixed")


## Extract info from PCA ----
eig <- get_eigenvalue(pca)
var <- get_pca_var(pca)
pca_sel <- eig[, "eigenvalue"] > 1
ci <- seq_len(sum(pca_sel))
comp_names <- str_replace(colnames(pca$loadings), substr(colnames(pca$loadings), 2, 5), "")[ci]


## Clustered bi-plot ----
## Identify clusters from PCA coordinates and generate a biplot
set.seed(10164987)
clus <- kmeans(var$coord, centers = 5, nstart = 25)
grps <- cut(
  clus$cluster,
  breaks = 5,
  labels = c("Impairments", "Living conditions", "Disadvantage", "Knowledge", "Behavior")
)
fviz_pca_var(pca, col.var = grps, legend.title = "Cluster") +
  ggtitle(NULL) +
  theme_bw()
ggsave("data-raw/energy_poverty/pca_var_clus.png", bg = "white")


## Scree test ----
fviz_eig(pca, "eigenvalue", geom = "line", addlabels = TRUE, ncp = 20) +
  ggtitle(NULL) +
  theme_bw()
ggsave("data-raw/energy_poverty/pca_scree.png", bg = "white")


## PCA diagnostics ----
## Plot correlation, cos2 and rotation vector in a matrix
p1 <- ggcorrplot::ggcorrplot(
  var$cor[, ci],
  colors = RColorBrewer::brewer.pal(3, "PRGn"),
  legend.title = "Pearson coef.",
  ggtheme = theme_bw
) +
  scale_y_discrete(labels = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10")) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 10)
  )

p2 <- ggcorrplot::ggcorrplot(
  var$cos2[, ci],
  colors = RColorBrewer::brewer.pal(3, "PRGn"),
  legend.title = "Sq. Cosine",
  ggtheme = theme_bw
) +
  scale_y_discrete(labels = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10")) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 10)
  )

p3 <- ggcorrplot::ggcorrplot(
  sweep(var$coord, 2, sqrt(eig[1:ncol(var$coord), 1]), FUN = "/")[, ci],
  colors = RColorBrewer::brewer.pal(3, "PRGn"),
  legend.title = "Loadings",
  ggtheme = theme_bw
) +
  scale_y_discrete(labels = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10")) +
  theme(axis.text.x = element_text(size = 10, angle = 90), axis.text.y = element_text(size = 10))

wrap_plots(p1, p2, p3, nrow = 3) +
  plot_layout(guides = "collect")
ggsave("data-raw/energy_poverty/pca_corplot.png")


## Add progress bar to GW functions (and fix a bug)
body(gwpca)[[26]][[3]] <- quote(cli::cli_progress_along(1:ep.n))
#body(gw_nsprcomp)[[27]][[3]] <- quote(cli::cli_progress_along(1:ep.n))
#body(gw_nsprcomp)[[27]][[4]][[8]][[3]] <- quote(temp$rotation[var.n, k])

## Find optimal bandwidth ----
bw_gwpca <- bw.gwpca(
  as_Spatial(st_sf(pov_for_pca, geometry = loc)),
  vars = names(pov_for_pca),
  k = 5,
  kernel = "exp",
  adaptive = TRUE
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

library(readr)
library(janitor)
library(dplyr)
library(purrr)
library(sf)
library(fastDummies)


undo_dummy <- function(df, col_prefix) {
  is_sf <- inherits(df, "sf")
  if (is_sf) {
    geom <- st_geometry(df)
    df <- st_drop_geometry(df)
  }
  cols <- names(df)[names(df) %>% startsWith(col_prefix)]
  base_level.idx <- rowSums(df[cols]) == 0
  df[!base_level.idx, col_prefix] <- cols[apply(
    df[!base_level.idx, cols], 1, which.max
  )]
  df[cols] <- NULL
  if (is_sf)
    df <- st_sf(df, geometry = geom)
  df
}


clusters <- read_csv2("data-ext/persona/persona_clusters.csv") %>%
  select(uuid = "Respondent code", cluster = cluster_country) %>%
  mutate(cluster = if_else(cluster == 0, 1, cluster))

cb <- readRDS("data-ext/codebook.rds")
survey_local <- readRDS("data-ext/survey_local.rds") %>%
  select(
    uuid,
    all_of(cb[cb$og_var %in% "c63_1", ]$variable),
    all_of(cb[cb$og_var %in% "c59_1", ]$variable),
    all_of(cb[cb$og_var %in% "c44_2", ]$variable),
    all_of(cb[cb$og_var %in% "c12_5", ]$variable),
    all_of(cb[cb$og_var %in% "c68_2", ]$variable)
  ) %>%
  undo_dummy("c12_5") %>%
  select(
    uuid,
    q1 = c63_1,
    q2 = c59_1,
    q3 = c44_2,
    q4 = c12_5,
    q5 = c68_2
  )

dict <- list(
  c12_5_significantly_decrease = 1, c12_5_decrease = 2,
  c12_5_same_as_today = 3, c12_5_increase = 4,
  c12_5_significantly_increase = 5, c12_5_prefer_not_to_say = NA_real_
)

survey_local$q4 <- unlist(dict[survey_local$q4])

survey_local <- left_join(survey_local, clusters, by = "uuid", multiple = "first")

geom <- st_geometry(survey_local)
survey_local <- st_drop_geometry(survey_local)
survey_local <- fastDummies::dummy_cols(
  survey_local,
  select_columns = setdiff(names(survey_local), "uuid"),
  ignore_na = TRUE
) %>%
  mutate(across(everything(), .fns = function(.x) {
    if (all(.x %in% c(0, 1, NA))) as.logical(.x) else .x
  })) %>%
  as_tibble()

survey_local <- st_sf(survey_local, geometry = geom) %>%
  as_tibble() %>%
  st_as_sf()

nuts0 <- readRDS("data-ext/bounds/nuts0.rds")
nuts1 <- readRDS("data-ext/bounds/nuts1.rds")
nuts2 <- readRDS("data-ext/bounds/nuts2.rds")

survey_local <- survey_local %>%
  st_join(select(nuts0, nuts0 = "name"), st_nearest_feature) %>%
  st_join(select(nuts1, nuts1 = "name"), st_nearest_feature) %>%
  st_join(select(nuts2, nuts2 = "name"), st_nearest_feature) %>%
  mutate(
    q1 = if_else(q1 == 8, NA, q1),
    q4 = if_else(q4 == 8, NA, q4),
    q5 = if_else(q5 == 6, NA, q5)
  )

"%len%" <- function(x, y) if (length(x)) x else y 

agg_persona <- function(x) {
  if (is.logical(x)) {
    mean(x, na.rm = TRUE)
  } else if (is.numeric(x)) {
    as.integer(names(sort(table(x), decreasing = TRUE))[1]) %len% NA
  } else {
    unique(x)
  }
}

srv_nuts0 <- aggregate(
  survey_local %>% select(-uuid, -nuts1, -nuts2),
  nuts0,
  FUN = agg_persona
) %>%
  dplyr::as_tibble() %>%
  sf::st_as_sf()

srv_nuts1 <- aggregate(
  survey_local %>% select(-uuid, -nuts2),
  nuts1,
  FUN = agg_persona
) %>%
  dplyr::as_tibble() %>%
  sf::st_as_sf() %>%
  filter(!is.na(nuts0))

srv_nuts2 <- aggregate(
  survey_local %>% select(-uuid),
  nuts2,
  FUN = agg_persona
) %>%
  dplyr::as_tibble() %>%
  sf::st_as_sf() %>%
  filter(!is.na(nuts0))

saveRDS(
  list(nuts0 = srv_nuts0, nuts1 = srv_nuts1, nuts2 = srv_nuts2),
  "inst/extdata/persona.rds"
)

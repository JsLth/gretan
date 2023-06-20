library(pacman)

pacman::p_load(
  "giscoR", "dplyr", "cli", "sf"
)

if (!exists("nuts3")) {
 nuts3 <- giscoR::gisco_get_nuts(
   year = "2021",
   nuts_level = "3",
   country = countries,
   epsg = "3035",
   resolution = "01",
   spatialtype = "RG",
   cache = TRUE
 ) %>%
    st_transform(3035)
}

survey_local <- readRDS("survey_local.rds")

pov <- survey_local %>%
  select(starts_with(c("c16", "c18", "c22", "c24", "c26", "c28",
                       "c29", "c30", "c31", "c32", "c55")))
cohesion <- survey_local %>%
  select(starts_with(c("c39", "c46")))

trust <- survey_local %>%
  select(starts_with("c47"))

nid3 <- nuts3$nid[st_nearest_feature(survey_local, nuts3)]
survey_local <- bind_cols(survey_local, nuts3 = nid3)

cli::cli_progress_bar(
  name = "Sampling NUTS-3",
  total = length(unique(survey_local$nuts3))
)
cenv <- environment()

survey_jittered <- survey_local %>%
  group_by(nuts3) %>%
  group_map(function(x, group) {
    cli_progress_update(.envir = cenv)
    name <- group[[1]]
    st_sample(nuts3[nuts3$nid %in% name, ]$geometry, nrow(x))
  }) %>%
  do.call(c, .)

gridid <- grid$GRD_ID[st_nearest_feature(survey_local, grid)]
survey_local <- bind_cols(survey_local, grid = gridid)

cli::cli_progress_bar(
  name = "Sampling INSPIRE grid",
  total = length(unique(survey_local$grid))
)
cenv <- environment()

survey_jittered <- survey_local %>%
  group_by(grid) %>%
  group_map(function(x, group) {
    cli::cli_progress_update(.envir = cenv)
    id <- group[[1]]
    st_sample(grid[grid$GRD_ID %in% id, ]$geometry, nrow(x))
  }) %>%
  do.call(c, .)
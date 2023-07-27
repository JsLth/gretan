library(pacman)

pacman::p_load(
  "giscoR", "dplyr", "cli", "sf", "terra"
)

# Get strata
lau <- giscoR::gisco_get_lau(year = "2020", epsg = "3035", cache = TRUE)

# Get survey data
survey_local <- readRDS("data-ext/survey_local.rds")

# Get population data
popgrid <- terra::vect("C:/Users/PC/Downloads/grid_1km_surf.gpkg")
grid <- terra::rast(
  popgrid,
  crs = terra::crs(t),
  extent = terra::ext(t),
  resolution = 1
)
popgrid <- terra::rasterize(
  x = popgrid,
  y = grid,
  field = "TOT_P_2021",
  fun = mean,
  na.rm = TRUE
)


pov <- survey_local %>%
  select(starts_with(c("c16", "c18", "c22", "c24", "c26", "c28",
                       "c29", "c30", "c31", "c32", "c55")))
cohesion <- survey_local %>%
  select(starts_with(c("c39", "c46")))

trust <- survey_local %>%
  select(starts_with("c47"))

lid <- lau$LAU_ID[st_nearest_feature(survey_local, lau)]
survey_local <- bind_cols(survey_local, lid = lid)

cli::cli_progress_bar(
  name = "Sampling NUTS-3",
  total = length(unique(survey_local$nuts3))
)
cenv <- environment()

## Classic approach
## Single stratified spatial sampling
# survey_jittered <- survey_local %>%
#   group_by(nuts3) %>%
#   group_map(function(x, group) {
#     cli_progress_update(.envir = cenv)
#     name <- group[[1]]
#     st_sample(nuts3[nuts3$nid %in% name, ]$geometry, nrow(x))
#   }) %>%
#   do.call(c, .)

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
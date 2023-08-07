library(pacman)

pacman::p_load(
  "giscoR", "dplyr", "cli", "sf", "terra"
)

# Get admin boundaries
cli_alert_info("Downloading LAU")
lau <- giscoR::gisco_get_lau(year = "2020", epsg = "3035", cache = TRUE)

# Get survey data
cli_alert_info("Loading survey data")
survey_local <- readRDS("data-ext/survey_local.rds")

# Get population data
# popgrid <- vect("grid_1km_point.gpkg")
# grid <- rast(
#   popgrid,
#   crs = crs(popgrid),
#   extent = ext(popgrid),
#   resolution = 1000
# )
# popgrid <- rasterize(
#   x = popgrid,
#   y = grid,
#   field = "TOT_P_2021",
#   filename = "data-ext/popgrid_1km.tif",
#   overwrite = TRUE
# )
popgrid <- rast("data-ext/popgrid_1km.tif")


pov <- survey_local %>%
  select(starts_with(c("c16", "c18", "c22", "c24", "c26", "c28",
                       "c29", "c30", "c31", "c32", "c55")))
cohesion <- survey_local %>%
  select(starts_with(c("c39", "c46")))

trust <- survey_local %>%
  select(starts_with("c47"))

lid <- lau$LAU_ID[st_nearest_feature(survey_local, lau)]
survey_local <- bind_cols(survey_local, lid = lid)

survey_local <- survey_local %>%
  mutate(place_type = case_when(
    c5_village_small_town_less_than_10_000_people == 1 ~ "Village",
    c5_medium_large_town_10_000_to_100_000_people == 1 ~ "Town",
    c5_city_over_100_000_people == 1 ~ "City",
    c5_very_large_city_over_1_000_000_people == 1 ~ "Large city"
  ))



## Classic approach
## Single stratified spatial sampling
# cli::cli_progress_bar(
# name = "Sampling NUTS-3",
# total = length(unique(survey_local$nuts3))
# )
# cenv <- environment()
# survey_jittered <- survey_local %>%
#   group_by(nuts3) %>%
#   group_map(function(x, group) {
#     cli_progress_update(.envir = cenv)
#     name <- group[[1]]
#     st_sample(nuts3[nuts3$nid %in% name, ]$geometry, nrow(x))
#   }) %>%
#   do.call(c, .)


# Two-stage stratified spatial sampling
cli_progress_bar(
  name = "Sampling by LAU",
  total = length(unique(survey_local$lid))
)
cenv <- environment()

faulty_lids <- NULL
survey_sampled <- survey_local %>%
  group_by(lid) %>%
  group_map(function(by_lau, group) {
    try(cli_progress_update(.envir = cenv))
    lau_id <- group$lid
    geom <- lau[lau$LAU_ID %in% lau_id, ]
    poprast <- crop(popgrid, geom, mask = TRUE)
    by_place <- by_lau %>%
      group_by(place_type) %>%
      group_map(function(by_size, group) {
        th_dict <- list(
          "Village" = 0,
          "Town" = 100,
          "City" = 300,
          "Large city" = 1500
        )
        threshold <- th_dict[[group$place_type]]
        # If there are less cells above the selected threshold, go down one
        # level (e.g. large city to city) until sampling is possible.
        # There are five reasons this might be happening:
        # - Survey error: Respondents gave the wrong answer
        # - Sleaziness error: Respondents say they live in a city when they
        # actually live in a rural area near the city
        # - Perception error: Mismatch between typology thresholds and
        # respondents perception. Respondents say their town is a city.
        # - Georeferencing error: Mismatch between LAU boundaries and
        # respondents' perception
        # - Resolution error: Resolution is too coarse to uncover the place type
        # respondents refer to
        while (!length(c(poprast[poprast > threshold]))) {
          faulty_lids <<- c(faulty_lids, lau_id)
          new_index <- which(th_dict == threshold) - 1
          if (new_index == 0) stop("No grid cells to sample in.")
          threshold <- th_dict[[new_index]]
        }
        popgrid <- st_as_sf(as.polygons(poprast))
        popgrid <- popgrid[popgrid$mean >= threshold, ]
        samp <- st_sample(popgrid, size = nrow(by_size), type = "random")
        st_geometry(by_size) <- samp
        by_size
      }) %>%
      bind_rows()
  }) %>%
  bind_rows()

saveRDS(survey_sampled, file = "data-ext/survey_resampled.rds")

# Questions:
# - Population density raster or human settlement data? (perhaps with voronoi
# polygons for maximum coverage). There is a problem with assuming that
# population density corresponds to place size information as population
# density can spike in small areas whereas place sizes usually refer to
# urban, peri-urban or rural clusters. Eurostat solves this by not only taking
# into account population density, but contiguous clusters of high population
# density resulting in their urban-rural typology (which is only available
# at LAU level sadly). A solution to this could be using human settlement data
# and creating a continuous spatial dataset by combining settlement points
# using voronoi polygons.
# 
# - Polygonize raster before sampling? This would solve the problem that for
# one raster cell only one point can be sampled. This is particularly
# problematic when small, but populated places (e.g. Espinho, Portugal) have
# many respondents, but not enough raster cells to sample all the points.
# This could be solved by either using polygons (where multiple sampled
# points can fall into one grid cell) or by using a downsampled population
# raster (at the cost of accuracy)
# 
# - What if urban-rural typologies don't match respondents answers? In some
# cases respondents say they live in a large city when in reality they live
# in a rural area.

# plot differences
facet_points <- bind_rows(Original = survey_local,
                          Geoimputed = survey_sampled, .id = "stage") %>%
  mutate(stage = factor(stage, levels = c("Original", "Geoimputed")))

facet_bounds <- lau %>%
  mutate(nuts0 = countrycode::countrycode(CNTR_CODE, "eurostat", "country.name")) %>%
  filter(nuts0 %in% unique(facet_points$nuts0)) %>%
  group_by(nuts0) %>%
  summarise(geometry = st_union(geometry)) %>%
  bind_rows(Original = ., Geoimputed = ., .id = "stage") %>%
  mutate(stage = factor(stage, levels = c("Original", "Geoimputed")))

ps <- list()
for (stage in unique(facet_bounds$stage)) {
  for (country in unique(facet_bounds[facet_bounds$stage %in% stage, ]$nuts0)) {
    ps[[paste0(stage, "_", country)]] <- ggplot() +
      geom_sf(data = facet_bounds[facet_bounds$stage %in% stage & facet_bounds$nuts0 %in% country, ]) +
      geom_sf(data = facet_points[facet_points$stage %in% stage & facet_points$nuts0 %in% country, ]) +
      coord_sf(crs = st_crs(3035)) +
      theme_bw()
  }
}

p <- GGally::ggmatrix(
  ps, nrow = 2,
  ncol = 16,
  xAxisLabels = unique(facet_bounds$nuts0),
  yAxisLabels = c("Original", "Geoimputed")
)

ggsave("geoimp_test.png", plot = p, width = 40, height = 10)

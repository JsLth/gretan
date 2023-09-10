library(pacman)

pacman::p_load(
  "giscoR", "dplyr", "cli", "sf", "terra", "ggplot2", "GGally"
)

# Get admin boundaries
cli_alert_info("Downloading LAU")
lau <- giscoR::gisco_get_lau(year = "2020", epsg = "3035")

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

clid <- lau$GISCO_ID[st_nearest_feature(survey_local, lau)]
survey_local <- bind_cols(survey_local, clid = clid)

survey_local <- survey_local %>%
  mutate(place_type = case_when(
    c5_village_small_town_less_than_10_000_people == 1 ~ "Village",
    c5_medium_large_town_10_000_to_100_000_people == 1 ~ "Town",
    c5_city_over_100_000_people == 1 ~ "City",
    c5_very_large_city_over_1_000_000_people == 1 ~ "Large city"
  ))



# Two-stage stratified spatial sampling
cli_progress_bar(
  name = "Sampling by LAU",
  total = length(unique(survey_local$clid))
)
cenv <- environment()

popgrid <- focal(
  popgrid,
  fun = "mean",
  na.rm = TRUE,
  na.policy = "omit"
)

survey_sampled <- survey_local %>%
  group_by(clid) %>%
  group_map(function(by_lau, unit) {
    try(cli_progress_update(.envir = cenv), silent = TRUE)
    lau_id <- unit$clid

    # Select geometry of primary sampling unit (PSU)
    geom <- vect(lau[lau$GISCO_ID %in% lau_id, ]$geometry)

    # Crop population grid to PSU geometry
    psu <- popgrid %>%
      crop(geom, mask = TRUE) %>%
      as.polygons() %>%
      intersect(geom)
    by_lau %>%
      group_by(place_type) %>%
      group_map(function(by_size, place) {
        # Match place type to population grid
        th_dict <- list(
          "Village" = c(0, 299),
          "Town" = c(300, 799),
          "City" = c(800, 1999),
          "Large city" = c(2000, Inf)
        )
        threshold <- th_dict[[place$place_type]]

        # If there are less cells above the selected threshold, go down one
        # level (e.g. large city to city) or up one level until sampling is
        # possible.
        index <- index0 <- which(th_dict %in% list(threshold))
        while (!nrow(psu[between(psu$focal_mean, threshold[1], threshold[2])])) {
          if (index0 > 2) index <- index - 1
          if (index0 <= 2) index <- index + 1
          if (between(index, 1, 4)) {
            threshold <- th_dict[[index]]
          } else {
            threshold <- c(0, Inf)
          }
        }

        # Filter PSU to secondary sampling unit (SSU)
        ssu <- psu[between(psu$focal_mean, threshold[1], threshold[2])]

        # Take a sample within SSU
        samp <- NULL
        add <- 0
        while (!length(samp) >= nrow(by_size)) {
          set.seed(45468575)
          samp <- spatSample(ssu, size = nrow(by_size) + add, method = "random")
          add <- add + 1
        }

        if (length(samp) > nrow(by_size)) {
          samp <- samp[1:nrow(by_size)]
        }

        # Replace geometries of geocoded dataset
        st_geometry(by_size) <- st_geometry(st_as_sf(samp))
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
facet_points <- bind_rows(
  Original = survey_local,
  Geoimputed = survey_sampled, .id = "stage"
) %>%
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
  ps,
  nrow = 2,
  ncol = 16,
  xAxisLabels = unique(facet_bounds$nuts0),
  yAxisLabels = c("Original", "Geoimputed")
)

ggsave("data-raw/energy_poverty/geoimp_compare_all.png", plot = p, width = 40, height = 10)


facet_points_dkat <- facet_points[facet_points$nuts0 %in% c("Austria", "Denmark"), ]
facet_bounds_dkat <- facet_bounds[facet_bounds$nuts0 %in% c("Austria", "Denmark"), ]

dkat_ps <- list()
for (stage in c("Original", "Geoimputed")) {
  for (country in c("Austria", "Denmark")) {
    dkat_ps[[paste0(stage, "_", country)]] <- ggplot() +
      geom_sf(data = facet_bounds_dkat[
        facet_bounds_dkat$stage %in% stage &
          facet_bounds_dkat$nuts0 %in% country,
      ]) +
      geom_sf(
        data = facet_points_dkat[
          facet_points_dkat$stage %in% stage &
            facet_points_dkat$nuts0 %in% country,
        ],
        size = 0.5
      ) +
      coord_sf(crs = st_crs(3035)) +
      theme_bw()
  }
}

dkat_ps$Bounds_Austria <- ggplot() +
  geom_sf(data = lau[lau$CNTR_CODE %in% "AT", ]) +
  coord_sf(crs = st_crs(3035)) +
  theme_bw()

dkat_ps$Bounds_Denmark <- ggplot() +
  geom_sf(data = lau[lau$CNTR_CODE %in% "DK", ]) +
  coord_sf(crs = st_crs(3035)) +
  theme_bw()

dkat_p <- GGally::ggmatrix(
  dkat_ps,
  nrow = 3,
  ncol = 2,
  xAxisLabels = c("Austria", "Denmark"),
  yAxisLabels = c("Original", "Geoimputed", "LAU units"),
  showAxisPlotLabels = FALSE
)

ggsave("data-raw/energy_poverty/geoimp_compare.png", plot = dkat_p)




# survey_sampled %>%
#   st_join(select(lau, clid = "GISCO_ID"), join = st_within) %>%
#   st_drop_geometry() %>%
#   group_by(clid) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n)) %>%
#   filter(n > 10 & n < 20)
#
# survey_sampled %>%
#   mutate(place_type = case_when(
#     c5_village_small_town_less_than_10_000_people == 1 ~ "Village",
#     c5_medium_large_town_10_000_to_100_000_people == 1 ~ "Town",
#     c5_city_over_100_000_people == 1 ~ "City",
#     c5_very_large_city_over_1_000_000_people == 1 ~ "Large city"
#   )) %>%
#   st_join(select(lau, clid = "GISCO_ID"), join = st_within) %>%
#   st_drop_geometry() %>%
#   group_by(clid) %>%
#   group_by(place_type, .add = TRUE) %>%
#   summarise(n = n()) %>%
#   arrange(desc(clid))

exclid <- "AT_31235"

exlau <- lau[lau$GISCO_ID %in% exclid, ]
expop <- crop(popgrid, exlau, mask = TRUE) %>%
  classify(matrix(c(
    0, 299, 1,
    300, 799, 2,
    800, 1999, 3,
    2000, Inf, 4
  ), ncol = 3, byrow = TRUE)) %>%
  categories(
    value = data.frame(
      id = 1:4,
      category = c("0 - 299 inh.", "300 - 799 inh.", "800 - 1,999 inh.", "> 2,000 inh.")
    )
  )
expts <- survey_local[st_within(survey_local, exlau, sparse = FALSE), ] %>%
  select(geometry)
eximp <- survey_sampled[st_within(survey_sampled, exlau, sparse = FALSE), ] %>%
  mutate(place_type = case_when(
    c5_village_small_town_less_than_10_000_people == 1 ~ "Village",
    c5_medium_large_town_10_000_to_100_000_people == 1 ~ "Town",
    c5_city_over_100_000_people == 1 ~ "City",
    c5_very_large_city_over_1_000_000_people == 1 ~ "Large city"
  )) %>%
  select(place_type)
expts <- bind_rows(eximp, bind_cols(expts[1, ], place_type = "Centroid")) %>%
  mutate(place_type = factor(place_type, levels = c(
    "Centroid", "Large city", "City", "Town", "Village"
  )))

expop <- st_intersection(st_as_sf(as.polygons(expop)), exlau) %>%
  mutate(category = factor(category, levels = c(
    "0 - 299 inh.", "300 - 799 inh.", "800 - 1,999 inh.", "> 2,000 inh."
  )))

concept <- ggplot() +
  geom_sf(data = expop, aes(fill = category), color = NA, na.rm = TRUE) +
  geom_sf(data = exlau, fill = NA, linewidth = 1.5, color = "black") +
  geom_sf(data = expts, aes(color = place_type), size = 5) +
  theme_bw() +
  scale_fill_manual(
    name = "Population density",
    function(breaks) {
      breaks[is.na(breaks)] <- "N/A"
      breaks
    },
    na.value = "transparent",
    values = c("#333333", "#818181", "#ABABAB", "#CCCCCC")
  ) +
  scale_color_manual(
    name = "Place type",
    values = c(
      Centroid = "#000000",
      Village = "#FFFFB2",
      Town = "#FECC5C",
      City = "#FD8D3C",
      `Large city` = "#E31A1C"
    )
  ) +
  theme_void()

ggsave("data-raw/energy_poverty/concept.png", bg = "white")

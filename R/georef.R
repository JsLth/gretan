library(giscoR)
library(haven)
library(sf)
library(dplyr)
library(janitor)
library(reclin2)
library(countrycode)
library(stringr)

# Read survey data
survey <- haven::read_sav(
  "~/Datasets delivery/Final Weighted Dataset - complete interviews.sav",
  encoding = "utf-8"
)

# Remove nominal coding
survey$country <- haven::as_factor(survey$country)
survey$c2 <- haven::as_factor(survey$c2)
survey$c3 <- haven::as_factor(survey$c3)

# Filter out rows with no spatial information
survey <- survey[!is.na(survey$c2) & !is.na(survey$c3) & !is.na(survey$country), ]

countries <- c(
  "Austria", "Belgium", "Czechia", "Denmark", "Finland", "France", "Germany",
  "Greece", "Hungary", "Ireland", "Italy", "Netherlands", "Poland", "Portugal",
  "Romania", "Spain"
) %>%
  countrycode(origin = "country.name", destination = "eurostat")

# Retrieve NUTS boundaries from GISCO
nuts0 <- giscoR::gisco_get_nuts(
  year = "2021",
  nuts_level = "0",
  epsg = "3035",
  resolution = "10",
  spatialtype = "RG",
  cache = TRUE
)
nuts1 <- giscoR::gisco_get_nuts(
  year = "2021",
  nuts_level = "1",
  epsg = "3035",
  resolution = "10",
  spatialtype = "RG",
  cache = TRUE
)
nuts2 <- giscoR::gisco_get_nuts(
  year = "2021",
  nuts_level = "2",
  country = countries,
  epsg = "3035",
  resolution = "03",
  spatialtype = "RG",
  cache = TRUE
)

# Clean boundary datasets
nuts0 <- nuts0[c("NUTS_ID", "NUTS_NAME")]
nuts1 <- nuts1[c("NUTS_ID", "NUTS_NAME", "CNTR_CODE")]
nuts2 <- nuts2[c("NUTS_ID", "NUTS_NAME", "CNTR_CODE")]
nuts0 <- rename(nuts0, country = "NUTS_NAME")
nuts1 <- rename(nuts1, place = "NUTS_NAME", code = "CNTR_CODE")
nuts2 <- rename(nuts2, place = "NUTS_NAME", code = "CNTR_CODE")

# Retrieve local regions corresponding to C3 level
lau <- sf::st_read("https://gisco-services.ec.europa.eu/distribution/v2/lau/geojson/LAU_RG_01M_2021_3035.geojson") %>%
  select(LAU_ID, LAU_NAME, CNTR_CODE) %>%
  rename(locality = "LAU_NAME", code = "CNTR_CODE") %>%
  filter(code %in% countries)
com <- sf::st_read("https://gisco-services.ec.europa.eu/distribution/v2/communes/geojson/COMM_RG_01M_2016_3035.geojson") %>%
  select(COMM_ID, COMM_NAME, CNTR_CODE) %>%
  rename(locality = "COMM_NAME", code = "CNTR_CODE") %>%
  filter(code %in% countries) %>%
  filter(!locality %in% lau$locality)
lau <- bind_rows(lau, com)


# Remove country specifications after place names because these are very
# inconsistent
nuts1$place <- str_remove_all(nuts1$place, "\\([A-Z]{2}\\)") %>% trimws()
nuts2$place <- str_remove_all(nuts2$place, "\\([A-Z]{2}\\)") %>% trimws()
survey$c2   <- str_remove_all(survey$c2,   "\\([A-Z]{2}\\)") %>% trimws()

survey$c3 <- survey$c3 %>%
  str_remove_all("União das freguesias") %>%
  str_replace_all("St.", "Sankt") %>%
  str_remove_all(regex(", .*stadt.*", ignore_case = TRUE)) %>%
  str_replace_all("Sligo Strandhill", "Sligo-Sanktandhill") %>% # manual revision
  str_replace_all("Stillorgan", "Sanktllorgan") %>%
  str_replace_all("Siegen", "Siegen, Universitätsstadt")

# standardize spelling of places
replace <- c(`'` = "", "\u03bc" = "u", "´" = "")
survey$clean_c2 <- janitor::make_clean_names(survey$c2, allow_dupes = TRUE, replace = replace)
survey$clean_c3 <- janitor::make_clean_names(survey$c3, allow_dupes = TRUE, replace = replace)
nuts1$clean_c2  <- janitor::make_clean_names(nuts1$place, allow_dupes = TRUE, replace = replace)
nuts2$clean_c2  <- janitor::make_clean_names(nuts2$place, allow_dupes = TRUE, replace = replace)
lau$clean_c3    <- janitor::make_clean_names(lau$locality, allow_dupes = TRUE, replace = replace)

# Survey country labels are standardized while nuts country labels are
# localized. Recode to match survey labelling.
nuts0 <- mutate(nuts0, country = case_match(country,
  "Česko" ~ "Czechia",
  "Deutschland" ~ "Germany",
  "Danmark" ~ "Denmark",
  "Österreich" ~ "Austria",
  "Belgique/België" ~ "Belgium",
  "Ελλάδα" ~ "Greece",
  "España" ~ "Spain",
  "Suomi/Finland" ~ "Finland",
  "Magyarország" ~ "Hungary",
  "Éire/Ireland" ~ "Ireland",
  "Italia" ~ "Italy",
  "Nederland" ~ "Netherlands",
  "Polska" ~ "Poland",
  "România" ~ "Romania",
  "Portugal" ~ "Portugal",
  "France" ~ "France"
))

survey$code <- countrycode::countrycode(
  survey$country,
  origin = "country.name",
  destination = "eurostat"
)

nuts12 <- bind_rows(nuts1, nuts2)

# Record linkage for C2 regions
# The idea is to fuzzy match regions based on the heuristic Jaro Winkler string
# distance. Every record of a country is matched with every other record of
# the same country and a score is calculated based on how well they match.
pairs <- reclin2::pair_blocking(nuts12, survey, on = "code")

# Compute string distances
reclin2::compare_pairs(
  pairs,
  on = "clean_c2",
  default_comparator = jaro_winkler(),
  inplace = TRUE
)

# Fit model
m <- reclin2::problink_em(~clean_c2, data = pairs)

# Compute predictions for each pair
pairs <- predict(m, pairs = pairs, add = TRUE, type = "all")

# Select matching regions and link back to survey dataset
reclin2::select_threshold(pairs, "weight", variable = "threshold", threshold = 2, inplace = TRUE)
survey_regional <- pairs %>%
  reclin2::link(selection = "threshold", all_y = TRUE) %>%
  as_tibble() %>%
  mutate(geometry = geometry %>%
           st_sfc() %>% # fix empty geometries
           st_centroid()) %>% # compute centroids for easier spatial aggregation
  st_as_sf()

# Record linkage for C3
pairs <- reclin2::pair_blocking(lau, survey, on = "code")

# Compute string distances
reclin2::compare_pairs(
  pairs,
  on = "clean_c3",
  default_comparator = jaro_winkler(threshold = 0.9),
  inplace = TRUE
)

# Fit model
m <- reclin2::problink_em(~clean_c3, data = pairs)

# Compute predictions for each pair
pairs <- predict(m, pairs = pairs, add = TRUE)

# Select matching regions and link back to survey dataset
survey_local <- pairs %>%
  group_by(.y) %>%
  slice_max(order_by = weights, with_ties = FALSE) %>%
  ungroup() %>%
  bind_cols(threshold = TRUE) %>%
  as.data.table() %>%
  reclin2::link(selection = "threshold", x = lau, y = survey, all_y = TRUE) %>%
  as_tibble() %>%
  mutate(geometry = geometry %>%
           st_sfc() %>% # fix empty geometries
           st_centroid()) %>% # compute centroids for easier spatial aggregation
  st_as_sf() %>%
  filter(!is.na(.x))

# test
t <- survey_local
t$heating_over_60 <- ifelse(t$c5 > 2, 1, 0)
t <- aggregate(t["heating_over_60"], nuts2, FUN = sum)



# Stand Feb 16:
# - C2 und C3 können zu 99% georeferenziert werden
# - C3 scheint generell nützlicher, da von der kleinsten Ebene zuverlässig
#   auf höhere Ebenen aggregiert werden kann
# - Geoinformationen wurden noch nicht validiert, sieht aber generell gut aus
# - ca. 300 Angaben sind "Prefer not to say", 1 Angabe ist falsch
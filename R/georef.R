library(giscoR)
library(haven)
library(sf)
library(dplyr)
library(janitor)
library(reclin2)
library(countrycode)
library(stringr)
library(fastDummies)
library(leaflet)
library(readxl)
library(purrr)

# Problem:
# Die Daten haben unterschiedliche Skalenniveaus (Skalar, Ordinal, Likert)
# Wie aggregiert man diese Daten am besten einheitlich?
# 
# Gedanken:
# Dummy-Spalten für ordinale Daten finden und dann Summe und Mittel berechnen
# Median und Mittel für Skalar und Likert
# 
# 
# Problem 2:
# Manche Variablen sind bereits in Dummy-Form, andere aber nicht. Wie lässt sich
# automatisch bestimmen, welche Variable in Dummy-Form gebracht werden muss?

# Read survey data
survey <- haven::read_sav(
  "~/Datasets delivery/Final Weighted Dataset - complete interviews.sav",
  encoding = "utf-8"
) %>%
  janitor::clean_names()

codebook <- readxl::read_xlsx(
  "~/Datasets delivery/Codebook & Datamap.xlsx",
  skip = 1,
  sheet = "Variables"
) %>%
  janitor::clean_names() %>%
  mutate(
    variable = janitor::make_clean_names(variable),
    level = str_to_lower(measurement_level)
  ) %>%
  select(variable, label, level) %>%
  #filter(str_starts(variable, "c35|c38|c48")) %>%
  filter(!(str_ends(variable, "_open") & !level == "scale")) %>% # filter useless open questions
  filter(!str_starts(variable, regex("p|b[0-9]"))) %>% # only citizen
  filter(variable %in% c("id", "d1", "d2", # filter technical columns
                         str_match_all(variable, "^c[0-9]{1,2}.*"))) %>%
  filter(!variable %in% c("c2", "c3")) %>% # filter geo questions
  mutate(is_dummy = map_lgl(survey[.$variable], ~length(attr(.x, "labels")) == 1)) %>% # find dummies based on number of values
  mutate(category = if_else(is_dummy, str_remove_all(variable, "_.*$"), variable)) %>%
  mutate(is_likert = !is_dummy & map_lgl(survey[.$variable], function(.x) {
      lab <- names(attr(.x, "labels"))
      if (is.null(lab)) {
        FALSE
      } else {
        all(str_detect(setdiff(lab, c("Prefer not to say", "I do not know")), "^[0-9]+$"))
      }
    }
  )) %>%
  mutate(needs_dummy = level == "ordinal" & !variable == "id" & !is_dummy & !is_likert) %>%
  mutate(
    option = if_else(is_dummy, str_split_i(label, " - ", 1), NA),
    label = if_else(is_dummy, str_split_i(label, " - ", 2), label)
  )

# c44_4
# 

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
           st_sfc() %>% # fix geometries
           st_centroid()) %>% # compute centroids for easier spatial aggregation
  st_as_sf() %>%
  filter(!is.na(.x)) %>%
  select(all_of(codebook$variable)) %>%
  mutate(across(everything(), .fns = function(x) { # remove SPSS labels
    if (inherits(x, "haven_labelled")) {
      haven::as_factor(x, levels = "label", ordered = TRUE)
    } else {
      x
    }
  }))


# Recode rating scale questions as numeric
# non_answers <- c("Prefer not to say", "I do not know")
# survey_local <- survey_local %>%
#   as_tibble() %>%
#   mutate(across(
#     where(~length(levels(.x)) > 1) & # select columns that are no dummies
#       where(~all(str_detect(setdiff(levels(.x), non_answers), "^[0-9]+$"))) & # select columns with numerical labels
#       -geometry,
#     .fns = as.numeric
#   ))

# Select categorical columns that have no dummies yet
to_be_dummified <- survey_local %>%
  st_drop_geometry() %>%
  select(where(~length(levels(.x)) > 1)) %>% # select columns that are no dummies
  select(where(is.factor)) %>%
  names()

# Create dummy columns
survey_local <- fastDummies::dummy_cols(
  survey_local,
  select_columns = to_be_dummified,
  ignore_na = TRUE,
  remove_selected_columns = TRUE
) %>%
  # adjust columns that were dummies before
  mutate(across(where(is.factor), ~case_when(is.na(.x) ~ 0, .default = 1))) %>%
  st_as_sf()

survey_nuts0 <- aggregate(
  survey_local,
  nuts0,
  FUN = function(x) {
    if (is.numeric(x)) {
      mean(x, na.rm = TRUE)
    } else {
      sum(x, na.rm = TRUE)
    }
  }
) %>%
  as_tibble() %>%
  st_as_sf()

survey_nuts1 <- aggregate(
  survey_local,
  nuts1,
  FUN = function(x) {
    if (is.numeric(x)) {
      mean(x, na.rm = TRUE)
    } else {
      sum(x, na.rm = TRUE)
    }
  }
) %>%
  as_tibble() %>%
  st_as_sf()

survey_nuts2 <- aggregate(
  survey_local,
  nuts2,
  FUN = function(x) {
    if (is.numeric(x)) {
      mean(x, na.rm = TRUE)
    } else {
      sum(x, na.rm = TRUE)
    }
  }
) %>%
  as_tibble() %>%
  st_as_sf()


pal <- leaflet::colorFactor("Reds", NULL, n = 5)
leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addPolygons(
    data = sf::st_transform(t["c18_1"], 4326),
    fillColor = ~pal(c18_1),
    fillOpacity = 0.7,
    weight = 1,
    color = "black",
    opacity = 0.5,
    popup = htmltools::htmlEscape(t$c18_1)
  )


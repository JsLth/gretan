library(giscoR)
library(haven)
library(sf)
library(dplyr)

home <- ifelse(.Platform$OS.type == "windows", "~", "~/Documents")

# Read survey data
survey <- haven::read_sav(
  file.path(home, "Datasets delivery/Final Weighted Dataset - complete interviews.sav"),
  encoding = "utf-8"
)

# Remove nominal coding
survey$country <- haven::as_factor(survey$country)
survey$c2 <- haven::as_factor(survey$c2)
survey$c3 <- haven::as_factor(survey$c3)
survey$c2 <- gsub("(\\(.+\\))|")

countries <- c(
  "Austria", "Belgium", "Czechia", "Denmark", "Finland", "France", "Germany",
  "Greece", "Hungary", "Ireland", "Italy", "Netherlands", "Poland", "Portugal",
  "Romania", "Spain"
)

# Retrieve NUTS boundaries from GISCO
nuts0 <- giscoR::gisco_get_nuts(
  year = "2021",
  nuts_level = "0",
  epsg = "3035",
  resolution = "01",
  spatialtype = "RG",
  cache = TRUE
)
nuts2 <- giscoR::gisco_get_nuts(
  year = "2021",
  nuts_level = "1",
  country = countries,
  epsg = "3035",
  resolution = "01",
  spatialtype = "RG",
  cache = TRUE
)
nuts3 <- giscoR::gisco_get_nuts(
  year = "2021",
  nuts_level = "3",
  country = countries,
  epsg = "3035",
  resolution = "01",
  spatialtype = "RG",
  cache = TRUE
)
nuts0 <- nuts0[c("NUTS_ID", "NUTS_NAME")]
nuts2 <- nuts2[c("NUTS_ID", "NUTS_NAME")]
nuts3 <- nuts3[c("NUTS_ID", "NUTS_NAME")]
nuts0 <- rename(nuts0, country = "NUTS_NAME")
nuts2 <- rename(nuts2, region = "NUTS_NAME")
nuts3 <- rename(nuts3, municipality = "NUTS_NAME")

# Survey country labels are standardized while nuts country labels are
# localized. Recode to match survey labelling.
nuts0 <- mutate(nuts0, country = case_match(country,
  "Česko" ~ "Czechia",
  "Deutschland" ~ "Germany",
  "Danmark" ~ "Denmark",
  "Österreich" ~ "Austria",
  "Belgique/België" ~ "Belgium",
  "Elláda" ~ "Greece",
  "España" ~ "Spain",
  "Suomi/Finland" ~ "Finland",
  "Magyarország" ~ "Hungary",
  "Éire/Ireland" ~ "Ireland",
  "Italia" ~ "Italy",
  "Nederland" ~ "Netherlands",
  "Polska" ~ "Poland",
  "România" ~ "Romania"
))

# The GRETA survey seems to make use of different regional definitions
# About 10% of municipalities and 50% of regions can be joined with NUTS2
# and NUTS 3 levels, respectively.
# I think there are two possible approaches:
# 1. Go through national geodata authorities and stack the national boundaries
# of the fitting boundaries
# 2. Geocode toponyms as point data and perform spatial joins onto a more
# standardized type of boundary (NUTS2/3).
# 
# Due to the better applicability and ease of linking to context data I tend
# to appraoch 2.
survey0 <- nuts0 %>%
  right_join(survey, by = "country", multiple = "all", keep = FALSE) %>%
  as_tibble() %>%
  st_as_sf()
survey2 <- nuts2 %>%
  right_join(survey, by = c("region" = "c2"), multiple = "all", keep = FALSE) %>%
  as_tibble() %>%
  st_as_sf()
survey3 <- nuts3 %>%
  right_join(survey, by = c("municipality" = "c3"), multiple = "all", keep = FALSE) %>%
  as_tibble() %>%
  st_as_sf()
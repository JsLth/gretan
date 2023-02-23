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

topics <- tribble(
  ~category, ~title, ~topic,
  "d1", "Gender", "A",
  "c1", "Age", "A",
  "d2", "Occupation", "A",
  "c4", "Education", "A",
  "c5", "Place size", "A",
  "c16", "Cooling system", "C",
  "c17", "Cooling system usage", "C",
  "c18", "Heating system", "C",
  "c19", "Heating system configuration", "C",
  "c20", "Heating system usage", "C",
  "c21", "Energy items", "C",
  "c22", "Heating costs", "C",
  "c23", "Heating costs - accuracy", "C",
  "c24", "Hot water costs", "C",
  "c25", "Hot water costs - accuracy", "C",
  "c26", "Energy costs", "C",
  "c27", "Energy costs - accuracy", "C",
  "c28_1", "Energy poverty - ability to pay", "C",
  "c28_2", "Energy poverty - threats", "C",
  "c28_3", "Energy poverty - safety in winter", "C",
  "c28_4", "Energy poverty - safety in summer", "C",
  "c29", "Housing type", "D",
  "c30", "Housing area", "D",
  "c31", "Housing age", "D",
  "c32", "Major renovations", "D",
  "c33", "Energy Performance Certificates", "D",
  "c34", "Energy rating", "D",
  "c35", "Number of cars", "E",
  "c36", "Modes of transport", "E",
  "c37", "Plane travel", "E",
  "c38", "Frequency of plane travel", "E",
  "c39_1", "Neighborhood - Trust", "F",
  "c39_2", "Neighborhood - Mistrust", "F",
  "c39_3", "Neighborhood - Willingness to help", "F",
  "c39_4", "Neighborhood - Friendliness", "F",
  "c39_5", "Neighborhood - Community spirit", "F",
  "c39_6", "Neighborhood - Community support", "F",
  "c39_7", "Neighborhood - Respect", "F",
  "c39_8", "Neighborhood - Tolerance", "F",
  "c39_9", "Neighborhood - Belonging", "F",
  "c40", "Support for policy", "F",
  "c41", "Support for Citizens' Assemblies", "F",
  "c42", "Fairness of decision making", "F",
  "c43", "Acceptance of decision making", "F",
  "c44_1", "Identity - environmentally friendly consumer", "G",
  "c44_2", "Identity - concerned with environment", "G",
  "c44_3", "Identity - environmental behavior 1", "G",
  "c44_5", "Identity - environmental behavior 2", "G",
  "c45_1", "Energy transition - No knowledge", "H",
  "c45_2", "Energy transition - Rejection", "H",
  "c45_3", "Energy transition - Little contribution", "H",
  "c45_4", "Energy transition - Much contribution", "H",
  "c46_1", "Energy activities - Saving energy", "H",
  "c46_2", "Energy activities - Tracking energy consumption", "H",
  "c46_3", "Energy activities - Talking with others", "H",
  "c46_4", "Energy activities - Engaging in local projects", "H",
  "c46_5", "Energy activities - Demonstrating", "H",
  "c46_6", "Energy activities - Joining an energy cooperative", "H",
  "c47_1", "Institutional trust - EU", "I",
  "c47_2", "Institutional trust - National politicians", "I",
  "c47_3", "Institutional trust - Regional politicians", "I",
  "c47_4", "Institutional trust - Local politicians", "I",
  "c47_5", "Institutional trust - National agencies", "I",
  "c47_6", "Institutional trust - Legal system", "I",
  "c47_7", "Institutional trust - Scientists", "I",
  "c47_8", "Institutional trust - Industry", "I",
  "c47_9", "Institutional trust - Public media", "I",
  "c48_1", "Adults in the household", "A",
  "c48_2", "Children in the household", "A",
  "c49", "Tenant or owner?", "A",
  "c50", "Cost of living", "A",
  "c51", "Special conditions", "A",
  "c52", "Household occupancy", "A",
  "c53", "Stable income", "A",
  "c54", "Income description", "A",
  "c55", "Income", "A",
  "c56_1", "Political identity - national policies", "J",
  "c56_2", "Political identity - social policies", "J",
  "c56_3", "Political identity - conservative policies", "J",
  "c56_4", "Political identity - liberal policies", "J",
  "c56_5", "Political identity - environmental policies", "J",
  "c57", "Cooperative self-generation", "J",
  "c58", "Future cooperative self-generation", "J",
  "c59", "Severity of effects from fossil fuel energy", "J",
  "c60", "Time of effects from fossil fuel energy", "J",
  "c61", "Personal effects from cooperative self-generation", "J",
  "c62", "Support for cooperative self-generation", "J",
  "c63", "Knowledge and resources for cooperative self-generation", "J",
  "c64", "Relation to businesses on self-generation - currently", "J",
  "c65", "Relation to businesses on self-generation - optimally", "J",
  "c66", "Relation to government on self-generation - currently", "J",
  "c67", "Relation to government on self-generation - optimally", "J",
  "c68", "Feelings towards cooperative self-generation", "J",
  "c69", "Use of ustainable transport", "J",
  "c70", "Future use of sustainable transport", "J",
  "c71", "Severity of effects from unsustainable transport", "J",
  "c72", "Time of effects from unsustainable transport", "J",
  "c73", "Personal effects from sustainable transport", "J",
  "c74", "Support for sustainable transport", "J",
  "c75", "Knowledge and resources for sustainable transport", "J",
  "c76", "Relation to businesses on sustainable transport - currently", "J",
  "c77", "Relation to businesses on sustainable transport - optimally", "J",
  "c78", "Relation to government on sustainable transport - currently", "J",
  "c79", "Relation to government on sustainable transport - optimally", "J",
  "c80", "Feelings towards sustainable transport", "J",
  "c81", "Use of electric vehicles", "J",
  "c82", "Future use of electric vehicles", "J",
  "c83", "Severity of effects from gasoline and diesel", "J",
  "c84", "Time of effects from gasoline and diesel", "J",
  "c85", "Personal effects from electric vehicles", "J",
  "c86", "Support for electric vehicles", "J",
  "c87", "Knowledge and resources for electric vehicles", "J",
  "c88", "Relation to businesses on electric vehicles - currently", "J",
  "c89", "Relation to businesses on electric vehicles - optimally", "J",
  "c90", "Relation to government on electric vehicles - currently", "J",
  "c91", "Relation to government on electric vehicles - optimally", "J",
  "c92", "Feelings towards electric vehicles", "J",
  "c93", "Use of gas in appliances", "J",
  "c94", "Future use of gas in appliances", "J",
  "c95", "Severity of effects from gas in appliances", "J",
  "c96", "Time of effects from gas in appliances", "J",
  "c97", "Personal effects from replacing gas in appliances", "J",
  "c98", "Support for replacement of gas in appliances", "J",
  "c99", "Knowledge and resources for replacing gas in appliances", "J",
  "c100", "Relation to businesses on gas in appliances - currently", "J",
  "c101", "Relation to businesses on gas in appliances - optimally", "J",
  "c102", "Relation to government on gas in appliances - currently", "J",
  "c103", "Relation to government on gas in appliances - optimally", "J",
  "c104", "Feelings towards replacing gas in appliances", "B",
  "c6_1", "Energy activities - Unplug unused devices", "B",
  "c6_2", "Energy activities - Search for energy-efficient products", "B",
  "c6_3", "Energy activities - Turn off lighs", "B",
  "c6_4", "Energy activities - Encourage friends and family", "B",
  "c6_5", "Energy activities - Participate in carpooling", "B",
  "c6_6", "Energy activities - Travel without a car", "B",
  "c7", "Energy information sources", "B",
  "c8", "Trust for energy information sources", "B",
  "c9", "Social media and energy information", "B",
  "c10", "Topics for energy information", "B",
  "c11_1", "Topic complexity - Electricity consumption", "B",
  "c11_2", "Topic complexity - Heating/cooling consumption", "B",
  "c11_3", "Topic complexity - Energy spending", "B",
  "c11_4", "Topic complexity - Share of renewables", "B",
  "c11_5", "Topic complexity - Implementing green solutions", "B",
  "c12", "National energy development", "B",
  "c13_1", "Energy saving - Lower water temperature", "B",
  "c13_2", "Energy saving - Shorter showers", "B",
  "c13_3", "Energy saving - Drive slower", "B",
  "c13_4", "Energy saving - Full dishwasher", "B",
  "c13_5", "Energy saving - Use public transport", "B",
  "c14_1", "Attitude - Importance of energy efficiency", "B",
  "c14_2", "Attitude - Concern for energy use", "B",
  "c14_3", "Attitude - Inconvenience", "B",
  "c14_4", "Attitude - Energy behavior", "B",
  "c14_5", "Attitude - Ability to conserve energy", "B",
  "c14_6", "Attitude - Positive impact", "B",
  "c14_7", "Attitude - Comfort", "B",
  "c14_8", "Attitude - Conservation", "B",
  "c14_9", "Attitude - National economy", "B",
  "c14_10", "Attitude - Moral obligation", "B",
  "c14_11", "Attitude - Different views", "B",
  "c14_12", "Attitude - Personal finances", "B",
  "c15_1", "Attitude - Need for renewables", "B",
  "c15_2", "Attitude - Role of government", "B",
  "c15_3", "Attitude - climate change", "B",
  "c15_4", "Attitude - Local and national energy issues", "B",
  "c15_5", "Attitude - Participation", "B",
  "c15_6", "Attitude - Energy affordability", "B",
  "c15_7", "Attitude - Energy independence", "B",
  "c15_8", "Attitude - Energy independence 2", "B",
  "c15_9", "Attitude - Energy sources", "B",
  "c15_10", "Attitude - Energy justice", "B"
) %>%
  mutate(topic = case_match(topic,
    "A" ~ "Demographics",
    "B" ~ "Energy literacy and information",
    "C" ~ "Energy efficiency / energy use",
    "D" ~ "Buildings",
    "E" ~ "Mobility",
    "F" ~ "Social cohesion and collective action",
    "G" ~ "Environmental awareness",
    "H" ~ "Contribution to energy transition",
    "I" ~ "Conjoint analysis",
    "J" ~ "3-stage model"
  ))

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
  filter(!str_detect(label, "if applicable|<none>|all that apply")) %>% # filter weird optional questions with no proper label
  filter(!str_starts(variable, regex("p|b[0-9]"))) %>% # only citizen
  filter(variable %in% c("id", "d1", "d2", # filter technical columns
                         str_match_all(variable, "^c[0-9]{1,2}.*"))) %>%
  filter(!variable %in% c("c2", "c3")) %>% # filter geo questions
  mutate(is_dummy = map_lgl(survey[.$variable], ~length(attr(.x, "labels")) == 1)) %>% # find dummies based on number of values
  mutate(category = if_else(is_dummy, str_remove_all(variable, "_.*$"), variable)) %>%
  mutate(is_likert = !is_dummy & map_lgl(survey[.$variable], function(.x) {
      lab <- names(attr(.x, "labels"))
      likert <- c(
        "decrease", "agree", "between", "in favour",
        "very", "not at all", "greater", "strongly", "somewhat",
        "rarely", "occasionally", "[0-9]\\s?\\-\\s?[0-9]"
      ) %>%
        paste(collapse = "|") %>%
        regex(ignore_case = TRUE)
      non_answers <- c(
        "do not know", "prefer not to say", "none of the above"
      )
      if (is.null(lab)) {
        FALSE
      } else {
        all(str_detect(setdiff(lab, non_answers), "^[0-9]+$")) ||
          any(str_detect(lab, likert) & !str_detect(lab, "Afternoon"))
      }
    }
  )) %>%
  mutate(needs_dummy = level == "ordinal" & !variable == "id" & !is_dummy & !is_likert) %>%
  mutate( # split question labels for dummies
    option = if_else(is_dummy, str_split_i(label, " - ", 1), NA),
    label = if_else(is_dummy, str_split_i(label, " - ", 2), label)
  ) %>%
  mutate( # split question labels for multi-item questions
    subitem = if_else(str_detect(label, " - "), str_split_i(label, " - ", 1), NA),
    label = if_else(str_detect(label, " - "), str_split_i(label, " - ", 2), label)
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

# Select categorical columns that have no dummies yet
to_be_dummified <- codebook %>%
  filter(needs_dummy) %>%
  pull(variable)

# Create dummy columns
survey_local <- fastDummies::dummy_cols(
  survey_local,
  select_columns = to_be_dummified,
  ignore_na = TRUE,
  remove_selected_columns = TRUE
) %>%
  # adjust columns that were dummies before
  mutate(across(where(is.factor), ~case_when(is.na(.x) ~ FALSE, .default = TRUE))) %>%
  st_as_sf()

# Creates an extended codebook that includes all dummies in seperate rows.
# `variable`: Unique identifier for each column in the `survey_local` dataset
# `og_var`  : Matches unique identifiers with the original variable identifier
#             defined in the codebook. In other words, matches the dummies that
#             were created using fastDummies with their non-dummy counterparts
#             in the original codebook (e.g. matches c12_12_decrease to c12_12).
# `category`: Matches pre-defined dummies with their underlying question. In
#             other words, matches the option of a question to the question
#             (e.g. matches c17_1 to c17).
# `label`   : Question or statement (without options or subitems)
# `title`   : Human-readable description of a question
# `topic`   : Question topic as defined in the questionnaire
# `option`  : For dummy questions, stores the option dummy label to a question.
# `subitem` : For multi-item questions, stores the item label.
cb_ext <- tibble(variable = setdiff(names(survey_local), "geometry")) %>%
  mutate(og_var = case_when( # remove dummy extensions
    !variable %in% codebook$variable ~ str_remove_all(variable, "_([^_]*)$"),
    TRUE ~ variable
  )) %>%
  left_join(codebook, by = c("og_var" = "variable")) %>%
  left_join(topics, by = "category") %>%
  mutate(option = case_when(
    variable != og_var ~ str_remove(str_extract(variable, "_([^_]*)$"), "_"),
    TRUE ~ option
  )) %>%
  select(variable, og_var, category, label, title, topic, option, subitem, is_likert) %>%
  filter(variable != "id") %>%
  mutate(variable = janitor::make_clean_names(variable)) %>%
  filter(!variable %in% (c("c35_1", "c35_2", "c35_3", "c35_4"))) %>%
  mutate(variable = str_remove(variable, fixed("_open")))

survey_local <- janitor::clean_names(survey_local)
  
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


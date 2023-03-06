library(giscoR)
library(haven)
library(sf)
library(dplyr)
library(janitor)
#library(reclin2) do not attach for compatibility reasons
library(stringr)
library(fastDummies)
library(readxl)
library(purrr)

topics <- dplyr::tribble(
  ~category, ~title, ~topic,
  "d1", "Gender", "A",
  "c1", "Age", "A",
  "d2", "Occupation", "A",
  "c4", "Education", "A",
  "c5", "Place size", "A",
  "c6", "Energy behavior", "B",
  "c7", "Energy information sources", "B",
  "c8", "Trust for energy information sources", "B",
  "c9", "Social media and energy information", "B",
  "c10", "Topics for energy information", "B",
  "c11_1", "Complexity of energy issues", "B",
  "c12", "National energy development", "B",
  "c13", "Energy saving", "B",
  "c14", "Opinion of energy behavior", "B",
  "c15", "Opinion on energy politics", "B",
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
  "c39", "Opinion about the neighborhood", "F",
  "c44", "Environmental identity", "G",
  "c45", "Contribution to the energy transition", "H",
  "c46", "Energy activities", "H",
  "c47", "Institutional trust", "I",
  "c48", "People in the household", "A",
  "c49", "Tenant or owner?", "A",
  "c50", "Cost of living", "A",
  "c51", "Special conditions", "A",
  "c52", "Household occupancy", "A",
  "c53", "Stable income", "A",
  "c54", "Income description", "A",
  "c55", "Income", "A",
  "c56", "Political identity", "J",
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
  "c104", "Feelings towards replacing gas in appliances", "B"
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

# Read in the provided codebook, filter out some stuff that's not needed and
# add columns that help with cleaning
codebook <- readxl::read_xlsx(
  "~/Datasets delivery/Codebook & Datamap.xlsx",
  skip = 1,
  sheet = "Variables"
) %>%
  janitor::clean_names() %>%
  mutate(
    variable = janitor::make_clean_names(variable),
    is_metric = purrr::map_lgl(
      survey[variable],
      ~!haven::is.labelled(.x) & is.numeric(.x)
    )
  ) %>%
  select(variable, label, is_metric) %>%
  filter(!(stringr::str_ends(variable, "_open") & !is_metric)) %>% # filter useless open questions
  filter(!stringr::str_detect(label, "if applicable|<none>|all that apply")) %>% # filter weird optional questions with no proper label
  filter(!stringr::str_starts(variable, stringr::regex("p|b[0-9]"))) %>% # only citizen
  filter(variable %in% c("id", "d1", "d2", # filter technical columns
                         stringr::str_match_all(variable, "^c[0-9]{1,2}.*"))) %>%
  filter(!variable %in% c("c2", "c3")) %>% # filter geo questions
  filter(!variable %in% "c8_13") %>% # this subitem is not suitable for mapping
  mutate(label = stringr::str_remove_all(label, stringr::fixed(", please specify"))) %>% # remove label appendices that don't look good in the app
  mutate(label = stringr::str_remove_all(label, ":$")) %>%
  mutate(label = stringr::str_remove_all(label, stringr::fixed(" - Please answer the following questions"))) %>%
  mutate(label = stringr::str_remove_all(label, "\\s\\[OPEN\\]")) %>%
  mutate(is_pdummy = map_lgl( # pseudo dummies = Yes/No questions - don't need dummifying
    survey[variable],
    ~is.labelled(.x) & all(names(attr(.x, "labels")) %in% c("Yes", "No")))) %>%
  mutate(is_dummy = purrr::map_lgl( # find dummies based on number of labels
    survey[variable],
    ~length(attr(.x, "labels")) == 1)) %>% 
  mutate(needs_dummy = !is_metric &
           !variable == "id" &
           !is_dummy &
           !is_pdummy) %>%
  mutate( # split question labels for dummies
    option = dplyr::if_else(is_dummy, stringr::str_split_i(label, " - ", 1), NA),
    label = dplyr::if_else(is_dummy, stringr::str_split_i(label, " - ", 2), label)
  ) %>%
  mutate( # split question labels for multi-item questions
    subitem = dplyr::if_else(
      stringr::str_detect(label, " - "),
      stringr::str_split_i(label, " - ", 1),
      NA
    ),
    label = dplyr::if_else(
      stringr::str_detect(label, " - "),
      stringr::str_split_i(label, " - ", 2),
      label
    )
  ) %>%
  mutate(category = if_else(
    is_dummy | !is.na(subitem) | stringr::str_detect(variable, "_open"),
    stringr::str_remove_all(variable, "_.*$"),
    variable
  ))


# Remove nominal coding from geo columns
survey$country <- haven::as_factor(survey$country)
survey$c2 <- haven::as_factor(survey$c2)
survey$c3 <- haven::as_factor(survey$c3)

# Filter out rows with no spatial information
survey <- survey[!is.na(survey$c2) & !is.na(survey$c3) & !is.na(survey$country), ]

# Define countries and their currencies
countries <- data.frame(
  name = c(
    "Austria", "Belgium", "Czechia", "Denmark", "Finland", "France", "Germany",
    "Greece", "Hungary", "Ireland", "Italy", "Netherlands", "Poland", "Portugal",
    "Romania", "Spain"
  ),
  iso = c(
    "AT", "BE", "CZ", "DK", "FI", "FR", "DE", "EL", "HU", "IE", 
    "IT", "NL", "PL", "PT", "RO", "ES"
  ),
  curr = c(
    "EUR", "EUR", "CZK", "DKK", "EUR", "EUR", "EUR", "EUR", "HUF", "EUR", "EUR",
    "EUR", "PLN", "EUR", "RON", "EUR"
  )
)

# Read boundary data
nuts0 <- readRDS("data/nuts0.rds")
nuts1 <- readRDS("data/nuts1.rds")
nuts2 <- readRDS("data/nuts2.rds")
lau <- readRDS("data/lau.rds")
com <- readRDS("data/com.rds") %>%
  filter(!name %in% lau$name)
lau <- dplyr::bind_rows(lau, com)

# Remove country specifications after place names because these are very
# inconsistent
nuts1$place <- stringr::str_remove_all(nuts1$name, "\\([A-Z]{2}\\)") %>% trimws()
nuts2$place <- stringr::str_remove_all(nuts2$name, "\\([A-Z]{2}\\)") %>% trimws()
survey$c2   <- stringr::str_remove_all(survey$c2,   "\\([A-Z]{2}\\)") %>% trimws()

# Manually revise some place names
survey$c3 <- survey$c3 %>%
  stringr::str_remove_all("União das freguesias") %>%
  stringr::str_replace_all("St.", "Sankt") %>%
  stringr::str_remove_all(stringr::regex(", .*stadt.*", ignore_case = TRUE)) %>%
  stringr::str_replace_all("Sligo Strandhill", "Sligo-Sanktandhill") %>%
  stringr::str_replace_all("Stillorgan", "Sanktllorgan") %>%
  stringr::str_replace_all("Siegen", "Siegen, Universitätsstadt")

# Standardize spelling of places
replace <- c(`'` = "", "\u03bc" = "u", "´" = "")
survey$clean_c2 <- janitor::make_clean_names(survey$c2, allow_dupes = TRUE, replace = replace)
survey$clean_c3 <- janitor::make_clean_names(survey$c3, allow_dupes = TRUE, replace = replace)
nuts1$clean_c2  <- janitor::make_clean_names(nuts1$place, allow_dupes = TRUE, replace = replace)
nuts2$clean_c2  <- janitor::make_clean_names(nuts2$place, allow_dupes = TRUE, replace = replace)
lau$clean_c3    <- janitor::make_clean_names(lau$name, allow_dupes = TRUE, replace = replace)

# Survey country labels are standardized while nuts country labels are
# localized. Recode to match survey labelling.
nuts0 <- mutate(nuts0, name = dplyr::case_match(name,
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

# Append NUTS-1 and NUTS-2 because the C2 column includes both
nuts12 <- dplyr::bind_rows(nuts1, nuts2)

# Record linkage for C3 regions
# The idea is to fuzzy match regions based on the heuristic Jaro Winkler string
# distance. Every record of a country is matched with every other record of
# the same country and a score is calculated based on how well they match.
pairs <- reclin2::pair_blocking(lau, survey, on = "code")

# Compute string distances
reclin2::compare_pairs(
  pairs,
  on = "clean_c3",
  default_comparator = reclin2::jaro_winkler(threshold = 0.9),
  inplace = TRUE
)

# Fit model
m <- reclin2::problink_em(~clean_c3, data = pairs)

# Compute predictions for each pair
pairs <- predict(m, pairs = pairs, add = TRUE)

# Select matching regions and link back to survey dataset
survey_local <- pairs %>%
  dplyr::group_by(.y) %>%
  dplyr::slice_max(order_by = weights, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::bind_cols(threshold = TRUE) %>%
  data.table::as.data.table() %>%
  reclin2::link(selection = "threshold", x = lau, y = survey, all_y = TRUE) %>%
  dplyr::as_tibble() %>%
  mutate(geometry = geometry %>%
           sf::st_sfc() %>% # fix geometries
           sf::st_centroid()) %>% # compute centroids for easier spatial aggregation
  sf::st_as_sf() %>%
  filter(!is.na(.x)) %>%
  select(all_of(codebook$variable)) %>%
  mutate(dplyr::across(dplyr::everything(), .fns = function(x) { # remove SPSS labels
    if (haven::is.labelled(x)) {
      haven::as_factor(x, levels = "label", ordered = TRUE)
    } else {
      x
    }
  })) %>%
  mutate(dplyr::across(
    dplyr::all_of(codebook$variable[codebook$is_pdummy]),
    .fns = function(x) {
      dplyr::case_match(as.character(x),
        "Yes" ~ TRUE,
        "No"  ~ FALSE,
        .default = NA
      )
    }
  ))

# Select categorical columns that have no dummies yet
to_be_dummified <- codebook %>%
  filter(needs_dummy) %>%
  dplyr::pull(variable)

# Create dummy columns
survey_local <- fastDummies::dummy_cols(
  survey_local,
  select_columns = to_be_dummified,
  ignore_na = TRUE,
  remove_selected_columns = TRUE
) %>%
  # adjust columns that were dummies before
  mutate(dplyr::across(
    dplyr::where(is.factor),
    ~dplyr::case_when(is.na(.x) ~ 0, .default = 1))
  ) %>%
  sf::st_as_sf() %>%
  dplyr::relocate(geometry, .before = ncol(.))

to_be_deleted <- c(
  "c35_1", "c35_2", "c35_3", "c35_4", "c48_1", "c48_2",
  "c38_1", "c38_2", "c38_3", "c38_4", "c38_5"
)

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
cb_ext <- dplyr::tibble(variable = setdiff(names(survey_local), "geometry")) %>%
  mutate(og_var = case_when( # remove dummy extensions
    !variable %in% codebook$variable ~ stringr::str_remove_all(variable, "_([^_]*)$"),
    TRUE ~ variable
  )) %>%
  dplyr::left_join(codebook, by = c("og_var" = "variable"), multiple = "all") %>%
  dplyr::left_join(topics, by = "category") %>%
  mutate(option = dplyr::case_when(
    variable != og_var ~ stringr::str_remove(
      stringr::str_extract(variable, "_([^_]*)$"), "_"
    ),
    TRUE ~ option
  )) %>%
  filter(variable != "id") %>%
  filter(!variable %in% to_be_deleted) %>%
  mutate(variable = janitor::make_clean_names(variable))

saveRDS(cb_ext, file = "data/codebook.rds")

survey_local <- survey_local %>%
  select(-dplyr::all_of(to_be_deleted)) %>%
  janitor::clean_names()

# Harmonize currencies
not_euro <- countries[!countries$curr %in% "EUR", ]
not_euro <- dplyr::left_join(nuts0, not_euro, by = c("nid" = "iso")) %>%
  filter(!is.na(curr))
survey_local <- survey_local %>%
  mutate(c50 = dplyr::case_when(
    sf::st_within(geometry, not_euro[1, ], sparse = FALSE)[, 1] ~
      priceR::convert_currencies(c50, "DKK", "EUR", as.Date("2022-10-01")),
    sf::st_within(geometry, not_euro[2, ], sparse = FALSE)[, 1] ~
      priceR::convert_currencies(c50, "PLN", "EUR", as.Date("2022-10-01")),
    sf::st_within(geometry, not_euro[3, ], sparse = FALSE)[, 1] ~
      priceR::convert_currencies(c50, "RON", "EUR", as.Date("2022-10-01")),
    sf::st_within(geometry, not_euro[4, ], sparse = FALSE)[, 1] ~
      priceR::convert_currencies(c50, "CZK", "EUR", as.Date("2022-10-01")),
    sf::st_within(geometry, not_euro[5, ], sparse = FALSE)[, 1] ~
      priceR::convert_currencies(c50, "HUF", "EUR", as.Date("2022-10-01")),
    TRUE ~ c50
  ))


# link codebook to survey dataset
for (x in names(survey_local)) {
  if (x %in% cb_ext$variable) {
    attributes(survey_local[[x]]) <- as.list(cb_ext[cb_ext$variable %in% x, ])
  }
}

survey_nuts0 <- aggregate(
  survey_local %>% select(-id),
  nuts0,
  FUN = mean,
  na.rm = TRUE
) %>%
  dplyr::as_tibble() %>%
  sf::st_as_sf()

count_nuts0 <- aggregate(survey_local["id"], nuts0, FUN = length) %>%
  dplyr::rename(sample = "id")
survey_nuts0 <- sf::st_join(survey_nuts0, count_nuts0, st_equals)

survey_nuts1 <- aggregate(
  survey_local,
  nuts1,
  FUN = mean,
  na.rm = TRUE
) %>%
  dplyr::as_tibble() %>%
  sf::st_as_sf()

count_nuts1 <- aggregate(survey_local["id"], nuts1, FUN = length) %>%
  dplyr::rename(sample = "id")
survey_nuts1 <- sf::st_join(survey_nuts1, count_nuts1, st_equals)

survey_nuts2 <- aggregate(
  survey_local,
  nuts2,
  FUN = mean,
  na.rm = TRUE
) %>%
  dplyr::as_tibble() %>%
  sf::st_as_sf()

count_nuts2 <- aggregate(survey_local["id"], nuts2, FUN = length) %>%
  dplyr::rename(sample = "id")
survey_nuts2 <- sf::st_join(survey_nuts2, count_nuts2, st_equals)

saveRDS(survey_nuts0, "data/srv_nuts0.rds")
saveRDS(survey_nuts1, "data/srv_nuts1.rds")
saveRDS(survey_nuts2, "data/srv_nuts2.rds")

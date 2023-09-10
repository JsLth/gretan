# Description: Reads the raw survey data, georeferences them and creates a
#              codebook to prepare the data for presentation
# Author:      Jonas Lieth
# R version:   R version 4.2.1 (2022-06-23 ucrt)
# OS:          Windows 10 x64 (build 22621)
# Requirements:
#   - GRETA multinational survey results in ./greta_survey/
#   - Boundary data created by bounds.R (needs to be in evaluated environment)
# Packages:
#   - giscoR 0.3.3
#   - haven 2.5.1
#   - sf 1.0-9
#   - dplyr 2.3.1
#   - janitor 2.2.0
#   - reclin2 0.2.0
#   - stringr 1.5.0
#   - fastDummies 1.6.3
#   - readxl 1.4.2
#   - purrr 1.0.1

library(giscoR)
library(haven)
library(sf)
library(dplyr)
library(janitor)
library(reclin2)
library(stringr)
library(fastDummies)
library(readxl)
library(purrr)
library(usethis)

# Variables are hard to sort because they're not just numerics, but combine
# a 'version-like' structure (i.e. 2.1.1) with letters and words. This function
# uses the base numeric_version function to convert variables to versions and
# then sort them
sort_variables <- function(var) {
  x <- case_when( # id, d1 and d2 should always be first
    var %in% "id" ~ "0_1",
    var %in% "d1" ~ "0_2",
    var %in% "d2" ~ "0_3",
    TRUE ~ var
  ) %>%
    str_extract_all("[0-9]{1,3}(_[0-9]{1,2})?(_[0-9]{1,2})?") %>% # extract numeric part
    str_replace_all("_", ".") # bring name into 'version' form

  numeric_version(x) %>% # convert to numeric version
    sort() %>% # sort numeric versions
    as.character() %>% # convert back to character
    match(x) %>%
    # In some cases, multiple variables get the same index (e.g. 2_1_a, 2_1_b).
    # After sorting, these cases are grouped and then converted to a sequence
    # (e.g. c(1, 1, 1) -> c(1, 2, 3))
    ave(as.character(.), FUN = function(i) {
      seq(i[1], i[1] + length(i) - 1, 1)
    })
}

# Handle survey values during aggregation. If a variable is a likert item,
# compute median, if it's an interval numeric, compute mean. Otherwise,
# select by frequency.
# If a likert item has
aggregate_survey <- function(x) {
  vec <- get0("e", envir = parent.frame(2))
  if (is.null(vec)) {
    is_likert <- FALSE
  } else {
    is_likert <- isTRUE(attr(vec, "is_likert"))
  }

  if (is_likert) {
    nr <- strtoi(attr(vec, "labels")[[1]])
    nr <- nr[!is.na(nr)]
    nr <- x[x %in% nr]
    as.integer(median(nr))
  } else if (is.numeric(x) || is.logical(x)) {
    mean(x, na.rm = TRUE)
  } else {
    names(which.max(table(x)))
  }
}

st_centroid2 <- function(x, ..., of_largest_polygon = FALSE) {
  st_set_geometry(x, st_centroid(st_geometry(x), of_largest_polygon = TRUE))
}

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
  "c11", "Complexity of energy issues", "B",
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
  "c64", "Current relation to businesses on self-generation", "J",
  "c65", "Optimal relation to businesses on self-generation", "J",
  "c66", "Current relation to government on self-generation", "J",
  "c67", "Optimal relation to government on self-generation", "J",
  "c68", "Feelings towards cooperative self-generation", "J",
  "c69", "Use of ustainable transport", "J",
  "c70", "Future use of sustainable transport", "J",
  "c71", "Severity of effects from unsustainable transport", "J",
  "c72", "Time of effects from unsustainable transport", "J",
  "c73", "Personal effects from sustainable transport", "J",
  "c74", "Support for sustainable transport", "J",
  "c75", "Knowledge and resources for sustainable transport", "J",
  "c76", "Current relation to businesses on sustainable transport", "J",
  "c77", "Optimal relation to businesses on sustainable transport", "J",
  "c78", "Current relation to government on sustainable transport", "J",
  "c79", "Optimal relation to government on sustainable transport", "J",
  "c80", "Feelings towards sustainable transport", "J",
  "c81", "Use of electric vehicles", "J",
  "c82", "Future use of electric vehicles", "J",
  "c83", "Severity of effects from gasoline and diesel", "J",
  "c84", "Time of effects from gasoline and diesel", "J",
  "c85", "Personal effects from electric vehicles", "J",
  "c86", "Support for electric vehicles", "J",
  "c87", "Knowledge and resources for electric vehicles", "J",
  "c88", "Current relation to businesses on electric vehicles", "J",
  "c89", "Optimal relation to businesses on electric vehicles", "J",
  "c90", "Current relation to government on electric vehicles", "J",
  "c91", "Optimal relation to government on electric vehicles", "J",
  "c92", "Feelings towards electric vehicles", "J",
  "c93", "Use of gas in appliances", "J",
  "c94", "Future use of gas in appliances", "J",
  "c95", "Severity of effects from gas in appliances", "J",
  "c96", "Time of effects from gas in appliances", "J",
  "c97", "Personal effects from replacing gas in appliances", "J",
  "c98", "Support for replacement of gas in appliances", "J",
  "c99", "Knowledge and resources for replacing gas in appliances", "J",
  "c100", "Current relation to businesses on gas in appliances", "J",
  "c101", "Optimal relation to businesses on gas in appliances", "J",
  "c102", "Current relation to government on gas in appliances", "J",
  "c103", "Optimal relation to government on gas in appliances", "J",
  "c104", "Feelings towards replacing gas in appliances", "B"
) %>%
  mutate(topic = case_match(
    topic,
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
  "data-ext/greta_survey/final_dataset.sav",
  encoding = "utf-8"
) %>%
  janitor::clean_names()

cat("Total respondents:", nrow(survey), "\n")

# Read in the provided codebook, filter out some stuff that's not needed and
# add columns that help with cleaning
codebook <- readxl::read_xlsx(
  "data-ext/greta_survey/codebook.xlsx",
  skip = 1,
  sheet = "Variables"
) %>%
  janitor::clean_names() %>%
  mutate(
    question = label,
    variable = janitor::make_clean_names(variable),
    is_metric = purrr::map_lgl(
      survey[variable],
      ~ !haven::is.labelled(.x) & is.numeric(.x)
    )
  ) %>%
  select(variable, question, is_metric) %>%
  filter(!(stringr::str_ends(variable, "_open") & !is_metric)) %>% # filter useless open questions
  filter(!stringr::str_detect(question, "if applicable|<none>|all that apply")) %>% # filter weird optional questions with no proper label
  filter(!stringr::str_starts(variable, stringr::regex("p|b[0-9]"))) %>% # only citizen
  filter(variable %in% c(
    "id", "d1", "d2", # filter technical columns
    stringr::str_match_all(variable, "^c[0-9]{1,2}.*")
  )) %>%
  filter(!variable %in% c("c2", "c3")) %>% # filter geo questions
  filter(!variable %in% "c8_13") %>% # this subitem is not suitable for mapping
  mutate(question = stringr::str_remove_all(question, stringr::fixed(", please specify"))) %>% # remove label appendices that don't look good in the app
  mutate(question = stringr::str_remove_all(question, ":$")) %>%
  mutate(question = stringr::str_remove_all(question, stringr::fixed(" - Please answer the following questions"))) %>%
  mutate(question = stringr::str_remove_all(question, "\\s\\[OPEN\\]")) %>%
  mutate(is_likert = map_lgl( # a likert item is an item whose labels contain
    # at least 2 sequential numerics as the first character
    variable,
    function(x) {
      lab <- survey[[x]] %>%
        attr("labels") %>%
        names()
      if (is.character(lab) && all(nzchar(lab))) {
        lab <- strtoi(substr(lab, 1, 1))
        lab <- lab[!is.na(lab)]
        return(length(lab) > 1 && all(abs(diff(lab)) == 1))
      } else if (is.character(lab)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  )) %>%
  mutate(is_pdummy = map_lgl( # pseudo dummies = Yes/No questions - don't need dummifying
    survey[variable],
    ~ is.labelled(.x) & all(names(attr(.x, "labels")) %in% c("Yes", "No"))
  )) %>%
  mutate(is_dummy = purrr::map_lgl( # find dummies based on number of labels
    survey[variable],
    ~ length(attr(.x, "labels")) == 1
  )) %>%
  mutate(needs_dummy = !is_metric &
    !variable == "id" &
    !is_dummy &
    !is_pdummy &
    !is_likert) %>%
  mutate( # split question labels for dummies
    option = dplyr::if_else(is_dummy, stringr::str_split_i(question, " - ", 1), NA),
    question = dplyr::if_else(is_dummy, stringr::str_split_i(question, " - ", 2), question)
  ) %>%
  mutate( # split question labels for multi-item questions
    subitem = dplyr::if_else(
      stringr::str_detect(question, " - "),
      stringr::str_split_i(question, " - ", 1),
      NA
    ),
    question = dplyr::if_else(
      stringr::str_detect(question, " - "),
      stringr::str_split_i(question, " - ", 2),
      question
    )
  ) %>%
  mutate(category = if_else(
    is_dummy | !is.na(subitem) | stringr::str_detect(variable, "_open"),
    stringr::str_remove_all(variable, "_.*$"),
    variable
  )) %>%
  mutate(labels = map(variable, function(x) {
    lab <- names(attr(survey[[x]], "labels"))
    if (!any(nzchar(lab))) {
      lab <- as.character(seq(1, length(lab)))
    }
    if (length(lab) == 1) {
      i <- 1
      lab <- NULL
      x <- substr(x, 1, nchar(x) - 1)
      while (!is.null(survey[[paste0(x, i)]])) {
        lab <- c(lab, names(attr(survey[[paste0(x, i)]], "labels")))
        i <- i + 1
      }
    }
    stringr::str_remove_all(lab, ", please specify:")
  }))


# Remove nominal coding from geo columns
survey$country <- haven::as_factor(survey$country)
survey$c2 <- haven::as_factor(survey$c2)
survey$c3 <- haven::as_factor(survey$c3)

# Filter out rows with no spatial information
survey <- survey[!is.na(survey$c2) & !is.na(survey$c3) & !is.na(survey$country), ]

cat("Citizens:", nrow(survey), "\n")

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

nuts0 <- readRDS("data-ext/bounds/nuts0.rds")
nuts1 <- readRDS("data-ext/bounds/nuts1.rds")
nuts2 <- readRDS("data-ext/bounds/nuts2.rds")
lau <- readRDS("data-ext/bounds/lau.rds")
com <- readRDS("data-ext/bounds/com.rds")
grid <- readRDS("data-ext/bounds/grid.rds")

# Prepare lau
lau <- bind_rows(lau, com[!com$name %in% lau$name, ])

# Remove country specifications after place names because these are very
# inconsistent
nuts1$place <- str_remove_all(nuts1$name, "\\([A-Z]{2}\\)") %>% trimws()
nuts2$place <- str_remove_all(nuts2$name, "\\([A-Z]{2}\\)") %>% trimws()
survey$c2 <- str_remove_all(survey$c2, "\\([A-Z]{2}\\)") %>% trimws()

# Manually revise some place names
survey$c3 <- survey$c3 %>%
  str_remove_all("União das freguesias") %>%
  str_replace_all(fixed("St."), "Sankt") %>%
  str_remove_all(regex(", .*stadt.*", ignore_case = TRUE)) %>%
  str_replace_all("Siegen", "Siegen, Universitätsstadt")

# Standardize spelling of places
replace <- c(`'` = "", "\u03bc" = "u", "´" = "")
survey$clean_c2 <- make_clean_names(survey$c2, allow_dupes = TRUE, replace = replace)
survey$clean_c3 <- make_clean_names(survey$c3, allow_dupes = TRUE, replace = replace)
nuts1$clean_c2 <- make_clean_names(nuts1$place, allow_dupes = TRUE, replace = replace)
nuts2$clean_c2 <- make_clean_names(nuts2$place, allow_dupes = TRUE, replace = replace)
lau$clean_c3 <- make_clean_names(lau$name, allow_dupes = TRUE, replace = replace)

# Survey country labels are standardized while nuts country labels are
# localized. Recode to match survey labelling.
nuts0 <- mutate(nuts0, name = dplyr::case_match(
  name,
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

# Filter out non-responses to geo question
survey <- survey[!tolower(survey$c2) %in% "prefer not to say" &
  !tolower(survey$c3) %in% "prefer not to say", ]

cat("With geo-information:", nrow(survey), "\n")

# Record linkage for C3 regions
# The idea is to fuzzy match regions based on the heuristic Jaro Winkler string
# distance. Every record of a country is matched with every other record of
# the same country and a score is calculated based on how well they match.
pairs <- pair_blocking(lau, survey, on = "code")

# Compute string distances
compare_pairs(
  pairs,
  on = "clean_c3",
  default_comparator = cmp_jarowinkler(threshold = 0.9),
  inplace = TRUE
)

# Fit model
m <- problink_em(~clean_c3, data = pairs)

# Compute predictions for each pair
pairs <- predict(m, pairs = pairs, add = TRUE, type = "all")

# Select matching regions and link back to survey dataset
survey_local <- pairs %>%
  group_by(.y) %>%
  slice_max(order_by = weight, with_ties = FALSE) %>%
  ungroup() %>%
  bind_cols(threshold = TRUE) %>%
  data.table::as.data.table() %>%
  reclin2::link(selection = "threshold", x = lau, y = survey, all_y = TRUE) %>%
  as_tibble() %>%
  mutate(geometry = geometry %>%
    sf::st_sfc() %>% # fix geometries
    sf::st_centroid()) %>% # compute centroids for easier spatial aggregation
  sf::st_as_sf() %>%
  filter(!is.na(.x)) %>%
  select(all_of(codebook$variable)) %>%
  mutate(across(everything(), .fns = function(x) { # remove SPSS labels
    labs <- names(attr(x, "labels"))
    if (haven::is.labelled(x) && all(nzchar(labs))) {
      haven::as_factor(x, levels = "label", ordered = TRUE)
    } else {
      x
    }
  })) %>%
  mutate(across(
    all_of(codebook$variable[codebook$is_pdummy]),
    .fns = function(x) {
      dplyr::case_match(as.character(x),
        "Yes" ~ TRUE,
        "No" ~ FALSE,
        .default = NA
      )
    }
  )) %>%
  mutate(across(
    all_of(codebook$variable[codebook$is_likert]),
    .fns = as.numeric
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
    ~ dplyr::case_when(is.na(.x) ~ 0, .default = 1)
  )) %>%
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
  filter(!variable %in% to_be_deleted) %>%
  mutate(variable = janitor::make_clean_names(variable)) %>%
  mutate(option = if_else(nzchar(option), option, NA_character_)) %>%
  slice(sort_variables(og_var))

survey_local <- survey_local %>%
  select(-dplyr::all_of(to_be_deleted)) %>%
  relocate(geometry, .after = last_col()) %>%
  janitor::clean_names() %>%
  select(all_of(cb_ext$variable))

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

survey_local <- survey_local %>%
  st_join(select(nuts0, nuts0 = "name"), st_nearest_feature) %>%
  st_join(select(nuts1, nuts1 = "name"), st_nearest_feature) %>%
  st_join(select(nuts2, nuts2 = "name"), st_nearest_feature) %>%
  st_join(grid, st_nearest_feature)

cat("Final:", nrow(survey_local), "\n")

saveRDS(survey_local, "data-ext/survey_local.rds")

srv_nuts0 <- aggregate(
  survey_local %>% select(-id, -nuts1, -nuts2),
  nuts0,
  FUN = aggregate_survey
) %>%
  dplyr::as_tibble() %>%
  sf::st_as_sf()

count_nuts0 <- aggregate(survey_local["id"], nuts0, FUN = length) %>%
  dplyr::rename(sample = "id")
srv_nuts0 <- st_join(srv_nuts0, count_nuts0, st_equals)

srv_nuts1 <- aggregate(
  survey_local %>% select(-id, -nuts2),
  nuts1,
  FUN = aggregate_survey
) %>%
  dplyr::as_tibble() %>%
  sf::st_as_sf() %>%
  filter(!is.na(nuts0))

count_nuts1 <- aggregate(survey_local["id"], nuts1, FUN = length) %>%
  dplyr::rename(sample = "id")
srv_nuts1 <- st_join(srv_nuts1, count_nuts1, st_equals)

srv_nuts2 <- aggregate(
  survey_local %>% select(-id),
  nuts2,
  FUN = aggregate_survey
) %>%
  dplyr::as_tibble() %>%
  sf::st_as_sf() %>%
  filter(!is.na(nuts0))

count_nuts2 <- aggregate(survey_local["id"], nuts2, FUN = length) %>%
  dplyr::rename(sample = "id")
srv_nuts2 <- st_join(srv_nuts2, count_nuts2, st_equals)

srv_grid <- aggregate(
  survey_local %>% select(-id),
  grid,
  FUN = aggregate_survey
) %>%
  as_tibble() %>%
  st_as_sf()

count_grid <- aggregate(survey_local["id"], grid, FUN = length) %>%
  rename(sample = "id")
srv_grid <- st_join(srv_grid, count_grid, st_equals)

output <- list("cb_ext", "srv_nuts0", "srv_nuts1", "srv_nuts2", "srv_grid")

output <- lapply(output, as.name)
output$internal <- TRUE
output$overwrite <- TRUE
output$compress <- "bzip2"

do.call(use_data, output)

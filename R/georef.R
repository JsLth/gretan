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


check_survey_country <- function(country, survey, level = "c2", year = "2021") {
  vcc <- survey[survey$country %in% country, ][[level]]
  vcc <- na.omit(unique(vcc))
  
  nuts <- lapply(1:4, function(x) {
    if (x < 4) {
      giscoR::gisco_get_nuts(
        year = year,
        epsg = "4326",
        resolution = "60",
        nuts_level = x,
        country = country
      )
    } else if (level == "c3") {
      lau <- giscoR::gisco_get_lau(
        year = "2020",
        epsg = "4326",
        country = country
      )
      lau <- dplyr::rename(lau, NAME_LATN = "LAU_NAME")
    }
  })
  nuts[lengths(nuts) == 0] <- NULL
  
  diff <- lapply(nuts, function(x) setdiff(unique(x$NAME_LATN), vcc))
  clevel <- which.min(lengths(diff) / sapply(nuts, nrow))
  cnuts <- nuts[[clevel]]$NAME_LATN

  rnuts <- janitor::make_clean_names(cnuts)
  rvcc <- janitor::make_clean_names(vcc)
  diff <- setdiff(rvcc, rnuts)

  cli::cli_text("For {country}, the {level} column in the survey dataset likely corresponds to the NUTS-{clevel} level.")
  cli::cli_text("In their own spelling, {length(setdiff(vcc, cnuts))} regions do not match.")
  cli::cli_ul(setdiff(vcc, cnuts))
  cli::cat_line()
  cli::cli_text("In their cleaned form, {length(diff)} region{?s} do not match.")
  cli::cli_ul(diff)
  cli::cat_line()
}

# Stand 14.2.:
# - C2 ist ~ NUTS1 bis NUTS2
# - C3 ist manchmal LAU und manchmal irgendwas anderes
# - Die hartnäckigsten Schreibunterschiede in C2 lassen sich mit janitor aushebeln
# - Der Rest wahrscheinlich mit Regex
# - C3 funktioniert leider noch gar nicht, da nicht einheitlich :(
# - Manche Länder haben echt öde NUTS1/2 Regionen -> Alternieren zwischen C2 und C3?
#     - z.B. Portugal: NUTS1 teilt das Land in Portugal und Inseln ein, C3 könnte das ganze etwas spannender machen
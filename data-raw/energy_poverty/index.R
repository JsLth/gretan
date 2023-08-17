library(dplyr)
library(stringr)
library(purrr)
library(sf)

pov <- readRDS("data-ext/survey_resampled.rds") %>%
  select(starts_with(c(
    "c6",  # Energy saving #1
    "c11", # Energy complexity
    "c13", # Energy saving #2
    "c16", # Cooling system
    "c18", # Heating system
    "c19", # Heating system configuration
    "c22", # Heating costs
    "c24", # Hot water costs
    "c26", # Energy costs
    "c28", # Consensual energy poverty
    "c29", # Housing type
    "c30", # Housing area
    "c31", # Housing age
    "c32", # Major renovations
    "c33", # Performance certificate
    "c34", # Energy rating
    "c48", # Household size
    "c50", # Cost of living
    "c51", # Special conditions
    "c55"  # Income
  )), "id", "nuts0", "nuts1", "nuts2")

cb <- readRDS("data-ext/codebook.rds") %>%
  filter(variable %in% names(pov))

all_cats <- unique(tail(cb$category, -1))
all_vars <- unique(tail(cb$og_var, -1))

for (v in all_vars) {
  cb_v <- cb[cb$og_var %in% v, ]
  opts <- cb_v$option
  ext_var <- cb_v$variable
  if (!all(is.na(opts))) {
    for (o in seq_along(opts)) {
      col <- which(grepl(v, ext_var))[[o]]
      pov[ext_var[col]] <- if_else(
        pov[ext_var[col]][[1]] == 1, opts[o], NA
      )
    }
    pov[[ext_var[1]]] <- as.factor(do.call(
      coalesce,
      as.list(st_drop_geometry(pov[ext_var]))
    ))
    pov[tail(ext_var, -1)] <- NULL
    names(pov)[names(pov) %in% ext_var[1]] <- v
  }
}
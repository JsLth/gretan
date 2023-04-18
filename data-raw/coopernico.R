# Description: This script describes an exploratory analysis for two case
#              studies from the Coopérnico project in Portugal. 
# Author:      Dennis Abel, Jonas Lieth
# R version:   R version 4.2.1 (2022-06-23 ucrt)
# OS:          Windows 10 x64 (build 22621)
# Requirements:
#   - Coopernico investment data
#   - CP4 polygon data
#   - Municipality data
# Packages:
#   - readxl 1.4.2
#   - sf 1.0-9
#   - dplyr 1.1.0
#   - rmapshaper 0.4.6

library(readxl)
library(sf)
library(dplyr)
library(rmapshaper)

# Let's load the data first
coopernico <- readxl::read_xlsx("coopernico_data/coopernico-sample.xlsx", 1)

# The file contains 57 observations for two funding projects of Coopernico: Lar S. Silvestre and
# Escola JG Zarco. The data reports the gender of the sponsors, the ZIP code of residence and the
# amount of financial investment. The ZIP variable reports four-digit ZIP codes not the more detailed
# seven-digit ZIP codes. Portuguese ZIP codes are a combination of four and three digits. The first four
# report a correspondence area which normally is equivalent to the "old" districts (Freguesias before 2014).
# Four digit ZIP codes which end on 1,6 or 9 are normally associated with commercial or public addresses and
# not necessarily represented areas (I do not find any of these in the data). The additional three digits
# would have reported more detailed street segments but they are not included in the data. The data also reports one entry from Switzerland, which can be
# removed for now.
coopernico <- subset(coopernico, ZIP != "Switzerland")

# Public sources do not offer ZIP-code-based shapefiles for Portugal. There is one Github-repository, though,
# which transformed ZIP code data into a shapefile. We will use this data.
# https://github.com/temospena/CP7
ZIP_shape <- st_read("coopernico_data/pt-cp4-shapes/CP4_EstimativaPoligonos.shp") %>%
  st_make_valid()

# Use official municipality geometries instead of automated wonky zip
# code boundaries.
# Municipality geometries: https://dados.gov.pt/en/datasets/concelhos-de-portugal/
concelhos <- st_read("coopernico_data/pt-concelhos-shapes/concelhos.shp") %>%
  rmapshaper::ms_simplify( # Official geometries are very complex and take a long time to plot
    keep = 0.1, # Keep 10% of original edges
    method = NULL, # Modified Visvalingam algorithm
    keep_shapes = FALSE # don't remove small shapes
  )

# We are interested in the geographical distribution of the Coopernico sponsors. Let's
# collapse the Coopernico data into ZIP-code-level data and merge with the shapefile.
coopernico_ZIP <- coopernico %>%
  group_by(ZIP) %>% 
  summarize(
    no_obs = n(),
    no_escola = sum(project == "Escola JG Zarco"),
    no_silvestre = sum(project == "Lar S. Silvestre"),
    total_amount = sum(amount),
    mean_amount = round(mean(amount), 0)
  )

# Now, we have ZIP-level Coopernico data and can merge this with the shapefile.
ZIP_shape <- merge(
  ZIP_shape,
  coopernico_ZIP, 
  by.x = "CP4", 
  by.y = "ZIP", 
  all.x = TRUE
)

# Extract centroid from spatial geometry
ZIP_shape$geometry <- st_centroid(ZIP_shape$geometry)

coopernico <- aggregate(
  ZIP_shape["total_amount"],
  concelhos,
  FUN = sum,
  na.rm = TRUE
) %>%
  st_join(select(concelhos, name = NAME_2), st_equals) %>%
  mutate(across(everything(), .fns = ~replace(.x, is.na(.x), 0)))

output <- list("coopernico")

#st_write(coopernico, "inst/sql/ind_analyses.sqlite", layer = "coopernico")

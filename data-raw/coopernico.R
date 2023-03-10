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
#   - ggrepel 0.9.3
#   - ggspatial 1.1.7
#   - readxl 1.4.2
#   - readr 2.1.4
#   - sf 1.0-9
#   - raster 3.6-14
#   - exactextractr 0.9.1
#   - spdep 1.2-7
#   - tidyverse 2.0.0
#   - rmapshaper 0.4.6

library(ggrepel)
library(ggspatial)
library(readxl)
library(readr)
library(sf)
library(raster)
library(exactextractr)
library(spdep)
library(tidyverse)
library(rmapshaper)

# Let's load the data first
coopernico <- readxl::read_xlsx("inst/coopernico_data/coopernico_two_cases_long.xlsx", 1)

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
ZIP_shape <- sf::st_read("inst/coopernico_data/ZIP_shapefile/CP4_EstimativaPoligonos.shp") %>%
  st_make_valid()

# SUGGESTION: Use official municipality geometries instead of automated wonky zip
# code boundaries. Zip code regions and parishes do not always overlap, but
# municipalities seem to do. The following code reads in a lookup table,
# converts CP7 to CP4, deselects all columns with detailed or smaller scale
# descriptions and then removes all duplicate rows, so that only unique
# CP4-to-municipality links remain. We then read the municipality shapefile
# and join both tables by their name (this step might be a bit inaccurate if
# municipality names differ, let's hope they don't).
# Link zip codes to parishes using https://github.com/dssg-pt/mp-mapeamento-cp7
# Municipality geometries: https://dados.gov.pt/en/datasets/concelhos-de-portugal/
# zip_lookup <- readr::read_csv("https://github.com/dssg-pt/mp-mapeamento-cp7/raw/main/output_data/cod_post_freg_matched.csv")
# zip_lookup$CodigoPostal <- substr(zip_lookup$CodigoPostal, 1, 4)
# zip_lookup <- zip_lookup[, c("CodigoPostal", "Distrito", "Concelho")]
# zip_code <- zip_lookup[!duplicated(zip_lookup), ]
concelhos <- sf::st_read("inst/coopernico_data/concelhos-shapefile/concelhos.shp") %>%
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

concelhos <- aggregate(
  ZIP_shape["total_amount"],
  concelhos,
  FUN = sum,
  na.rm = TRUE
) %>%
  st_join(select(concelhos, name = NAME_2), st_equals) %>%
  mutate(across(everything(), .fns = ~replace(.x, is.na(.x), 0)))

saveRDS(concelhos, "data/coopernico.rds")

# Standardize observations with GDP per capita on zip code level
# SUGGESTION: Use the normaized investment amount. The number of observations
# does not necessarily depict properly the investment intensity at a given place
# as investments vary between three figures and five figures.
#ZIP_shape$total_amount <- ZIP_shape$total_amount / ZIP_shape$gdp_cap

# Create project database
coopernico_projects <- data.frame(project = c("Escola JG Zarco", "Lar S. Silvestre"), 
                                  lat = c(38.700671, 39.879235), lng = c(-9.237519, -7.396333))

coopernico_projects <- st_as_sf(coopernico_projects, coords = c("lng", "lat"), remove = FALSE, 
                                crs = 4326, agr = "constant")

# Create regional capitals database (five continental regions = NUTS 2)
capitals <- data.frame(city = c("Lisboa", "Porto", "Coimbra", "Faro", "Evora"), 
                       lat = c(38.725267, 41.162142, 40.211111, 37.016111, 38.566667), 
                       lng = c(-9.150019, -8.621953, -8.429167, -7.935, -7.9))

capitals <- st_as_sf(capitals, coords = c("lng", "lat"), remove = FALSE, crs = 4326, agr = "constant")


# First step: create a neighbour's list. spdep's poly2nb command recognizes
# objects created by the sf package. Here, we define neighbourhood as sharing at
# least one vertex (queen=TRUE)
nb <- poly2nb(concelhos, queen = TRUE)

# Second step: Creating weights for our neighbours with the nb2listw command.
# Style "W" (row-standardization) assigns equal weights to all observations
# =1/#neighbours, which also means that "polygons along the edges of the study
# area will  base their lagged values on fewer polygons thus potentially over-
# or under-estimating the true nature ofthe spatial autocorrelation in the data.
# Alternative more robust style: "B". Zero.policy allows for lists of
# non-neighbors (set to FALSE would result in an error since there are
# observations without any neighbours (= islands)).
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# SUGGESTIONS: Use a list of weights, e.g. inverse distance or exponential
# weighting. By default, only those zip regions that are immediately adjacent
# to another neighborhood are weighted 1. Using a distance weighting, regions
# are weighted decreasing by distance which is a way of interpolating the 
# investment data and might show more reasonable results if the investment data
# turns out to be incomplete or lacking in certain areas.
# Also, the weight matrix standardization could be changed to "S". By default,
# weights of peripheral regions are distorted while for uniform and global
# coding schemes, regions with many neighbors are distorted. S is a lot more
# robust to outliers and weighs all regions more or less equally.
# see: https://doi.org/10.1068%2Fa310165
lw <- spdep::nb2listwdist(
  nb,
  sf::st_centroid(sf::st_geometry(concelhos)),
  type = "idw",
  alpha = 2,
  style = "S",
  longlat = TRUE,
  zero.policy = TRUE
)


### Sponsors lag
concelhos$sponsors_lag <- lag.listw(lw, concelhos$total_amount, zero.policy = TRUE)


# This distribution of the sponsors lag makes sense: most postal code areas
# do not have any sponsor and therefore for most areas, this also means that all
# neighbours do not have any sponsor.

# Local Moran's I
locm <- localmoran(concelhos$total_amount, listw = lw, zero.policy = TRUE)

# The next step is to derive the quadrants and set the coloring scheme. I like to color the border of each polygon 
# with the color of their local moran score, regardless of their pvalue, and then fill only the significant ones.

# Define significance for the maps
significance <- 0.05

n <- length(concelhos$total_amount)
vec <- c(1:n)
vec <- locm[,5] < significance

# Derive quadrants
q <- attributes(locm)$quadr$mean

# set coloring scheme
colors <- c(1:n)
for (i in 1:n) {
  if (q[i] == "High-High") colors[i] <- "red"
  if (q[i] == "Low-Low")   colors[i] <- "blue"
  if (q[i] == "Low-High")  colors[i] <- "lightblue"
  if (q[i] == "High-Low")  colors[i] <- "pink"
}

# Mark all non-significant regions white
locm.dt <- as.numeric(as.factor(q)) * vec
colors1 <- colors
for (i in 1:n) {
  if ( !(is.na (locm.dt[i])) )  {
    if (locm.dt[i]==0) colors1[i] <- "white"
  }
}

labels = unique(q)


# finale version
theme_set(theme_classic())

# Scatterplot really doesn't make much sense with a count variable with range 0-3. If we get
# the full dataset, it might make more sense.
# Consider using spdep::moran.plot? It does the same job, but fancier
moran_scatter <- ggplot(
  data = concelhos,
  aes(x = sponsors_lag, y = total_amount, label=concelhos$name)
) +
  geom_point(color = colors, pch = 1, size = 2) +
  geom_point(color = colors1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "darkred") +
  geom_text(size = 4, position = position_nudge(y = 0.03)) +
  xlab("Spatial lag") + 
  ylab("No of sponsors in postal code area") +
  theme(panel.grid.major = element_line(
    color = gray(0.8),
    linetype = "dashed", size = 0.5)
  ) +
  ggtitle("Scatterplot of Moran's I")


moran_map <- ggplot(data = concelhos) +
  geom_sf(aes(fill = colors1), color = NA, show.legend = FALSE) +
  scale_fill_manual(values = c("blue", "pink", "red", "white")) +
  geom_sf(aes(color = colors), fill = NA, lwd = 0.5) +
  scale_colour_manual(
    values = c("blue", "lightblue", "pink", "red"),
    labels = labels
  ) +
  labs(color = "Local Moran's I") +
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle("Geographical clustering") +
  theme(panel.grid.major = element_line(color = gray(0.8), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue")) +
  coord_sf(xlim = c(-10, -6),
           ylim = c(37, 42))

moran_map_lisboa <- moran_map +
  coord_sf(xlim = c(-9, -9.5), ylim = c(38.5, 39))

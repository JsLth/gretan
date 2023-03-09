# Description: This script describes an exploratory analysis for two case studies from the Coopernico project
#              in Portugal. 
# Author:      Dennis Abel, Jonas Lieth
# R version:   R version 4.2.1 (2022-06-23 ucrt)
# OS:          Windows 10 x64 (build 22621)
# Requirements:
#   - Coopernico investment data
#   - CP4 polygon data
#   - Municipality data
#   - Gridded GDP and population data
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
library(ggrepel)
library(ggspatial)
library(readxl)
library(readr)
library(sf)
library(raster)
library(exactextractr)
library(spdep)
library(tidyverse)

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
ZIP_shape <- sf::st_read("inst/coopernico_data/ZIP_shapefile/CP4_EstimativaPoligonos.shp")

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
zip_lookup <- readr::read_csv("https://github.com/dssg-pt/mp-mapeamento-cp7/raw/main/output_data/cod_post_freg_matched.csv")
zip_lookup$CodigoPostal <- substr(zip_lookup$CodigoPostal, 1, 4)
zip_lookup <- zip_lookup[, c("CodigoPostal", "Distrito", "Concelho")]
zip_code <- zip_lookup[!duplicated(zip_lookup), ]

concelhos <- sf::st_read("inst/coopernico_data/concelhos-shapefile/concelhos.shp")

# Turns out we have 763 unique zip codes, 306 unique municipalities and
# 308 municipality polygons
length(unique(zip_code$CodigoPostal))
length(unique(zip_code$Concelho))
nrow(concelhos)

ZIP_shape <- merge(
  concelhos,
  zip_code,
  by.y = "Concelho",
  by.x = "NAME_2",
  all.x = TRUE
)
ZIP_shape$CodigoPostal <- as.numeric(ZIP_shape$CodigoPostal)

# FIX: The boundary shapefile contains geometries with crossing edges and duplicate
# vertices. This is alright for libraries like GEOS, but S2 has stricter
# requirements. Instead of turning off S2 (like previously), we can repair the
# corrupted geometries using `sf::st_make_valid`.
sf::sf_use_s2(FALSE)
#ZIP_shape <- sf::st_make_valid(ZIP_shape)

# SUGGESTION: Use gridded population and GDP data to normalize the investment
# data. This might be reasonable to avoid simply depicting a map of economic
# power as rich regions tend to invest more than poor regions in general. The
# two data sources are:
# https://doi.org/10.5061/dryad.dk1j0 (gridded GDP) and
# https://doi.org/10.7927/H4JW8BX5 (gridded population)
# Both files were converted to TIFF and clipped to the extent of Portugal.
# In the following, the raster data is read in, aggregated to zonal statistics
# and divided by each other to create an sf column containg the GDP per capita
# values. After reading in the investment data, it will be divided by the GDP
# per capita column to normalize the investments per region.
pop_grid <- raster::raster("inst/coopernico_data/gridded_data/portugal_pop_gridded.tif")
gdp_grid <- raster::raster("inst/coopernico_data/gridded_data/portugal_gdp_gridded.tif")

gdp_vals <- exactextractr::exact_extract(gdp_grid, ZIP_shape, fun = "sum")

pop_vals <- exactextractr::exact_extract(pop_grid, ZIP_shape, fun = "sum")

ZIP_shape <- cbind(
  gdp_vals,
  pop_vals,
  ZIP_shape
)
names(ZIP_shape)[1:2] <- c("gdp", "pop")

ZIP_shape$gdp_cap <- ZIP_shape$gdp / ZIP_shape$pop

# Plot spatial distribution of GDP per capita. Economic power is concentrated
# in the Lisbon and Porto areas.
ggplot(data = ZIP_shape, aes(fill = gdp_cap)) +
  geom_sf(color = NA) +
  ggplot2::coord_sf(xlim = c(-10, -6), ylim = c(37, 42.5))

# Let's plot the shapefile. Set xlim and ylim to continental Portugal and
# exclude islands in the Atlantic (acores, Madeira).
#ggplot(data = ZIP_shape) + 
#  geom_sf()+
#  coord_sf(xlim = c(-10, -6),
#           ylim = c(37, 42))

# We are interested in the geographical distribution of the Coopernico sponsors. Let's
# collapse the Coopernico data into ZIP-code-level data and merge with the shapefile.
coopernico_ZIP <- coopernico %>%
  group_by(ZIP) %>% 
  summarize(no_obs = n(),
            no_escola = sum(project=="Escola JG Zarco"),
            no_silvestre = sum(project=="Lar S. Silvestre"),
            total_amount = sum(amount),
            mean_amount = round(mean(amount),0))

# Now, we have ZIP-level Coopernico data and can merge this with the shapefile.
ZIP_shape <- merge(ZIP_shape, coopernico_ZIP, 
                   by.x=c("CodigoPostal"), 
                   by.y=c("ZIP"), 
                   all.x=TRUE)

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

# Let's plot the data
ggplot(data = ZIP_shape) + 
  geom_sf(aes(fill = total_amount), color = NA) +
  geom_sf(data = capitals) +
  geom_point(data=coopernico_projects, aes(x=lng, y=lat), size=4, shape=23, fill="darkred") +
  geom_text_repel(data = capitals, aes(x = lng, y = lat, label = city), 
                  fontface = "bold", nudge_x=c(-0.6,-0.5,-1,0.5,1),
                  nudge_y=c(0.1,0,-0.1,-0.1,-0.1)) +
  ggtitle("Geographical distribution of Coopernico sponsors and investments",
          subtitle="For two Coopernico projects (red dots): Escola JG Zarco and Lar S. Silvestre") +
  scale_fill_distiller(palette="Spectral", "Total investments by \nlocation (in Euro)") +
  theme_bw() +
  xlab("Longitude") + 
  ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-10, -6),
           ylim = c(37, 42))

# Zoom in on Lisbon area
ggplot(data = ZIP_shape) + 
  geom_sf(aes(fill=total_amount), color="white") +
  geom_point(data=coopernico_projects, aes(x=lng, y=lat), size=4, shape=23, fill="darkred") +
  ggtitle("Coopernico sponsors and investments in Lisboa region") +
  scale_fill_distiller(palette="Spectral", "Total investments by \nlocation (in Euro)") +
  theme_bw() +
  xlab("Longitude") + 
  ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-9.5, -8.75),
           ylim = c(38.4, 39.05))

################################################################################################
## NUTS 3 Excursion ############################################################################
# Fine-grained ZIP level data is very useful for statistical analysis. For plotting,
# however, a higher level hierarchical level might make more sense. There are official
# correspondence tables for ZIP codes to NUTS3. This could be a good alternative.

# Load NUTS-postal code correspondence table for Portugal
#correspondence <- read.csv(file="data/pc2020_PT_NUTS-2021_v1.0/pc2020_PT_NUTS-2021_v1.0.csv",
#                           header=TRUE, sep=";")

# Data table is a bit messy
#colnames(correspondence)[1] <- "NUTS3"

#correspondence$NUTS3 <- str_sub(correspondence$NUTS3, 2, nchar(correspondence$NUTS3)-1)
#correspondence$CODE <- str_sub(correspondence$CODE, 2, nchar(correspondence$CODE)-5)

# Because we removed the last three digits of the postal code, the correspondence table
# now includes a lot of duplicates. Let's only keep the rows with distinct values and remove
# all duplicates.
#correspondence <- correspondence %>% 
#  distinct()
# We should now only have distinct values in the postalcode variable. If we find duplicates here,
# this would mean that a four-digit postal code area overlaps (at least) two NUTS3 regions
# (that would be so nice for the merging part because we wouldn't know in which NUTS3 region
# the individual sponsor is located).
#duplicated(correspondence$CODE)
# There are indeed overlaps between four-digit postal code areas and NUTS 3 areas. I even
# found an overlap on NUTS2 level. This could be really problematic for merging at a later stage.
# We should inquire if they could report freguesias instead of postal codes - that would be more
# accurate for merging with official data.

# Let's find out if our two samples are affected by this overlap. If we append the NUTS info
# to our original dataset and it does not increase sample size, this would mean that there is no
# observation in our dataset which is affected by overlapping regions.
#ZIP_shape <- merge(ZIP_shape, correspondence, 
#                   by.x=c("CP4"), 
#                   by.y=c("CODE"), 
#                   all.x=TRUE)
# Merging increased the sample size considerably (from 507 to 575). This means that we have several
# observations in the data with postal codes which are ambiguous concerning NUTS3 level. That
# is a problem.
################################################################################################

### Further spatial analysis based on postal code area

# Extract centroid from spatial geometry
ZIP_shape$centroid <- st_centroid(ZIP_shape$geometry)

# Calculate Moran's I
# Let's calculate Moran's I based on the number of sponsors in each polygon.

# For the calculation of neighbours, the missing values in all areas where no
# sponsor exists are problematic. We set these missing values to zero.
ZIP_shape <- ZIP_shape %>% 
  replace(is.na(.),0)

# First step: create a neighbour's list. spdep's poly2nb command recognizes
# objects created by the sf package. Here, we define neighbourhood as sharing at
# least one vertex (queen=TRUE)
nb <- poly2nb(ZIP_shape, queen=TRUE)

# The list lists all neighbours of each observation in the dataset
nb[[1]]
nb[[3]]

# Second step: Creating weights for our neighbours with the nb2listw command.
# Style "W" (row-standardization) assigns equal weights to all observations
# =1/#neighbours, which also means that "polygons along the edges of the study
# area will  base their lagged values on fewer polygons thus potentially over-
# or under-estimating the true nature ofthe spatial autocorrelation in the data.
# Alternative more robust style: "B". Zero.policy allows for lists of
# non-neighbors (set to FALSE would result in an error since there are
# observations without any neighbours (= islands)).
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

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
  sf::st_centroid(sf::st_geometry(ZIP_shape)),
  type = "idw",
  alpha = 2,
  style = "S",
  longlat = TRUE,
  zero.policy = TRUE
)

# to see the weights of the first observation:
lw$weights[1]
# which makes sense because it has four neighbours
lw$weights[3]
# Observation 3 has more neighbours, which means that each individual neighbour has a 
# lower weight compared to the neighbours of observation 1.

### Sponsors lag
ZIP_shape$sponsors_lag <- lag.listw(lw, ZIP_shape$total_amount, zero.policy=TRUE)

hist(ZIP_shape$sponsors_lag)
# This distribution of the sponsors lag makes sense: most postal code areas
# do not have any sponsor and therefore for most areas, this also means that all
# neighbours do not have any sponsor.

# Local Moran's I
locm <- localmoran(ZIP_shape$total_amount, listw = lw, zero.policy=TRUE)

## In order to plot the distribution of local Moran's I, we need the lagged variable created above.
myvar <- ZIP_shape$total_amount
lagvar <- ZIP_shape$sponsors_lag

# get the mean of each
m.myvar <- mean(ZIP_shape$total_amount)
m.lagvar <- mean(ZIP_shape$sponsors_lag)

# The next step is to derive the quadrants and set the coloring scheme. I like to color the border of each polygon 
# with the color of their local moran score, regardless of their pvalue, and then fill only the significant ones.

# Define significance for the maps
significance <- 0.05
plot.only.significant <- TRUE

n <- length(ZIP_shape$total_amount)
#
vec <- c(1:n)
vec <- locm[,5] < significance

# FIX: Quadrants are already calculated by spdep::localmoran! No need to
# create them by hand. :)
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

colors2 <- colors
colors2 <- paste(colors2,vec)
pos = list()
for (i in 1:n) {
  pos[[i]] <- c(which(ZIP_shape$total_amount==colors2["blue 0"]))
}

blue0 <- which(colors2=="blue 0")
red0 <- which(colors2=="red 0")
lightblue0 <- which(colors2=="lightblue 0")
pink0 <- which(colors2=="pink 0")
lb <- 6
labels=c("High-High", "High-Low", "Low-High", "Low-Low")

### find ZIP codes of clusters
ZIP_shape$cluster_codes <- ZIP_shape$CP4

for (i in 1:n) {   
  if (colors1[[i]]!="red") ZIP_shape$cluster_codes[i] <- NA 
}


# finale version
theme_set(theme_classic())

# Scatterplot really doesn't make much sense with a count variable with range 0-3. If we get
# the full dataset, it might make more sense.
# Consider using spdep::moran.plot? It does the same job, but fancier
moran_scatter <- ggplot(data = ZIP_shape, aes(x=sponsors_lag,y=total_amount, label=ZIP_shape$cluster_codes)) +
  geom_point(color=colors, pch=1, size=2) +
  geom_point(color=colors1) +
  geom_smooth(method="lm",se=FALSE, linetype="dashed", color="darkred") +
  geom_text(size=4,position=position_nudge(y=.03)) +
  xlab("Spatial lag") + 
  ylab("No of sponsors in postal code area") +
  theme(panel.grid.major = element_line(color = gray(.8), linetype ="dashed", size = 0.5)) +
  ggtitle("Scatterplot of Moran's I")


moran_map <- ggplot(data = ZIP_shape) +
  geom_sf(data=ZIP_shape)+
  geom_sf(aes(fill=colors1), color=NA, show.legend=FALSE) +
  scale_fill_manual(values=c("blue","pink", "red", "white")) +
  geom_sf(aes(color=colors),fill=NA, lwd=0.5) +
  scale_colour_manual(values=c("blue","lightblue", "pink", "red"), labels = c("Low-Low", "Low-High", "High-Low", "High-High")) +
  labs(color="Local Moran's I") +
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle("Geographical clustering") +
  #geom_sf_text(aes(label = cluster_codes), size=4, position=position_jitter(width = 1, height = 1)) +
  theme(panel.grid.major = element_line(color = gray(.8), linetype ="dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue")) +
  coord_sf(xlim = c(-10, -6),
           ylim = c(37, 42))

moran_map_lisboa <- moran_map +
  coord_sf(xlim = c(-9, -9.5), ylim = c(38.5, 39))

# Cluster's of high sponsorship in Lisbon and Porto areas.

saveRDS(ZIP_shape, "data/coopernico.rds")
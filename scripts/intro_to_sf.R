# Introduction to SF

# This script provides an introduction into how to use simple features shape
# files in R with the package sf. Topics covered include, reading, plotting,
# subsetting, mutating, and writing shape files.

# setup -------------------------------------------------------------------

library(sf)
library(tidyverse)

# read in the shapefiles --------------------------------------------------

# Get sites data:

sites <- 
  read_csv('data/raw/sites.csv')

# States:



# DC census data (now you):



# simple maps -------------------------------------------------------------

# Plot states:

states 

# Plot census data (now you):

dc_census

# subsetting --------------------------------------------------------------

states %>% 
  ggplot() +
  geom_sf() +
  theme_void()

# Now you! Plot dc_census such that census tracts with a population of 0 are
# filled with the color red and all other tracts are gray:

dc_census %>% 
  ggplot() +
  geom_sf() +
  theme_void()

# mutation ----------------------------------------------------------------

states %>% 
  filter(region == 'Norteast') %>% 
  ggplot() +
  geom_sf() +
  theme_void()

# Now you! Plot the population density per unit land area in Washington DC:

dc_census %>% 
  transmute(aland = aland/1000000) %>% 
  ggplot() +
  geom_sf() +
  theme_void()

# saving sf files ---------------------------------------------------------

# Save as in ESRI-shapefile format:

sites

# Save as a geojson file:

states

# Save as an rds file:

dc_census

# Save as a kml file:

sites

# Note: To read these files (not run):

# st_read('data/raw/shapefiles/sites.shp')

# st_read('data/processed/states.geojson')

# read_rds('data/processed/dc_census.rds')

# st_read('data/processed/sites.kml')

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
  read_csv('data/raw/sites.csv') %>% 
  st_as_sf(
    coords = c('lon', 'lat'), # longitude always comes first
    crs = 4326 # use EPSG code to reference the CRS wanted. GPS system here
  ) %>% 
  st_transform(crs = 5070)

# States:

states <- 
  st_read('data/raw/states.shp') %>% 
  st_transform(
    st_crs(sites)
  ) %>% 
  set_names(
    names(.) %>%  # Add a "." here to refer to previous data set into the pipe
      tolower()
  )

# DC census data (now you):

dc_census <-
  st_read('data/raw/dc_census.shp') %>% 
  st_transform(
    st_crs(sites)
    # This line could help to use another CRS from the geometry objects
  ) %>% 
  set_names(
    names(.) %>% 
      tolower # Adding "()" or not makes no differences
  )

# simple maps -------------------------------------------------------------

# Plot states:

states %>% 
  ggplot() + # No need to specify the x and y axis, already included in sf data
  geom_sf() +
  theme_void()

# Plot census data (now you):

dc_census %>% 
  ggplot() +
  geom_sf() +
  theme_void()

# All data has to share the same CRS. 
# Next step - unifying the CRS into the same one.

# subsetting --------------------------------------------------------------

states %>% 
  filter(region == 'South') %>% 
  ggplot() +
  geom_sf() +
  geom_sf(
    data = states %>% 
      filter(name %in% c('Maryland', 'Virginia')),
    fill = 'red') +
  theme_void()

# Now you! Plot dc_census such that census tracts with a population of 0 are
# filled with the color red and all other tracts are gray:

dc_census %>% 
  ggplot() +
  geom_sf() +
  geom_sf(
    data = dc_census %>% 
      filter(population == 0),
    fill = 'red') +
  theme_void()

# mutation ----------------------------------------------------------------

states %>% 
  filter(region == 'Norteast') %>% 
  mutate(pop_density = ttl__15/area) %>% 
  ggplot() +
  geom_sf(aes(fill = pop_density)) +
  theme_void()

# Now you! Plot the population density per unit land area in Washington DC:

dc_census %>% 
  transmute(aland = aland/1000000,
            # Change the unit from (m^2) to (km^2)
            pop_density = population/aland
            # 'aland' here uses the adjusted variable
            ) %>% 
    # Diff between mutate() & transmutate():
    # the latter drops the existing variables
  ggplot() +
  geom_sf(aes(fill = pop_density)) +
  theme_void()

# saving sf files ---------------------------------------------------------

# Save as in ESRI-shapefile format:
  # CRS of sites has been changed, 
  # and stored as an sf object but not an .csv file

sites %>% 
  st_write('data/raw/sites.shp')

# Save as a .geojson file:
  # json = java script object notation
  # good way to save bc it's not platform-specific
  # only 1 file including everything

states %>% 
  st_write('data/states.geojson')
  
# Save as an rds file:
  # rds = R data set, for use in R only
  # 1 file including everything

dc_census %>% 
  write_rds('data/dc_census.rds')

# Save as a kml file:
  # .kml is used for Google earth

sites %>% 
  st_write('data/sites.kml')
 
# Note: To read these files (not run):

# st_read('data/raw/shapefiles/sites.shp')

# st_read('data/processed/states.geojson')

# read_rds('data/processed/dc_census.rds')

# st_read('data/processed/sites.kml')

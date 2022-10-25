# setup -----------------------------------------------

library(sf)
library(tmap)
library(tidyverse)

# Set tmap mode

tmap_mode('view')

# Read in polygon shapefile of grocery stores:

dc_grocery <- 
  st_read('data/raw/shapefiles/dc_grocery.geojson')

# Read in polygon shapefile of census tracts in DC:

dc_census <-
  st_read('data/raw/shapefiles/dc_census.geojson') %>% 
  set_names(
    tolower(
      names(.)))

# Read in point shapefile of buildings in DC:

dc_buildings <- 
  st_read('data/raw/shapefiles/dc_buildings.geojson') %>% 
  
  # transform the CRS of dc_buildings to the same as the previous two
  
  st_transform(
    crs = st_crs(dc_census))

# Generate a tmap ---------------------------------------------------------

# Add the base maps

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  dc_census %>% 
  st_join(dc_buildings) %>% 
  st_join(dc_grocery) %>% 
  mutate(
    average_distance = 
      mean(
        map_dfr(dc_buildings,
            ~ .x %>% 
              min(st_distance(dc_grocery,
                              by_element = TRUE))))) %>% 
  
  # I tried to find out the minimum distance for each building to a grocery
  # store, but it seems that I failed
  
  tm_shape() +
  tm_polygons(col = 'average_distance',
              style = 'kmeans')













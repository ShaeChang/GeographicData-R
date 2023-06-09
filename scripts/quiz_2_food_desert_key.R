# Quiz 2: Food deserts in the District

# setup -------------------------------------------------------------------

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
  st_read('data/raw/shapefiles/dc_buildings.geojson')

# Your task ---------------------------------------------------------------

# Without modifying the above or assigning an object to your global environment,
# generate a tmap that displays the average (mean) distance, in kilometers,
# between buildings in a given census tract and their nearest grocery store.

# Calculate distance between buildings and grocery stores:

dc_buildings %>% 
  st_transform(
    st_crs(dc_census)) %>% 
  mutate(
    grocery_dist = 
      st_distance(
        .,
        st_union(dc_grocery)) %>% 
      units::set_units('km')) %>% 
  
  # Calculate average distance by census tract:
  
  st_join(dc_census) %>% 
  as_tibble() %>% 
  group_by(geoid) %>% 
  summarize(
    dist = mean(grocery_dist)) %>% 
  
  # Join with census tracts:
  
  full_join(
    dc_census,
    .,
    by = 'geoid') %>% 
  
  # Map:
  
  tm_shape(name = 'Grocery distance') +
  tm_polygons(
    title = 'Distance to grocery (km)',
    col = 'dist')

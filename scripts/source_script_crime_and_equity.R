# Source script for crime & equity analysis

library(tidyverse)

# A single non-spatial file:

nlcd_key <- 
  read_rds('data/processed/birds_cicadas_lc.rds') %>% 
  pluck('nlcd_key')

# raster data -------------------------------------------------------------

rasters <- 
  terra::rast('data/raw/rasters/dc_stack.tif')

# The digital elevation model for DC has a different resolution and thus
# needs to be stored outside of the stack (for now):

dem_dc <-
  terra::rast('data/raw/rasters/dem_dc_10m.tif')

# load shapefiles ---------------------------------------------------------

list(
  
  # Polygon shapefile of US Census data for Washington DC:
  
  census =
    sf::st_read('data/raw/shapefiles/dc_census.geojson') %>% 
    select(GEOID, INCOME, POPULATION),
  
  # Multipolygon shapefiles for DC city parks:
  
  dc_land = 
    sf::st_read('data/raw/shapefiles/dc_land.geojson'),
  
  dc_street_trees = 
    sf::st_read('data/raw/shapefiles/dc_street_trees.geojson'),
  
  forests =
    sf::st_read('data/raw/shapefiles/dc_forests.geojson'),
  
  water =
    sf::st_read('data/raw/shapefiles/dc_rivers.geojson'),
  
  # Point data for violent crimes:
  
  crimes =
    read_csv('data/raw/dc_crimes.csv') %>% 
    filter(offense_group == 'violent') %>% 
    st_as_sf(
      coords = c('longitude', 'latitude'),
      crs = 4326)) %>% 
  
  # Pre-processing:
  
  map(
    ~ .x %>% 
      set_names(
        names(.) %>% 
          tolower()) %>% 
      
      # Transform to EPSG 5070:
      
      sf::st_transform(crs = 5070) %>% 
      
      # Fix invalid geometries:
      
      sf::st_make_valid()) %>% 
  
  # Send to the global environment:
  
  list2env(.GlobalEnv)





  




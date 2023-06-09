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
  st_read('data/raw/shapefiles/dc_buildings.geojson')

# Tmap --------------------------------------------------------------------

dc_buildings %>% 
  
  # transform the crs of dc_buildings,
  
  st_transform(
    
    # to be the same crs as dc_grocery's
    
    crs = st_crs(dc_grocery)) %>% 
  
  # create a new field to calculate the distance of each building to the 
  # grocery polygon
  
  mutate(distance_to_grocery = 
           
           # calculate distances from buildings to the generated multipolygon
           
           st_distance(.,
                       dc_grocery %>% 
                         
                         # unionize the grocery polygons to one multipolygon
                         
                         st_union()) %>% 
           
           # set the units of the calculated distance to km
           
           units::set_units('km')) %>% 
  
  # spatial join the two sf objects since there are no common keys
  
  st_join(dc_census) %>% 
  
  # convert to a tibble to enable the following statements
  
  as_tibble() %>% 
  
  # group the buildings' fields by census tract
  
  group_by(geoid) %>% 
  
  # calculate the average distance to grocery stores in each census tract
  
  summarise(mean_distance = mean(distance_to_grocery)) %>% 
  
  # join this calculated tibble of census tracts to the sf object
  # switch the order to ensure the output an sf object
  
  full_join(dc_census, .) %>% 
  
  # generate a tmap and name it by our purpose
  
  tm_shape(name = 'Food deserts by census tract') +
  
  # add a polygon layer
  
  tm_polygons(
    
    # name it by the meaning of our core feature
    
    title = 'Avg distance to groceries (km)',
    
    # use 'kmeans' as a style
    
    style = 'kmeans',
    
    # to put on the color of every census tract to showcase its mean distance
    # from buildings to grocery stores
    
    col = 'mean_distance',
    
    # choose an appropriate palette for coloring
    
    palette = 'YlOrRd')




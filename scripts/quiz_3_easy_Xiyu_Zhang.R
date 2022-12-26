# Quiz 3 Easy
# Xiyu Zhang

# setup -----------------------------------------------

library(sf)
library(tmap)
library(tidyverse)

# Set tmap mode

tmap_mode('view')

# Read in the lifeline file:

rat_density <- 
  read_rds('data/raw/quiz_3_rats_lifeline.rds')

# Read in polygon shapefile of census tracts in DC:

dc_census <-
  st_read('data/raw/shapefiles/dc_census.geojson') %>% 
  set_names(
    tolower(
      names(.)))

# Data processing ---------------------------------------------------------

dc_census %>% 
  left_join(
    rat_density %>% 
      
      # create a new classified variable from average densities
      
      mutate(
        density_classified = 
          case_when(
            rat_density < 0.1 ~ 'low',
            rat_density < 0.4 ~ 'medium',
            rat_density < 0.75 ~ 'high',
            TRUE ~ 'very high') %>% 
          
          # arrange the density classes in order from the lowest to the highest
          
          fct_relevel(
            c('low', 'medium', 'high', 'very high')))) %>% 
  
  # generate a tmap that displays the average rat density by census tract
  
  tm_shape() +
  tm_polygons(col = 'density_classified')
  
  
  

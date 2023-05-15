# Classify and reclassification with spatial objects

# setup -------------------------------------------------------------------

library(sf)
library(tmap)
library(tidyverse)

# Set tmap mode for the whole document:

tmap_mode('view')

# Load data:

  # using multiple scripts when operating a massive project,
  # and it's parsimonious to use a source script to avoid repetition

source('scripts/source_script_bird_mortality.R')

# classifying polygons ----------------------------------------------------

# Now you! Use if else to classify census tracts where the median income is lower
# than DC’s median income as “low” and all other tracts as “high” and assign the
# results to income_class:

census %>%
  filter(
    !is.na(income)) %>% 
  mutate(
    income_class = 
      if_else(income < median(income, na.rm = TRUE),
              'low',
              'high')) %>% 
  
  # visualize it
  
  tm_shape() +
  tm_polygons(col = 'income_class')

# The lower quartile:

census %>% 
  filter(
    !is.na(income)) %>% 
  pull(income) %>% 
  quantile(probs = 0.75)

# Now you! Classify median income by census tract as "low" if it is less than or
# equal to the lower quartile, "high" it is greater than or equal to the upper
# quartile, or otherwise "medium":

census %>% 
  filter(
    !is.na(income)) %>% 
  mutate(
    income_class = 
      case_when(
        income <= income %>% 
          quantile(probs = 0.25) ~ 'low',
        income >= income %>% 
          quantile(probs = 0.75) ~ 'high',
        TRUE ~ 'medium')) %>% 
  tm_shape() +
  tm_polygons(col = 'income_class')

# Spatial union by field values:

census %>% 
  filter(
    !is.na(income)) %>% 
  mutate(
    income_class = 
      case_when(
        income <= income %>% 
          quantile(probs = 0.25) ~ 'low',
        income >= income %>% 
          quantile(probs = 0.75) ~ 'high',
        TRUE ~ 'medium')) %>% 
  
  # automatically unionize polygons by the grouped data.
  # can be a spatial operation with sf files
  
  group_by(income_class) %>% 
  summarise() %>% 
  tm_shape() +
  tm_polygons(col = 'income_class')

# Assign income_dc to the global environment:

income_dc <-
  census %>% 
  filter(
    !is.na(income)) %>% 
  mutate(
    income_class = 
      case_when(
        income <= income %>% 
          quantile(probs = 0.25) ~ 'low',
        income >= income %>% 
          quantile(probs = 0.75) ~ 'high',
        TRUE ~ 'medium') %>% 
      forcats::fct_relevel(
        c('low', 'medium', 'high')) %>% 
      
      # set the releveled categories into integer to store this new order,
      # since fct_relevel does not work well with sf object
      
      as.integer()) %>% 
  group_by(income_class) %>% 
  summarise()

# Rasterize polygons:

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  # convert a polygon to a raster and generate a tmap

  terra::rasterize(
  x = income_dc %>% 
    terra::vect(),
  y = rasters,
  field = 'income_class') %>% 
  tm_shape() +
  tm_raster(
    palette = c('red', 'yellow', 'blue'),
    alpha = 0.5,
    
    # change the style into categorical again and assign the relative name,
    # since the features don't directly work with raster, and treated the levels
    # as doubles if not modified
    
    style = 'cat',
    labels = c('low', 'medium', 'high'))

# Now you! Use a forcats function to convert the income classes to a factor
# ordered as "low", "medium", and "high".

# Now you! Modify the tm_raster function such that it treats the raster values
# as categorical.

# classifying continuous rasters ------------------------------------------

# Reclass matrix: 对连续栅格进行分类

tribble(
  ~ from, ~ to, ~ becomes,
       0,   80,         0,
      80,  100,         1) %>% 
  as.matrix()

# Now you! Using cut, determine which combination of include.lowest and right
# would correspond with canopy values >= 80 and no additional NA values within
# our masked raster:

tibble(values = 0:10) %>% 
  mutate(
    new_value = 
      cut(
        values,
        breaks = c(0, 8, 10),
        
        # it is necessary to test the behavior of 'include.lowest' and 'right'
        # every time to get familiar to it
        
        include.lowest = TRUE,
        right = FALSE)) 

# Classify forested pixels:

forest <- 
  tribble(
    ~ from, ~ to, ~ becomes,
         0,   80,         0,
        80,  100,         1) %>% 
  as.matrix() %>% 
  terra::classify(
    rasters$canopy, # the target raster
    rcl = ., # matrix for classification
    include.lowest = TRUE,
    right = FALSE)

# Mapping forest in DC:

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  forest %>% 
  tm_shape() +
  tm_raster(
    title = 'forest',
    palette = c(NA, '#208142'),
    style = 'cat',
    alpha = 0.8)

# reclassifying categorical rasters ---------------------------------------

# 对离散数据类型的rasters进行归类重组

# Forest, as described by the nlcd data:

forest_nlcd <- 
  
  # Reclass matrix:
  
  nlcd_key %>% 
  transmute(
    from = id,
    to = if_else(
      str_detect(name, 'Forest'),
      1,
      NA_real_)) %>% 
  
  # the NA value has to remain the same class, here, for number, it is 'real'
  
  as.matrix() %>% 
  
  # when the classify matrix only has 2 columns, don't need the
  # 'include.lowest' or 'right' arguments
  
  # Classify raster
  
  terra::classify(
    rasters$nlcd,
    rcl = .)
  
# Now you! Modify the above to generate a two-column tibble where all
# non-forested pixels are assigned the value 1 and non-forested pixels are
# assigned to the value 0.

# Now you! Reclassify rasters$nlcd to forest and non-forest above.

# Mapping forest in DC:

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  forest_nlcd %>% 
  tm_shape() +
  tm_raster(
    title = 'forest',
    palette = c(NA, '#208142'),
    style = 'cat',
    alpha = 0.8)

# rasters to polygons -----------------------------------------------------

  # method: need to convert rasters to SpatVectors first and then sf objects

# Convert rasters to polygons:

forest_sf <- 
  forest_nlcd %>% 
  terra::as.polygons() %>% # made a SpatVector object
  st_as_sf() %>% # made an sf object
  st_make_valid() # make the invalid polygons fixed

# Mapping forest in DC:

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +

tm_shape(forest_sf) +
  tm_polygons(
    title = 'forest',
    col = '#208142',
    style = 'cat',
    alpha = 0.8)

# a bit of raster math ----------------------------------------------------

# Now you! Using the NLCD raster, set water pixels to NA and all other pixels to
# the numeric value 1:

land <- 
  nlcd_key %>% 
  transmute( 
    
    # use 'transmute' bc other columns are useless 
    # when creating a classifying matrix
    
    from = id,
    to = 
      if_else(
      name == 'Open water',
      NA_real_,
      1)) %>% 
  as.matrix() %>%
  
  # Reclassify raster:
  
  terra::classify(
    rasters$nlcd,
    rcl = .)

# Mapping land in DC: (open water is shown as NA value)

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +

tm_shape(land) +
  tm_raster(
    title = 'land',
    style = 'cat',
    alpha = 0.8)

# Matrix math: 介绍矩阵乘法的基本规则

mat <-
  matrix(
    1:4, 
    nrow = 2,
    byrow = FALSE)

# Global canopy cover mean for Washington, DC:

{rasters$canopy * land} %>% # for pipe, curly brace is used
  
  # by multiplying, remove the water pixels from the canopy raster
  
  terra::global(mean, na.rm = TRUE) # now the mean should be higher

# Now you! Remove water pixels from canopy cover and plot the resultant data
# with tmap:

{rasters$canopy * land} %>% 
  tm_shape() +
  tm_raster(palette = 'YlGn',
            alpha = 0.6)




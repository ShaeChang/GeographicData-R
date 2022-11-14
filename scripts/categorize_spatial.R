# Classify and reclassification with spatial objects

# setup -------------------------------------------------------------------

library(sf)
library(tmap)
library(tidyverse)

# Set tmap mode for the whole document:

tmap_mode('view')

# Load data:

source('scripts/source_script_bird_mortality.R')

# classifying polygons ----------------------------------------------------

# Now you!Use if else to classify census tracts where the median income is lower
# than DC’s median income as “low” and all other tracts as “high” and assign the
# results to income_class:

census %>%
  filter(
    !is.na(income)) %>% 
  mutate(
    income_class = 
      median(income))

# The lower quartile:

census %>% 
  filter(
    !is.na(income)) %>% 
  pull(income) %>% 
  quantile(probs = 0.25)

# Now you! Classify median income by census tract as "low" if it is less than or
# equal to the lower quartile, "high" it is greater than or equal to the upper
# quartile, or otherwise "medium":

census %>% 
  filter(
    !is.na(income)) %>% 
  mutate(
    income_class = income)

# Spatial union by field values:



# Assign income_dc to the global environment:



# Rasterize polygons:

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +

terra::rasterize(
  x = income_dc,
  y = rasters,
  field = 'income_class') %>% 
  tm_shape() +
  tm_raster()

# Now you! Use a forcats function to convert the income classes to a factor
# ordered as "low", "medium", and "high".

# Now you! Modify the tm_raster function such that it treats the raster values
# as categorical.

# classifying continuous rasters ------------------------------------------

# Reclass matrix:

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
        include.lowest = TRUE,
        right = FALSE))

# Classify forested pixels:

forest <- 
  tribble(
    ~ from, ~ to, ~ becomes,
         0,   80,         0,
        80,  100,         1) %>% 
  as.matrix()

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

# Forest, as described by the nlcd data:

forest_nlcd <- 
  
  # Reclass matrix:
  
  nlcd_key %>% 
  transmute(
    from = id,
    to = name)

# Now you! Modify the above to generate a two-column tibble where all
# non-forested pixels are assigned the value 1 and non-forested pixels are
# assigned to the value 0.

# Now you! Reclassify rasters$nlcd to forest and non-forest:



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

# Convert rasters to polygons:

forest_sf <- 
  forest_nlcd

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
  transmute()

# Mapping land in DC:

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +

tm_shape(land) +
  tm_raster(
    title = 'land',
    style = 'cat',
    alpha = 0.8)

# Matrix math:

mat <-
  matrix(
    1:4, 
    nrow = 2,
    byrow = FALSE)

# Global canopy cover mean for Washington, DC:

rasters$canopy %>% 
  terra::global(mean, na.rm = TRUE)

# Now you! Remove water pixels from canopy cover and plot the resultant data
# with tmap:

rasters$canopy %>% 
  tm_shape() +
  tm_raster(palette = 'YlGn',
            alpha = 0.6)




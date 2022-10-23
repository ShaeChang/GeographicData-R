# Advanced sf: Geometry operations

# setup -------------------------------------------------------------------

library(tmap)
library(sf)
library(tidyverse)

# Set the tmap mode for the entire document:

tmap_mode('view')

# load polygons -----------------------------------------------------------

polygons <-
  list(
    
    # Polygon shapefile of US Census data for Washington DC:
    
    census =
      st_read('data/raw/shapefiles/dc_census.geojson') %>% 
      select(GEOID, INCOME, POPULATION),
    
    # Multipolygon shapefile for Rock Creek Park:
    
    rock_creek = 
      st_read('data/raw/shapefiles/rock_creek_park.geojson'),
    
    # Polygon shapefile of DC waterbodies:
    
    water = 
      st_read('data/raw/shapefiles/Waterbodies_2019.geojson')) %>% 
  
  # Pre-processing:
  
  map(
    ~ .x %>% 
      set_names(
        names(.) %>% 
          tolower()) %>% 
      
      # EPSG 32618 is UTM zone 18N for Washington DC:
      
      st_transform(crs = 32618))

# Dissolve inner borders from polygons:



# Remove water from census shape:



# load points -------------------------------------------------------------

points <-
  list(
    
    # iNaturalist observations of cicada:
    
    cicadas = 
      read_csv('data/raw/cicadas_brood_x_2021.csv') %>% 
      select(datetime, longitude, latitude) %>% 
      filter(lubridate::month(datetime) == 5) %>% 
      st_as_sf(
        coords = c('longitude', 'latitude'),
        crs = 4326),
    
    # Location of sick birds from DC's wildlife rehab center:
    
    birds = 
      st_read('data/raw/shapefiles/sick_birds.geojson'))

# Now you! Modify the script above such that the point data only include
# observations in Washington DC.

# invalid polygons --------------------------------------------------------

# Basemap:

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  # Add census layer:
  
  polygons$census %>%
  tm_shape(name = 'Census tracts') +
  tm_polygons(alpha = 0.6) +
  
  # Add water layer:
  
  polygons$water %>%
  tm_shape(name = 'Water bodies') +
  tm_polygons(col = 'blue')

# cicadas and sick birds --------------------------------------------------

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  # Add cicada observations:
  
  points$cicadas %>%
  tm_shape(name = 'Cicadas') +
  tm_markers() +
  
  # Add locations of sick birds:
  
  points$birds %>% 
  tm_shape(name = 'Sick birds') +
  tm_dots(col = 'red',
          size = 0.05)

# Now you! Use purrr::map to calculate the number of sick birds and cicadas per
# census block:

counts_by_census <-
  points

# Basemap:

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  # Cicadas:
  
  counts_by_census$cicadas %>% 
  tm_shape(name = 'Cicadas by census tract') +
  tm_polygons(
    title = 'Cicadas',
    style = 'kmeans',
    col = 'n',
    palette = 'viridis') +
  
  # Birds:
  
  counts_by_census$birds %>% 
  tm_shape(name = 'Bird mortality by census tract') +
  tm_polygons(
    title = 'Sick birds',
    style = 'kmeans',
    col = 'n',
    palette = 'YlOrRd',
    n = 5)


# area calculations -------------------------------------------------------

# Calculate cicadas per square km:

counts_by_census$cicadas

# Modify the code block above such that area is added as a column in the
# counts_by_census$cicadas.

# Now you! Modify the counts_by_census map function such that it calculates the
# density of cicadas and bird mortalities (per km^2) by census tract.

# unions and differences --------------------------------------------------

# Union - District of Columbia

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  polygons$census %>% 
  tm_shape() +
  tm_polygons(alpha = 0.7) +
  
  polygons$rock_creek %>% 
  tm_shape() +
  tm_polygons(col = '#228b22') +
  
  polygons$water %>% 
  tm_shape() +
  tm_polygons(col = 'blue')

# Difference - census files

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  polygons$census %>% 
  tm_shape() +
  tm_polygons(alpha = 0.7)


# centroids ---------------------------------------------------------------

# Basemap:

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  # Cicadas:
  
  counts_by_census$cicadas %>% 
  tm_shape(name = 'Cicadas by census tract') +
  tm_polygons(
    title = 'Cicadas',
    style = 'quantile',
    col = 'density',
    palette = 'viridis') +
  
  # Birds:
  
  counts_by_census$birds %>% 
  tm_shape(name = 'Bird mortality by census tract') +
  tm_polygons(
    title = 'Sick birds',
    style = 'quantile',
    col = 'density',
    palette = 'YlOrRd',
    n = 5)

# buffers and intersections -----------------------------------------------

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  unionized_polygons$rock_creek %>% 
  tm_shape() +
  tm_polygons(col = '#228b22') +
  
  points$birds %>% 
  tm_shape(name = 'Sick birds') +
  tm_dots(col = 'red',
          size = 0.05)

# distance calculations ---------------------------------------------------

# Distance to park:

points$cicadas %>% 
  st_distance(polygons$rock_creek)

# Add distance to Rock Creek Park to points$cicadas as a vector called
# distance_to_park:

points$cicadas

# Now you! Modify the tmap below such that cicada points are colored by
# distance-from-park:

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  unionized_polygons$rock_creek %>%
  tm_shape() +
  tm_polygons(col = '#228b22') +
  
  points$cicadas %>% 
  tm_shape() +
  tm_dots()
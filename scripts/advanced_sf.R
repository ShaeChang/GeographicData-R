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
      
      st_transform(crs = 32618) %>% 
      
      # fix invalid geometries:
      
      st_make_valid()) # can use st_is_valid() to check whether it is valid

# Dissolve inner borders from polygons:

unionized_polygons <-
  polygons %>% 
  purrr::map(~ .x %>% 
      
      # first unionized
      
      st_union() %>% 
      
      # then convert the type of polygon back to sf for further usage
      
      st_sf())

# Remove water from census shape:

census_no_water <-
  polygons$census %>% 
  st_difference(unionized_polygons$water)

  # why? what does st_difference do exactly? minus, minus y from x

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
      st_read('data/raw/shapefiles/sick_birds.geojson')) %>% 
  
  # Process the shapes:
  
  map(
    ~ .x %>% 
      
      # transform to the same CRS as the polygons to filter
      
      st_transform(crs = st_crs(polygons$census)) %>% 
      
      # filter points to the Washington DC region:
      
      st_filter(polygons$census)) # just like a spatial join

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
  st_make_valid() %>% # because one of the polygon has self-intersection
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
  tm_markers() + # cluster the points by default. 
  # Once zoom in, blue markers marked cicadas
  
  # Add locations of sick birds:
  
  points$birds %>% 
  tm_shape(name = 'Sick birds') +
  tm_dots(col = 'red',
          size = 0.05)
  # no cluster for birds, 
  # but make the markers red and a little bit larger for emphasize

# Now you! Use purrr::map to calculate the number of sick birds and cicadas per
# census block:

counts_by_census <-
  points %>% 
  
  # why use purrr::map here
  
  map(
    ~ st_join(census_no_water, .x) %>% 
      
      # order matters here, why?
      
      as_tibble() %>% # don't forget 'as_tibble' for the speed of calculation
      group_by(geoid) %>% 
      summarise(n = n()) %>% 
      left_join(census_no_water, .) %>% 
      mutate(area = st_area(.)
               %>% units::set_units('km^2'),
             density = n/area))

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
    col = 'density',
    palette = 'viridis') +
  
  # Birds:
  
  counts_by_census$birds %>% 
  tm_shape(name = 'Bird mortality by census tract') +
  tm_polygons(
    title = 'Sick birds',
    style = 'kmeans',
    col = 'density',
    palette = 'YlOrRd',
    n = 5) +
  
  polygons$water %>%
  st_make_valid() %>% 
  tm_shape(name = 'Water bodies') +
  tm_polygons(col = 'blue')

# However, the tracts are in different sized. 
# Thus we need to implement area calculations.

# area calculations -------------------------------------------------------

# Calculate cicadas per square km:

counts_by_census$cicadas %>% 
  mutate(area = 
           st_area(.) %>% 
           
           # set the unit to km^2
           
           units::set_units('km^2'),
         density = n/area)

  # convert the unit of count for better communication,
  # considering the final scale of 'cicadas per square km'

# Modify the code block above such that area is added as a column in the
# counts_by_census$cicadas.

# Now you! Modify the counts_by_census map function such that it calculates the
# density of cicadas and bird mortalities (per km^2) by census tract.

# unions and differences --------------------------------------------------

# Union - District of Columbia
# purpose: cut the water body off from calculating densities

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  polygons$census %>%
  
  # remove any water from the census shape file
  
  st_difference(unionized_polygons$water) %>% 
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
  
  # center all values in one dot by 'centroid',
  # for the convenience of comparing the two distribution in one picture
  
  st_centroid() %>% 
  tm_shape(name = 'Bird mortality by census tract') +
  tm_dots(
    title = 'Sick birds',
    style = 'quantile',
    col = 'density',
    palette = 'YlOrRd',
    n = 5,
    size = 0.1)

# buffers and intersections -----------------------------------------------

# calculate distance between sick birds and the Rock Creek Park,
# due to the suspect that the RCP is a source of cicadas.

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  unionized_polygons$rock_creek %>% 
  
  # add a buffer because animals will travel to rock creek for food and 
  # water within 500m
  
  st_buffer(dist = 500) %>% 
  
  # use intersections because the buffer exceed the north border of DC area
  # opposite to st_difference()
  
  st_intersection(unionized_polygons$census) %>% 
  tm_shape() +
  tm_polygons(col = 'yellow') +
  
  unionized_polygons$rock_creek %>% 
  tm_shape() +
  tm_polygons(col = '#228b22') +
  
  points$birds %>% 
  tm_shape(name = 'Sick birds') +
  tm_dots(col = 'red',
          size = 0.05)

# distance calculations ---------------------------------------------------

# Is the park really the source of cicadas?

# every cicadas' distance to park:

points$cicadas %>% 
  st_distance(unionized_polygons$rock_creek) %>% 
  
  # it used to be by_element = TRUE, but does not work
  # to reach the same goal to generate a vector, I use the following:
  
  as.vector()

# Add distance to Rock Creek Park to points$cicadas as a vector called
# distance_to_park:

points$cicadas %>% 
  mutate(
    distance_to_park = 
      st_distance(
        .,
        unionized_polygons$rock_creek) %>% 
      as.vector())

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
  mutate(
    distance_to_park = 
      st_distance(
        .,
        unionized_polygons$rock_creek) %>% 
      as.vector()) %>% 
  tm_shape() +
  tm_dots(col = 'distance_to_park',
          
          # use 'kmeans' clusters
          
          style = 'kmeans')

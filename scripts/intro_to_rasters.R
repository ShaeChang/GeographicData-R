# Introduction to rasters

# setup -------------------------------------------------------------------

library(tmap)
library(sf)
library(tidyverse)

tmap_mode('view')

# load polygons -----------------------------------------------------------

# Polygon files:

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
      
      # Fix invalid geometries:
      
      st_make_valid())

# Dissolve inner borders from polygons:

unionized_polygons <-
  polygons %>%
  map(
    ~ st_union(.x) %>% 
      st_sf())

# Remove water from census shape:

census_no_water <-
  polygons$census %>% 
  st_difference(
    st_union(polygons$water))

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
      
      # Transform to the same CRS as the polygons:
      
      st_transform(crs = st_crs(polygons$census)) %>% 
      
      # Filter points to the Washington DC region:
      
      st_filter(polygons$census))

# load rasters ------------------------------------------------------------

# cropping before transformation will definitely save time, 
# because transforming rasters is also memory intensive

temp <-
  unionized_polygons$census %>% 
  st_transform(5070) %>% 
  terra::vect()

imp <-
  terra::rast('data/raw/rasters/impervious_surface.tif')

terra::plot(imp)

# Now you! Complete the code below to read in all of the rasters at once:

rasters <-
  list.files('data/raw/rasters/',
             full.names = TRUE) %>% 
  map(
    
    # 'map' could be both used on vectors and lists
    
    ~ terra::rast(.x) %>% 
      terra::crop(temp) %>% 
      terra::mask(temp)) %>% 
  set_names('canopy', 'imp', 'nlcd') %>% 
  
  # create a raster stack - with plenty of raster layers
  
  terra::rast()

rasters_prj <-
  rasters %>% 
  terra::project(y = 'epsg:32618',
                 
                 # ? use this method to preserve the type of each layers
                 
                 method = 'near') 
  
  # epsg 32618 is the UTM Zone that appropriate for Washington DC,
  # and also the CRS of our shapefiles

rm(imp)

rm(temp)

# the SpatVector ----------------------------------------------------------

# Convert an sf object to a SpatVector:
  # to use an sf object, usually convert it to a SpatVector.
  # the latter has different format and feature names comparing to an sf object

points$birds %>% 
  terra::vect() %>% 
  
  # however, as a SpatVector don't work with tidyverse,
  # we maintain it as an sf object as long as we can. In this case:
  
  st_as_sf()

# pre-processing ----------------------------------------------------------

rasters$can %>% 
  
  # subset the raster to DC area.
  # Because rasters are memory intensive, better to crop剪裁 the raster to a 
  # smaller object as early in the process as possible
  
  terra::crop(temp) %>% 
  
  # however, 'crop' subsets data to an extent, but does not remove
  # regions that don't overlap
  # Use 'mask' function to turn all values outside of the shape file to NA.
  
  terra::mask(temp) %>% 
  terra::plot()

# Now you! Use temp, the rasters list, and purrr::map() to crop and mask all
# rasters to the DC shapefile: 
# (the result has been pasted into the 'load raster' sector)

rasters %>% 
  map(
    ~ .x %>% 
      terra::crop(temp) %>% 
      terra::mask(temp))

# tmap --------------------------------------------------------------------

# Basemap:

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  # Canopy cover:
  # (a layer of a raster stack could be used by the same reference method
  # as an item from a list)
  
  tm_shape(rasters_prj$canopy) +
  
  # add some transparency by using 'alpha'
  
  tm_raster(alpha = 0.6,
            
            # change the color palette to better represent forests
            
            palette = 'Greens') +
  
  # Impervious surface:
  
  tm_shape(rasters_prj$imp) +
  tm_raster(alpha = 0.6,
            palette = 'OrRd') +
  
  # Land cover:
  
  tm_shape(rasters_prj$nlcd) +
  
  # 'cat' means 'categorical raster'
  
  tm_raster(palette = 'cat',
            style = 'cat')

# extracting data from rasters --------------------------------------------

# Global summary statistic, mean:

rasters$imp %>% 
  
  # to use the 'global' syntax, need to remove all NA values first
  
  terra::global(mean, na.rm = TRUE)
  
  # the output shows that the impervious land cover is 38%

# Mean impervious surface by census tract:

polygons$census %>% 
  st_transform(crs = 5070) %>% 
  
  # convert the sf object to a SpatVector, in use of the 'extract' function
  
  terra::vect() %>% 
  terra::extract(
    
    # the name of raster that is extracting from
    
    rasters$imp,
    
    # the name of the raster that is extracting to 
    
    .,
    
    # the summary statistics of interest, here it is 'mean'
    
    mean,
    
    # exclude NAs
    
    na.rm = TRUE) %>% 
  
  # only perserve a vector of the mean impervious surface 
  # of all of the census tract
  
  pull()

# Now you! Add a field to polygons$census that represents the proportion of
# impervious surface in each polygon:

polygons$census %>% 
  
  # can use 'mutate' for an sf object, too
  
  mutate(
    imp = 
      polygons$census %>% 
      st_transform(crs = 5070) %>% 
      terra::vect() %>% 
      terra::extract(
        rasters$imp,
        .,
        mean,
        na.rm = TRUE) %>% 
      pull())

# Extract impervious surface to points: (What does it mean, in practical?)

points$birds %>%
  mutate(
    imp = 
      points$birds %>% 
      st_transform(crs = 5070) %>% 
      terra::vect() %>% 
      
      # at each census tract where the number of sick birds found
      
      terra::extract(rasters$imp, .) %>% 
      pull(imp))

  # where does the 30m statistics come from? 
  # because of the size of the pixels?

# Now you! Add a field to points$birds that represents the proportion of
# impervious surface within 500 m of each location:

points$birds %>% 
  mutate(
    imp = 
      points$birds %>% 
      st_transform(crs = 5070) %>% 
      
      # generate a 500 meters' buffer around the point
      
      st_buffer(500) %>% 
      terra::vect() %>% 
      terra::extract(
        rasters$imp,
        .,
        mean,
        na.rm = TRUE) %>% 
      pull(imp))

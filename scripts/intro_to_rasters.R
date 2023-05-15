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

# read in a raster file from the terra package

imp <-
  terra::rast('data/raw/rasters/impervious_surface.tif')
  
# resolution here means 30 meters * 30 meters in the real world

terra::plot(imp)

# Now you! Complete the code below to read in all of the rasters at once:

rasters <-
  list.files('data/raw/rasters/',
             full.names = TRUE) %>% 
  map(
    
    # 'map' could be both used on vectors and lists
    
    ~ terra::rast(.x) %>% 
      terra::crop(
        unionized_polygons$census %>% 
          st_transform(5070) %>% 
          terra::vect()) %>% 
      terra::mask(
        unionized_polygons$census %>%
          st_transform(5070) %>%
          terra::vect())) %>%
  set_names('canopy', 'imp', 'nlcd') %>% 
  
  # create a raster stack - with plenty of raster layers
  
  terra::rast()

# Transforming projections for the plotting purpose

rasters_prj <-
  rasters %>% 
  terra::project(
    
    # epsg 32618 is the UTM Zone that appropriate for Washington DC,
    # and also the CRS of our shapefiles
    
    y = 'epsg:32618',
    
    # "nlcd" is a categorical layer meaning "land cover". the default method
    # "bilinear" is problematic with categorical layers
    
    # use "near" instead, 
    # to preserve classes of the layers, only for a plotting purpose
    
    method = 'near') # notice that the colors of layers remain same now

rm(imp)

rm(temp)

# the SpatVector ----------------------------------------------------------

# Convert an sf object to a SpatVector:
  # to use an sf object in **rasters**, usually convert it to a SpatVector.
  # not "SpatRaster" but "SpatVector"!

points$birds %>% 
  terra::vect() %>% 
  
  # however, as a SpatVector don't work with tidyverse,
  # we maintain it as an sf object as long as we can. In this case:
  
  st_as_sf()

# pre-processing ----------------------------------------------------------

rasters$can %>% 
  
  # subset the raster's extent to DC area.
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
  tm_raster(
    
    # add some transparency by using 'alpha'
    
    alpha = 0.6,
    
    # change the color palette to better represent forests
    
    palette = 'Greens') +
  
  # Impervious surface:
  
  tm_shape(rasters_prj$imp) +
  tm_raster(alpha = 0.6,
            palette = 'OrRd') +
  
  # Land cover:
  
  tm_shape(rasters_prj$nlcd) +
  tm_raster(
    
    # 'cat' means 'categorical raster'. by default, this argument treats the 
    # input as continuous data, as such we need to change
    
    palette = 'cat',
    style = 'cat')

# extracting data from rasters --------------------------------------------

# Global summary statistic, mean:

  # use "raster" but not projected "raster_prj" for accuracy

rasters$imp %>% 
  
  # to use the 'global' syntax, need to remove all NA values first
  
  terra::global(mean, na.rm = TRUE)
  
  # the output shows that the impervious land cover is 38%

# Mean impervious surface by "census tract":

polygons$census %>% 
  st_transform(crs = 5070) %>% 
  
  # convert the sf object to a SpatVector, in use of the 'extract' function
  
  terra::vect() %>% 
  terra::extract(
    
    # the name of raster that is extracting from
    
    rasters$imp,
    
    # the name of the generated SpatVector that is extracting to 
    
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
  
  # can use 'mutate' for an sf object, but not raster, and that's why we prefer
  # sf objects in the most of the cases
  
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

# Extract impervious surface to points: 
# (What does it mean, in practical? 知道每一只鸟生病时候的土地硬化程度)

points$birds %>%
  
  # make it a field by using "mutate"
  # in sf objects, "feature" is like rows while "field" represents columns
  
  mutate(
    imp_30m2 = 
      points$birds %>% 
      st_transform(crs = 5070) %>% 
      terra::vect() %>% 
      
      # return the impervious surface that each location a sick bird was found,
      # since "the impervious surface" is a continuous variable
      
      terra::extract(rasters$imp, .) %>% 
      pull(imp))
  
  # Now, this new field represent the impervious surface around 30 meters (bc of
  # the resolution) of each point that a sick bird was found. 
  # However, birds can travel more than 30 meters, so:

# Now you! Add a field to points$birds that represents the proportion of
# impervious surface within 500 m of each location:

points$birds %>% 
  mutate(
    imp_500m2 = 
      points$birds %>% 
      st_transform(crs = 5070) %>% 
      
      # generate a 500 meters' buffer around the point, 
      # by transform point values into polygons
      
      st_buffer(500) %>% 
      terra::vect() %>% 
      terra::extract(
        rasters$imp,
        .,
        
        # these two arguments are added bc now it's from a sf polygon,
        # but not point
        
        mean,
        na.rm = TRUE) %>% 
      pull(imp))

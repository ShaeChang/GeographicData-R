# In-class quiz: District crime

library(sf)
library(lubridate)
library(tmap)
library(tidyverse)

# Read in and process census data:

census <-
  st_read('data/raw/shapefiles/dc_census.geojson') %>%
  set_names(
    names(.) %>% 
      tolower()) %>% 
  select(!statefp)

# Read in and process crimes data:

crimes <-
  read_csv('data/raw/dc_crimes.csv') %>%
  
  # Subset to violent crimes in 2020:
  
  filter(offense_group == 'violent',
         year(date_time) == 2020) %>% 
  
  # Make spatial:
  
  st_as_sf(
    coords = c('longitude', 'latitude'),
    crs = 4326) %>% 
  
  # Transform to the same CRS as census:
  
  st_transform(crs = st_crs(census))

# A vector of unique offenses:

offenses <-
  crimes %>% 
  pull(offense) %>% 
  unique()

# question 2 --------------------------------------------------------------

# [1.5] Using purrr::map() and the offenses, crimes, census objects:
# - [1.0] Generate a list of shapefiles that tallies the number of violent
#   crimes committed by census tract and offense. You should have one shapefile
#   per type of offense;
# - [0.25] Name your list items by offense type;
# - [0.25] Assign the list to your global environment with the name
#   "crimes_by_offense".

# Generate a list of shapefiles tallying the number of crimes:

crimes_by_offense <-
  
  # use 'map' function to simplify repetitive operations:
  
  purrr::map(
    
    # set the variable
    
    offenses,
    
    # write the function
    
    ~ crimes %>% 
      
      # filter the type of the offense by the variable
      
      filter(offense == .x) %>% 
      
      # use geometry to find out which tract each crime occurred in
      
      st_join(census %>% 
                select(geoid)) %>% 
      
      # convert to a tibble for faster operation
      
      as_tibble() %>% 
      
      # count the number of the specific crimes by census tract
      
      group_by(geoid) %>% 
      summarise(n = n()) %>% 
      
      # counting the total number of each crime in all tracts
      
      left_join(census %>% 
                  select(geoid, geometry),
                .)) %>% 
  
  # name list elements by offense types
  
  set_names(offenses)

# question 3 --------------------------------------------------------------

# [0.5] Using a single purrr::map() function, transform the CRS of
# crimes_by_offense to 4326 [0.25] and assign the object to your global
# environment with the name crimes_by_offense_4326 [0.25].

# Transform the CRS of crime_by_offense to 4326

crimes_by_offense_4326 <-
  crimes_by_offense %>% 
  
  # use 'map' function to iterate through each element of the list
  
  purrr::map(
    ~ .x %>% 
      
      # set the CRS of the variable to 4326
      
      st_transform(crs = 4326))

# question 4 --------------------------------------------------------------

# [1.0] Using a single purrr::map() function:
# - [0.5] Convert the CRS of crimes and census objects to EPSG 4326;
# - [0.25] Assign the names “crimes” and “census” to the list items;
# - [0.25] Assign the list to your global environment with the name shapes_4326.

# Convert the CRS of crimes and census objects

shapes_4326 <-
  
  # make a list of the two object
  
  list(crimes,
       census) %>% 
  
  # use 'map' function to iterate through the 2 objects in the list
  
  purrr::map(
    
    # set the CRS to 4326
    
    ~ st_transform(.x, 
                   crs = 4326)) %>% 
  
  # set the names to the list items
  
  set_names('crimes', 'census')

# question 5 --------------------------------------------------------------

# [1.5] Using shapes_4326 and crimes_by_offense_4326, generate an interactive 
# tmap [0.5] where:
# - [0.25] The fill color of census tracts is determined by the number of
#   robberies in a given tract;
# - [0.25] Homicides are displayed as clusters of points;
# - [0.25] OpenStreetMap and Esri.WorldImagery are provided as background 
#   layers;
# - [0.25] The layers are named “Robberies” and “Homicides”.

# no maps in this question

# Generate an interactive tmap

# set the tmap mode into 'view' for an interactive tmap

tmap_mode('view')

# add two required base maps

tm_basemap(
  c('OpenStreetMap',
    'Esri.WorldImagery')) +
  
  # add a layer of robberies
  
  tm_shape(crimes_by_offense_4326$robbery,
           
           # name the layer
           
           name = 'Robberies') +
  
  # fills color of tracts by the number of robberies
  
  tm_polygons(col = 'n') +
  
  # add a layer displays the number of homicides
  
  tm_shape(crimes_by_offense_4326$homicide,
           
           # name the layer
           
           name = 'Homicides') +
  
  # displays the number of homicides as cluster of points
  
  tm_dots(clustering = TRUE)

# Extra credit ------------------------------------------------------------
# what is the eastern most and the western most state in the United States?

# The easternmost state is Maine; 
# The westernmost state is Washington at Cape Alava.






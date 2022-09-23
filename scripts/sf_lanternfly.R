# setup -------------------------------------------------------------------

library(lubridate)
library(sf)
library(tidyverse)

# A "new" theme:

theme_set(
  new = theme_bw())

# Load iNaturalist observations of spotted lanternflies (Note: The locations
# were recorded with the CRS EPSG 4326):

spotted_lanternfly <-
  read_rds('data/raw/spotted_lanternfly.rds') %>% 
  filter(quality_grade == 'research') %>% 
  select(
    !c(description,
       image_url,
       place_guess,
       quality_grade,
       user))

# Now you! Load the shapefiles states.shp, counties_low_res.geojson, and
# counties.geojson into your global environment.

states <-
  st_read('data/raw/states.shp') %>% 
  st_transform(crs = 5070) %>% 
  set_names(
    names(.) %>% 
      tolower())

counties_low_res <-
  st_read('data/raw/shapefiles/counties_low_res.geojson') %>% 
  st_transform(crs = 5070) %>% 
  set_names(
    names(.) %>% 
      tolower())
  
counties <-
  st_read('data/raw/shapefiles/counties.geojson') %>% 
  st_transform(crs = 5070) %>% 
  set_names(
    names(.) %>% 
      tolower())

# Now you! Modify your code above such that the spatial data are projected with
# the CRS EPSG 5070:

# Now you! Modify your code above such that all names are lowercase.

# subset shapefiles using non-spatial joins -------------------------------

# Plot the states and counties_low_res files:

states %>% 
  ggplot() +
  geom_sf()

counties_low_res %>% 
  ggplot() +
  geom_sf()

# Use a semi-join to subset the counties to just those in the conterminous
# United States:

counties_low_res %>% 
  semi_join( 
    
    # actually 'inner_join' does the same work here. 
    # what is the difference ???
    
    states %>% 
      as_tibble(), # every time convert shape file to a tibble first
    by = c('state_name' = 'name') # join the common columns
  ) %>% 
  ggplot() +
  geom_sf()

# Now you! Subset the counties file to just counties in states where
# spotted_lanternfly have been observed:

counties_lanternfly <-
  counties %>% 
  semi_join( 
    
    # 'semi_join' means filter join, opposite to 'anti_join'
    # filter 'counties' based on the presence in 'spotted_lanternfly'
    
    spotted_lanternfly,
    by = c('state_name' = 'state'))

# A map with county details for states with spotted lanternfly observations:

states %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = counties_lanternfly)  # a map with two layers
  
# summarize and join (non-spatial) ----------------------------------------

# Number of observations by state:

spotted_lanternfly %>% 
  group_by(state) %>% 
  summarize(n = n()) %>% 
  inner_join(
    states,
    ., # change the order of join
    by = c('name' = 'state') # order matters here, too
  ) %>% 
  
  # with non-spatial/spatial joins, always join the non-spacial file 
  # to the spacial file! Or you will get a tibble but not sf object.
  
  ggplot() +
  geom_sf(aes(fill = n))

# Now you generate a map where states are colored by the number of spotted
# lanternfly observations and states with no observations are colored gray.

spotted_lanternfly %>% 
  group_by(state) %>% 
  summarise(n = n()) %>% 
  full_join( # join all records of both sides
    states,
    .,
    by = c('name' = 'state')
  ) %>% 
  ggplot() +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c(
    option = 'plasma', # an aesthetics
    na.value = '#dcdcdc') # use light gray to present the NA data

# Spatial joins -----------------------------------------------------------

# Now you! Convert the spotted_lanternfly data frame to a spatial points object
# with the same CRS as the counties file:

lanternfly_sf <-
  spotted_lanternfly %>% 
  st_as_sf(
    coords = c('longitude', 'latitude'), # longitude comes first
    crs = 4326) %>%  
  
    # why use crs = 4326 but not crs = 5070 directly???
  
  st_transform(crs = st_crs(counties))

# Conduct a spatial join between lanternfly_sf and counties_lanternfly:

lanternfly_sf %>% 
  st_join(  # spatial join: 'st_join'
    counties_lanternfly %>% 
      select(geoid)) %>% 
  
    # just add geoid (of each county) to the sf object
    # the st_join is based on geometry, but why don't select 'geometry' column?
  
  as_tibble() %>% 
  group_by(geoid) %>% 
  summarise(n = n())

# Now you! Map counties by the number of spotted lanternflies observed:

lanternfly_sf %>% 
  st_join(
    counties_lanternfly %>% 
      select(geoid)) %>% 
  as_tibble() %>% 
  group_by(geoid) %>% 
  summarise(n = n()) %>% 
  inner_join( 
    # if only wanna see counties with observations, use 'inner_join'
    # otherwise use 'full_join'
    counties_lanternfly,
    .,
    by = 'geoid' # can simply write the same column name like this
  ) %>% 
  ggplot() +
  geom_sf(
    aes(fill = n),
    color = NA) + # remove the thick border of counties
  scale_fill_viridis_c(
    na.value = '#dcdcdc') 

# Now you! Create a map that shows the number of spotted lanternfly observations
# in impacted counties above the states in the conterminous US:

lanternfly_sf %>% 
  st_join(
    counties_lanternfly %>% 
      select(geoid)) %>% 
  as_tibble() %>% 
  group_by(geoid) %>% 
  summarise(n = n()) %>% 
  inner_join(
    counties_lanternfly,
    .,
    by = 'geoid'
  ) %>% 
  ggplot() +
  geom_sf(data = states) + # have to put this layer first, don't know why
  geom_sf(
    aes(fill = n),
    color = NA) + 
  scale_fill_viridis_c(
    na.value = '#dcdcdc')

# Subset states to those with spotted lanternfly observations using a spatial
# filter:

states %>% 
  st_filter(lanternfly_sf)

# Modify the map so that only states with spotted lanternfly observations are
# shown:

lanternfly_sf %>% 
  
  # Join counties to lanternflies
  
  st_join(
    counties_lanternfly %>% 
      select(geoid)) %>% 
  as_tibble() %>% 
  
  # Tally lanternflies by county
  
  group_by(geoid) %>% 
  summarise(n = n()) %>% 
  
  # Join tally to counties
  
  inner_join(
    counties_lanternfly,
    .,
    by = 'geoid'
    
    # Plotting data
    
  ) %>% 
  ggplot() +
  
  # background states
  
  geom_sf(
    data = states %>%
      st_filter(lanternfly_sf)) + # modify the filtered data
  
  # Affected counties
  
  geom_sf(
    aes(fill = n),
    color = NA) + 
  scale_fill_viridis_c(
    na.value = '#dcdcdc') +
  
  # State borders
  
  geom_sf(
    data = states %>%
      st_filter(lanternfly_sf),
    fill = NA) # layers are in order

# Now you! Add state lines to the top of the map:
  # shown above.
  # comments added

# A review of joins -------------------------------------------------------

# Non-spatial joins with sf files are equivalent to data frame joins:

states %>% 
  semi_join(
    spotted_lanternfly, 
    by = c('name' = 'state')) %>% 
  ggplot() +
  geom_sf()

# Non-spatial joins can be a powerful tool when used in conjunction with
# summarize:

spotted_lanternfly %>% 
  group_by(state) %>% 
  summarize(n = n()) %>% 
  full_join(
    states, 
    .,
    by = c('name' = 'state')) %>% 
  ggplot() +
  geom_sf(aes(fill = n))

# Filtering spatial joins allow us to filter based on location:
# similar to 'semi_join'

states %>% 
  st_filter(lanternfly_sf) %>% 
  ggplot() +
  geom_sf()

# Intersecting spatial joins allow us to compare data between shapefiles:

lanternfly_sf %>% 
  st_join(states) %>% 
  as_tibble() %>% 
  group_by(name) %>% 
  summarize(n = n()) %>% 
  full_join(
    states,
    .,
    by = 'name') %>%
  ggplot() +
  geom_sf(aes(fill = n))

# Find out more!

?st_join

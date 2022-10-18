# Applications of the purrr package!

# setup -------------------------------------------------------------------

library(lubridate)
library(sf)
library(tmap)
library(tidyverse)

# Set plot theme:

theme_set(
  new = theme_bw())

# Assign a read directory:

read_dir <-
  'data/processed/lanternfly_tmap'

# Load iNaturalist observations of spotted lanternflies (Note:  These data were
# recorded in EPSG 4326):

spotted_lanternfly <-
  read_dir %>% 
  file.path('lanternfly_proc.rds') %>% 
  read_rds() %>% 
  st_as_sf(
    coords = c('longitude', 'latitude'),
    crs = 4326) %>% 
  st_transform(crs = 5070)

# Now you! Modify the code block above such that it produces a shapefile with
# the CRS EPSG 5070.

# Now you! Modify the code below with a map statement that will read in the files
# counties_east.geojson and states_east.geojson and store them as a list.

shapes <-
  list.files(
    read_dir,
    pattern = 'geojson',
    full.names = TRUE) %>% 
  map(
    ~ st_read(.x)) %>% 
  set_names('counties', 'states')

# Now you! Use a spatial join to subset the counties to just those that contain
# lanternflies:

shapes$counties %>% 
  st_filter(spotted_lanternfly)

# Repeat the process with states:

shapes$states %>% 
  st_filter(spotted_lanternfly)

# Now you! Use purrr::map to subset "shapes" to polygons that contain
# observations of spotted lanternfly. Assign the resultant object to the global
# environment with the name "fly_shapes":

fly_shapes <-
  shapes %>% 
  map(
    ~ st_filter(.x, spotted_lanternfly))

# Now you! Calculate the total number of observations in fly_shapes$counties by
# geoid:

spotted_lanternfly %>% 
  st_join(
    
    # use the spatial join because there are no common keys in the two data sets
    
    fly_shapes$counties %>% 
      select(geoid)) %>% 
  
  # when we join a spot file to a multi-polygon file, the polygon info is loss
  
  as_tibble() %>% 
  
  # it's better to convert to a tibble to use 'group' & 'summarize'
  # to speed up the process for the computer
  
  group_by(geoid) %>% 
  summarize(n = n())

# Repeat the process with states:

spotted_lanternfly %>% 
  st_join(
    fly_shapes$states %>% 
      select(geoid)) %>% 
  as_tibble() %>% 
  group_by(geoid) %>% 
  summarize(n = n())

# Now you! Use purrr::map to calculate the total number of lanternfly
# observations by county and state:

fly_shapes %>% 
  map(
    ~ spotted_lanternfly %>% 
      st_join(.x %>% 
                select(geoid)) %>% 
      as_tibble() %>% 
      group_by(geoid) %>% 
      summarize(n = n()))

# Now you! Plot the number of lanternfly observations per county. Include
# shapes$states as a background layer for the plot:

spotted_lanternfly %>% 
  st_join(
    fly_shapes$counties %>% 
      select(geoid)) %>% 
  as_tibble() %>% 
  group_by(geoid) %>% 
  summarize(n = n()) %>% 
  left_join(
    
    # any ordinary joins work, don't need st_join bc the spatial info has lost
    
    fly_shapes$counties,
    .,
    
    # the first tibble of the left_join should be the first data set
    
    by = 'geoid') %>% 
  ggplot() +
  geom_sf(data = shapes$states) +
  
  # this layer comes first to put it into the bottom
  
  geom_sf(aes(fill = n),
          color = NA) +
  scale_fill_viridis_c(option = 'plasma')

# Plot the number of lanternfly observations by state:

spotted_lanternfly %>% 
  st_join(
    fly_shapes$states %>% 
      select(geoid)) %>% 
  as_tibble() %>% 
  group_by(geoid) %>% 
  summarize(n = n()) %>% 
  left_join(
    fly_shapes$states,
    .,
    by = 'geoid') %>% 
  ggplot() +
  geom_sf(data = shapes$states) +
  geom_sf(aes(fill = n),
          color = NA) +
  scale_fill_viridis_c(option = 'plasma')

# Now you! Use purrr::map to plot the number of lanternfly observations by
# county and states. Include shapes$states as a background layer for the
# plot:

fly_shapes %>% 
  map(
    ~ spotted_lanternfly %>% 
      st_join(.x %>% 
          select(geoid)) %>% 
      as_tibble() %>% 
      group_by(geoid) %>% 
      summarize(n = n()) %>% 
      left_join(
        .x,
        .,
        by = 'geoid') %>% 
      ggplot() +
      geom_sf(data = shapes$states) +
      geom_sf(aes(fill = n),
              color = NA) +
      scale_fill_viridis_c(option = 'plasma'))

# Now you! Plot the number of observations by county in 2019:

spotted_lanternfly %>% 
  filter(year(datetime) == 2019) %>% 
  st_join(fly_shapes$counties %>% 
      select(geoid)) %>% 
  as_tibble() %>% 
  group_by(geoid) %>% 
  summarize(n = n()) %>% 
  left_join(
    fly_shapes$counties,
    .,
    by = 'geoid') %>% 
  ggplot() +
  geom_sf(data = shapes$states) +
  geom_sf(aes(fill = n),
          color = NA) +
  scale_fill_viridis_c(option = 'plasma')

# Now you! Plot the number of observations by county in from 2015 to 2022 (there
# will be many separate plots):

map(
  2015:2022, # this is our variable here
  ~ spotted_lanternfly %>% 
    filter(year(datetime) == .x) %>% 
    st_join(fly_shapes$counties %>% 
              select(geoid)) %>% 
    as_tibble() %>% 
    group_by(geoid) %>% 
    summarize(n = n()) %>% 
    left_join(
      fly_shapes$counties,
      .,
      by = 'geoid') %>% 
    ggplot() +
    geom_sf(data = shapes$states) +
    geom_sf(aes(fill = n),
            color = NA) +
    labs(title = .x) +
    scale_fill_viridis_c(option = 'plasma',
                         limits = c(0, 900)))

# Now you! Add year as the title of each plot. (modified as above)

# Set scale limits: (same color represents same colors by adding 'limits')

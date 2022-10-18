
# Assignment 04
# Xiyu Zhang

# Set up ----------------------------------------------------------

# Load libraries

library(sf)
library(lubridate)
library(tidyverse)

# Set the theme of all plots

theme_set(
  new = theme_void())

# Read in and process cicadas data

# assign the data to global environment with the specific name

cicadas_temp<-
  
  # load the data as a tibble
  
  read_csv('data/raw/cicadas_brood_x_2021.csv') %>%
  as_tibble() %>% 
  
  # subset the data where the quality grade is 'research'
  
  filter(quality_grade == 'research') %>% 
  
  # subset the data to the fields datetime, scientific_name, 
  # user, longitude, and latitude
  
  select(datetime, scientific_name, user, longitude, latitude)
  
  # select(c(datetime:user, longitude:latitude)) 更简洁的选择方式

# Read in and process counties data

# assign the data to global environment with the specific name

counties_temp <-
  
  # load the data as a simple features shapefile
  
  st_read('data/raw/shapefiles/counties.geojson') %>% 
  
  # convert all field name to lowercase
  
  set_names(
    names(.) %>% 
      tolower()) %>% 
  
  # transform the CRS to EPSG 2283
  
  st_transform(crs = 2283)

# Subsetting data ---------------------------------------------------------

# read in and process cicadas data

# assign the subset object to global environment

cicadas_brood_x <-
  
  # subset the data that could be identified to species
  
  cicadas_temp %>% 
  filter(scientific_name %in% 
           c('Magicicada cassinii', 
             'Magicicada septendecim',
             'Magicicada septendecula'))
  
# remove cicadas_temp from the global environment

rm(cicadas_temp)

# subset counties in DMV area

# assign to global environment with specific name

counties <-
  
  # subset counties in DMV area
  
  counties_temp %>% 
  filter(state_name %in%
           c('District of Columbia',
             'Maryland',
             'Virginia'))

# remove counties_temp from the global environment

rm(counties_temp)

# convert and subset cicadas_brood_x

# assign specific name to the subject in global environment

cicadas_sf <-
  
  # convert it to an sf point file
  
  cicadas_brood_x %>% 
  st_as_sf(
    coords = c('longitude', 'latitude'),
    crs = 4326) %>% 
  
  # transform the CRS to the same projection as counties
  
  st_transform(
    st_crs(counties)) %>% 
  
  # subset the data to observations in DMV area
  
  st_filter(counties)

# remove cicadas_brood_x from global environment

rm(cicadas_brood_x)

# Data exploration --------------------------------------------------------

# generate a kable displaying that each species of cicada was first 
# observed in each state

cicadas_sf %>% 
  
  # conduct a spatial join based on geometry, so that get to know the state 
  # of each observation 
  
  st_join(
    counties %>% 
      select(state_name)) %>% 
  
  # convert the sf object into a tibble
  
  as_tibble() %>% 
  
  # filter the earliest datetime of observation 
  # for each species in each state
  
  group_by(scientific_name, state_name) %>% 
  filter(datetime == min(datetime)) %>% 
  
  # select the columns of interest
  
  select(state_name, scientific_name, datetime) %>% 
  
  # arrange the table from earliest to latest datetime
  
  arrange(datetime) %>% 
  
  # generate a kable for display
  
  kableExtra::kable() %>% 
  kableExtra::kable_styling(latex_options = "striped")

# generate a choropleth map of counties visualizing the number of 
# cicadas that were observed in each county

cicadas_sf %>% 
  
  # conduct a spatial join, combine each observation with a county
  
  st_join(
    counties %>% 
      select(name)) %>% 
  
  # convert the sf object into a tibble
  
  as_tibble() %>% 
  
  # count the total observation amount in each county
  
  group_by(name) %>% 
  summarise(n = n()) %>% 
  
  # combine the geometry information of counties with observation amounts,
  # 'counties' comes first to keep counties with NA value
  
  full_join(counties, .) %>% 
  
  # generate a choropleth map
  
  ggplot() +
  
  # the fill color is determined by number of observations
  
  geom_sf(aes(fill = n)) +
  
  # counties with no observations colored light gray
  
  scale_fill_viridis_c(
    na.value = '#dcdcdc') 















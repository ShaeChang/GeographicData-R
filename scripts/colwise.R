
# setup -----------------------------------------------------------------

library(tidyverse)

source('scripts/source_script.R')

# Read in the data:

iris <- 
  read_rds('data/raw/iris.rds')

read_rds('data/processed/weather_tidy.rds') %>% 
  list2env(.GlobalEnv)

# across ----------------------------------------------------------------

observations

# iterating mutation across multiple variables with mutate_at():

observations %>% 
  mutate_at(
    .vars = c('precip', 
              'snow', 
              'temperature_min',
              'temperature_max'),
    .funs = ~ as.numeric(.))

# iterating mutation across multiple variables with across:

observations %>% 
  mutate(
    across(
      .cols = precip:temperature_max,
      .fns = ~ as.numeric(.)))

# ... or more simply:

observations %>% 
  mutate(
    across(
      precip:temperature_max,
      ~ as.numeric(.)))

# Now you! Use summarize and across to calculate the mean value for each of
# the weather measurements at each weather stations:

observations %>% 
  mutate(
    across(
      precip:temperature_max,
      ~ as.numeric(.))) %>% 
  group_by(station) %>% 
  summarize_at(
    c('precip',
      'snow',
      'temperature_min',
      'temperature_max'),
    ~ mean(., na.rm = TRUE))

# across and where ------------------------------------------------------

# but what about the _if family of functions?

observations %>% 
  mutate(
    across(
      precip:temperature_max,
      ~ as.numeric(.))) %>% 
  group_by(station) %>% 
  summarize_if(
    is.numeric,
    ~ mean(., na.rm = TRUE))

# Using across with where:

observations %>% 
  mutate(
    across(
      precip:temperature_max,
      ~ as.numeric(.))) %>% 
  group_by(station) %>% 
  summarize(
    across(
      where(is.numeric),
    ~ mean(., na.rm = TRUE)))

# Now you! For each species of iris, calculate the mean value of each numeric
# variable using summarize(), across(), and where().

iris %>% 
  group_by(species) %>% 
  summarize_if(
    is.numeric,
    ~ mean(.))

# across with multiple functions ----------------------------------------

# Calculate summary statistics for each station's temperature_min value:

observations %>% 
  mutate(
    across(
      precip:temperature_max,
      ~ as.numeric(.))) %>% 
  group_by(station) %>% 
  summarize(
    temperature_min_mean = mean(temperature_min, na.rm = TRUE),
    temperature_min_min = min(temperature_min, na.rm = TRUE),
    temperature_min_max = max(temperature_min, na.rm = TRUE))

# Alternatively, create a list of functions to run:

summary_stats <-
  list(
    mean = ~ mean(., na.rm = TRUE),
    min  = ~ min(., na.rm = TRUE),
    max  = ~ max(., na.rm = TRUE))

# Then calculate summary statistics for each station's temperature_min value:

observations %>% 
  mutate(
    across(
      precip:temperature_max,
      ~ as.numeric(.))) %>% 
  group_by(station) %>% 
  summarize(
    across(temperature_min, 
           summary_stats))

# Now you! Modify the code block, using across() and the summary_stats object,
# to calculate summary statistics for each iris species.

iris %>% 
  group_by(species) %>% 
  summarize(
    petal_length_mean = mean(petal_length))

# colwise filter --------------------------------------------------------

# Find an NA in any variable:

observations %>% 
  filter(
    if_any(
      everything(),
      ~ is.na(.)
    ))
    
# Find records where all numeric variables are NA:

observations %>% 
  mutate(
    across(
      precip:temperature_max,
      ~ as.numeric(.))) %>% 
  filter(
    if_all(
      where(is.numeric),
      ~ is.na(.)))
         
# Now you! Modify or add to the code block to filter the weather data to
# records in which either snow or precipitation are NA:

observations %>% 
  mutate(
    across(
      precip:temperature_max,
      ~ as.numeric(.)))

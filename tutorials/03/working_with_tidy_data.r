
# setup -------------------------------------------------------------------

library(tidyverse)

source('scripts/source_script.R')

# read in the data:

file.path(
  'data/processed',
  'weather_tidy.rds') %>% 
  read_rds() %>% 
  list2env(.GlobalEnv)

# mutation, assignment ----------------------------------------------------

stations

elevation

stations$elevation 

# mutate in base R --------------------------------------------------------

temp <-
  stations

# Add a column of elevation in feet:

temp$elevation <-
  temp$elevation*3.28

# Look again at the observations file:

observations

# Now you! Use as.numeric() to convert observations$precip from a character to a
# numeric column and assign it to column name precip:



# But as a warning:

as.numeric(c(1, 2, '3,1'))

# mutate in the tidyverse -------------------------------------------------

# Remove temp file:

rm(temp)

# Convert elevation from meters to feet:

mutate(
  stations,
  elevation = elevation*3.28)

# Now you! Re-arrange the above as a piped code block:

 

# Add a new variable with mutate

mutate(
  stations,
  elevation = elevation*3.28)

# multiple mutations ------------------------------------------------------

# Look again at observations:

observations

# Now you! Transform the snow column from a character vector to a numeric
# vector:


  
# Use chained mutate statements to transform snow and temperature_min:
  


# Separate multiple mutate statements with a comma:



# Now you! Modify our mutation statement from above such that snow,
# temperature_min, and temperature_max are all numeric.

 

# transmute ---------------------------------------------------------------

# Transmute to return just the columns "name" and elevation in feet:

stations

# Now you! Copy-and-paste your transmute code into the space below. Modify the
# code such that the key for the column “name” is station_name:



# subsetting date frames: by column ---------------------------------------

# Subset by position (index) with select:

stations

# Subset by multiple positions (indices) with select:

stations %>% 
  select()

# Subset by column name with select:

stations %>% 
  select()

# Subset by a range of column names with select:

stations %>% 
  select()

# Subset by a selection of column names:

stations %>% 
  select()

# Subset to all but a given column:

stations %>% 
  select(state)

# Subset to all but a selection of adjacent columns:

stations %>% 
  select(!state)

# Subset to all but a selection of non-adjacent columns:

stations %>% 
  select()

# Now you! Use select to subset stations to station, name, longitude, latitude,
# and elevation:

stations

# Modify the above with select(), such that the binding "station" is changed to
# "station_id":

stations %>% 
  select(station,
         name:state,
         longitude:elevation)

# Modify the above with rename(), such that the binding "station" is changed to
# "station_id":

stations %>% 
  select(station,
         name:state,
         longitude:elevation)

# Select with everything() to reorder columns:

stations %>% 
  select(state)

# Subset by condition:

stations %>% 
  select()

# Now you! The function is.character is a logical test for whether a value is a
# character. Subset the stations data frame to just character columns:

stations

# Subsetting the data and returning a vector with pull():

stations

# subsetting date frames: by row index ------------------------------------

# In the tidyverse, subset by row index with slice():

observations

# Subset rows by a vector of indices:

observations %>% 
  slice()

# Now you! Subset the observations data frame to the first and third values:

observations

# slice_head():

observations %>% 
  slice_head()

# slice_tail():

observations %>% 
  slice_tail()

# Now you! Subset the observations data frame to the last three rows of the data
# set and only the column date:

observations

# Arrange data by a column and subset the data:

observations %>% 
  mutate(
    temperature_min = 
      as.numeric(temperature_min)) %>% 
  arrange(temperature_min)

# Arrange data in descending order:

observations %>% 
  mutate(
    temperature_max = 
      as.numeric(temperature_max)) %>% 
  arrange(
    desc(temperature_max))

# Now you! Extract a vector of the dates on which the five highest temperatures
# were observed:

observations

# subsetting date frames: by row & condition ------------------------------

# Use the tidyverse function filter() to subset to stations greater than 40
# degrees latitude:

stations

# Get a vector of station IDs where the latitude is greater than 40:



# Now you! Use stations_north and the filter function to subset the observations
# to where the latitude of the station is greater than 40:

observations

# Subset the observations to the maximum precipitation value:

observations %>% 
  filter(
    precip == max(precip))

# Remove NA values with drop_na():

observations %>% 
  filter(
    precip == max(precip))

# Remove NA values with na.rm:

observations %>% 
  filter(
    precip == max(precip))

# Remove NA values with !is.na():

observations %>% 
  filter(
    precip == max(precip))

# Filter based on multiple conditions:

observations %>% 
  filter(
    lubridate::year(date) == 2010)

# Filter based on multiple conditions, a caveat:

observations %>% 
  filter(!is.na(precip),
         precip == max(precip))

# Now you! subset the data frame to the day in 2015 in which the maximum
# temperature was observed at weather stations above 40 degrees north:
 
observations

# Using distinct() to subset to unique columns:

observations %>% 
  select(station)

# Now you! Use subsetting to extract a vector that contains the unique dates of
# weather observations in 2015:

observations

# joins: an introduction --------------------------------------------------

observations

stations

# left join ---------------------------------------------------------------

# Join observations to stations:

stations %>% 
  left_join(observations,
            by = 'station')

# Rename the station id field of observations:

observations_with_id <-
  observations %>% 
  rename(station_id = station)

# Join observations to stations, with different key names:

stations %>% 
  left_join(observations_with_id,
            by = 'station')

# Two tables with compound keys:

observations_precip <-
  observations %>% 
  select(station:precip)

observations_snow <-
  observations %>% 
  select(station, date, snow)

# Join two tables with compound keys:

observations_precip %>% 
  left_join(observations_snow,
            by = 'station')

# Now you! Subset the stations table to station IDs in the vector stations_north
# and join with the observations table:

stations
 
# filtering joins ---------------------------------------------------------

# Filter station by latitude:

stations %>% 
  filter(latitude > 40)

# inner-join:

observations %>% 
  left_join(
    stations %>% 
      filter(latitude > 40),
    by = 'station')

# semi-join:

observations %>% 
  left_join(
    stations %>% 
      filter(latitude > 40),
    by = 'station')

# anti-join:

observations %>% 
  left_join(
    stations %>% 
      filter(latitude > 40),
    by = 'station')

# Now you! Subset observations to those that did not occur in California
# (CA):




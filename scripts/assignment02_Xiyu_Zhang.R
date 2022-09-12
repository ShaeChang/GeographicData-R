
# Library corresponding packages ------------------------------------------

library(tidyverse)
library(readr)

# Import the messy data ---------------------------------------------------

messy_weather <- 
  read_csv("messy_weather.csv")

# Separate 2 levels of observation ----------------------------------------

# Longitude, latitude, elevation, state and name are all station attributes.

station <- 
  select(messy_weather,
         station,
         longitude,
         latitude,
         elevation,
         state, 
         name) %>%
  distinct()

# The station of recording, date and variable are all recording attributes.

weather_record_1 <- 
  select(messy_weather,
         station,
         year:march_31)

# Make weather records tidy -----------------------------------------------

# Rename the column names for next step to combine year, month and day together.

colnames(weather_record_1) <-
  c('station',
    'year',
    'month',
    'variable',
    1:31)

# Make the recording tidy.

weather_record_2 <- 
  pivot_longer(weather_record_1,
               cols = '1':'31',
               names_to = 'day',
               values_to = 'value') %>%
  unite(col = 'date',
        c(year,
          month,
          day),
        sep = '-') %>%
  pivot_wider(names_from = variable,
              values_from = value) %>%
  separate(col = temperature_min_max,
           into = c('temperature_min',
                    'temperature_max'),
           sep = ':')

# Separate 2 more levels of observation -----------------------------------

# Snow is only a type of precipitation, so it is an observation in lower level.

weather_snow <- 
  select(weather_record_2,
         station,
         date,
         snow) %>%
  distinct()

# The other attributes are on the same level of observation. Remove "snow".

weather_record <- 
  select(weather_record_2,
         -'snow')

# The deliverable result --------------------------------------------------

# The tibbles are ordered according to the level of observation.

Weather_deliverable <-
  list(station,
       weather_record,
       weather_snow)



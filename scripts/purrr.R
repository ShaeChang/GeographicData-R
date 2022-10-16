
# setup -------------------------------------------------------------------

library(lubridate)
library(tidyverse)

iris_tbl <-
  read_rds('data/raw/iris.rds')

# split-apply-combine, Rube Goldberg method -------------------------------

# Now you! Subset iris_tbl to observations of Iris setosa:

iris_tbl



# Now you! Write a custom function that will calculate the mean sepal length of
# any of the iris species:



# Now you! Use your custom function to generate a list of mean sepal lengths for
# the three iris species:




# introducing purrr! ------------------------------------------------------




# A simpler way ....

iris_tbl

# mapping when it matters! ------------------------------------------------

# Read in the shapefiles, convert column names to lower case, then transform the
# CRS to EPSG 5070:

states <-
  st_read('data/raw/shapefiles/states.shp') %>% 
  set_names(
    names(.) %>% 
      tolower()) %>% 
  st_transform(5070)

counties_low_res <-
  st_read('data/raw/shapefiles/counties_low_res.geojson') %>% 
  set_names(
    names(.) %>% 
      tolower()) %>% 
  st_transform(crs = 5070)

counties <-
  st_read('data/raw/shapefiles/counties.geojson') %>% 
  set_names(
    names(.) %>% 
      tolower()) %>% 
  st_transform(crs = 5070)

# Read in as a list:

my_shapes <-
  list(
    st_read('data/raw/shapefiles/states.shp'),
    st_read('data/raw/shapefiles/counties_low_res.geojson'),
    st_read('data/raw/shapefiles/counties.geojson'))

# Read in all at once:






# setup -------------------------------------------------------------------

library(lubridate)
library(tidyverse)
library(sf)

iris_tbl <-
  read_rds('data/raw/iris.rds')

# split-apply-combine, Rube Goldberg method -------------------------------

# Now you! Subset iris_tbl to observations of Iris setosa:

list(
  iris_tbl %>% 
    filter(species == 'setosa') %>% 
    pull(sepal_length) %>% 
    mean(),
  
  iris_tbl %>% 
    filter(species == 'versicolor') %>% 
    pull(sepal_length) %>% 
    mean(),
  
  iris_tbl %>% 
    filter(species == 'virginica') %>% 
    pull(sepal_length) %>% 
    mean())

# Now you! Write a custom function that will calculate the mean sepal length of
# any of the iris species:

get_mean_sepal <- 
  function(x) { 
    
    # set the variable as 'x'
    
    iris_tbl %>% 
      filter(species == x) %>% 
      pull(sepal_length) %>% 
      mean()
  }

get_mean_sepal('setosa')

# Now you! Use your custom function to generate a list of mean sepal lengths for
# the three iris species:

c(  
  
  # to output a vector but not a list, replace 'list' into 'c'
  
  get_mean_sepal('setosa'),
  get_mean_sepal('versicolor'),
  get_mean_sepal('virginica'))

# To generate a two-column tibble with species name and mean sepal length

tibble(
  species = 
    iris_tbl %>% 
    pull(species) %>% 
    unique(),
  sepal_length = 
    c(
      get_mean_sepal('setosa'),
      get_mean_sepal('versicolor'),
      get_mean_sepal('virginica')))

  # still, there are a lot of necessary repetition. so:

# introducing purrr! ------------------------------------------------------

iris_tbl %>% 
  pull(species) %>% 
  unique() %>% 
  map( 
    
    # the 'map' function in Purrr provides a irritation across 3 species
    
    function(x) { 
      iris_tbl %>% 
        filter(species == x) %>% 
        pull(sepal_length) %>% 
        mean()
    })

iris_tbl %>% 
  pull(species) %>% 
  unique() %>% 
  map( 
    ~ iris_tbl %>% 
      
      # '~' means running a formula, another word for 'function'
      
      filter(species == .x) %>% 
      
      # add '.' before 'x' to let the formula to know where to find variables
      
      pull(sepal_length) %>% 
      mean())

iris_tbl %>% 
  pull(species) %>% 
  unique() %>% 
  map_dbl( 
    
    # change 'map' into 'map_dbl' to turn the output from a list to a vector
    # (change the combine component)
    
    ~ iris_tbl %>% 
      filter(species == .x) %>% 
      pull(sepal_length) %>% 
      mean())

iris_tbl %>% 
  pull(species) %>% 
  unique() %>% 
  map_dfr( 
    
    # change 'map' into 'map_dfr' to turn the output to a dataframe
    
    ~ tibble(
      species = .x,
      sepal_length = 
        iris_tbl %>% 
        filter(species == .x) %>% 
        pull(sepal_length) %>% 
        mean()))
    
    # how does 'map_dfr' work here:
    # each irritation produce a one-row tibble with 2 columns

# A simpler way ....

iris_tbl %>% 
  group_by(species) %>% 
  summarise(sepal_length = mean(sepal_length))
  
  # 'group_by' & 'summarize' are actually a split-and-combine operation

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
    st_read('data/raw/shapefiles/counties.geojson')) %>% 
  set_names(
    'states',
    'counties_low_res',
    'counties') %>% 
  map(
    
    # 'map' could both run on list variables and vectors.
    # In this case, it is a list
    
    ~ .x %>% 
      set_names(
        names(.) %>% 
          tolower()) %>% 
      st_transform(crs = 5070))

# Read in and process the data in a single map function all at once:

file.path(
  "data/raw/shapefiles",
  c("states.shp", 
    "counties_low_res.geojson", 
    "counties.geojson"))

mapped_my_shapes <-
  file.path(
    "data/raw/shapefiles",
    c("states.shp", 
      "counties_low_res.geojson", 
      "counties.geojson")) %>% 
  map(
    ~ st_read(.x) %>% 
      set_names(
        names(.) %>% 
          tolower()) %>% 
      st_transform(crs = 5070)) %>% 
  set_names(
    'states',
    'counties_low_res',
    'counties')




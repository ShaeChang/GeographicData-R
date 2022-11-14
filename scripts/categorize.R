# Categorizing and re-categorizing vectors in a data frame

# setup -------------------------------------------------------------------

library(tidyverse)

# Read in cicadas and birds and send the files to the global environment:

read_rds('data/processed/birds_cicadas_lc.rds') %>% 
  
  # assign each individualist item to global environment
  
  list2env(.GlobalEnv)

# the cut function --------------------------------------------------------

tibble(values = 1:10) %>% 
  mutate(
    classified = 
      cut(
        values, 
        breaks =  c(1, 5, 10),
        labels = c('small', 'big'),
        include.lowest = TRUE,
        right = TRUE))

# Now you! Modify the code below such that impervious surface values that are
# greater than 40 percent impervious surface are classified as high all other
# values are classified as high urban intensity:

cicadas %>% 
  mutate(
    urban_intensity = 
      cut(
        imp, 
        breaks =  c(-Inf, 40, Inf),
        
        # negative and positive infinity are used if the boundary is uncertain
        
        labels = c('low', 'high'),
        include.lowest = TRUE, # need to use TRUE to include 0, but not NA
        right = TRUE))

# if_else -----------------------------------------------------------------

tibble(values = 1:10) %>% 
  mutate(
    classified = 
      if_else(values <= 3,
              'low',
              'high'))

  # 'if_else' is much more parsimonious than 'cut'

# Now you! NCLD land cover classes 41 through 43 represent different types of
# forest. Tabulate the number of forest and non-forest observations.

cicadas %>% 
  mutate(
    classified = 
      if_else(nlcd %in% 41:43,
              'forest',
              
              # special 'NA' is need to generate an NA of character type
              
              NA_character_))
  
  # 'if_else' does better than 'ifelse' since it is restircting the class of 
  # the positive and the negative value to the same type. 

# Using if_else to fix bad coordinates:

bad_coords %>% 
  transmute(
    
    # why use transmute but not mutate?
    
    id,
    x = 
      if_else(
        longitude > 0,
        latitude, 
        longitude),
    y = 
      if_else(
        latitude < 0,
        longitude,
        latitude)) %>% 
  rename(longitude = x, 
         latitude = y)

  # a stringr interlude -----------------------------------------------------

# if_else with str_detect:

nlcd_key %>% 
  mutate(
    name = 
      if_else(
        str_detect(name, 'Forest'), # capitalize the first character
        'forest',
        'non-forest'))

# Categorizing with stringr, the issue at hand:

birds %>% 
  pull(age) %>% 
  unique()

# if_else method:

birds %>% 
  mutate(
    age = 
      if_else(
        age %in% c('Nestling', 'Fledgling'),
        
        # Your code is correct, mine was not ...
        
        'Juvenile',
        age)) %>% 
  pull(age) %>% 
  unique()

# String replace method:

birds %>% 
  mutate(
    age = 
      str_replace(
        age,
        'Nestling|Fledgling', # means nestling or fledgling
        'Juvenile'
        )) %>% 
  pull(age) %>% 
  unique()

# Now you! Use str_detect and if_else to reclassify the name field in nlcd_key
# to “developed” and “undeveloped land”:

nlcd_key %>% 
  mutate(
    name = 
      if_else(
        str_detect(name, 'Dev'), # don't have to search the full word
        'developed',
        'undeveloped'
      ))

# case_when ---------------------------------------------------------------

# A case_when statement:

tibble(values = 1:10) %>% 
  mutate(
    classified = 
      case_when(
        values <= 3 ~ 'low',
        values <= 7 ~ 'medium',
        TRUE ~ 'high'))

      # formulas are evaluated in order. They are separated by comma
      # that's why we can use 'TRUE' in the last formula

# Now you! Modify the code below, using impervious surface and case_when to
# classify developed land into low (<= 20%), medium (<=  60%), and high
# development intensity:

birds %>% 
  mutate(
    development_intensity = 
      case_when(
        imp <= 20 ~ 'low',
        imp <= 60 ~ 'medium',
        TRUE ~ 'high'))

# Reclassify carefully!

birds %>% 
  transmute(
    age,
    new_age = 
      case_when(
        age == 'Juvenile' ~ age,
        age %in% c('Nestling', 'Fledgling') ~ 'Juvenile',
        is.na(age) ~ 'Unknown',
        TRUE ~ 'Adult')) %>% 
  distinct()


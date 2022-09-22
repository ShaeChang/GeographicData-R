# In this session, you will use filtering (and some other tools) to continue
# your exploration of the temporal and spatial distributions of the spotted
# lanternfly.

# setup -------------------------------------------------------------------

library(lubridate)
library(tidyverse)

# Load iNaturalist observations of spotted lanternflies:

spotted_lanternfly <-
  read_rds('data/raw/spotted_lanternfly.rds') %>% 
  filter(quality_grade == 'research') %>% 
  select(!c(place_guess, quality_grade)) 

# Now you! Modify the above to filter records to those that are "research"
# quality and remove the `quality_grade` field.

# review of filtering -----------------------------------------------------

# Filter by one variable and one condition:

spotted_lanternfly %>%
  filter(year(datetime) == 2020) %>% 
  group_by(state) %>% 
  summarize(n = n()) %>% 
  ggplot(
    aes(x = reorder(state, n),
        
        # order the column 'state' by the column 'n',
        # and put 'state' on the x axis
        
        y = n)) + 
  geom_bar(stat = 'identity') +
  coord_flip() +
  
  # to flip the coordination
  
  scale_y_continuous(expand = c(0, 0)) +
  
  # set each row to start from (0, 0)
  # by default it's 5% remaining blank from start of every row
  
  labs(
    title = 'iNaturalist observations of spotted lanternfly',
    x = 'State',
    y = 'Number of observations') +
  
    # even the coordination is flipped,
    # x and y axis remain the original one
  
  theme_classic() # use a classic theme here

# Filter by one variable and multiple conditions:

spotted_lanternfly %>%
  filter(year(datetime) %in% 2021:2022) %>% 
  
  # use '%in%' grammar for a semi- 'or' condition
  
  group_by(state) %>% 
  summarize(n = n()) %>% 
  ggplot(
    aes(x = reorder(state, n),
        y = n)) + 
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = 'iNaturalist observations of spotted lanternfly',
    x = 'State',
    y = 'Number of observations') +
  theme_classic()

# Filter by one variable and multiple conditions, negated:

spotted_lanternfly %>%
  filter(!year(datetime) %in% 2021:2022) %>% 
  
  # add '!'
  
  group_by(state) %>% 
  summarize(n = n()) %>% 
  ggplot(
    aes(x = reorder(state, n),
        y = n)) + 
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = 'iNaturalist observations of spotted lanternfly',
    x = 'State',
    y = 'Number of observations') +
  theme_classic()

# Now you! Modify the script below such that the states of Pennsylvania and New
# Jersey are removed from the plot:

spotted_lanternfly %>% 
  filter(!state %in% c('Pennsylvania', 'New Jersey')) %>% 
  group_by(state) %>% 
  summarize(n = n()) %>% 
  ggplot(
    aes(x = reorder(state, n),
        y = n)) + 
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = 'iNaturalist observations of spotted lanternfly',
    x = 'State',
    y = 'Number of observations') +
  theme_classic()

# Filter by multiple variables:

spotted_lanternfly %>% 
  filter(!state %in% c('Pennsylvania', 'New Jersey'),
         year(datetime) %in% 2021:2022) %>% 
  
  # to be more parsimonious, 
  # put the 2 conditions into 1 filter
  
  group_by(state) %>% 
  summarize(n = n()) %>% 
  ggplot(
    aes(x = reorder(state, n),
        y = n)) + 
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = 'iNaturalist observations of spotted lanternfly',
    x = 'State',
    y = 'Number of observations') +
  theme_classic()

# Filter by lots of variables and conditions:

spotted_lanternfly %>% 
  filter(longitude >= -81,
         longitude <= -73,
         latitude >= 39,
         latitude <= 42) %>% 
  pivot_longer(
    longitude:latitude,
    names_to = 'axis',
    values_to = 'coordinates') %>% 
  mutate(
    axis = factor(axis, levels = c('longitude', 'latitude'))) %>% 
  
  # x axis = longitude, y axis = latitude, longitude should come first
  # by default, R places facets alphabetically
  # add mutate to change. 'levels' provides with an order

  
  ggplot(aes(x = coordinates)) +
  geom_density(fill = '#dcdcdc') +
  facet_wrap(~ axis,
             scales = 'free') +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = 'Density of observations by longitude and latitude',
    x = 'Coordinates',
    y = 'Density') +
  theme_classic()

# Now you! Modify the code above by removing observations that are greater
# than -73.

# Now you! Filter the code above to observations that are between 39 and 42
# degrees latitude.

# filtering ... a regex interlude -----------------------------------------

# Filtering based on partial string matches (literal characters):

spotted_lanternfly %>% 
  filter(description %>% 
           str_detect('Egg')) %>% 
  
  # search for literal character: 
  # an exact upper / lower case, in an exact order
  
  group_by(
    month = 
      month(datetime,
            label = TRUE)) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x = month, y = n)) +
  geom_bar(stat = 'identity') +
  labs(
    title = 'Number of spotted lanternfly observations by month',
    x = 'Month',
    y = 'Number of observations') +
  theme_classic()

# Filtering based on literal and metacharacters:

spotted_lanternfly %>% 
  filter(description %>% 
           str_detect('[Ee]gg')) %>% 
  
  # search for metacharacters: 
  # both upper / lower case, in an exact order
  # for more info, refer to the book ##Regular Expressions##
  
  group_by(
    month = 
      month(datetime,
            label = TRUE)) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x = month, y = n)) +
  geom_bar(stat = 'identity') +
  labs(
    title = 'Number of spotted lanternfly observations by month',
    x = 'Month',
    y = 'Number of observations') +
  theme_classic()

# grouped filtering -------------------------------------------------------

# Filter a grouped data set:

spotted_lanternfly %>% 
  filter(year(datetime) == 2021) %>% 
  group_by(state) %>% 
  
  # mutate(n = n()) %>% , or directly use n()
  # use 'mutate' but not 'summarise' here 
  # to keep the original 'datetime' data from losing
  
  filter(n() > 10) %>% 
  ggplot(aes(x = datetime)) +
  geom_density(fill = '#dcdcdc') +
  facet_wrap(
    ~ state, 
    scales = 'free_y') +
  labs(
    title = 'Temporal density distribution spotted lanternfly observations by state in 2021',
    x = 'Date',
    y = 'Density') +
  theme_classic()

# Now you! Modify the code block we just generated such that it filters the data
# to states with more than 10 observations in 2021, without assigning a new
# column.


# Now you! The plot below shows the proportion of iNaturalist spotted lanternfly
# observations by state and year. Modify the plot such that only states with 100
# or more observations in a given year are included:

spotted_lanternfly %>% 
  group_by(
    state,
    year = year(datetime)) %>% 
  summarize(n = n(),
            .groups = 'drop') %>% 
  group_by(year) %>% 
  mutate(percent_of_obs = n/sum(n) * 100) %>% 
  filter(n > 100) %>% 
  ggplot(aes(x = year, 
             y = percent_of_obs, 
             fill = state)) +
  geom_bar(
    stat = 'identity',
    color = 'black') +
  labs(
    title = 'Proportion of spotted lanternfly observations by state and year',
    x = 'Year',
    y = 'Proportion of observations (%)') +
  theme_classic()

# Now you! Use filter to  modify the lollipop plot below such that only the
# first year that spotted lanternflies were observed is included:

spotted_lanternfly %>% 
  group_by(state) %>% 
  filter(datetime == min(datetime)) %>% 
  
  # first group the data by state, then filter it by min()
  
  mutate(year = year(datetime)) %>% 
  ggplot(
    aes(
      x = reorder(state, desc(year)),
      y = year)) +
  geom_segment(
    aes(x = reorder(state, desc(year)),
        xend = state, 
        y = 2014, 
        yend = year),
    color = "#6f8faf") +
  geom_point(
    color ="#6f8faf",
    size = 4) +
  coord_flip() +
  labs(
    title = 'First year spotted lanternflies were observed by state',
    x = 'State',
    y = 'Year') +
  theme_classic()

# filtering multiple variables, the sequel  -------------------------------

# Filtering with if_all subsets a data frame to where all columns satisfy a
# logical condition:

read_rds('data/raw/spotted_lanternfly.rds') %>% 
  select(image_url, description) %>% 
  filter(
    if_all(  # 'if_all' here means 'and'
      image_url:description,
      ~ !is.na(.x) 
      
      # '.x' is to specify the location of the variable of the formula
      # return data that all rows are not NA
      
    )
  )

# Filtering with if_any subsets a data frame to where one or more columns
# satisfy a logical condition:

read_rds('data/raw/spotted_lanternfly.rds') %>% 
  select(image_url, description) %>% 
  filter(
    if_any(  # 'if_any' means 'or'
      image_url:description,
      ~ !is.na(.x) 
      
      # '.x' is to specify the location of the variable of the formula
      # return data that at least one row is not NA
      
    )
  )


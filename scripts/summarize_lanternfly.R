
# setup -------------------------------------------------------------------

library(lubridate)
library(tidyverse)

# Load iNaturalist observations of spotted lanternflies:

spotted_lanternfly <-
  read_rds('data/raw/spotted_lanternfly.rds') %>% 
  select(!c(place_guess, image_url, description))

# group and summarize data by one variable --------------------------------

# Quality grade:

spotted_lanternfly %>% 
  group_by(quality_grade) %>% 
  summarise(n = n())
  # could tell the number of observations of each group (research or casual)

# Which states have the most spotted lanternfly observations?

spotted_lanternfly %>% 
  group_by(state) %>% 
  summarise(n = n()) %>% 
  arrange(
    desc(n)) %>% # set the order and into descended order
  slice(1:5) # select the first 5 answers

# Now you! Which users have submitted the most spotted lanternfly 
# observations?

spotted_lanternfly %>% 
  group_by(user) %>% 
  summarise(n = n()) %>% 
  arrange(
    desc(n)
  ) %>% 
  slice(1:10) # top 10 users

# Group and summarize data by year and month ------------------------------

# How many observations per year?

spotted_lanternfly %>% 
  group_by(year = year(datetime)) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = year, y = n)) +
  geom_bar(stat = 'identity') +
  theme_bw() # different from theme_void and will display axis ticks

# How many states had observations per year?

spotted_lanternfly %>% 
  group_by(year = year(datetime)) %>% 
  summarise(n = 
              state %>% 
              unique() %>% 
              length()) %>%  # How many unique states in each year
  ggplot(aes(x = year, y = n)) +
  geom_bar(stat = 'identity') +
  theme_bw()

# How many observations per month?

spotted_lanternfly %>% 
  group_by(
    month = month(datetime,
            label = TRUE)) %>% # show the name of each month on x axis 
  
  # but why here in 'group_by' ???
  
  summarise(n = n()) %>%  
  ggplot(aes(x = month, y = n)) +
  geom_bar(stat = 'identity') +
  theme_bw()

# How many observations by month and year?

spotted_lanternfly %>% 
  group_by(
    month = round_date(datetime, 'month')) %>% 
  
    # 'round_date' takes the date-time object and rounds it to the nearest 
    # integer value of a month's unit here
  
  summarise(n = n()) %>%  
  ggplot(aes(x = month, y = n)) +
  geom_bar(stat = 'identity') +
  theme_bw()

# Now you! Generate a plot of the number of observers by year and month:

spotted_lanternfly %>% 
  group_by(
    month = round_date(datetime, 'month')
  ) %>% 
  summarise(n = 
              user %>% 
              unique() %>% 
              length()) %>% 
  ggplot(aes(x = month, y = n)) +
  geom_bar(stat = 'identity') +
  theme_bw()

# other summary functions -------------------------------------------------

# Latitudinal range expansion, northern boundary:

spotted_lanternfly %>% 
  group_by(year = year(datetime)) %>% 
  summarize(latitude = max(latitude)) %>% 
  ggplot(aes(x = year, y = latitude)) +
  geom_point() +
  geom_line() +
  theme_bw()

  # might suggest lanternflies are heading north

# Now you! Generate a plot that shows the western limits of the spotted
# lanternfly observations for each year:

spotted_lanternfly %>% 
  group_by(year = year(datetime)) %>% 
  summarise(longitude = min(longitude)) %>% 
  ggplot(aes(x = year, y = longitude)) +
  geom_point() +
  geom_line() +
  theme_bw()

# multiple summaries ------------------------------------------------------

# Latitudinal range expansion, combined:

spotted_lanternfly %>% 
  group_by(year = year(datetime)) %>% 
  summarize(
    southern = min(latitude),
    northern = max(latitude)) %>% 
  pivot_longer( 
    
    # 'pivot_longer' is used to 
    # put southern and northern data in one column
    
    southern:northern,
    names_to = 'limits',
    values_to = 'latitude'
  ) %>% 
  ggplot(
    aes(x = year,
        y = latitude,
        color = limits)) + # add color for different categories
  geom_point() +
  geom_line() +
  theme_bw()

# Now you! Has the range of spotted lanternfly expanded longitudinally?

spotted_lanternfly %>% 
  group_by(year = year(datetime)) %>% 
  summarise(
    western = min(longitude),
    eastern = max(longitude)) %>% 
  pivot_longer(
    western:eastern,
    names_to = 'limits',
    values_to = 'longitude') %>% 
  ggplot(
    aes(x = year, 
        y = longitude,
        color = limits)) +
  geom_point() +
  geom_line() +
  theme_bw()

# group and summarize data by multiple variables --------------------------

spotted_lanternfly %>% 
  group_by(
    year = year(datetime),
    state) %>% 
  
  # group the stats both in years and states, separately
  
  summarize(n = n(),
            .groups = 'drop') %>% 
  
  # Why? I don't see any difference
  
  ggplot(
    aes(x = year,
        y = n)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~state) + # to map the plots by states.
  theme_bw()






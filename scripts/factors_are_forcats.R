# Review of factors and introduction to forcats

# setup -----------------------------------------------------------------

library(tidyverse)

source('scripts/source_script.R')

read_rds('data/raw/forcats_data.rds') %>% 
  list2env(.GlobalEnv)

# factors review --------------------------------------------------------

# What is a factor:

age

# Factors are dangerous and useful because of restrictive behavior:

bad_age

# Now you! Use str_replace to change the value “juv” to “juvenile” then
# convert the resultant value to a factor with the levels “adult” and
# “juvenile”:

bad_age

# Factors can be ordered:

c('small', 'medium', 'large')

# Now you! Convert the character vector "spring" to a factor object where
# month levels are ordered by when they occur then tally the number
# of values per month:

spring

# forcats, recode -------------------------------------------------------

# Recode levels fct_recode:

bad_age %>% 
  table()

# Factors ... now with real data!

bird_measures

# Now you! Use fct_recode() to map the levels of sex to the values 'female',
# 'male', and 'unknown':

# forcats,  collapse ----------------------------------------------------

# fct_collapse:

bird_measures %>% 
  count(age)

# Now you! Use fct_collapse to map the observed sex of birds to the factor
# levels “known” or “unknown”:

bird_measures

# forcats, drop ---------------------------------------------------------

# Plot wing length by species:

bird_measures %>% 
  group_by(common_name) %>% 
  filter(n() > 10) %>% 
  ungroup() %>%
  mutate(common_name = 
           factor(common_name)) %>% 
  ggplot(
    aes(x = common_name,
        y = wing)) +
  geom_boxplot(fill = '#dcdcdc') +
  coord_flip() +
  labs(title = 'Wing length by bird species',
       x = 'Species',
       y = 'Wing') +
  theme_minimal()

# Plot wing length of Gray Catbirds by age class:

bird_measures %>% 
  filter(age != 'U',
         common_name == 'Gray Catbird') %>% 
  mutate(age = 
           age %>%
           fct_collapse(
             Juvenile = 'HY',
             'Young adult' = 'SY',
             Adult = c('AHY', 'ASY'))) %>% 
  ggplot(
    aes(x = age,
        y = wing)) +
  geom_boxplot(fill = '#dcdcdc') +
  labs(title = 'Gray Catbird wing length by age class',
       x = 'Age Class',
       y = 'Wing') +
  theme_classic()

# Now you! Modify the plotting code below such that it displays the months in
# the order that they occur.

bird_measures %>% 
  group_by(common_name) %>% 
  filter(n() > 10) %>% 
  ungroup() %>%
  filter(common_name == 'Gray Catbird') %>% 
  ggplot(
    aes(x = month,
        y = mass)) +
  geom_boxplot(fill = '#dcdcdc') +
  scale_y_continuous(
    limits = c(30, 45),
    expand = c(0, 0)) +
  labs(title = 'Wing length by bird species',
       x = 'Month',
       y = 'Mass') +
  theme_light()

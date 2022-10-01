
# setup -------------------------------------------------------------------

# Load libraries:

library(tidyverse)

# read in data ------------------------------------------------------------

# Read in the country-level information:

countries <-
  read_csv('data/processed/world_bank_countries.csv') 

# Read in population data:

world_pop <-
  read_csv(
    'data/raw/API_SP.POP.TOTL_DS2_en_csv_v2_2763937.csv', 
    skip = 3) %>% 
  
  # Reshape the columns into rows:
  
  pivot_longer(
  names_to = 'year',
  values_to = 'population',
  `1960`:`2020`) %>% 
  
  # Rename columns with a space:
  
  rename(country_code = 'Country Code') %>% 
  
  # Select columns of interest:
  
  select(country_code, year, population)

# Read in the co2 data:

co2 <-
  read_csv('data/raw/API_EN.ATM.CO2E.PC_DS2_en_csv_v2_2764620.csv',
           skip = 3) %>% 
  
  # Reshape the columns into rows:
  
  pivot_longer(
    names_to = 'year',
    values_to = 'co2',
    `1960`:`2020`) %>% # what is '...66' ??
  
  # Rename the country column with a space:

  rename(country_code = 'Country Code') %>% 
  
  # Select columns of interest:
  
  select(country_code, year, co2)

# normalize and clean data ------------------------------------------------

# Put same level of observation in the same table
  
population_co2 <-
  
  # Join the population and co2 tibbles, and keep all observations
  
  full_join(
    world_pop,
    co2,
    by = c('country_code', 'year')) %>% 
  
  # Change the year column into numeric
  
  mutate(year = 
           as.numeric(year))

# plot population density and co2 emissions in Africa ---------------------

# Modify the data of interest 

africa_population_co2 <-
  population_co2 %>% 
  
  #
  
  full_join(countries,
            by = 'country_code') %>% 
  
  # Subset the tibble to only include African countries:
  
  filter(country_georegion %>% 
           str_detect('Africa')) %>% 
  
  # Select variables of interest
  
  select(country_name, land_area, population, co2, year) %>% #?? should include year
  
  # Calculate the population density:
  
  mutate(population_density = population/land_area) %>% 
  select(country_name, population_density, co2, year) %>% 
  
  # Remove the empty value ?? should I do that
  
  filter(
    if_all(
      population_density:co2,
      ~ !is.na(.x)))
  
# Visualize the change in population in Africa over time:
  
africa_population_co2 %>% 
  ggplot(aes(x = year,
             y = population_density,
             color = country_name)) +
  geom_point() +
  geom_line() +
  theme_bw()

# Visualize the change as a faceted plot:

africa_population_co2 %>% 
  
  # Reshape the data:
  
  pivot_longer('population_density':'co2',
               names_to = 'variable',
               values_to = 'value') %>% 
  
  # Create a 2-facet plot:
  
  ggplot(aes(x = year,
             y = value,
             color = country_name)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ variable,
             scales = 'free') +
  theme_classic()
  
  
  
  
  







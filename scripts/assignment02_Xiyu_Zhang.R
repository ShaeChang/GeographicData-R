# Tidying messy weather
# Xiyu Zhang

# Set up ------------------------------------------------------------------

library(tidyverse)
library(readr)

messy_weather <- 
  read_csv("data/messy_weather.csv")

# Separate 2 levels of observation ----------------------------------------

# Station-level observations (短小的注释，control + shift + C 自动注释)

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
         year:march_31) # Also, could negatively select needed columns

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
  
  # 可以用names_prefix = 'march_'的语法，这是pivot的一个argument，去掉重复的词头
  # 此时就不用上面那一步，转化每列名称
  
  unite(col = 'date',
        c(year,
          month,
          day),
        sep = '-') %>%
  
  # 这里应当转化其形式为日期.(评论应当总是有上下两行空行)
  
  mutate(
    
    #因为这里有mutate和as_date两个函数，所以把它放在两行中
    
    date = lubridate::as_date(date)
    ) %>% 
  
  pivot_wider(names_from = variable,
              values_from = value) %>%
  separate(col = temperature_min_max,
           into = c('temperature_min',
                    'temperature_max'),
           sep = ':') %>% 
  
  # 调整数字格式，从chr to dbl
  # 可以用以下的函数，类似for loop的语法，在每个column中实行
  
  mutate(
    across(
      precip:temperature_max,
      ~ as.numeric(.x))
  )

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
  
  # 实际上，应当在这整个script中使用list函数
  
  list(station,
       weather_record,
       weather_snow) %>% 
  
  # .rds 格式的文件保存
  
  write_rds('data/tidy_weather.rds') 



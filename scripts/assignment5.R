
# 2. Load libraries -------------------------------------------------------

library(sf)
library(tmap)
library(tidyverse)

# 3. Load data and set the tmap mode --------------------------------------

# Load data

source('scripts/source_script_bird_mortality.R')

# Set the tmap mode for the whole document

tmap_mode('view')

# Fix a current bug

tmap::tmap_options(check.and.fix = TRUE)

# Remove unnecessary objects

rm(cicadas, birds)

# 4. convert.. ------------------------------------------------------------

# convert the census data to a SpatRaster object

income_dc <-
  terra::rasterize(
    
    # 不知道这里还要不要像categorize_spatial.R里面一样先转变成SpatVector，
    # 先写完再看视频再说
    
    # the name of the shapefile
    
    x = census, 
    
    # the name of the raster file being the base of the conversion
    
    y = rasters,
    
    # the field of interest is "income"
    
    field = 'income')

# remove census from global environment

rm(census)

# 5. DC parks -------------------------------------------------------------

#  process data describing the park area in DC

dc_parks <-
  parks_and_rec %>% 
  
  # subset DC parks features to those "woodland" are not NA
  
  filter(!is.na(woodland)) %>% 
  
  # add a field to reclassify 'woodland'
  
  mutate(park_type = woodland %>% 
           
           # recode the previous values as the new values
           
           fct_recode(Forested = 'Y',
                      `Not forested` = 'N')) %>% 
  
  # subset data to 'objectid' and 'park_type' columns
  
  select(objectid, park_type)

# remove 'park_and_rec'

rm(parks_and_rec)

# 6. reclass matrix -------------------------------------------------------

# create a reclass matrix

rcl_nlcd <-
  
  # using nlcd_key
  
  nlcd_key %>% 
  
  # apply transmute to create new columns and to discard the old ones
  
  transmute(
    
    # 'is' column represents the current categories in the nlcd raster
    
    is = id,
    
    # 'becomes' column is the new value. 
    # 'case_when' is used for multiple classes
    
    becomes = case_when(
      
      # set NA_real_ for water to match the class of the values
      
      name == 'Open water' ~ NA_real_,
      
      # use str_detect to convert multiple values to 1
      
      str_detect(name, 'Developed') ~ 1,
      
      # use str_detect to convert multiple values to 2
      
      str_detect(name, 'Forest') ~ 2,
      
      # set the default as 3 to convert the remaining categories
      
      .default = 3)) %>% 
  
  # turn the resultant object to a matrix
  
  as.matrix()

# remove nlcd_key from global environment

rm(nlcd_key)

# 7. reclassify the raster ------------------------------------------------

# reclassify the nlcd raster to developed and forested land

developed_and_forested_land <-
  
  # use function classify() to reclassify
  
  terra::classify(
    
    # the raster that need to be relassified
    
    rasters$nlcd,
    
    # the two-column reclassify matrix
    
    rcl = rcl_nlcd)

# remove rcl_nlcd from the global environment

rm(rcl_nlcd)

# 8. reclass matrix for impervious surface --------------------------------

# create a reclass matrix for numeric values and assign it to the certain name

urban_rcl <-
  
  # create a tibble using an easier to read row-by-row layout using tribble()
  
  tribble(
    
    # the first two columns are "from" "to" of the input values,
    # while the third column "becomes" has the new value for that range
    
    ~ from, ~ to, ~ becomes,
    0,        20,       1,
    20,       80,       2,
    80,      100,       3) %>% 
  
  # turn the resultant object to a matrix
  
  as.matrix()

# Modify me!

tibble(values = 0:10) %>% 
  mutate(
    classes = 
      cut(values, 
          breaks = c(0, 2, 8, 10),
          
          # modified combination of the two arguments
          
          include.lowest = TRUE,
          right = FALSE))

# classify the impervious land raster

urban_intensity <-
  
  # use classify() to classify
  
  terra::classify(
    
    # raster needs to be classified
    
    rasters$imp,
    
    # the reclassify matrix
    
    rcl = urban_rcl,
    
    # the two arguments that decide the wanted margins as above
    
    include.lowest = TRUE,
    right = FALSE)

# remove urban_rcl from global environment

rm(urban_rcl)

# 9. National Park Service ------------------------------------------------

nps_parks <-
  nps %>% 
  
  # filter to features where the name includes the two specific strings
  
  filter(str_detect(name, 'Park |Parkway')) %>% # 这是一个问题，我不知道行不行
  
  # Combine the geometries of parks that share the same name, 
  # first group by name
  
  group_by(name) %>% 
  
  # and then combine the geometrics
  
  summarise() %>% 
  
  # Add a field that represents the average canopy cover for each park
  
  mutate(
    
    # create a new field
    
    canopy = 
      
      # 这里为什么不用terra::vect()转化成SpatVector?
      
      # extract canopy cover data from raster$canopy
      
      terra::extract(
        
        # the raster that to extract data from
        
        rasters$canopy,
        
        # the polygon that extract data to
        
        .,
        
        # with the concern of the average canopy cover
        
        mean, 
        
        # remove NA values
        
        na.rm = TRUE) %>% 
      
      # pull the extracted mean values to assign them to the new field
      
      pull()) %>% 
  
  # add a field to represent whether a park is on average forested
  
  mutate(
    
    # create a new field, 
    # and categorize the park type by parks' average canopy cover
    
    park_type = if_else(
      
      # if the canopy cover is greater than or equal to 60% then 'Forested'
      
      canopy >= 60,
      'Forested',
      
      # the remaining are assigned as 'Not Forested'
      
      'Not Forested') %>% 
      
      # ensure the levels are in the correct order, 'Not Forested' comes first
      
      fct_relevel(
        c('Not Forested', 'Forested'))) %>% 
  
  # remove the canopy and name fields by only transmute the remaining two fields
  
  transmute(geometry, park_type)

# remove nps and rasters from the global environment

rm(nps, rasters)

# 10. generate tmap -------------------------------------------------------

tm_basemap(
  
  # use the two specified background layers
  
  c('OpenStreetMap',
    'Esri.WorldImagery')) +
  
  # Income
  
  tm_shape(
    
    # the raster's name
    
    income_dc,
    
    # name the layer
    
    name = 'Income') +
  tm_raster(
    
    # name the layer legend
    
    title = 'Income (Dollars)',
    
    # change the default orange tone to distinguish from other layers' palettes
    
    palette = 'Blues') +
  
  # DC parks
  
  tm_shape(
    
    # sf object's name
    
    nps_parks,
    
    # name the layer
    
    name = 'DC parks') +
  tm_polygons(
    
    # name the layer legend
    
    title = 'Forest coverage',
    
    # specify 'park_type' to be the colors
    
    col = 'park_type',
    
    # assign an appropriate color to each category, green to 'forested'
    
    palette = 
      c('#ffffbf', '#91cf60')) +
  
  # Developed and Forested Land
  
  tm_shape(
    
    # name of the raster
    
    developed_and_forested_land,
    
    # name the layer
    
    name = 'Land Cover') +
  tm_raster(
    
    # name the layer's legend
    
    title = 'Land cover',
    
    # assign its colors to the field 'nlcd'
    
    col = 'nlcd',
    
    # assign a meaningful and pleasant palette, the same green to forest
    
    palette = 
      c('#fc8d62', '#91cf60', '#bdbdbd'),
    
    # give each category meaningful labels
    
    labels = 
      c('Developed', 'Forest', 'Undeveloped Openland'),
    
    # ensure this layer been plotted as a categorical layer
    
    style = 'cat') +
  
  # urban intensity
  
  tm_shape(
    
    # name of the raster
    
    urban_intensity,
    
    # give name to the layer
    
    name = 'Urban Intensity') +
  tm_raster(
    
    # name the layer's legend
    
    title = 'Impervious surface intensity',
    
    # assign pleasant and distinguishable palette
    
    palette = 
      c('#fde0dd', '#fa9fb5', '#c51b8a'),
    
    # ensure this raster to be plotted as categorical
    
    style = 'cat',
    
    # assign a label to each level
    
    labels = 
      c('Low', 'Medium', 'High')) +
  
  # NPS parks
  
  tm_shape(
    
    # name of the sf object
    
    nps_parks,
    
    # name the layer
    
    name = 'National Park Service parks') +
  
  tm_polygons(
    
    # name the layer's legend
    
    title = 'Park type',
    
    # assign "park_type" to be shown as the color
    
    col = 'park_type',
    
    # assign meaningful palette, and the same green to 'Forested'
    
    palette = 
      c('#ffffbf', '#91cf60'))

















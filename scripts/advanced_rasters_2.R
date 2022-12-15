# Spatial autocorrelation!

# setup -------------------------------------------------------------------

library(ape)
library(pgirmess)
library(spdep)
library(gstat)
library(sf)
library(tmap)
library(tidyverse)

tmap_mode('view')

# Census shapefile:

census <- 
  st_read('data/raw/shapefiles/dc_census.geojson') %>% 
  st_transform(crs = 5070) %>% 
  set_names(
    names(.) %>% 
      tolower())

# The extent of Washington DC:

dc_extent <-
  c(xmin = 1609777,
    xmax = 1630412,
    ymin = 1912781,
    ymax = 1937182) %>% 
  
  # another Spat terra object, which is SpatExtent. (SpatVector & SpatRaster)
  terra::ext()

# Path to rasters:

raster_files <-
  list.files(
    'data/raw/rasters',
    pattern = 'tif$') 
    # 'tif$' is an anchor meta character, to test whether a string ends with 
    # letters 'tif'

# Load rasters:

rasters <-
  # generate a full file path for each '.tif' files
  file.path(
    'data/raw/rasters',
    raster_files) %>% 
  
  # use purrr::map to iterating the reading process across the file path
  map(
    ~ terra::rast(.x)) %>% 
  set_names(
    
    # use 'str_remove' to only retain the file names without the file extensions
    str_remove(raster_files, '.tif')) %>% 
  
  # handy way to subset list items, use purrr::keep
  keep(
    names(.) %>% 
      # any strings including 'imperv' for impervious or 'can' for 'canopy cover'
      str_detect('imperv|can')) %>% 
  
  # inside a new map func, crop the two rasters into the same extent 
  # for generating a raster stack next step
  map(
    ~terra::crop(.x, dc_extent)) %>% 
  
  # generate a raster stack
  terra::rast()

# Now you! In the code block above, crop impervious surface and canopy dc_extent
# and store the output as a raster stack.

# Remove unneeded files

rm(raster_files, dc_extent)

# sampling points ---------------------------------------------------------

# Generate regular point samples:

samples <- 
  rasters %>% 
  terra::spatSample(
    size = 1200,
    method= 'regular', 
    as.point = TRUE) %>% 
  st_as_sf()

# Now you! Generate a tmap that shows sampling points, canopy cover, and 
# impervious surface:

# make canopy cover the bottom layer

tm_shape(
  rasters$canopy_cover,
  name = 'Canopy') +
  tm_raster(
    title = 'Canopy (%)',
    palette = 'Greens',
    style = 'cont',
    alpha = 1) + # since it's bottom layer, make it fully opaque
  
  # Add impervious layer:
  
  tm_shape(
    rasters$impervious_surface,
    name = 'Pavement') +
  tm_raster(
    title = 'Impervious (%)',
    palette = 'Oranges',
    style = 'cont',
    alpha = 0.7) + # add transparency to show the bottom layer
  
  # Add sample point layer:
  
  tm_shape(
    samples,
    name = 'Samples') +
  tm_dots(size = 0.005)

# testing spatial dependence, method 1 ------------------------------------

# Note: This method uses ape

# Generate a distance matrix, example:
# a matrix of distances between all of our sampling points

distance_matrix_example <- 
  samples %>% 
  slice(1:4) %>% # subset the sample to only the first 4 points using 'slice'
  terra::vect() %>% # convert the sf object into SpatVector
  terra::distance() %>% # calculate the distance
  as.matrix() # convert a distance object to matrix, since the Moran calculation
  # requires the distance is symmetrical.

# Calculate the inverse of the distance matrix:

inverse_distance_matrix_example <- 
  1/distance_matrix_example

# Set diagonals to zero: (bc 1/0 is infinity in the previous object)

diag(inverse_distance_matrix_example) <- 0

# With all of the data:

distance_matrix <- 
  samples %>% 
  terra::vect() %>% 
  terra::distance(.) %>%
  as.matrix()

inverse_distance_matrix <- 
  1/distance_matrix

diag(inverse_distance_matrix) <- 0

# Calculate Moran's I with the ape package:

ape::Moran.I(
  samples$canopy_cover, # a vector of values that want to test
  inverse_distance_matrix, # the inverse distance matrix
  alternative = 'greater') # an argument suggests the alternative hypothesis

  # The null hypothesis: the data are distributed randomly
  # The alternative hypothesis: the data are clustered.
  # Method: a one-sided test - whether the clumping in the data are greater 
  # than what we might observe under our null hypothesis

  # Result: p-value is less than 0.001.
  # Report: there is less than a 0.1 percent chance that the observed pattern is
  # consistent with a spatially random process. The observed pattern in the data
  # provides supportive evidence for positive spatial autocorrelation in canopy
  # cover.

  # another alternative hypothesis: 'less' indicating data are negatively
  # spatial correlated and the p-value is 1 here. That means no support for this
  # alternative hypothesis.

  # other alternative hypothesis: 'two-sided'. p-value is 0 here, showing that 
  # there is strong evidence that there's spatial structure in the data.

# testing spatial dependence, method 2 ------------------------------------

# Note: This method uses the spdep package. 

# Target: to test if income data are spatially structured in DC.
# The previous method deals with points, while this method deals with polygons.

# Remove NA values from census:

census_no_na <-
  census %>% 
  filter(!is.na(income))

# Define neighbors: (create a neighbor object but not a distance matrix)

neighbors <- # generate a list of neighboring polygons 
  census_no_na %>% 
  spdep::poly2nb() # 'poly2nb' means polygons to neighbors

# Weights for our neighbors:

neighbor_weights <- # this object indicates how close one polygon is to another
  spdep::nb2listw(neighbors) # means neighbors to a list of weights

# Calculate Moran's I:

spdep::moran(
  x = census_no_na$income, # vector of values for examination
  listw = neighbor_weights, # list of neighbor weights
  n  = length(neighbor_weights$neighbours), # the number of neighbors
  S0 = Szero(neighbor_weights)) %>% # the global sum of neighbor weights
  pluck('I') # extract the Moran's I value from the resultant object

# Monte Carlo Moran's I test: test whether the data are spatially autocorrelated

mc_test <- 
  spdep::moran.mc(
    x = census_no_na$income,
    listw = neighbor_weights, 
    nsim = 10000, # stands for the number of simulations, 
    # the number of draws from the data
    alternative = 'greater')

# Plot the test:

plot(mc_test)
  # the Moran's I statistic is also about 6.2 shown in the plot
  # the pseudo p-value way less than 0.01 percent, we can strongly refuse the 
  # null hypothesis. Strong evidence supports that the data are positively 
  # spatial autocorrelated. - The income in DC is highly clustered.

# Now you! using census_no_na, conduct a Monte Carlo Moran's I test of incomes
# using a single chained analysis:

census_no_na %>% 
  spdep::poly2nb() %>% 
  spdep::nb2listw() %>% 
  spdep::moran.mc(
    x = census_no_na$income,
    listw = .,
    nsim = 10000,
    alternative = 'greater')

# assessing the scale of autocorrelation: Moran correlogram ---------------
# 从上一section的：数据是否空间自相关，到这一section：我们的数据自相关程度的大小
# 更多内容见notion期末project一页的笔记

# Plot correlogram in pgirmess:

pgirmess::correlog(
  coords = 
    census_no_na %>% 
    st_centroid() %>% # calculate the centroid of each polygon
    st_coordinates(), # extract the matrix of coordinates of these centroids
  z = census_no_na$income, # despite the x and y being the coordinates, z value
  # here provides the value on the specific location
  method = 'Moran',
  nbclass = 15) %>% # the number of bins that we want to divide our data into
  plot()

# add a horizontal line at 0, where there's no spatial autocorrelation
abline(h = 0, lty = 'dashed')

  # Result analysis:
  # The red dot means the p-value is less than 0.05. That means income data up
  # to a distance of 5km are positively spatially autocorrelated. Also, that
  # means samples within this distance are not statistically independent.

# Now you! Use pgirmess::correlog to examine the scale at which canopy cover
# is spatially autocorrelated:

pgirmess::correlog(
  coords = samples %>%
    st_coordinates(),
  z = samples$canopy_cover,
  method = 'Moran',
  nbclass = 15) %>% 
  plot()

abline(h = 0, lty = 'dashed')

# assessing the scale of autocorrelation: semivariogram -------------------
# This plot is used to assess the scale at which a variable is independent.

# Terminology:
# semivariance: the variance that could be explained by the location of the data
# 在纵轴上位置越低说明相似性越高，横轴代表数据点之间的距离，随着距离趋向无穷远，
# 纵轴的值semivariance一定趋于一个稳定的最高值，sill。
# Range: the distance at which we consider samples to be spatially independent，
# 也就是从距离为0到无限接近sill的那段横轴距离，在这段距离中数据是空间自相关的。

# Example: generate a semivariogram of income:

  # generate an intercept-only model to be the baseline: (?)

income_variogram <- 
  census_no_na %>% 
  gstat::variogram(income ~ 1, data = .)

plot(
  income_variogram, 
  pch = 19,
  col = 'black')

# Provide starting values to fit a variogram model:

model_variogram_starts <-
  gstat::vgm( # by vgm(), and just by eyeballing the data coming up a best guess
    psill = 1.5E9,
    model = 'Sph', # the type of model for the fitted curve, 
    # choose 'sph' for spherical
    # (use command 'show.vgms()' to find the potential curves to choose from)
    nugget = 4E8,
    range = 5000) # all of the estimations came from eyeballing,
    # being just starting points for the model to converge so don't need to be
    # extremely precise.

# Fit the model with 'fit.variogram' function:

fitted_variogram <-
  gstat::fit.variogram( 
    income_variogram, # provide the variogram
    model_variogram_starts) # as long as our guess for the starting values

plot(income_variogram, 
     model = fitted_variogram,
     pch = 19,
     col = 'black')

  # from typing 'fitted_variogram', find out the fitted range is about 6.4km,
  # the distance we consider our samples to be spatially independent.

# Now you! Generate a variogram model of canopy cover values:

canopy_variogram <- 
  gstat::variogram(canopy_cover ~ 1, data = samples)

plot(
  canopy_variogram,
  pch = 9,
  col = 'black')

canopy_variogram_starts <-
  gstat::vgm(
    psill = 800, # even for a model as 'Exp', we can set 'psill', that's great!
    model = 'Exp',
    nugget = 480,
    range = 5000) # what does range mean in an 'Exp' model?

fitted_variogram_canopy <-
  gstat::fit.variogram(
    canopy_variogram,
    canopy_variogram_starts)

plot(
  canopy_variogram,
  model = fitted_variogram_canopy,
  pch = 19,
  col = 'black')

# when is spatial autocorrelation a problem? ------------------------------

# Join census to sampling data and subset to relevant locations (not water!):

# why the crs are different here?

samples_with_income <-
  samples %>% 
  st_join(
    census %>% 
      select(income)) %>% 
  filter(
    !is.na(income),
    !(canopy_cover == 0 & impervious_surface == 0))

simple_model <- 
  lm(canopy_cover ~ income, 
     data = samples_with_income)

summary(simple_model)

samples_with_income %>% 
  ggplot(aes(x = income, y = canopy_cover)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    formula = y ~ x)

# If the samples are not independent, then we are violating an assumption for 
# a linear regression.
# We test this by examining the spatial autocorrelation of the residuals.

samples_with_income$residuals <-
  # add residuals to the samples with income data frame
  simple_model$residuals

# Is there evidence of spatial autocorrelation in the residuals?
# the difference here: we are testing the residuals, but not the raw data

pgirmess::correlog(
  coords = 
    st_coordinates(samples_with_income),
  z = samples_with_income$residuals,
  method = 'Moran',
  nbclass = 15) %>% 
  plot()

abline(h = 0, lty = 'dashed')
  # the result suggests the residuals are spatially autocorrelated within 3km,
  # which violates our model assumptions.

# How to address reisidual spatial autocorrelation?
# 1. reduce the effective sample size
# 2. switch to a spatially explicit model, or to one that includes autocovariance
# structure.

# Spatial autocorrelation in the residuals should be perceived as an opportunity
# to explore our research topic further.

# Now you! Determine the minimum distance between two different sample points
# in sampling:

samples %>% 
  terra::vect() %>% 
  terra::distance() %>% 
  min()
# By knowing that the minimum distance between our samples is 630m, we know that
# any spatial autocorrelation that occurs below this value cannot be evaluated.

# If regular samples are used, then it's hard to evaluated as demonstrated above.
# On the contrary, random sampling can help us recognize spatial autocorrelation
# at low processing cost!
# Conclusion: random points are better in a real-world research.

# interpolation - spatial autocorrelation is useful! ----------------------
# That means to use point values to predict values between points. 
# 类似于自动填色系统

# Blank raster:

blank_stars <-
  rasters$canopy_cover %>%  
  terra::aggregate(8) %>% # reduce the cells, change the resolution
  stars::st_as_stars() %>% # (what is star??)
  set_names('canopy_cover')

# Inverse-distance-weighting (IDW) model:

idw_model <-
  gstat(formula = canopy_cover ~ 1, 
        data = samples) %>% 
  
  # IDW prediction (returned as SpatRaster):
  
  predict(newdata = blank_stars) %>% 
  select(var1.pred) %>% 
  terra::rast()

# Plot the IDW:

tm_shape(rasters$canopy_cover) +
  tm_raster(palette = 'Greens') + 
  
  idw_model %>% 
  tm_shape(name = 'idw') +
  tm_raster(
    title = 'idw_prediction',
    palette = 'Greens') +
  
  tm_shape(samples) +
  tm_dots(size = 0.005)

# Kriging model:

krige_model <- 
  gstat(formula = canopy_cover ~ 1, 
        model = fitted_canopy_variogram, 
        data = samples) %>% 
  predict(newdata =  blank_stars) %>% 
  select(var1.pred) %>% 
  terra::rast()

# Plotting Kriging results

tm_shape(rasters$canopy_cover) +
  tm_raster(palette = 'Greens') +
  
  krige_model %>% 
  tm_shape(name = 'krige') +
  tm_raster(
    title = 'krige_prediction',
    palette = 'Greens') +
  
  tm_shape(samples) +
  tm_dots(size = 0.005)

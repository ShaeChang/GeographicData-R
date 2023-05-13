
# assignment 6 R script


# R6 ----------------------------------------------------------------------

# Classify campsites as 'Comfy' or 'Flannel' using the temperature in Dec

# 关键在于，要把sf文件转换为raster文件，并且提取出来之前raster文件中带
# temperature的信息（同geometry）

camp_points <-
  campsites %>% # 这里不用改crs，因为crs是都一样的，省掉st_transform
  
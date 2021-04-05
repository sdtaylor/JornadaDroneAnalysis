library(tidyverse)
library(raster)
library(sf)
#library(exactextractr)

library(randomForest)

#-------------------------
# extract pixel values for training data
#-------------------------
ibp_plants = sf::read_sf('./drone_analysis/data/gis/ibp_plant_outlines.geojson')

band_names = c('b1','b2','b3','b4','b5')
ibp = raster::brick('./drone_analysis/data/imagery/uav_true_color/GIBPE_Aug12_2020.tif')
ibp = raster::aggregate(ibp, fact=5)
names(ibp) <- band_names

pixels = exactextractr::exact_extract(ibp, ibp_plants)

combine_pixels_and_class = function(p, c){
  p %>%
    dplyr::select(band_names) %>%
    mutate(plant = c)
}
pixels_df = map2_df(pixels, ibp_plants$Class_name, combine_pixels_and_class)
#-------------------------
# build model
#-------------------------
# Excluding yucca as their percent cover is really small
pixels_df = pixels_df %>%
  filter(plant %in% c('Bare','GRASS','PRGL'))

pixels_df$plant = as.factor(pixels_df$plant)

cover_model = randomForest(plant ~ b1 + b2 + b3 + b4 + b5, 
                  data=pixels_df)

pixels_df$predicted_plant = predict(cover_model, type='response')

# model within sample confusion matrix.
caret::confusionMatrix(pixels_df$predicted_plant, pixels_df$plant)

#-------------------------
# apply to full images
#-------------------------
# ibp
ibp_predicted = raster::predict(ibp, cover_model)
raster::writeRaster(ibp_predicted, './drone_analysis/data/ibp_predicted_cover.tif')

# nort
nort = raster::brick('./drone_analysis/data/imagery/uav_true_color/NORT_Aug14_2019.tif')
nort = raster::aggregate(nort, fact=5)
names(nort) <- band_names

nort_predicted = raster::predict(nort, cover_model)
raster::writeRaster(nort_predicted, './drone_analysis/data/nort_predicted_cover.tif')

# p9
p9 = raster::brick('./drone_analysis/data/imagery/uav_true_color/P9_Jul30_2019.tif')
p9 = raster::aggregate(p9, fact=5)
names(p9) <- band_names

p9_predicted = raster::predict(p9, cover_model)
raster::writeRaster(p9_predicted, './drone_analysis/data/p9_predicted_cover.tif')

library(tidyverse)
library(sf)
#library(raster)
#library(exactextractr)

source('spatial_helper_functions.R')
source('analysis_config.R')

raster_list = list.files('~/data/jornada_drone_imagery/ortho/', pattern = '*tif$', recursive = TRUE, full.names = TRUE)
all_rois = st_read(final_roi_file)

all_extracted_data = tibble()

for(site in jorn_sites){
  site_rois = all_rois %>%
    filter(site_id == site) 
  
  site_image_list = raster_list[grep(site, raster_list)]

  for(f in site_image_list){
    for(band_i in 1:5){
      r = raster::raster(f, band=band_i)
      
      # from 'GIBPE_Aug28_2018_index_ndvi.tif' to 'Aug28-2018' to date object
      date_str = paste(str_split(basename(f),'_')[[1]][2:3], collapse = '-')
      date = as.Date(date_str, format = '%b%d-%Y')
      
      # Loading into memory not neccissarily speeding things up with
      # using the exactextractr package
      #r = raster::readAll(r)
      
      extracted = site_rois %>%
        as_tibble() %>%
        dplyr::select(-geometry)
      
      extracted$value = exactextractr::exact_extract(r, site_rois, fun = 'mean')
      extracted$date = date
      extracted$band = band_i
      
      # Throw in the date string from the filename for debugging
      #extracted$date_str = date_str
      
      all_extracted_data = all_extracted_data %>%
        bind_rows(extracted)
    }
  }
  
}

write_csv(all_extracted_data, roi_ortho_file)

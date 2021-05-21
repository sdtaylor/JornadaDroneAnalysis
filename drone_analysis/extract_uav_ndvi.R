library(tidyverse)
library(sf)
#library(raster)
#library(exactextractr)


# This extracts NDVI from the drone imagery time series at 
# all the randomly place roi (aka. simualted pixels)

source('spatial_helper_functions.R')
source('analysis_config.R')

raster_list = list.files('~/data/jornada_drone_imagery/ndvi', pattern = '*tif$', full.names = TRUE, recursive = T)
all_rois = st_read(random_roi_file)

all_extracted_data = tibble()

# It takes a lot of memory to process all ROI's, so do them just a few hundred at a time.
roi_chunks = split(sample(all_rois$roi_id), ceiling(seq_along(all_rois$roi_id)/2000))

for(site in jorn_sites){
  site_image_list = raster_list[grep(site, raster_list)]

  for(f in site_image_list){
    r = raster::raster(f)
    # from 'GIBPE_Aug28_2018_index_ndvi.tif' to 'Aug28-2018' to date object
    date_str = paste(str_split(basename(f),'_')[[1]][2:3], collapse = '-')
    date = as.Date(date_str, format = '%b%d-%Y')

    
    # Loading into memory not neccissarily speeding things up with
    # using the exactextractr package
    r = raster::readAll(r)

    for(roi_chunk_set in roi_chunks){
      site_rois = all_rois %>%
        filter(site_id == site,
               roi_id %in% roi_chunk_set)
      
      extracted = site_rois %>%
        as_tibble() %>%
        dplyr::select(-geometry)
      
      # all values within each simulated pixel
      all_extracted_values=exactextractr::exact_extract(r, site_rois, fun = NULL)
      
      # stats of all NDVI values within each simulated pixel
      # exact_extract return a list of length nrow(site_rows), where each element is a 2 column
      # matrix of all values within the roi.
      # extracted$ndvi        = exactextractr::exact_extract(r, site_rois, fun = 'mean')
      # extracted$ndvi_sd     = exactextractr::exact_extract(r, site_rois, fun = 'stdev')
      # extracted$percent_na  = exactextractr::exact_extract(r, site_rois, fun = function(value, cov_frac){mean(is.na(value))})
      # extracted$n_subpixels = exactextractr::exact_extract(r, site_rois, fun = function(value, cov_frac){length(value)})
      
      extracted$ndvi        = purrr::map_dbl(all_extracted_values, function(x){mean(x$value, na.rm=T)})
      extracted$ndvi_sd     = purrr::map_dbl(all_extracted_values, function(x){sd(x$value, na.rm=T)})
      extracted$percent_na  = purrr::map_dbl(all_extracted_values, function(x){mean(is.na(x$value))})
      extracted$n_subpixels = purrr::map_dbl(all_extracted_values, function(x){length(x$value)})
  
      extracted$date = date
      
      # Throw in the date string from the filename for debugging
      #extracted$date_str = date_str
      
      all_extracted_data = all_extracted_data %>%
        bind_rows(extracted)
    
    }
  }
  
}

write_csv(all_extracted_data, random_roi_ndvi_file)

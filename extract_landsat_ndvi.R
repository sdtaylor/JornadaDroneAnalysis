library(tidyverse)
library(sf)
#library(raster)
#library(exactextractr)

source('analysis_config.R')

# Landsat data is L8 analysis read data downloaded from AppEEARS (https://lpdaacsvc.cr.usgs.gov/appeears/)
# ~21GB total so it is not stored in repo
raster_list = list.files('~/data/jornada_drone_imagery/landsat/', pattern = '*tif$', recursive = TRUE, full.names = TRUE)

site_boundaries = st_read('data/gis/site_boundaries.geojson')

all_extracted_data = tibble()

for(f in raster_list){
  r = raster::raster(f)
  filename_parts = str_split(basename(f), '_')[[1]]
  
  date_str = str_sub(filename_parts[4],4,10)
  date = as.Date(date_str,'%Y%j')
  
  band = filename_parts[3]
  
  for(site in jorn_sites){
    site_bounds = site_boundaries %>%
      filter(site_id == site) 
    
    band_values = exactextractr::exact_extract(r, site_bounds, fun = function(x,y){x})
    band_data = tibble(value = band_values,
                       band  = band,
                       date  = date,
                       site_id = site)
    band_data$landsat_pixel_id = paste(site,1:nrow(band_data),sep='-')
    
    all_extracted_data = all_extracted_data %>%
      bind_rows(band_data)
  }
}

cloud_free_qa = c(322, 386, 834, 898)

processed = all_extracted_data %>%
  filter(!is.na(value)) %>%
  pivot_wider(names_from = 'band', values_from = 'value') %>%
  rename_with(tolower) %>%
  filter(pixelqa %in% cloud_free_qa) %>% 
  mutate(ndvi = (srb5-srb4)/(srb5+srb4))

write_csv(processed, 'data/landsat_ndvi.csv')

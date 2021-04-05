library(sf)
library(tidyverse)

source('analysis_config.R')

all_roi_cover = tibble()

for(site in jorn_sites){
  rois = sf::read_sf('data/gis/site_rois_3_final.geojson') %>%
    filter(site_id == site)
  
  cover_raster_file = case_when(
    site=='P9'    ~ 'data/p9_predicted_cover.tif',
    site=='NORT'  ~ 'data/nort_predicted_cover.tif',
    site=='GIBPE' ~ 'data/ibp_predicted_cover.tif'
  )
  
  cover_categories = tribble(
    ~value, ~cover_class,
    1,       'soil',
    2,       'grass',
    3,       'mesquite'
  )
  
  cover_raster = raster::raster(cover_raster_file)  
  
  process_exactextract_cover = function(df, r){
    tots_pixels = nrow(df)
    df %>%
      count(value) %>%
      left_join(cover_categories, by='value') %>%
      mutate(percent_cover = n/tots_pixels,
             roi_id = r) %>%
      select(roi_id, cover_class, percent_cover)
  }
  
  cover_values = exactextractr::exact_extract(cover_raster, rois, fun=NULL)
  
  cover_values = map2_df(cover_values, rois$roi_id, process_exactextract_cover)  
  cover_values$site_id = site
  
  all_roi_cover = all_roi_cover %>%
    bind_rows(cover_values)
}

x = all_roi_cover %>% 
  mutate(percent_cover = round(percent_cover, 3)) %>%
  filter(!is.na(cover_class)) %>%
  pivot_wider(names_from=cover_class, values_from='percent_cover')

write_csv(x, roi_percent_cover_file)

library(sf)
library(tidyverse)

source('analysis_config.R')

all_roi_cover = tibble()

all_rois = sf::read_sf(random_roi_file)

for(site in jorn_sites){
  rois = all_rois %>%
    filter(site_id == site) 

  cover_raster_file = case_when(
    site=='NORT'  ~ './drone_analysis/data/nort_shrub_cover.tif',
  )
  
  cover_categories = tribble(
    ~value, ~cover_class,
    0,       'soil',
    1,       'mesquite'
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



roi_pixel_sizes = rois %>%
  sf::st_set_geometry(NULL) %>%
  select(site_id, roi_id, pixel_size)

x = all_roi_cover %>% 
  mutate(percent_cover = round(percent_cover, 3)) %>%
  filter(!is.na(cover_class)) %>%
  pivot_wider(names_from=cover_class, values_from='percent_cover', values_fill=0) %>%
  left_join(roi_pixel_sizes, by=c('roi_id','site_id'))

write_csv(x, random_roi_percent_cover_file)

library(tidyverse)
library(sf)

# ---------------------
# This script takes the primary hand placed ROIs and randomly
# places smaller ROI's within all of them.
# eg, I had place a 4m ROI over a grassy area. Here numerous ROIs
# at 4,2, and 0.5m (defined in analysis_config.R) are placed within that grassy area. 
# ---------------------

source('spatial_helper_functions.R')
source('analysis_config.R')

primary_rois = st_read('data/gis/site_rois_2_with_placement.geojson')
primary_rois$roi_id = as.character(primary_rois$roi_id)

set.seed(1)

for(i in 1:nrow(primary_rois)){
  p = primary_rois[i,]
  
  subset_rois = subset_roi_info %>%
    filter(site_id == p$site_id, plant==p$plant)
  
  for(i2 in 1:nrow(subset_rois)){
    x = polygon_sample(p, 
                       n=subset_rois$n_subcells[i2],
                       new_polygon_scale = subset_rois$scale[i2])
    
    x$scale   = subset_rois$scale[i2]
    x$site_id = p$site_id
    x$plant   = p$plant
    x$roi_id  = paste(p$roi_id, subset_rois$scale[i2] , 1:nrow(x), sep = '-')
    x$primary_roi_id = p$roi_id
    
    if(exists('new_subset_rois')){
      new_subset_rois = bind_rows(new_subset_rois, x)
    } else {
      new_subset_rois = x
    }
  }
}

primary_rois$roi_type = 'primary'
new_subset_rois$roi_type = 'subset'

final_roi = bind_rows(primary_rois, new_subset_rois)
st_write(final_roi, dsn = 'data/gis/site_rois_3_final.geojson', driver='GeoJSON')


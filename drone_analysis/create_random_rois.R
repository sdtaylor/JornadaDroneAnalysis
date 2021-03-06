library(tidyverse)
library(sf)

# ---------------------
# This script creates a large number of ROI's randomly
# placed around each site to simulate different
# levels of fractional veg cover.
# ---------------------


# ---------------------
boundaries = sf::st_read('./drone_analysis/data/gis/site_boundaries.geojson')
source('analysis_config.R')
source('spatial_helper_functions.R')

for(site in jorn_sites){
  b = boundaries %>%
    filter(site_id == site)
  
  for(i in 1:nrow(random_roi_sizes)){
    random_points = st_sample(b, size = random_roi_sizes$n_rois[i])
    
    cell_geoms = points_to_squares(random_points,
                                   scale = random_roi_sizes$pixel_size[i])
    
    cell_features = st_sf(pixel_size = random_roi_sizes$pixel_size[i], 
                          site_id = site,
                          plant = 'none',
                          geometry  = cell_geoms,
                          crs   = st_crs(boundaries),
                          precision = 1000)
    
    if(exists('all_rois')){
      all_rois = rbind(all_rois, cell_features)
    } else {
      all_rois = cell_features
    }
  }
}


# every roi gets a unique id
all_rois$roi_id = paste0('r-',1:nrow(all_rois))

st_write(all_rois, dsn = random_roi_file, driver='GeoJSON')



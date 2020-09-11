library(tidyverse)
library(sf)

#######################
# This script randomly locates the appropriate sized cells at each site.
# These are not the ROIs used in the analysis, but are used as a template
# to hand place everything on the correct plants using qgis.
#######################


####################################
boundaries = sf::st_read('data/gis/site_boundaries.geojson')
source('roi_config.R')
source('spatial_helper_functions.R')

for(site in c('P9', 'GIBPE', 'NORT')){
  b = boundaries %>%
    filter(site_id == site)

  roi_info = primary_roi_info %>%
    filter(site_id == site)
  
  for(i in 1:nrow(roi_info)){
    random_points = st_sample(b, size = roi_info$n_cells[i])
    
    cell_geoms = points_to_squares(random_points,
                                   scale = roi_info$scale[i])

    cell_features = st_sf(scale = roi_info$scale[i], 
                          site  = site,
                          plant = roi_info$plant[i],
                          geom  = cell_geoms,
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
all_rois$roi_id = 1:nrow(all_rois)

st_write(all_rois, dsn = 'data/gis/site_rois_1_starter-2.geojson', driver='GeoJSON')



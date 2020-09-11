library(tidyverse)
library(sf)

#######################
# This script randomly locates the approate sized cells at each site.
# These are not the ROIs used in the analysis, they are still
# placed by hand using qgis
#######################

make_cell_coords = function(ll_x, ll_y, scale){
  # Make x,y coordinates of a fully connected square.
  coords = rbind(c(ll_x, ll_y),
                 c(ll_x, ll_y + scale),
                 c(ll_x + scale, ll_y + scale),
                 c(ll_x+scale, ll_y),
                 c(ll_x, ll_y))
  return(coords)
}


####################################
boundaries = sf::st_read('data/gis/site_boundaries.geojson')
source('roi_config.R')

for(site in c('P9', 'GIBPE', 'NORT')){
  b = boundaries %>%
    filter(site_id == site)

  roi_info = primary_roi_info %>%
    filter(site_id == site)
  
  
  for(i in 1:nrow(roi_info)){
    random_points = st_sample(b, size = roi_info$n_cells[i])
    random_points = st_coordinates(random_points) %>% as_tibble()
    
    coord_list = list()
    for(cell_i in 1:nrow(random_points)){
      cell_coords = make_cell_coords(ll_x = random_points$X[cell_i],
                                     ll_y = random_points$Y[cell_i],
                                     scale = roi_info$scale[i])
      coord_list[[cell_i]] = cell_coords
    }

    cell_geoms = lapply(coord_list, function(x) st_polygon(list(x)))
    cell_features = st_sf(scale = roi_info$scale[i], 
                          site  = site,
                          plant = roi_info$plant[i],
                          geom  = st_sfc(cell_geoms),
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

st_write(all_rois, dsn = 'data/gis/site_rois_1_starter.geojson', driver='GeoJSON')



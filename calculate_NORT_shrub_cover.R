library(sf)
library(tidyverse)

# A quick calculation of shrub cover at NORT.
# uses the 30m polygons in site_rois.
# within those I annotated by hand shrub polygons, so this is the percent shrub within the polygons.
# uses the 10 30m plots to get some variation.

large_polygons = sf::read_sf('data/gis/site_rois_3_final.geojson') %>%
  filter(site_id == 'NORT', scale==30)

shrub_polygons = sf::read_sf('data/gis/NORT_shrubs.geojson') %>%
  st_buffer(0)

shrub_areas = c()
for(polygon_i in 1:nrow(large_polygons)){
  
  shrubs_within_polygon = st_intersection(large_polygons[polygon_i,], shrub_polygons)
  total_area = sum(as.numeric(st_area(shrubs_within_polygon)))
  
  shrub_areas = c(shrub_areas, total_area)
}

shrub_percent_cover = shrub_areas/900
print(paste0('NORT Shrub cover: ', round(mean(shrub_percent_cover),3), ' ±',round(sd(shrub_percent_cover),3),' S.D.'))

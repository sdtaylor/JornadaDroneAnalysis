library(sf)
library(tidyverse)

# A quick calculation of shrub cover at NORT.
# uses the 30m polygons in site_rois.
# within those I annotated by hand shrub polygons, so this is the percent shrub within the polygons.
# uses the 10 30m plots to get some variation.

rois = sf::read_sf('data/gis/site_rois_3_final.geojson') %>%
  filter(site_id == 'NORT')

shrub_polygons = sf::read_sf('data/gis/NORT_shrubs.geojson') %>%
  st_buffer(0)

shrub_areas = tibble(roi_id=character(), total_area=numeric(), shrub_area=numeric())

for(this_roi_id in rois$roi_id){
  focal_roi = rois %>% 
    filter(roi_id == this_roi_id)
  
  shrubs_within_polygon = st_intersection(focal_roi, shrub_polygons)
  shrub_area = sum(as.numeric(st_area(shrubs_within_polygon)))
  roi_area   = sum(as.numeric(st_area(focal_roi)))
  
  shrub_areas = shrub_areas %>%
    add_row(roi_id = this_roi_id, total_area = roi_area, shrub_area = shrub_area)

}

shrub_areas$shrub_percent_cover = with(shrub_areas, shrub_area/total_area)

write_csv(shrub_areas, 'data/NORT_roi_shrub_cover.csv')
